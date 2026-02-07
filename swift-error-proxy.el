;;; swift-error-proxy.el --- Unified error parsing proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, xcode, development
;; URL: https://github.com/konrad1977/swift-development

;;; Commentary:

;; Unified error parsing and display proxy for Swift development tools.
;; Mirrors the architecture of swift-notification.el: a single dispatch
;; function with configurable backends, so callers never need to check
;; `fboundp' or worry about which error display system is available.
;;
;; Supported backends:
;; - `periphery': Rich clickable diagnostics in *Periphery* buffer
;; - `compilation': Standard Emacs compilation-mode buffer
;; - `minimal': Lightweight error/warning counting in mode-line only
;;
;; Also provides:
;; - `swift-error-proxy-has-errors-p': Single source of truth for error detection
;; - `swift-error-proxy-parse-test-output': Test output parsing
;; - Buffer lifecycle management (clear, kill, toggle)

;;; Code:

(require 'cl-lib)

;; Forward declarations
(declare-function periphery-run-parser "periphery")
(declare-function periphery-run-test-parser "periphery")
(declare-function periphery-kill-buffer "periphery")
(declare-function periphery-clear "periphery")
(declare-function periphery-toggle-buffer "periphery")
(declare-function swift-notification-send "swift-notification")

;; Availability detection (at load time)
(defvar swift-error-proxy--periphery-available-p
  (and (require 'periphery nil t)
       (fboundp 'periphery-run-parser))
  "Whether the periphery package is available for error parsing.")

(defgroup swift-error-proxy nil
  "Unified error parsing settings for Swift development tools."
  :group 'swift-development
  :prefix "swift-error-proxy-")

(defcustom swift-error-proxy-backend 'periphery
  "Backend to use for displaying build errors and diagnostics.
Options:
- \\='periphery: Rich clickable diagnostics in *Periphery* buffer
- \\='compilation: Standard Emacs compilation-mode in *Swift Build* buffer
- \\='minimal: Lightweight error/warning counts in mode-line only"
  :type '(choice (const :tag "Periphery (rich UI)" periphery)
                 (const :tag "Compilation mode" compilation)
                 (const :tag "Minimal (counts only)" minimal))
  :group 'swift-error-proxy)

(defcustom swift-error-proxy-large-output-threshold 100000
  "Output size in bytes above which intelligent truncation is applied.
Only relevant for the periphery backend."
  :type 'integer
  :group 'swift-error-proxy)

;;; ============================================================================
;;; Consolidated Error Patterns
;;; ============================================================================

(defconst swift-error-proxy-error-patterns
  '(":[0-9]+:[0-9]+: error:"
    "^xcodebuild: error:"
    "\\*\\* BUILD FAILED \\*\\*"
    "The following build commands failed:"
    "Could not resolve package dependencies"
    "SDK .* cannot be located"
    "error:.*platform.*not found"
    "error:.*provisioning profile"
    "error:.*[Cc]ode [Ss]ign"
    "Compiling for iOS [0-9.]+, but module"
    "^ld: "
    "Undefined symbol"
    "No such module"
    "^Command failed with exit code"
    "\\.compile\\.lock.*locked so long")
  "Comprehensive list of regex patterns indicating build errors.
This is the single source of truth for error detection across the package.")

(defconst swift-error-proxy-warning-pattern "\\bwarning:"
  "Regex pattern to detect warning keywords in build output.")

(defconst swift-error-proxy-error-keyword-pattern "\\berror:"
  "Regex pattern to detect error keywords in build output.")

;;; ============================================================================
;;; Error Detection
;;; ============================================================================

(defun swift-error-proxy-has-errors-p (output)
  "Return non-nil if OUTPUT contains build errors.
Uses the consolidated `swift-error-proxy-error-patterns' list.
This is the single source of truth for error detection across the package."
  (and output
       (stringp output)
       (cl-some (lambda (pattern)
                  (string-match-p pattern output))
                swift-error-proxy-error-patterns)))

(defun swift-error-proxy-has-warnings-p (output)
  "Return non-nil if OUTPUT contains build warnings."
  (and output
       (stringp output)
       (string-match-p swift-error-proxy-warning-pattern output)))

(defun swift-error-proxy-count-issues (output)
  "Count errors and warnings in OUTPUT.
Returns a cons cell (ERRORS . WARNINGS)."
  (let ((error-count 0)
        (warning-count 0))
    (when (and output (stringp output))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward swift-error-proxy-error-keyword-pattern nil t)
          (cl-incf error-count))
        (goto-char (point-min))
        (while (re-search-forward swift-error-proxy-warning-pattern nil t)
          (cl-incf warning-count))))
    (cons error-count warning-count)))

;;; ============================================================================
;;; Output Parsing (Main Dispatch)
;;; ============================================================================

(defun swift-error-proxy-parse-output (output &optional async context)
  "Parse build OUTPUT and display errors via the configured backend.
If ASYNC is non-nil, use idle timer to avoid blocking (periphery backend only).
CONTEXT is an optional keyword specifying the source of the output:
  :analyze - Static analyzer output (warnings displayed as Analyzer badges)
  :build   - Normal build output (default)
  :lint    - Linter output
Dispatches to the backend configured in `swift-error-proxy-backend'."
  (when (and output (stringp output) (> (length output) 0))
    (pcase swift-error-proxy-backend
      ('periphery
       (if swift-error-proxy--periphery-available-p
           (let ((parser-config (pcase context
                                  (:analyze '(:analyzer))
                                  (:lint '(:linter))
                                  (_ nil))))
             (if async
                 (swift-error-proxy--parse-periphery-async output parser-config)
               (condition-case err
                   (if parser-config
                       (apply #'periphery-run-parser output parser-config)
                     (periphery-run-parser output))
                 (error
                  (message "Periphery parse error: %s" (error-message-string err))
                  (swift-error-proxy--show-compilation-buffer output)))))
         ;; Periphery not available, fall back to compilation
         (swift-error-proxy--show-compilation-buffer output)))

      ('compilation
       (swift-error-proxy--show-compilation-buffer output))

      ('minimal
       (swift-error-proxy--show-minimal-analysis output))

      (_
       (swift-error-proxy--show-compilation-buffer output)))))

(defun swift-error-proxy-parse-test-output (output &optional callback)
  "Parse test OUTPUT and display results.
Call CALLBACK (if provided) when all tests pass.
Uses periphery's test parser when available, otherwise
falls back to compilation-mode display."
  (when (and output (stringp output) (> (length output) 0))
    (if (and swift-error-proxy--periphery-available-p
             (eq swift-error-proxy-backend 'periphery)
             (fboundp 'periphery-run-test-parser))
        (condition-case err
            (periphery-run-test-parser output callback)
          (error
           (message "Test parse error: %s" (error-message-string err))
           (swift-error-proxy--show-compilation-buffer output)))
      (swift-error-proxy--show-compilation-buffer output))))

;;; ============================================================================
;;; Backend Implementations
;;; ============================================================================

(defun swift-error-proxy--parse-periphery-async (output &optional parser-config)
  "Parse OUTPUT through periphery asynchronously to avoid blocking UI.
PARSER-CONFIG is an optional list of config args for `periphery-run-parser'.
Applies intelligent truncation for large outputs."
  (let ((text (if (> (length output) swift-error-proxy-large-output-threshold)
                  (swift-error-proxy--truncate-output output)
                output)))
    (run-with-idle-timer
     0.1 nil
     (lambda (text-arg config-arg)
       (condition-case err
           (if config-arg
               (apply #'periphery-run-parser text-arg config-arg)
             (periphery-run-parser text-arg))
         (error
          (message "Periphery async parse error: %s"
                   (error-message-string err)))))
     text parser-config)))

(defun swift-error-proxy--show-compilation-buffer (output)
  "Display build OUTPUT in a compilation-mode buffer."
  (let ((buf (get-buffer-create "*Swift Build*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (compilation-mode)
          (goto-char (point-min))))
      (display-buffer buf))))

(defun swift-error-proxy--show-minimal-analysis (output)
  "Run minimal error analysis on OUTPUT.
Only counts errors/warnings and reports via notification."
  (let* ((counts (swift-error-proxy-count-issues output))
         (error-count (car counts))
         (warning-count (cdr counts)))
    (when (or (> error-count 0) (> warning-count 0))
      (if (fboundp 'swift-notification-send)
          (swift-notification-send
           :message (format "%d error(s), %d warning(s)"
                            error-count warning-count)
           :seconds 3)
        (message "%d error(s), %d warning(s)" error-count warning-count)))))

;;; ============================================================================
;;; Output Truncation
;;; ============================================================================

(defun swift-error-proxy--truncate-output (output)
  "Truncate large OUTPUT intelligently, keeping error-relevant portions.
Keeps the last ~500 lines plus any error/warning lines from earlier."
  (let* ((output-len (length output))
         (cut-pos (let ((pos output-len)
                        (newline-count 0))
                    (while (and (> pos 0) (< newline-count 500))
                      (setq pos (1- pos))
                      (when (= (aref output pos) ?\n)
                        (setq newline-count (1+ newline-count))))
                    (1+ pos)))
         (error-lines '()))
    (if (<= cut-pos 0)
        output
      ;; Scan the truncated prefix for error/warning lines
      (with-temp-buffer
        (insert (substring output 0 cut-pos))
        (goto-char (point-min))
        (while (re-search-forward
                "^.*\\(?:error\\|warning\\|failed\\):.*$" nil t)
          (push (match-string 0) error-lines)))
      ;; Combine error lines + truncation marker + tail
      (concat
       (when error-lines
         (concat (string-join (nreverse error-lines) "\n")
                 "\n"))
       "... [truncated for performance] ...\n"
       (substring output cut-pos)))))

;;; ============================================================================
;;; Buffer Lifecycle
;;; ============================================================================

(defun swift-error-proxy-clear ()
  "Clear error display regardless of backend."
  (pcase swift-error-proxy-backend
    ('periphery
     (when (and swift-error-proxy--periphery-available-p
                (fboundp 'periphery-clear))
       (periphery-clear)))
    ('compilation
     (when-let* ((buf (get-buffer "*Swift Build*")))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer))))))
    (_ nil)))

(defun swift-error-proxy-kill-buffer ()
  "Kill error buffer regardless of backend."
  (pcase swift-error-proxy-backend
    ('periphery
     (when (and swift-error-proxy--periphery-available-p
                (fboundp 'periphery-kill-buffer))
       (periphery-kill-buffer)))
    ('compilation
     (when-let* ((buf (get-buffer "*Swift Build*")))
       (when (buffer-live-p buf)
         (kill-buffer buf))))
    (_ nil)))

(defun swift-error-proxy-toggle-buffer ()
  "Toggle error buffer visibility."
  (interactive)
  (pcase swift-error-proxy-backend
    ('periphery
     (if (and swift-error-proxy--periphery-available-p
              (fboundp 'periphery-toggle-buffer))
         (periphery-toggle-buffer)
       (message "Periphery package is not installed")))
    ('compilation
     (if-let* ((buf (get-buffer "*Swift Build*")))
         (if (get-buffer-window buf)
             (delete-window (get-buffer-window buf))
           (display-buffer buf))
       (message "No Swift Build buffer")))
    (_
     (message "No error buffer for minimal backend"))))

;;; ============================================================================
;;; Compatibility
;;; ============================================================================

(defun swift-error-proxy-effective-backend ()
  "Return the effective backend being used.
If periphery is configured but not available, returns \\='compilation."
  (if (and (eq swift-error-proxy-backend 'periphery)
           (not swift-error-proxy--periphery-available-p))
      'compilation
    swift-error-proxy-backend))

(provide 'swift-error-proxy)
;;; swift-error-proxy.el ends here
