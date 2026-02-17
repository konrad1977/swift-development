;;; swiftui-preview-dynamic.el --- Dynamic target injection preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: swift, swiftui, preview, ios

;;; Commentary:

;; This module provides zero-config SwiftUI preview functionality using
;; dynamic target injection.  It extracts #Preview blocks from Swift files
;; and creates a temporary PreviewHost target in the Xcode project to build
;; and capture previews.
;;
;; Features:
;; - Zero configuration required
;; - Works with existing #Preview macros
;; - Supports Xcode projects, workspaces, and SPM packages
;; - Fast builds (only builds required modules)
;; - External screenshot capture via simctl
;;
;; Usage:
;; M-x swiftui-preview-dynamic-generate

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'swiftui-preview-core)
(require 'swift-async nil t)
(require 'swift-notification nil t)
(require 'swift-project-settings nil t)
(require 'swiftui-preview-spm nil t)
(require 'swiftui-preview-standalone nil t)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--detect-preview-definitions "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")
(declare-function swiftui-preview-setup-check "swiftui-preview-setup")
(declare-function swiftui-preview-setup-wizard "swiftui-preview-setup")
(declare-function swiftui-preview-spm-generate "swiftui-preview-spm")
(declare-function swiftui-preview-standalone-generate "swiftui-preview-standalone")
(declare-function swift-async-run "swift-async")
(declare-function swift-async-run-sync "swift-async")
(declare-function ios-simulator-simulator-identifier "ios-simulator")
(declare-function swift-development-compile-app "swift-development")
(declare-function swift-project-settings-get-product-name "swift-project-settings")
(declare-function swift-project-settings-get-deployment-target "swift-project-settings")
(declare-function swift-project-settings-get-swift-module-name "swift-project-settings")
(declare-function swift-project-settings-get-build-dir "swift-project-settings")
(declare-function swift-notification-send "swift-notification")
(declare-function swift-notification-progress-start "swift-notification")
(declare-function swift-notification-progress-update "swift-notification")
(declare-function swift-notification-progress-finish "swift-notification")
(declare-function swift-notification-progress-cancel "swift-notification")
(declare-function xcode-project-project-root "xcode-project")
(declare-function swift-error-proxy-parse-output "swift-error-proxy")

;; External variables
(defvar xcode-build-config-enable-testability)
(defvar xcode-build-config-xcode-cache-dir)
(defvar xcode-project--current-xcode-scheme)
(defvar swiftui-preview-show-notifications)
(defvar swift-error-proxy-backend)

;;; Notification helpers (respects swiftui-preview-show-notifications)

(defun swiftui-preview-dynamic--notify-start (&rest args)
  "Start progress notification if notifications are enabled.
ARGS are passed to `swift-notification-progress-start'."
  (when (and (bound-and-true-p swiftui-preview-show-notifications)
             (fboundp 'swift-notification-progress-start))
    (apply #'swift-notification-progress-start args)))

(defun swiftui-preview-dynamic--notify-update (id &rest args)
  "Update progress notification ID if notifications are enabled.
ARGS are passed to `swift-notification-progress-update'."
  (when (and (bound-and-true-p swiftui-preview-show-notifications)
             (fboundp 'swift-notification-progress-update))
    (apply #'swift-notification-progress-update id args)))

(defun swiftui-preview-dynamic--notify-finish (id &optional message)
  "Finish progress notification ID if notifications are enabled.
MESSAGE is the completion message."
  (when (and (bound-and-true-p swiftui-preview-show-notifications)
             (fboundp 'swift-notification-progress-finish))
    (swift-notification-progress-finish id message)))

(defun swiftui-preview-dynamic--notify-cancel (id)
  "Cancel progress notification ID if notifications are enabled."
  (when (and (bound-and-true-p swiftui-preview-show-notifications)
             (fboundp 'swift-notification-progress-cancel))
    (swift-notification-progress-cancel id)))

(defvar swiftui-preview-dynamic--current-source-file nil)  ; forward decl

(defun swiftui-preview-dynamic--parse-build-errors (output)
  "Parse build OUTPUT through the error proxy.
Rewrites references to the generated PreviewHostApp.swift back to
the real source file so that periphery navigation works correctly."
  (when (fboundp 'swift-error-proxy-parse-output)
    (let ((rewritten
           (if (and output swiftui-preview-dynamic--current-source-file)
               (replace-regexp-in-string
                "/[^ \n:]*PreviewHostApp\\.swift"
                swiftui-preview-dynamic--current-source-file
                output t t)
             output)))
      (swift-error-proxy-parse-output rewritten))))

(defgroup swiftui-preview-dynamic nil
  "Dynamic target injection preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-dynamic-")

(defcustom swiftui-preview-dynamic-keep-target nil
  "If non-nil, keep the injected PreviewHost target after capture.
Useful for debugging build issues."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-verbose nil
  "If non-nil, show verbose output during preview generation."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-live-mode nil
  "Preview mode toggle.
When nil (default), snapshot mode: render one frame, save PNG,
show in Emacs buffer.  When non-nil, live mode: the preview app
stays running in the simulator for interactive use.  Screenshots
can be taken on-demand with `swiftui-preview-dynamic-refresh-live'."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

;;;###autoload
(defun swiftui-preview-dynamic-toggle-verbose ()
  "Toggle verbose mode for SwiftUI preview generation."
  (interactive)
  (setq swiftui-preview-dynamic-verbose (not swiftui-preview-dynamic-verbose))
  (message "SwiftUI Preview verbose mode: %s"
           (if swiftui-preview-dynamic-verbose "ON" "OFF")))

(defcustom swiftui-preview-save-location 'project
  "Where to save preview images.
\\=`project\\=' saves to .swift-development/previews/ in project root.
\\=`temp\\=' uses system temp directory (lost on restart).
\\=`custom\\=' uses `swiftui-preview-custom-save-directory'."
  :type '(choice (const :tag "Project directory" project)
                 (const :tag "Temporary" temp)
                 (const :tag "Custom directory" custom))
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-custom-save-directory nil
  "Custom directory for saving previews.
Used when `swiftui-preview-save-location' is \\=`custom\\='."
  :type '(choice (const nil) directory)
  :group 'swiftui-preview-dynamic)

;; Scripts dir is shared via swiftui-preview-core--scripts-dir

(defvar swiftui-preview-dynamic--temp-dir nil
  "Temporary directory for current preview generation.")

(defvar swiftui-preview-dynamic--current-project nil
  "Current project path being previewed.")

(defvar swiftui-preview-dynamic--current-source-file nil
  "Source file path currently being previewed.")

;; SDK/toolchain paths delegated to swiftui-preview-core:
;; (swiftui-preview-core-sdk-path) and (swiftui-preview-core-toolchain-path)

(defvar swiftui-preview-dynamic--pending-preview-buffer nil
  "Buffer to generate preview for after testability rebuild.")

(defvar swiftui-preview-dynamic--recompile-cache nil
  "Cache for recompiled object files during generate-all.
When non-nil, a plist with :source-file, :fresh-o, :object-files.
Subsequent previews from the same source file skip recompilation
and reuse the cached .o file.  Cleared when generate-all finishes.")

;;; Live mode state

(defvar swiftui-preview-dynamic--live-simulator-udid nil
  "UDID of simulator running the live preview app.")

(defvar swiftui-preview-dynamic--live-app-path nil
  "Path to the live preview .app bundle (for cleanup).")

(defvar swiftui-preview-dynamic--live-refresh-timer nil
  "Timer for periodic screenshot refresh in live mode.")

(defvar swiftui-preview-dynamic--live-bundle-id
  "com.swift-development.preview-host"
  "Bundle ID of the live preview app.")

;;; Project Type Detection



(defun swiftui-preview-dynamic--detect-project-type ()
  "Detect the project type for current file.
Returns a plist with :type and :path keys.
Type is one of: xcode-project, xcode-workspace, spm-package, standalone."
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory))
        (result nil))
    ;; Search upward
    (while (and dir (not result) (not (equal dir "/")))
      ;; Check for workspace first (but not inside .xcodeproj)
      (let ((workspaces (directory-files dir t "\\.xcworkspace$")))
        ;; Filter out workspaces inside .xcodeproj
        (setq workspaces (cl-remove-if
                          (lambda (ws)
                            (string-match-p "\\.xcodeproj/" ws))
                          workspaces))
        (when workspaces
          (setq result (list :type 'xcode-workspace
                            :path (car workspaces)
                            :root dir))))
      
      ;; Check for project
      (unless result
        (let ((projects (directory-files dir t "\\.xcodeproj$")))
          (when projects
            (setq result (list :type 'xcode-project
                              :path (car projects)
                              :root dir)))))
      
      ;; Check for Package.swift - but continue searching for workspace/project above
      (unless result
        (let ((package (expand-file-name "Package.swift" dir)))
          (when (file-exists-p package)
            ;; Found SPM package, but check if there's a workspace above first
            (let ((parent dir)
                  (found-workspace nil))
              (while (and parent (not found-workspace)
                          (not (string= parent "/")))
                (setq parent (file-name-directory (directory-file-name parent)))
                (let ((workspaces (directory-files parent t "\\.xcworkspace$")))
                  (setq workspaces (cl-remove-if
                                    (lambda (ws)
                                      (string-match-p "\\.xcodeproj/" ws))
                                    workspaces))
                  (when workspaces
                    (setq found-workspace (car workspaces)))))
              (if found-workspace
                  ;; Use workspace instead of SPM package
                  (setq result (list :type 'xcode-workspace
                                    :path found-workspace
                                    :root (file-name-directory found-workspace)))
                ;; No workspace above, use SPM package
                (setq result (list :type 'spm-package
                                  :path package
                                  :root dir)))))))
      
      (setq dir (file-name-directory (directory-file-name dir))))
    
    ;; If nothing found, it's standalone
    (or result (list :type 'standalone :path nil :root nil))))

(defun swiftui-preview-dynamic--detect-module ()
  "Detect module name from current file path.
Returns module name string or nil.
First tries Ruby script (detects sub-module from file path), then
falls back to swift-project-settings (returns main app target)."
  ;; Try Ruby script first - it detects the correct sub-module (e.g., KYC not Bruce)
  (or (when-let* ((file (buffer-file-name))
                  (script (expand-file-name "preview-helper.rb" swiftui-preview-core--scripts-dir)))
        (let ((output (shell-command-to-string
                       (format "ruby %s detect-module %s 2>/dev/null"
                               (shell-quote-argument script)
                               (shell-quote-argument file)))))
          (let ((module (string-trim output)))
            (unless (string-empty-p module)
              module))))
      ;; Fall back to swift-project-settings (main app target name)
      (when (fboundp 'swift-project-settings-get-product-name)
        (let* ((project-root (or (when (fboundp 'xcode-project-project-root)
                                   (xcode-project-project-root))
                                 (locate-dominating-file default-directory ".xcodeproj")
                                 (locate-dominating-file default-directory ".xcworkspace")))
               (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                         xcode-project--current-xcode-scheme)))
          (when (and project-root scheme)
            (swift-project-settings-get-product-name project-root scheme))))))

(defun swiftui-preview-dynamic--detect-module-async (callback)
  "Detect module name asynchronously, call CALLBACK with result or nil.
First tries Ruby script, falls back to swift-project-settings (sync but fast)."
  (let* ((file (buffer-file-name))
         (script (when file
                   (expand-file-name "preview-helper.rb"
                                     swiftui-preview-core--scripts-dir))))
    (if (and file script (file-exists-p script) (fboundp 'swift-async-run))
        (swift-async-run
         (list "ruby" script "detect-module" file)
         (lambda (output)
           (let ((module (when output (string-trim output))))
             (if (and module (not (string-empty-p module)))
                 (funcall callback module)
               ;; Fall back to swift-project-settings (fast, no process needed)
               (funcall callback
                        (when (fboundp 'swift-project-settings-get-product-name)
                          (let* ((project-root
                                  (or (when (fboundp 'xcode-project-project-root)
                                        (xcode-project-project-root))
                                      (locate-dominating-file default-directory ".xcodeproj")
                                      (locate-dominating-file default-directory ".xcworkspace")))
                                 (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                                           xcode-project--current-xcode-scheme)))
                            (when (and project-root scheme)
                              (swift-project-settings-get-product-name project-root scheme))))))))
         :timeout 10
         :error-callback
         (lambda (_err)
           ;; On error, try the sync fallback
           (funcall callback (swiftui-preview-dynamic--detect-module)))
         :process-key "preview-detect-module")
      ;; No async available or no file - use sync fallback
      (funcall callback (swiftui-preview-dynamic--detect-module)))))

(defalias 'swiftui-preview-dynamic--extract-imports
  #'swiftui-preview-core-extract-imports
  "Extract imports from current buffer.  Delegates to core.")

;;; Preview Host Generation
;; Color scheme detection delegated to core:
;; (swiftui-preview-core-detect-color-scheme BODY)
;; (swiftui-preview-core-detect-color-scheme-from-buffer)

(defun swiftui-preview-dynamic--generate-preview-host
    (preview-body imports temp-dir
     &optional testable-module color-scheme output-path)
  "Generate PreviewHostApp.swift in TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
TESTABLE-MODULE is the module to import with @testable.
COLOR-SCHEME is \"dark\", \"light\", or nil.
OUTPUT-PATH is the absolute path where the snapshot PNG will be saved.
Delegates Swift code generation to `swiftui-preview-core-write-host-app'."
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (color-scheme (or color-scheme
                           (swiftui-preview-core-detect-color-scheme
                            preview-body)))
         (use-testable
          (and testable-module
               (bound-and-true-p xcode-build-config-enable-testability))))

    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Color scheme: %s" (or color-scheme "none"))
      (message "[Preview] Output path embedded: %s" (or output-path "default"))
      (message "[Preview] Preview body (first 200 chars): %s"
               (substring preview-body 0
                          (min 200 (length preview-body)))))

    (swiftui-preview-core-write-host-app
     preview-body imports temp-dir filename
     color-scheme testable-module use-testable
     swiftui-preview-dynamic-live-mode output-path)))

;;; Ruby Script Interaction

(defalias 'swiftui-preview-dynamic--run-ruby-script
  #'swiftui-preview-core-run-ruby-script
  "Run Ruby script synchronously.  Delegates to core.")

(defun swiftui-preview-dynamic--inject-target (project-path preview-dir module-name imports &optional source-file)
  "Inject PreviewHost target into PROJECT-PATH.
PREVIEW-DIR contains the generated preview host files.
MODULE-NAME is the module to depend on.
IMPORTS is list of modules to import.
SOURCE-FILE is the original Swift file being previewed.
Returns the injection result including source_target if found."
  (let ((args (list "--project" project-path
                    "--preview-dir" preview-dir)))
    (when module-name
      (setq args (append args (list "--module" module-name))))
    (when imports
      (setq args (append args (list "--imports" (mapconcat #'identity imports ",")))))
    (when source-file
      (setq args (append args (list "--source-file" source-file))))
    (when swiftui-preview-dynamic-verbose
      (setq args (append args (list "--verbose"))))
    
    (apply #'swiftui-preview-dynamic--run-ruby-script "preview-inject-target.rb" args)))

(defalias 'swiftui-preview-dynamic--run-ruby-script-async
  #'swiftui-preview-core-run-ruby-script-async
  "Run Ruby script asynchronously.  Delegates to core.")

(defun swiftui-preview-dynamic--inject-target-async (project-path preview-dir module-name imports callback &optional source-file)
  "Inject PreviewHost target into PROJECT-PATH asynchronously.
PREVIEW-DIR contains the generated preview host files.
MODULE-NAME is the module to depend on.
IMPORTS is list of modules to import.
CALLBACK is called with injection result on success.
SOURCE-FILE is the original Swift file being previewed."
  (let ((args (list "--project" project-path
                    "--preview-dir" preview-dir)))
    (when module-name
      (setq args (append args (list "--module" module-name))))
    (when imports
      (setq args (append args (list "--imports" (mapconcat #'identity imports ",")))))
    (when source-file
      (setq args (append args (list "--source-file" source-file))))
    (when swiftui-preview-dynamic-verbose
      (setq args (append args (list "--verbose"))))
    (apply #'swiftui-preview-dynamic--run-ruby-script-async
           "preview-inject-target.rb" callback args)))

(defun swiftui-preview-dynamic--cleanup-target (project-path)
  "Remove PreviewHost target from PROJECT-PATH."
  (condition-case err
      (swiftui-preview-dynamic--run-ruby-script
       "preview-cleanup-target.rb"
       "--project" project-path)
    (error
     (when swiftui-preview-dynamic-verbose
       (message "[Preview] Cleanup warning: %s" (error-message-string err))))))

;;; Simulator Interaction

(defalias 'swiftui-preview-dynamic--find-simulator
  #'swiftui-preview-core-find-simulator
  "Find simulator UDID.  Delegates to core -> ios-simulator.")

(defalias 'swiftui-preview-dynamic--find-simulator-async
  #'swiftui-preview-core-find-simulator-async
  "Find simulator UDID async.  Delegates to core -> ios-simulator.")

(defalias 'swiftui-preview-dynamic--ensure-simulator-booted
  #'swiftui-preview-core-ensure-simulator-booted
  "Ensure simulator is booted.  Delegates to core -> ios-simulator.")

;; --check-simulator-booted-p removed; no longer needed since
;; ensure-simulator-booted delegates to ios-simulator.

(defalias 'swiftui-preview-dynamic--ensure-simulator-booted-async
  #'swiftui-preview-core-ensure-simulator-booted-async
  "Ensure simulator is booted async.  Delegates to core -> ios-simulator.")

;;; Build and Capture

(defun swiftui-preview-dynamic--get-output-path (project-root filename &optional preview-id)
  "Get output path for preview image.
PROJECT-ROOT is the project directory.
FILENAME is the base name for the preview (e.g., \"HomeView.swift\").
PREVIEW-ID, when non-nil, is appended to create a unique filename
\(e.g., \"Light\" produces \"HomeView-Light.png\")."
  (let ((preview-name (concat (file-name-sans-extension filename)
                               (when preview-id
                                 (concat "-" (replace-regexp-in-string
                                              "[[:space:]]+" "-" preview-id))))))
    (pcase swiftui-preview-save-location
      ('project
       (let ((preview-dir (expand-file-name ".swift-development/previews" project-root)))
         (make-directory preview-dir t)
         (expand-file-name (format "%s.png" preview-name) preview-dir)))
      ('temp
       (expand-file-name (format "preview-%s.png" preview-name) temporary-file-directory))
      ('custom
       (if swiftui-preview-custom-save-directory
           (progn
             (make-directory swiftui-preview-custom-save-directory t)
             (expand-file-name (format "%s.png" preview-name) swiftui-preview-custom-save-directory))
         (expand-file-name (format "preview-%s.png" preview-name) temporary-file-directory))))))

(defvar swiftui-preview-dynamic--build-callback nil
  "Callback function for async build completion.")

(defvar swiftui-preview-dynamic--build-context nil
  "Context for async build (simulator-udid, output-path, etc).")


(defun swiftui-preview-dynamic--project-root-and-scheme ()
  "Get current project root and scheme as a cons cell (ROOT . SCHEME).
Returns nil if not available."
  (let ((root (when (fboundp 'xcode-project-project-root)
                (xcode-project-project-root)))
        (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                  xcode-project--current-xcode-scheme)))
    (when (and root scheme)
      (cons root scheme))))

(defun swiftui-preview-dynamic--find-derived-data (project-path)
  "Find existing DerivedData directory for PROJECT-PATH.
Returns path to DerivedData or nil if not found.
Tries cached build-dir from settings first (deriving DerivedData root),
then checks project-local .build, then ~/Library/Developer/Xcode/DerivedData."
  ;; Try settings first - derive DerivedData root from cached target-build-dir
  (or
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (build-dir (when (fboundp 'swift-project-settings-get-build-dir)
                            (swift-project-settings-get-build-dir
                             (car info) (cdr info) "iphonesimulator"))))
     ;; build-dir is like .../Build/Products/Debug-iphonesimulator/
     ;; DerivedData root is 3 levels up
     (let ((derived-root (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (directory-file-name build-dir))))))))
       (when (and derived-root (file-directory-p derived-root))
         (when swiftui-preview-dynamic-verbose
           (message "[Preview] DerivedData from settings: %s" derived-root))
         derived-root)))
   ;; Fall back to filesystem scanning
   (let* ((project-dir (file-name-directory project-path))
          ;; Check for project-local .build (used by swift-development/Emacs builds)
          (local-build-dir (expand-file-name ".build" project-dir)))
     (if (file-directory-p local-build-dir)
         (progn
           (when swiftui-preview-dynamic-verbose
             (message "[Preview] Found local .build directory"))
           local-build-dir)
       ;; Fall back to Xcode DerivedData
       (let* ((project-name (file-name-sans-extension
                             (file-name-nondirectory project-path)))
              (xcode-derived-data
               (expand-file-name
                (if (boundp 'xcode-build-config-xcode-cache-dir)
                    xcode-build-config-xcode-cache-dir
                  "~/Library/Developer/Xcode/DerivedData")))
              (matching-dirs nil))
         (when (file-directory-p xcode-derived-data)
           (setq matching-dirs
                 (directory-files xcode-derived-data t
                                 (concat "^" (regexp-quote project-name) "-"))))
         (when matching-dirs
           (car (sort matching-dirs
                      (lambda (a b)
                        (time-less-p (nth 5 (file-attributes b))
                                    (nth 5 (file-attributes a))))))))))))

(defun swiftui-preview-dynamic--find-products-dir (derived-data)
  "Find the Build/Products directory with built modules in DERIVED-DATA.
Returns the path to the iphonesimulator products directory.
Tries cached build-dir from settings first, falls back to scanning."
  (or
   ;; Try settings first (returns the exact products dir)
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (build-dir (when (fboundp 'swift-project-settings-get-build-dir)
                            (swift-project-settings-get-build-dir
                             (car info) (cdr info) "iphonesimulator"))))
     (when (file-directory-p build-dir)
       (when swiftui-preview-dynamic-verbose
         (message "[Preview] Products dir from settings: %s" build-dir))
       build-dir))
   ;; Fall back to scanning
   (let ((products-base (expand-file-name "Build/Products" derived-data)))
     (when (file-directory-p products-base)
       ;; Look for any *-iphonesimulator directory, preferring Debug
       (let ((dirs (directory-files products-base t "-iphonesimulator$")))
         (or
          ;; Prefer Debug-iphonesimulator
          (cl-find-if (lambda (d) (string-match-p "Debug-iphonesimulator$" d)) dirs)
          ;; Or Debug (something)-iphonesimulator
          (cl-find-if (lambda (d) (string-match-p "Debug.*-iphonesimulator$" d)) dirs)
          ;; Or just the first one
          (car dirs)))))))

(defun swiftui-preview-dynamic--detect-ios-version (products-dir)
  "Detect iOS deployment target from saved settings or built modules.
Tries swift-project-settings first, falls back to scanning swiftmodule binaries.
Returns version string like \"26.0\" or \"17.0\" as fallback."
  (or
   ;; Try settings first (fast, reliable)
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (version (when (fboundp 'swift-project-settings-get-deployment-target)
                          (swift-project-settings-get-deployment-target (car info) (cdr info)))))
     (when swiftui-preview-dynamic-verbose
       (message "[Preview] iOS version from settings: %s" version))
     version)
   ;; Fall back to scanning swiftmodule binary
   (when products-dir
     (let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
            (product-name (when (and info (fboundp 'swift-project-settings-get-product-name))
                            (swift-project-settings-get-product-name (car info) (cdr info))))
            (swiftmodule-dir (or (when product-name
                                   (let ((path (expand-file-name (concat product-name ".swiftmodule") products-dir)))
                                     (when (file-directory-p path) path)))
                                 (car (directory-files products-dir t "\\.swiftmodule$")))))
       (when swiftmodule-dir
         (let ((module-file (expand-file-name "arm64-apple-ios-simulator.swiftmodule" swiftmodule-dir)))
           (when (file-exists-p module-file)
             (let ((output (shell-command-to-string
                            (format "strings %s 2>/dev/null | grep -oE 'ios[0-9]+\\.[0-9]+' | head -1"
                                    (shell-quote-argument module-file)))))
               (when (string-match "ios\\([0-9]+\\.[0-9]+\\)" output)
                 (match-string 1 output))))))))
   ;; Final fallback
   "17.0"))


(defun swiftui-preview-dynamic--find-module-sources-dir (source-file)
  "Find the root source directory for the module containing SOURCE-FILE.
For SPM packages, this is the Sources/<Module>/ directory.
Returns the directory path or nil."
  (let ((dir (file-name-directory source-file)))
    (catch 'found
      ;; Walk up from source file looking for Sources/<Module>/ pattern
      (while (and dir (not (string= dir "/")))
        (let ((parent (file-name-directory (directory-file-name dir))))
          (when (and parent
                     (string= (file-name-nondirectory
                               (directory-file-name parent))
                              "Sources"))
            (throw 'found dir))
          (setq dir parent)))
      nil)))

(defun swiftui-preview-dynamic--find-objc-include-dirs (derived-data)
  "Find ObjC include directories from SourcePackages in DERIVED-DATA.
Returns a list of directory paths for mixed-language module dependencies."
  (let ((checkouts (expand-file-name "SourcePackages/checkouts" derived-data))
        (dirs '()))
    (when (file-directory-p checkouts)
      (dolist (inc-dir (directory-files-recursively checkouts "^include$" t))
        (when (file-directory-p inc-dir)
          (push inc-dir dirs))))
    (nreverse dirs)))

(defun swiftui-preview-dynamic--recompile-stale-sources (object-files source-file products-dir derived-data ios-version)
  "Recompile the module containing SOURCE-FILE with whole-module optimization.
Always recompiles to ensure the preview reflects the latest saved code.

Uses swift-frontend with -whole-module-optimization to compile all source
files in the module into a single fresh .o that directly replaces the stale
module-level .o in OBJECT-FILES.  This correctly handles intra-module type
references and avoids duplicate symbol issues.

For per-file .o matches (non-WMO builds), falls back to single-file
recompilation with -primary-file for type context.

PRODUCTS-DIR is the Build/Products directory.
DERIVED-DATA is the DerivedData root.
IOS-VERSION is the deployment target (e.g., \"17.0\")."
  (let* ((source-base (file-name-sans-extension
                       (file-name-nondirectory source-file)))
         (target-triple (format "arm64-apple-ios%s-simulator" ios-version))
          (sdk-path (swiftui-preview-core-sdk-path))
          (toolchain-path (swiftui-preview-core-toolchain-path))
         ;; Collect .o base names for module matching
         (o-names (mapcar (lambda (f)
                            (file-name-sans-extension (file-name-nondirectory f)))
                          object-files))
         ;; Detect module name from source path + available .o files
         (module-name
          (or
           ;; 1. SPM convention: extract from /Sources/<Module>/ in path
           (when (string-match "/Sources/\\([^/]+\\)/" source-file)
             (let ((candidate (match-string 1 source-file)))
               (when (member candidate o-names) candidate)))
           ;; 2. Match any .o name that appears as a path component
           (cl-find-if
            (lambda (name)
              (string-match-p (concat "/" (regexp-quote name) "/") source-file))
            o-names)
           ;; 3. Fall back to project settings (product name)
           (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme)))
             (when (fboundp 'swift-project-settings-get-product-name)
               (swift-project-settings-get-product-name (car info) (cdr info))))
           ;; 4. Last resort
           "testpreview"))
         ;; Check if object-files has a per-file .o matching our source
         (per-file-match (cl-find-if
                          (lambda (f)
                            (string= (file-name-sans-extension
                                      (file-name-nondirectory f))
                                     source-base))
                          object-files))
         ;; Find the module-level .o if no per-file match
         (module-o (unless per-file-match
                     (cl-find-if
                      (lambda (f)
                        (string= (file-name-sans-extension
                                  (file-name-nondirectory f))
                                 module-name))
                      object-files)))
         ;; Find the module's source directory
         (module-sources-dir (swiftui-preview-dynamic--find-module-sources-dir source-file))
         ;; Write filelist of all module sources
         (filelist-path (when module-sources-dir
                          (let ((path (expand-file-name "module-sources.txt"
                                                        swiftui-preview-dynamic--temp-dir))
                                (sources (directory-files-recursively
                                          module-sources-dir "\\.swift$")))
                            (when sources
                              (with-temp-file path
                                (dolist (s sources)
                                  (insert s "\n")))
                              path))))
         ;; Macro plugin paths for #Preview and other macros
         (plugin-path (expand-file-name
                       "Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/host/plugins"
                       toolchain-path))
         (platform-plugin-path (expand-file-name
                                "Platforms/iPhoneOS.platform/Developer/usr/lib/swift/host/plugins"
                                toolchain-path))
         ;; ObjC include dirs from SourcePackages (needed for mixed modules)
         (objc-include-dirs
          (swiftui-preview-dynamic--find-objc-include-dirs derived-data))
         ;; Fresh .o output path - for module-level, replace the whole module
         (fresh-o (expand-file-name
                   (if module-o (concat module-name ".o") (concat source-base ".o"))
                   swiftui-preview-dynamic--temp-dir))
         ;; Fresh .swiftmodule output path -- so preview-host sees
         ;; updated type signatures (e.g. new init parameters)
         (fresh-swiftmodule (expand-file-name
                             (concat module-name ".swiftmodule")
                             swiftui-preview-dynamic--temp-dir))
         ;; Common compile flags
         (common-flags
          (append
           (list "-parse-as-library"
                 "-o" fresh-o
                 "-module-name" module-name
                 "-sdk" sdk-path
                 "-target" target-triple
                 "-I" products-dir
                 "-F" products-dir
                 "-F" (concat sdk-path "/System/Library/Frameworks"))
           ;; Emit fresh .swiftmodule so downstream imports see
           ;; the updated public/internal API
           (list "-emit-module"
                 "-emit-module-path" fresh-swiftmodule)
           ;; Plugin paths for macros (#Preview etc.)
           (when (file-directory-p plugin-path)
             (list "-plugin-path" plugin-path))
           (when (file-directory-p platform-plugin-path)
             (list "-plugin-path" platform-plugin-path))
           ;; Enable testability if configured
           (when (bound-and-true-p xcode-build-config-enable-testability)
             (list "-enable-testing"))
           ;; Add GeneratedModuleMaps if available
           (let ((gmm (expand-file-name
                       "Build/Intermediates.noindex/GeneratedModuleMaps-iphonesimulator"
                       derived-data)))
             (when (file-directory-p gmm)
               (list "-I" gmm)))
           ;; ObjC include dirs for transitive module dependencies
           (cl-mapcan (lambda (dir) (list "-Xcc" "-I" "-Xcc" dir))
                      objc-include-dirs)))
         ;; Build compile args based on whether we have module sources
         (compile-args
          (if (and module-o filelist-path)
              ;; Module-level .o: recompile entire module with WMO
              (append (list "xcrun" "swift-frontend" "-c"
                            "-whole-module-optimization"
                            "-filelist" filelist-path)
                      common-flags)
            (if filelist-path
                ;; Per-file .o with module sources: use -primary-file
                (append (list "xcrun" "swift-frontend" "-c"
                              "-primary-file" source-file
                              "-filelist" filelist-path)
                        common-flags)
              ;; No module sources found: simple single-file compile
              (append (list "xcrun" "--sdk" "iphonesimulator"
                            "swiftc" "-c" source-file)
                      common-flags)))))

    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Recompile: source=%s module=%s" source-base module-name)
      (message "[Preview] Recompile: per-file-match=%s module-o=%s"
               (when per-file-match (file-name-nondirectory per-file-match))
               (when module-o (file-name-nondirectory module-o)))
      (message "[Preview] Recompile: module-sources-dir=%s (WMO=%s)"
               module-sources-dir (if (and module-o filelist-path) "yes" "no")))

    ;; Always recompile -- use async process to avoid blocking Emacs
    (swiftui-preview-dynamic--notify-update 'swiftui-preview
     :percent 15 :message (format "Recompiling %s..." module-name))

    ;; Return the compile-args, fresh-o path, and context so the caller
    ;; can run the compilation asynchronously
    (list :compile-args compile-args
          :fresh-o fresh-o
          :fresh-swiftmodule fresh-swiftmodule
          :module-o module-o
          :per-file-match per-file-match
          :object-files object-files
          :source-base source-base
          :module-name module-name)))

(defun swiftui-preview-dynamic--recompile-async (recompile-spec callback)
  "Run the recompilation described by RECOMPILE-SPEC asynchronously.
RECOMPILE-SPEC is a plist from `--recompile-stale-sources'.
CALLBACK is called with the updated object-files list."
  (let* ((compile-args (plist-get recompile-spec :compile-args))
         (fresh-o (plist-get recompile-spec :fresh-o))
         (module-o (plist-get recompile-spec :module-o))
         (per-file-match (plist-get recompile-spec :per-file-match))
         (object-files (plist-get recompile-spec :object-files))
         (source-base (plist-get recompile-spec :source-base))
         (module-name (plist-get recompile-spec :module-name))
         (recompile-buf (get-buffer-create "*SwiftUI Preview Recompile*")))
    (with-current-buffer recompile-buf
      (let ((inhibit-read-only t)) (erase-buffer)))
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Recompile async: %s" (string-join compile-args " ")))
    (make-process
     :name "swiftui-preview-recompile"
     :buffer recompile-buf
     :command compile-args
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((exit-code (process-exit-status process)))
           (if (/= exit-code 0)
               ;; Recompilation failed -- abort the pipeline.
               ;; Do NOT continue with stale .o files as that would
               ;; replace a working live preview app with a broken one.
               (let ((err-output (with-current-buffer recompile-buf
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)))))
                 (when swiftui-preview-dynamic-verbose
                   (message "[Preview] Recompile FAILED (exit %d) for %s - aborting"
                            exit-code source-base))
                 (swiftui-preview-dynamic--parse-build-errors err-output)
                 (swiftui-preview-dynamic--notify-cancel 'swiftui-preview)
                 (message "Preview recompile failed — keeping current preview"))
              (funcall
               callback
               (cond
                ;; Module-level .o: replace with fresh whole-module .o
                (module-o
                 (when swiftui-preview-dynamic-verbose
                   (message "[Preview] Recompiled module %s -> %s (replacing %s)"
                            module-name (file-name-nondirectory fresh-o)
                            (file-name-nondirectory module-o)))
                 (mapcar (lambda (f)
                           (if (string= f module-o) fresh-o f))
                         object-files))
                ;; Per-file match: replace the matching .o
                (per-file-match
                 (when swiftui-preview-dynamic-verbose
                   (message "[Preview] Recompiled %s.o - replacing per-file match" source-base))
                 (mapcar (lambda (f)
                           (if (string= f per-file-match) fresh-o f))
                         object-files))
                ;; No match: add fresh .o
                (t
                 (when swiftui-preview-dynamic-verbose
                   (message "[Preview] Recompiled %s.o - no matching .o found, adding to list"
                            source-base))
                 (cons fresh-o object-files)))))))))))


(defun swiftui-preview-dynamic--ensure-testability-build (callback)
  "Ensure project is built with testability, then call CALLBACK.
If testability is already enabled and build exists, calls CALLBACK immediately.
Otherwise, enables testability, rebuilds, and then calls CALLBACK."
  (require 'xcode-build-config nil t)
  
  (if (bound-and-true-p xcode-build-config-enable-testability)
      ;; Already enabled, just call callback
      (funcall callback)
    ;; Need to enable and rebuild
    (message "Enabling testability for SwiftUI preview...")
    (setq xcode-build-config-enable-testability t)
    
    ;; Store callback for after build completes
    (setq swiftui-preview-dynamic--pending-preview-buffer (current-buffer))
    
    ;; Add a hook to run preview after build completes
    (add-hook 'compilation-finish-functions
              #'swiftui-preview-dynamic--after-testability-build)
    
    ;; Trigger rebuild
    (if (fboundp 'swift-development-compile-app)
        (progn
          (message "Rebuilding with testability enabled... (preview will start after build)")
          (swift-development-compile-app))
      (remove-hook 'compilation-finish-functions
                   #'swiftui-preview-dynamic--after-testability-build)
      (user-error "swift-development-compile-app not available. Please rebuild manually with testability"))))

(defun swiftui-preview-dynamic--after-testability-build (_buffer status)
  "Hook called after testability build completes.
_BUFFER is the compilation buffer, STATUS is the exit status string."
  ;; Remove ourselves from the hook
  (remove-hook 'compilation-finish-functions
               #'swiftui-preview-dynamic--after-testability-build)
  
  (if (string-match-p "finished" status)
      ;; Build succeeded - generate preview
      (progn
        (message "Testability build complete. Generating preview...")
        (when (buffer-live-p swiftui-preview-dynamic--pending-preview-buffer)
          (with-current-buffer swiftui-preview-dynamic--pending-preview-buffer
            ;; Small delay to ensure build output is fully processed
            (run-at-time 0.5 nil #'swiftui-preview-dynamic-generate))))
    ;; Build failed
    (message "Build failed. Fix errors and try preview again.")))

(defcustom swiftui-preview-dynamic-use-xcodebuild nil
  "If non-nil, use xcodebuild with injected target.
If nil, use direct swiftc compilation with existing DerivedData modules.
Set to nil if xcodeproj gem has issues with your Xcode version."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--build-async (project-path simulator-udid is-workspace callback)
  "Build PreviewHost scheme asynchronously.
PROJECT-PATH is the project or workspace path.
SIMULATOR-UDID is the target simulator.
IS-WORKSPACE is non-nil if PROJECT-PATH is a workspace.
CALLBACK is called with (app-path) on success or (nil error-msg) on failure."
  (let* ((existing-derived-data (swiftui-preview-dynamic--find-derived-data project-path)))
    
    ;; Choose build method based on configuration and available DerivedData
    (if (and (not swiftui-preview-dynamic-use-xcodebuild) existing-derived-data)
        ;; Use direct swiftc compilation
        (swiftui-preview-dynamic--build-with-swiftc existing-derived-data simulator-udid callback)
      ;; Use xcodebuild (original method)
      (swiftui-preview-dynamic--build-with-xcodebuild 
       project-path simulator-udid is-workspace existing-derived-data callback))))

(defun swiftui-preview-dynamic--build-with-xcodebuild (project-path simulator-udid is-workspace existing-derived-data callback)
  "Build using xcodebuild with injected PreviewHost target."
  (let* ((derived-data (or existing-derived-data
                          (expand-file-name "DerivedData" swiftui-preview-dynamic--temp-dir)))
         (build-args (list "xcodebuild" "build"
                           (if is-workspace "-workspace" "-project") project-path
                           "-scheme" "PreviewHost"
                           "-configuration" "Debug"
                           "-destination" (format "platform=iOS Simulator,id=%s" simulator-udid)
                           "-derivedDataPath" derived-data
                           "ONLY_ACTIVE_ARCH=YES"
                           "ENABLE_TESTABILITY=YES"
                           "GCC_TREAT_WARNINGS_AS_ERRORS=NO"))
         (output-buffer (get-buffer-create "*SwiftUI Preview Build*")))
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Using DerivedData: %s%s"
               derived-data
               (if existing-derived-data " (existing)" " (new)"))
      (message "[Preview] Build command: %s" (string-join build-args " ")))
    
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Building PreviewHost with xcodebuild...\n\n")))
    
    (setq swiftui-preview-dynamic--build-callback callback)
    (setq swiftui-preview-dynamic--build-context
          (list :derived-data derived-data))
    
    (let ((process (make-process
                    :name "swiftui-preview-build"
                    :buffer output-buffer
                    :command build-args
                    :sentinel #'swiftui-preview-dynamic--build-sentinel)))
      (message "Building preview with xcodebuild... (async)")
      process)))

(defcustom swiftui-preview-dynamic-excluded-modules '()
  "Modules to exclude from preview linking.
These are typically third-party SDKs that require external frameworks
not available in the simulator environment (e.g., payment SDKs, analytics).
Set this in your .dir-locals.el for project-specific exclusions.

Example:
  ((swift-mode . ((swiftui-preview-dynamic-excluded-modules
                   . (\"Adyen\" \"GoogleSignIn\" \"Sentry\")))))"
  :type '(repeat string)
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--find-intermediates-dir (derived-data)
  "Find the Intermediates.noindex directory with object files in DERIVED-DATA.
Returns list of paths to Objects-normal/arm64 directories.
Structure: Intermediates.noindex/PROJECT.build/Debug-iphonesimulator/TARGET.build/Objects-normal/arm64/"
  (let ((intermediates-base (expand-file-name "Build/Intermediates.noindex" derived-data))
        (result '()))
    (when (file-directory-p intermediates-base)
      ;; Find PROJECT.build directories
      (dolist (project-build-dir (directory-files intermediates-base t "\\.build$"))
        (when (file-directory-p project-build-dir)
          ;; Find Debug-iphonesimulator (or similar) directories
          (dolist (sim-dir (directory-files project-build-dir t "-iphonesimulator$"))
            (when (file-directory-p sim-dir)
              ;; Find TARGET.build directories inside
              (dolist (target-build-dir (directory-files sim-dir t "\\.build$"))
                (when (file-directory-p target-build-dir)
                  (let ((objects-dir (expand-file-name "Objects-normal/arm64" target-build-dir)))
                    (when (file-directory-p objects-dir)
                      (push objects-dir result))))))))))
    (nreverse result)))


(defun swiftui-preview-dynamic--is-app-entry-p (object-file)
  "Check if OBJECT-FILE is an app entry point that should be excluded.
Detects @main by checking for _main symbol in the object file."
  (let ((name (file-name-sans-extension (file-name-nondirectory object-file))))
    ;; Quick name-based check first (most App entry points end with App)
    (when (string-suffix-p "App" name)
      ;; Verify by checking for _main symbol
      (with-temp-buffer
        (call-process "nm" nil t nil "-g" "-j" "-U" object-file)
        (goto-char (point-min))
        (re-search-forward "^_main$" nil t)))))

(defun swiftui-preview-dynamic--detect-external-frameworks (products-dir)
  "Detect binary frameworks in PRODUCTS-DIR that aren't available in the simulator.
Returns list of framework names (e.g., (\"Adyen3DS2\" \"Sentry\"))."
  (let ((frameworks '()))
    (when (file-directory-p products-dir)
      (dolist (fw (directory-files products-dir t "\\.framework$"))
        (push (file-name-sans-extension (file-name-nondirectory fw)) frameworks)))
    (nreverse frameworks)))

(defun swiftui-preview-dynamic--links-external-framework-p (object-file external-frameworks)
  "Check if OBJECT-FILE auto-links any of EXTERNAL-FRAMEWORKS.
Uses objdump to inspect LC_LINKER_OPTION entries."
  (when external-frameworks
    (with-temp-buffer
      (call-process "objdump" nil t nil "--macho" "--all-headers" object-file)
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward "string #2 \\(.+\\)" nil t))
          (when (member (match-string 1) external-frameworks)
            (setq found t)))
        found))))

(defun swiftui-preview-dynamic--collect-object-files (products-dir derived-data)
  "Collect .o files from PRODUCTS-DIR and DERIVED-DATA Intermediates directory.
For SPM packages, links all available .o files to resolve transitive dependencies,
but auto-excludes modules that depend on external binary frameworks.
For Xcode projects, .o files are in Intermediates (excluding app entry points).
Also excludes modules in `swiftui-preview-dynamic-excluded-modules'."
  (let ((object-files '())
        (external-frameworks (swiftui-preview-dynamic--detect-external-frameworks products-dir)))
    (when (and external-frameworks swiftui-preview-dynamic-verbose)
      (message "[Preview] External frameworks detected: %s" external-frameworks))
    
    ;; First try Products dir (SPM-style builds) - link all .o to resolve transitive deps
    (when (and products-dir (file-directory-p products-dir))
      (dolist (file (directory-files products-dir t "\\.o$"))
        (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
          (unless (or (member module-name swiftui-preview-dynamic-excluded-modules)
                      (swiftui-preview-dynamic--links-external-framework-p file external-frameworks))
            (push file object-files)))))
    
    ;; If no .o files found in Products, try Intermediates.noindex (Xcode project builds)
    (when (and (null object-files) derived-data)
      (let ((intermediates-dirs (swiftui-preview-dynamic--find-intermediates-dir derived-data)))
        (dolist (int-dir intermediates-dirs)
          (when (file-directory-p int-dir)
            (dolist (file (directory-files int-dir t "\\.o$"))
              (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
                (unless (or (member module-name swiftui-preview-dynamic-excluded-modules)
                            (swiftui-preview-dynamic--is-app-entry-p file))
                  (push file object-files))))))))
    
    (nreverse object-files)))

(defun swiftui-preview-dynamic--copy-app-resources (products-dir app-dir object-files)
  "Copy resource bundles and assets from PRODUCTS-DIR into APP-DIR.
Copies SPM resource bundles for linked modules (based on OBJECT-FILES)
and Assets.car from the built app if available.
Returns list of copied resource names."
  (let ((copied-resources '())
        (linked-modules (mapcar (lambda (f)
                                  (file-name-sans-extension (file-name-nondirectory f)))
                                object-files)))
    (when (file-directory-p products-dir)
      ;; Copy SPM resource bundles (.bundle directories)
      (dolist (item (directory-files products-dir t "\\.bundle$"))
        (let* ((bundle-name (file-name-nondirectory item))
               ;; Bundle names are like "ModuleName_ModuleName.bundle"
               ;; Extract the module name (first part before underscore)
               (module-name (car (split-string (file-name-sans-extension bundle-name) "_"))))
          ;; Only copy bundles for modules we're actually linking
          (when (or (member module-name linked-modules)
                    ;; Also check if any linked module starts with bundle's module name
                    ;; (handles cases like MyModule_MyModule.bundle matching MyModule.o)
                    (seq-find (lambda (m) (string-prefix-p module-name m)) linked-modules))
            (let ((dest (expand-file-name bundle-name app-dir)))
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Copying bundle: %s" bundle-name))
              (copy-directory item dest t t t)
              (push bundle-name copied-resources)))))

      ;; Copy Assets.car from the built app (compiled asset catalog)
      (dolist (built-app (directory-files products-dir t "\\.app$"))
        (let ((assets-car (expand-file-name "Assets.car" built-app)))
          (when (file-exists-p assets-car)
            (let ((dest (expand-file-name "Assets.car" app-dir)))
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Copying Assets.car from %s" (file-name-nondirectory built-app)))
              (copy-file assets-car dest t)
              (push "Assets.car" copied-resources))))))
    (nreverse copied-resources)))

(defun swiftui-preview-dynamic--build-with-swiftc (derived-data _simulator-udid callback)
  "Build PreviewHost using swiftc with modules from DERIVED-DATA.
_SIMULATOR-UDID is unused (kept for API compat).  CALLBACK
receives (app-path nil) or (nil error-msg)."
  (let* ((products-dir (swiftui-preview-dynamic--find-products-dir derived-data))
         (preview-swift (expand-file-name "PreviewHostApp.swift" swiftui-preview-dynamic--temp-dir))
         (app-dir (expand-file-name "PreviewHost.app" swiftui-preview-dynamic--temp-dir))
         (executable (expand-file-name "PreviewHost" app-dir))
         (output-buffer (get-buffer-create "*SwiftUI Preview Build*")))
    
    (if (not products-dir)
        (funcall callback nil "No built products found. Build your project first with M-x swift-development-compile-app")
      
      (when swiftui-preview-dynamic-verbose
        (message "[Preview] Using products from: %s" products-dir))
      
      ;; Create app bundle structure
      (make-directory app-dir t)
      
      ;; Create Info.plist using core generator
      (with-temp-file (expand-file-name "Info.plist" app-dir)
        (insert (swiftui-preview-core-generate-info-plist
                 "com.swift-development.preview-host")))
      
      ;; Collect object files from Products or Intermediates
      (let* ((raw-object-files (swiftui-preview-dynamic--collect-object-files products-dir derived-data))
             ;; Detect iOS version from built modules (needed for recompilation)
             (ios-version (swiftui-preview-dynamic--detect-ios-version products-dir))
             ;; Check recompile cache -- if same source file was already
             ;; recompiled in this generate-all session, reuse the .o
             (cached (and swiftui-preview-dynamic--recompile-cache
                         swiftui-preview-dynamic--current-source-file
                         (equal (plist-get swiftui-preview-dynamic--recompile-cache :source-file)
                                swiftui-preview-dynamic--current-source-file)
                         (file-exists-p (plist-get swiftui-preview-dynamic--recompile-cache :fresh-o))))
             ;; Prepare recompilation spec (does NOT run the compiler yet)
             ;; Skip if we have a valid cache hit
             (recompile-spec
              (when (and swiftui-preview-dynamic--current-source-file
                         (not cached))
                (swiftui-preview-dynamic--recompile-stale-sources
                 raw-object-files
                 swiftui-preview-dynamic--current-source-file
                 products-dir derived-data ios-version)))
             ;; Continuation: runs after async recompile (or immediately if no recompile)
             (continue-build
              (lambda (object-files)
                ;; Copy resource bundles for linked modules into app bundle
                (let* ((copied-resources (swiftui-preview-dynamic--copy-app-resources
                                          products-dir app-dir object-files))
                       (target-triple (format "arm64-apple-ios%s-simulator" ios-version))
                       (sdk-path (swiftui-preview-core-sdk-path))
                       (toolchain-path (swiftui-preview-core-toolchain-path))
                       ;; Detect profiling/tsan from the original build configuration
                       ;; by checking a single representative .o file (fast)
                       (sample-o (car object-files))
                       (needs-profiling
                        (when sample-o
                          (zerop (call-process "nm" nil nil nil
                                              "-g" sample-o "-s" "__DATA" "__llvm_prf_cnts"))))
                       (needs-tsan
                        (when sample-o
                          (zerop (call-process "nm" nil nil nil
                                              "-g" sample-o "-s" "__DATA" "__tsan_default_options"))))
                       ;; Profile runtime library path
                       (clang-rt-dir (car (directory-files
                                           (concat toolchain-path "/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang")
                                           t "^[0-9]" t)))
                       ;; Find GeneratedModuleMaps for ObjC/mixed modules
                       (generated-modulemaps-dir
                        (let ((dir (expand-file-name
                                    "Build/Intermediates.noindex/GeneratedModuleMaps-iphonesimulator"
                                    derived-data)))
                          (when (file-directory-p dir) dir)))
                       ;; Find ObjC include dirs from SourcePackages (for transitive module deps)
                       (objc-include-dirs
                        (swiftui-preview-dynamic--find-objc-include-dirs derived-data))
                       ;; Build with swiftc - compile and link in one step
                       (build-args (append
                                    (list "xcrun" "--sdk" "iphonesimulator"
                                          "swiftc"
                                          "-parse-as-library"  ;; Required for @main attribute
                                          preview-swift
                                          "-o" executable
                                          "-sdk" sdk-path
                                          "-target" target-triple
                                          ;; Fresh .swiftmodule from recompile (must
                                          ;; come BEFORE products-dir so updated
                                          ;; type signatures take precedence)
                                          "-I" swiftui-preview-dynamic--temp-dir
                                          ;; Module search paths (original products)
                                          "-I" products-dir
                                          ;; Framework search paths
                                          "-F" products-dir
                                          "-F" (concat sdk-path "/System/Library/Frameworks")
                                          ;; Library search paths for linking
                                          "-L" products-dir
                                          "-L" (concat toolchain-path "/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/iphonesimulator")
                                          ;; System frameworks
                                          "-framework" "SwiftUI"
                                          "-framework" "UIKit"
                                          "-framework" "Foundation"
                                          "-framework" "Combine"
                                          "-framework" "CoreGraphics"
                                          "-framework" "CoreFoundation")
                                    ;; Generated module maps for ObjC/mixed modules
                                    (when generated-modulemaps-dir
                                      (list "-I" generated-modulemaps-dir))
                                    ;; ObjC include dirs for transitive module dependencies
                                    (cl-mapcan (lambda (dir) (list "-Xcc" "-I" "-Xcc" dir))
                                               objc-include-dirs)
                                    ;; Conditionally add thread sanitizer
                                    (when needs-tsan '("-sanitize=thread"))
                                    ;; Conditionally link profiling runtime
                                    (when (and needs-profiling clang-rt-dir)
                                      (list (expand-file-name "lib/darwin/libclang_rt.profile_iossim.a" clang-rt-dir)))
                                    ;; Add all object files
                                    object-files)))

                  (when swiftui-preview-dynamic-verbose
                    (message "[Preview] iOS version: %s" ios-version)
                    (message "[Preview] Target: %s" target-triple)
                    (message "[Preview] Object files: %s" object-files)
                    (message "[Preview] Copied resources: %s" copied-resources)
                    (message "[Preview] swiftc command: %s" (string-join build-args " ")))

                  (with-current-buffer output-buffer
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert "Building PreviewHost with swiftc...\n")
                      (insert (format "Products dir: %s\n" products-dir))
                      (insert (format "Object files: %s\n" (string-join (mapcar #'file-name-nondirectory object-files) ", ")))
                      (insert (format "Resource bundles: %s\n\n"
                                      (if copied-resources
                                          (string-join copied-resources ", ")
                                        "none")))))

                  (swiftui-preview-dynamic--notify-update 'swiftui-preview
                   :percent 30 :message "Compiling preview...")

                  (setq swiftui-preview-dynamic--build-callback callback)
                  (setq swiftui-preview-dynamic--build-context
                        (list :derived-data derived-data
                              :app-path app-dir))

                  (let ((process (make-process
                                  :name "swiftui-preview-build"
                                  :buffer output-buffer
                                  :command build-args
                                  :sentinel #'swiftui-preview-dynamic--swiftc-sentinel)))
                    (message "Building preview with swiftc... (async)")
                    process)))))

        ;; Kick off: recompile async if needed, then continue to build
        (cond
         ;; Cache hit: reuse previously recompiled .o and .swiftmodule
         ;; Copy into the current temp-dir so they survive cleanup
         (cached
          (let* ((cached-o (plist-get swiftui-preview-dynamic--recompile-cache :fresh-o))
                 (local-o (expand-file-name (file-name-nondirectory cached-o)
                                            swiftui-preview-dynamic--temp-dir))
                 (cached-swiftmod (plist-get swiftui-preview-dynamic--recompile-cache :fresh-swiftmodule))
                 (cached-objects (plist-get swiftui-preview-dynamic--recompile-cache :object-files))
                 ;; Replace the cached .o path with the local copy in object-files
                 (updated-objects (mapcar (lambda (f)
                                           (if (equal f cached-o) local-o f))
                                         cached-objects)))
            (copy-file cached-o local-o t)
            ;; Copy .swiftmodule so preview-host sees fresh type signatures
            (when (and cached-swiftmod (file-exists-p cached-swiftmod))
              (copy-file cached-swiftmod
                         (expand-file-name (file-name-nondirectory cached-swiftmod)
                                           swiftui-preview-dynamic--temp-dir)
                         t))
            ;; Update cache to point to the new copy so it survives
            ;; deletion of the previous temp-dir
            (plist-put swiftui-preview-dynamic--recompile-cache :fresh-o local-o)
            (plist-put swiftui-preview-dynamic--recompile-cache :object-files updated-objects)
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] Recompile CACHED: copied %s -> %s"
                       (file-name-nondirectory cached-o)
                       swiftui-preview-dynamic--temp-dir))
            (funcall continue-build updated-objects)))
         ;; Need to recompile: run async, then cache result and continue
         (recompile-spec
          (let ((source-file swiftui-preview-dynamic--current-source-file))
            (swiftui-preview-dynamic--recompile-async
             recompile-spec
             (lambda (object-files)
               ;; Cache the result for subsequent previews from the same file.
               ;; Copy .o and .swiftmodule to a stable cache dir outside
               ;; temp-dir so they survive cleanup after capture completes.
               (let ((fresh-o (plist-get recompile-spec :fresh-o))
                     (fresh-swiftmod (plist-get recompile-spec :fresh-swiftmodule)))
                 (when (and fresh-o (file-exists-p fresh-o))
                   (let* ((cache-dir (make-temp-file "swiftui-recompile-cache-" t))
                          (cached-o (expand-file-name
                                     (file-name-nondirectory fresh-o) cache-dir))
                          (cached-swiftmod
                           (when (and fresh-swiftmod (file-exists-p fresh-swiftmod))
                             (let ((dest (expand-file-name
                                          (file-name-nondirectory fresh-swiftmod)
                                          cache-dir)))
                               (copy-file fresh-swiftmod dest t)
                               dest))))
                     (copy-file fresh-o cached-o t)
                     ;; Update object-files to point to the cached copy
                     (let ((cached-objects (mapcar (lambda (f)
                                                    (if (equal f fresh-o) cached-o f))
                                                  object-files)))
                       (setq swiftui-preview-dynamic--recompile-cache
                             (list :source-file source-file
                                   :fresh-o cached-o
                                   :fresh-swiftmodule cached-swiftmod
                                   :cache-dir cache-dir
                                   :object-files cached-objects))))))
               (funcall continue-build object-files)))))
         ;; No recompile needed
         (t
          (funcall continue-build raw-object-files)))))))

(defun swiftui-preview-dynamic--swiftc-sentinel (process _event)
  "Handle swiftc build PROCESS completion _EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((exit-code (process-exit-status process))
           (callback swiftui-preview-dynamic--build-callback)
           (context swiftui-preview-dynamic--build-context)
           (app-path (plist-get context :app-path)))
      
      (if (= exit-code 0)
          (progn
            (swiftui-preview-dynamic--notify-update 'swiftui-preview
             :percent 60 :message "Installing...")
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] swiftc build succeeded: %s" app-path))
            (when callback
              (funcall callback app-path nil)))
        ;; Build failed - parse errors through periphery
        (swiftui-preview-dynamic--notify-cancel 'swiftui-preview)
        (let ((build-output (when (buffer-live-p (process-buffer process))
                              (with-current-buffer (process-buffer process)
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))
          (swiftui-preview-dynamic--parse-build-errors build-output)
          (display-buffer (process-buffer process)))
        (when callback
          (funcall callback nil "swiftc build failed. See *SwiftUI Preview Build* buffer"))))))

(defun swiftui-preview-dynamic--build-sentinel (process _event)
  "Handle build PROCESS completion _EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((exit-code (process-exit-status process))
           (callback swiftui-preview-dynamic--build-callback)
           (context swiftui-preview-dynamic--build-context)
           (derived-data (plist-get context :derived-data)))
      
      (if (= exit-code 0)
          ;; Find built app - search in common locations
          (let* ((products-dir (expand-file-name "Build/Products" derived-data))
                 (app-path
                  (or
                   ;; Try Debug first
                   (let ((p (expand-file-name "Debug-iphonesimulator/PreviewHost.app" products-dir)))
                     (when (file-directory-p p) p))
                   ;; Try any *-iphonesimulator directory
                   (car (seq-filter
                         (lambda (p)
                           (and (file-directory-p p)
                                (not (string-match-p "\\.dSYM" p))))
                         (directory-files-recursively
                          products-dir
                          "PreviewHost\\.app$"
                          t))))))
            (swiftui-preview-dynamic--notify-update 'swiftui-preview
             :percent 60 :message "Installing...")
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] Built app found: %s" app-path))
            (when callback
              (funcall callback app-path nil)))
        ;; Build failed - parse errors through periphery
        (swiftui-preview-dynamic--notify-cancel 'swiftui-preview)
        (let ((build-output (when (buffer-live-p (process-buffer process))
                              (with-current-buffer (process-buffer process)
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))
          (swiftui-preview-dynamic--parse-build-errors build-output)
          (display-buffer (process-buffer process)))
        (when callback
          (funcall callback nil "Build failed. See *SwiftUI Preview Build* buffer"))))))



(defcustom swiftui-preview-dynamic-crop-screenshot t
  "If non-nil, crop screenshot to a centered square region.
This provides a cleaner preview focused on just the view content,
removing status bar, home indicator, and excess background."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-width 900
  "Width of the crop region in pixels.
Default 900 works well for most views on iPhone Pro (1206px width).
Increase for wider views, decrease for narrower components."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-height 1000
  "Height of the crop region in pixels.
Default 1000 works well for most views.
Increase for taller views, decrease for shorter components."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-offset-y 750
  "Vertical offset from top for the crop region in pixels.
Default 750 centers the crop on an iPhone Pro screen (2622px height).
Adjust if your content is positioned differently."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--crop-image (image-path)
  "Crop IMAGE-PATH to a centered square region around the content.
Uses sips (macOS built-in) to crop the image in place.
Returns IMAGE-PATH on success."
  (when swiftui-preview-dynamic-crop-screenshot
    (let* ((dimensions (shell-command-to-string
                        (format "sips -g pixelWidth -g pixelHeight %s 2>/dev/null"
                                (shell-quote-argument image-path))))
           (width (when (string-match "pixelWidth: \\([0-9]+\\)" dimensions)
                    (string-to-number (match-string 1 dimensions))))
           (height (when (string-match "pixelHeight: \\([0-9]+\\)" dimensions)
                     (string-to-number (match-string 1 dimensions)))))
      (when (and width height (> height 0) (> width 0))
        ;; Calculate crop parameters for a centered region
        (let* ((crop-width swiftui-preview-dynamic-crop-width)
               (crop-height swiftui-preview-dynamic-crop-height)
               ;; Center horizontally
               (crop-x (max 0 (/ (- width crop-width) 2)))
               ;; Use configured Y offset
               (crop-y swiftui-preview-dynamic-crop-offset-y)
               ;; Ensure we don't crop outside the image
               (actual-width (min crop-width (- width crop-x)))
               (actual-height (min crop-height (- height crop-y))))
          (when (and (> actual-width 100) (> actual-height 100))
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] Cropping: %dx%d -> %dx%d (offset: %d,%d)"
                       width height actual-width actual-height crop-x crop-y))
            ;; Use sips with --cropOffset Y X followed by -c HEIGHT WIDTH
            (call-process "sips" nil nil nil
                          "--cropOffset" (number-to-string crop-y) (number-to-string crop-x)
                          "-c" (number-to-string actual-height) (number-to-string actual-width)
                          image-path))))))
  image-path)

(defcustom swiftui-preview-dynamic-show-simulator nil
  "If non-nil, show the Simulator window during preview capture.
When nil (default), capture runs headless for faster performance."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defalias 'swiftui-preview-dynamic--simctl
  #'swiftui-preview-core-simctl
  "Run simctl synchronously.  Delegates to core.")

(defun swiftui-preview-dynamic--capture (app-path simulator-udid output-path)
  "Install APP-PATH, launch, and wait for internal snapshot to OUTPUT-PATH.
The app renders the view internally with transparent background and saves to file.
The output path is embedded in the generated Swift code at build time,
so no environment variable propagation is needed.
Returns OUTPUT-PATH on success."
  (let ((bundle-id "com.swift-development.preview-host"))

    ;; Terminate any existing instance and uninstall to avoid stale binary
    (swiftui-preview-dynamic--simctl (list "terminate" simulator-udid bundle-id))
    (swiftui-preview-dynamic--simctl (list "uninstall" simulator-udid bundle-id))

    ;; Ensure output directory exists
    (make-directory (file-name-directory output-path) t)

    ;; Delete old output file if exists
    (when (file-exists-p output-path)
      (delete-file output-path))

    ;; Install fresh
    (swiftui-preview-dynamic--notify-update 'swiftui-preview
     :percent 70 :message "Installing...")
    (unless (swiftui-preview-dynamic--simctl (list "install" simulator-udid app-path))
      (error "Failed to install preview app"))

    ;; Only open Simulator window if explicitly requested
    (when swiftui-preview-dynamic-show-simulator
      (if (fboundp 'swift-async-run-sync)
          (swift-async-run-sync '("open" "-a" "Simulator") :timeout 5)
        (call-process "open" nil nil nil "-a" "Simulator"))
      (sleep-for 1))

    ;; Launch app - output path is embedded in the Swift code
    (swiftui-preview-dynamic--notify-update 'swiftui-preview
     :percent 80 :message "Capturing...")
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Sync capture: launching app, polling for %s" output-path))
    (swiftui-preview-dynamic--simctl (list "launch" simulator-udid bundle-id))

    ;; Wait for the app to generate the preview and exit
    ;; The app calls exit(0) after saving, so we poll for the file
    (let ((timeout 15.0)
          (poll-interval 0.3)
          (elapsed 0.0))
      (while (and (< elapsed timeout)
                  (not (file-exists-p output-path)))
        (sleep-for poll-interval)
        (setq elapsed (+ elapsed poll-interval)))

      (unless (file-exists-p output-path)
        ;; Fallback: try simctl screenshot if internal capture failed
        (message "Internal capture timed out, falling back to screenshot...")
        (swiftui-preview-dynamic--simctl
         (list "io" simulator-udid "screenshot" output-path))
        (when (file-exists-p output-path)
          (swiftui-preview-dynamic--crop-image output-path))))

    ;; Terminate app (may already be terminated)
    (swiftui-preview-dynamic--simctl (list "terminate" simulator-udid bundle-id))

    (if (file-exists-p output-path)
        output-path
      (error "Failed to generate preview"))))

(defalias 'swiftui-preview-dynamic--watch-for-file
  #'swiftui-preview-core-watch-for-file
  "Watch for file creation via OS notifications.  Delegates to core.")

(defalias 'swiftui-preview-dynamic--simctl-async
  #'swiftui-preview-core-simctl-async
  "Run simctl asynchronously.  Delegates to core.")

(defun swiftui-preview-dynamic--capture-launch-and-poll
    (simulator-udid bundle-id output-path callback)
  "Launch preview app on SIMULATOR-UDID and poll for OUTPUT-PATH.
BUNDLE-ID is the app bundle identifier.
The output path is embedded in the generated Swift code, so no
environment variable propagation is needed.
Calls CALLBACK with OUTPUT-PATH on success."
  (swiftui-preview-dynamic--notify-update 'swiftui-preview
   :percent 80 :message "Capturing...")
  (when swiftui-preview-dynamic-verbose
    (message "[Preview] Launching app, polling for: %s" output-path))
  (swiftui-preview-dynamic--simctl-async
   (list "launch" simulator-udid bundle-id)
   (lambda (_)
     (when swiftui-preview-dynamic-verbose
       (message "[Preview] App launched, polling for output file..."))
      (swiftui-preview-dynamic--watch-for-file
       output-path 15.0
       (lambda (found-path)
        (if found-path
            (progn
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Internal capture SUCCESS: %s" found-path))
              (swiftui-preview-dynamic--simctl-async
               (list "terminate" simulator-udid bundle-id) nil)
              (funcall callback found-path))
          ;; Fallback: screenshot
          (message "[Preview] Internal capture timed out, falling back to screenshot...")
          (swiftui-preview-dynamic--simctl-async
           (list "io" simulator-udid "screenshot" output-path)
           (lambda (_)
             (when (file-exists-p output-path)
               (swiftui-preview-dynamic--crop-image output-path))
             (swiftui-preview-dynamic--simctl-async
              (list "terminate" simulator-udid bundle-id) nil)
             (if (file-exists-p output-path)
                 (funcall callback output-path)
               (error "Failed to generate preview"))))))))))

(defun swiftui-preview-dynamic--capture-async (app-path simulator-udid output-path callback)
  "Install APP-PATH on SIMULATOR-UDID, capture to OUTPUT-PATH asynchronously.
Calls CALLBACK with OUTPUT-PATH on success, signals error on failure.
The output path is embedded in the generated Swift code at build time,
so no environment variable propagation is needed."
  (let ((bundle-id "com.swift-development.preview-host"))
    ;; Ensure output directory exists
    (make-directory (file-name-directory output-path) t)
    ;; Delete old output file
    (when (file-exists-p output-path)
      (delete-file output-path))
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Capture async: install %s, expect output at %s"
               (file-name-nondirectory app-path) output-path))
    ;; Terminate + uninstall old instance, then install + launch
    (swiftui-preview-dynamic--simctl-async
     (list "terminate" simulator-udid bundle-id)
     (lambda (_)
       (swiftui-preview-dynamic--simctl-async
        (list "uninstall" simulator-udid bundle-id)
        (lambda (_)
          (swiftui-preview-dynamic--notify-update 'swiftui-preview
           :percent 70 :message "Installing...")
          (swiftui-preview-dynamic--simctl-async
           (list "install" simulator-udid app-path)
           (lambda (_)
             (if swiftui-preview-dynamic-show-simulator
                 (progn
                   (swiftui-preview-dynamic--simctl-async
                    (list "launch" simulator-udid
                          "com.apple.iphonesimulator")
                    nil)
                   (run-at-time
                    1 nil
                    (lambda ()
                      (swiftui-preview-dynamic--capture-launch-and-poll
                       simulator-udid bundle-id output-path
                       callback))))
               (swiftui-preview-dynamic--capture-launch-and-poll
                simulator-udid bundle-id output-path
                callback)))
           (lambda (err)
             (error "Failed to install preview app: %s" err)))))))))

;;; Main Entry Points

;;;###autoload
(defun swiftui-preview-dynamic-generate (&optional preview-body preview-name)
  "Generate SwiftUI preview using dynamic target injection.
Works with Xcode projects and workspaces.
For SPM packages, use `swiftui-preview-spm-generate'.

When PREVIEW-BODY is provided, use it directly instead of extracting
from the buffer.  This enables generating a specific preview when
multiple #Preview blocks exist.

PREVIEW-NAME, when non-nil, is used to create a unique output filename
\(e.g., \"Light\" produces \"HomeView-Light.png\" instead of \"HomeView.png\").

If testability is not enabled, this will automatically enable it
and rebuild the project before generating the preview."
  (interactive)
  
  ;; Check setup (guard with fboundp since setup module is optional)
  (when (fboundp 'swiftui-preview-setup-check)
    (unless (swiftui-preview-setup-check)
      (when (fboundp 'swiftui-preview-setup-wizard)
        (swiftui-preview-setup-wizard))
      (unless (swiftui-preview-setup-check)
        (user-error "Setup incomplete. Run M-x swiftui-preview-setup-wizard"))))
  
  ;; Validate
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (unless (string-match-p "\\.swift$" (buffer-file-name))
    (user-error "Not a Swift file"))
  
  ;; Save file first so buffer and disk are in sync
  (save-buffer)
  
  ;; Get preview body and color scheme
  (let* ((preview-body (or preview-body (swiftui-preview--get-first-preview-body)))
         ;; Detect color scheme from the specific preview body, not the
         ;; whole buffer -- important when multiple previews have different
         ;; color schemes
         (color-scheme (if preview-body
                           (swiftui-preview-core-detect-color-scheme preview-body)
                         (swiftui-preview-core-detect-color-scheme-from-buffer))))
    (unless preview-body
      (user-error "No #Preview block found in file"))
    
    ;; Start progress notification
    (swiftui-preview-dynamic--notify-start
     :id 'swiftui-preview
     :title "Preview"
     :message "Preparing..."
     :percent 0)
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Extracted body: %s"
               (substring preview-body 0 (min 100 (length preview-body)))))
    
    ;; Check if we need testability and it's not enabled
    ;; Only check for swiftc mode (not xcodebuild which handles it differently)
    (if (and (not swiftui-preview-dynamic-use-xcodebuild)
             (not (bound-and-true-p xcode-build-config-enable-testability)))
        ;; Need to enable testability and rebuild first
        (swiftui-preview-dynamic--ensure-testability-build
         (lambda ()
           ;; This callback is called immediately if testability is already enabled
           ;; The hook will re-run this function after build completes
           nil))
      
      ;; Testability is enabled (or using xcodebuild) - proceed with preview
      ;; Detect project
      (let* ((project-info (swiftui-preview-dynamic--detect-project-type))
             (project-type (plist-get project-info :type))
             (project-path (plist-get project-info :path))
             (project-root (plist-get project-info :root)))
        
        ;; Handle based on project type
         (pcase project-type
           ((or 'xcode-project 'xcode-workspace)
            (swiftui-preview-dynamic--generate-for-xcode
             preview-body project-path project-root color-scheme
             preview-name))
          ('spm-package
           (if (fboundp 'swiftui-preview-spm-generate)
               (swiftui-preview-spm-generate)
             (user-error "SPM package detected. SPM preview not yet loaded")))
          ('standalone
           (if (fboundp 'swiftui-preview-standalone-generate)
               (swiftui-preview-standalone-generate)
             (user-error "Standalone file. Standalone preview not yet loaded")))
          (_
           (user-error "Unknown project type")))))))

(defun swiftui-preview-dynamic--find-xcodeproj (path)
  "Find .xcodeproj from PATH.
If PATH is a workspace, find the main .xcodeproj in the same directory.
If PATH is already a .xcodeproj, return it."
  (cond
   ((string-suffix-p ".xcodeproj" path)
    path)
   ((string-suffix-p ".xcworkspace" path)
    ;; Find .xcodeproj in the same directory as the workspace
    (let* ((dir (file-name-directory path))
           (workspace-name (file-name-sans-extension (file-name-nondirectory path)))
           (projects (directory-files dir t "\\.xcodeproj$")))
      ;; Prefer project with same name as workspace
      (or (cl-find-if (lambda (p)
                        (string= (file-name-sans-extension (file-name-nondirectory p))
                                 workspace-name))
                      projects)
          (car projects)
          (error "No .xcodeproj found for workspace %s" path))))
   (t
    (error "Unknown project type: %s" path))))

(defun swiftui-preview-dynamic--generate-error-handler (xcodeproj-path temp-dir err)
  "Handle errors during preview generation.
XCODEPROJ-PATH, TEMP-DIR for cleanup.  ERR is the error."
  (swiftui-preview-dynamic--notify-cancel 'swiftui-preview)
  (swiftui-preview-dynamic--show-error
   (if (stringp err) err (error-message-string err)))
  (when swiftui-preview-dynamic-use-xcodebuild
    (unless swiftui-preview-dynamic-keep-target
      (swiftui-preview-dynamic--cleanup xcodeproj-path)))
  (when (and temp-dir (file-exists-p temp-dir))
    (delete-directory temp-dir t)))

(defun swiftui-preview-dynamic--generate-for-xcode (preview-body project-path project-root &optional color-scheme preview-name)
  "Generate preview for Xcode project.
PREVIEW-BODY is the extracted #Preview content.
PROJECT-PATH is path to .xcodeproj or .xcworkspace.
PROJECT-ROOT is the project root directory.
COLOR-SCHEME is \"dark\", \"light\", or nil (detected from buffer).
PREVIEW-NAME, when non-nil, is appended to the output filename for
unique naming (e.g., \"Light\" -> \"HomeView-Light.png\").

This function is fully asynchronous - it does not block Emacs.
All subprocess calls (module detection, simulator lookup, Ruby scripts,
builds, and capture) run through `swift-async-run' with callback chains."
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (source-file (buffer-file-name))
         (xcodeproj-path (swiftui-preview-dynamic--find-xcodeproj project-path))
         (is-workspace (string-suffix-p ".xcworkspace" project-path))
         ;; Capture imports synchronously (pure elisp buffer scan, instant)
         (imports (swiftui-preview-dynamic--extract-imports))
         (output-path (swiftui-preview-dynamic--get-output-path
                        project-root filename preview-name))
         (temp-dir (make-temp-file "swiftui-preview-" t)))

    ;; Store for cleanup and source recompilation
    (setq swiftui-preview-dynamic--temp-dir temp-dir
          swiftui-preview-dynamic--current-project xcodeproj-path
          swiftui-preview-dynamic--current-source-file source-file)

    ;; Ensure SwiftUI is in imports
    (unless (member "SwiftUI" imports)
      (push "SwiftUI" imports))

    ;; Step 1: Detect module (async)
    (swiftui-preview-dynamic--detect-module-async
     (lambda (module-name)
       (condition-case err
           (progn
             ;; Add module to imports if detected
             (when (and module-name (not (member module-name imports)))
               (push module-name imports))
             (when swiftui-preview-dynamic-verbose
               (message "[Preview] Module: %s" module-name)
               (message "[Preview] Imports: %s" imports)
               (message "[Preview] Source file: %s" source-file)
               (message "[Preview] Xcodeproj: %s" xcodeproj-path)
               (message "[Preview] Is workspace: %s" is-workspace))
             ;; Step 2: Find simulator (async)
             (swiftui-preview-dynamic--find-simulator-async
              (lambda (simulator-udid)
                (condition-case err
                    (progn
                      (unless simulator-udid
                        (error "No simulator found. Boot a simulator first"))
                      ;; Step 3: Ensure simulator booted (async)
                      (swiftui-preview-dynamic--ensure-simulator-booted-async
                       simulator-udid
                       (lambda ()
                         (condition-case err
                             (progn
                               ;; Step 4: Generate preview host
                               (swiftui-preview-dynamic--notify-update
                                'swiftui-preview
                                :percent 10 :message "Generating host...")
                                (when swiftui-preview-dynamic-verbose
                                  (message "[Preview] Embedding output-path in Swift: %s"
                                           output-path))
                                (swiftui-preview-dynamic--generate-preview-host
                                 preview-body imports temp-dir
                                 module-name color-scheme output-path)
                               ;; Step 5+6+7+8: Build and capture
                               (let ((proceed-to-build
                                      (lambda ()
                                        (swiftui-preview-dynamic--build-async
                                         (if is-workspace
                                             project-path xcodeproj-path)
                                         simulator-udid
                                         is-workspace
                                         (lambda (app-path error-msg)
                                           (cond
                                            (error-msg
                                             (swiftui-preview-dynamic--generate-error-handler
                                              xcodeproj-path temp-dir
                                              error-msg))
                                            ((not app-path)
                                             (swiftui-preview-dynamic--generate-error-handler
                                              xcodeproj-path temp-dir
                                              "Build produced no app"))
                                            (t
                                             (condition-case cap-err
                                                  (if swiftui-preview-dynamic-live-mode
                                                      (swiftui-preview-dynamic--capture-live-async
                                                       app-path simulator-udid
                                                       (lambda (_captured)
                                                         ;; Close the *SwiftUI Preview* image buffer
                                                         ;; since live mode runs in the simulator
                                                         (swiftui-preview-dynamic--close-preview-buffer)
                                                         (swiftui-preview-dynamic--notify-finish
                                                          'swiftui-preview
                                                          "Live preview running")
                                                         (message "Live preview running in Simulator")))
                                                   (swiftui-preview-dynamic--capture-async
                                                    app-path simulator-udid output-path
                                                    (lambda (captured)
                                                      (swiftui-preview--display-image captured)
                                                      (swiftui-preview-dynamic--notify-finish
                                                       'swiftui-preview "Preview ready")
                                                      (when swiftui-preview-dynamic-use-xcodebuild
                                                        (unless swiftui-preview-dynamic-keep-target
                                                          (swiftui-preview-dynamic--cleanup
                                                           xcodeproj-path)))
                                                      (when (and temp-dir
                                                                 (file-exists-p temp-dir))
                                                        (delete-directory temp-dir t)))))
                                               (error
                                                (swiftui-preview-dynamic--generate-error-handler
                                                 xcodeproj-path temp-dir
                                                 cap-err))))))))))
                                 (if swiftui-preview-dynamic-use-xcodebuild
                                     (progn
                                       (message "Injecting preview target...")
                                       (swiftui-preview-dynamic--inject-target-async
                                        xcodeproj-path temp-dir
                                        module-name imports
                                        (lambda (_result)
                                          (funcall proceed-to-build))
                                        source-file))
                                   (funcall proceed-to-build))))
                           (error
                            (swiftui-preview-dynamic--generate-error-handler
                             xcodeproj-path temp-dir err))))))
                  (error
                   (swiftui-preview-dynamic--generate-error-handler
                    xcodeproj-path temp-dir err))))))
         (error
          (swiftui-preview-dynamic--generate-error-handler
           xcodeproj-path temp-dir err)))))))

(defun swiftui-preview-dynamic--cleanup (project-path)
  "Clean up injected target from PROJECT-PATH."
  (when project-path
    (message "Cleaning up...")
    (swiftui-preview-dynamic--cleanup-target project-path)
    ;; Also clean temp dir
    (when (and swiftui-preview-dynamic--temp-dir
               (file-exists-p swiftui-preview-dynamic--temp-dir))
      (delete-directory swiftui-preview-dynamic--temp-dir t)
      (setq swiftui-preview-dynamic--temp-dir nil))))

(defun swiftui-preview-dynamic--show-error (error-msg)
  "Show error with details and retry option.
ERROR-MSG is the error message to display."
  (let ((buffer (get-buffer-create "*SwiftUI Preview Error*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "SwiftUI Preview Error\n")
        (insert "=====================\n\n")
        (insert error-msg "\n\n")
        (insert "Possible causes:\n")
        (insert "  - Module not found (check file location)\n")
        (insert "  - Build errors in dependencies\n")
        (insert "  - Missing imports\n\n")
        (insert "Actions:\n")
        (insert "  M-x swiftui-preview-dynamic-generate  - Retry\n")
        (insert "  M-x swiftui-preview-dynamic-cleanup   - Manual cleanup\n")
        (insert "  See *SwiftUI Preview Build* buffer for build output\n")
        (special-mode)))
    (display-buffer buffer)))

;;;###autoload
(defun swiftui-preview-dynamic-cleanup ()
  "Manually clean up injected PreviewHost target."
  (interactive)
  (if swiftui-preview-dynamic--current-project
      (progn
        (swiftui-preview-dynamic--cleanup swiftui-preview-dynamic--current-project)
        (setq swiftui-preview-dynamic--current-project nil)
        (message "Cleanup complete"))
    (message "No project to clean up")))

;;;###autoload
(defun swiftui-preview-dynamic-select ()
  "Select which #Preview to generate if multiple exist.
Prompts the user with preview names and generates the selected one."
  (interactive)
  (let ((previews (swiftui-preview--detect-preview-definitions)))
    (if (null previews)
        (user-error "No #Preview blocks found")
      (let ((macro-previews (cl-remove-if-not
                             (lambda (p) (eq (plist-get p :type) 'preview-macro))
                             previews)))
        (if (<= (length macro-previews) 1)
            (swiftui-preview-dynamic-generate)
          ;; Multiple previews - let user choose
          (let* ((choices (mapcar (lambda (p)
                                    (cons (or (plist-get p :name) "Preview")
                                          (plist-get p :body)))
                                  macro-previews))
                 (selected-name (completing-read "Select preview: "
                                                 (mapcar #'car choices)
                                                 nil t))
                 (selected-body (cdr (assoc selected-name choices))))
            (if selected-body
                (swiftui-preview-dynamic-generate selected-body)
              (user-error "No body found for preview '%s'" selected-name))))))))

;;; Live Preview Mode

(defun swiftui-preview-dynamic--close-preview-buffer ()
  "Close the *SwiftUI Preview* image buffer window if it is visible.
In live mode the preview runs interactively in the Simulator,
so the static image buffer is not needed."
  (when (boundp 'swiftui-preview-buffer-name)
    (when-let* ((buf (get-buffer swiftui-preview-buffer-name))
                (win (get-buffer-window buf t)))
      (delete-window win))))

(defun swiftui-preview-dynamic--capture-live-async
    (app-path simulator-udid callback)
  "Install APP-PATH on SIMULATOR-UDID and launch for live interaction.
Opens the Simulator window and leaves the app running.
Calls CALLBACK with APP-PATH when done.  Does NOT terminate the app."
  (let ((bundle-id swiftui-preview-dynamic--live-bundle-id))
    ;; Store state for refresh/stop
    (setq swiftui-preview-dynamic--live-simulator-udid simulator-udid
          swiftui-preview-dynamic--live-app-path app-path)
    ;; Terminate + uninstall old, then install + launch
    (swiftui-preview-dynamic--simctl-async
     (list "terminate" simulator-udid bundle-id)
     (lambda (_)
       (swiftui-preview-dynamic--simctl-async
        (list "uninstall" simulator-udid bundle-id)
        (lambda (_)
          (swiftui-preview-dynamic--notify-update
           'swiftui-preview :percent 70 :message "Installing...")
          (swiftui-preview-dynamic--simctl-async
           (list "install" simulator-udid app-path)
           (lambda (_)
             ;; Open Simulator.app so the user can interact
             (swiftui-preview-dynamic--simctl-async
              (list "launch" simulator-udid
                    "com.apple.iphonesimulator")
              nil)
             (run-at-time
              0.5 nil
              (lambda ()
                ;; Launch the preview app
                (swiftui-preview-dynamic--notify-update
                 'swiftui-preview
                 :percent 90 :message "Launching live...")
                (swiftui-preview-dynamic--simctl-async
                 (list "launch" simulator-udid bundle-id)
                 (lambda (_)
                   (swiftui-preview-dynamic--notify-finish
                    'swiftui-preview
                    "Live preview running")
                   (funcall callback app-path))))))
           (lambda (err)
             (error "Failed to install live preview: %s"
                    err)))))))))

;;;###autoload
(defun swiftui-preview-dynamic-refresh-live ()
  "Take a screenshot of the running live preview app.
Displays the result in the Emacs preview buffer."
  (interactive)
  (unless swiftui-preview-dynamic--live-simulator-udid
    (user-error "No live preview running"))
  (let ((output-path (expand-file-name
                      "live-preview.png" temporary-file-directory))
        (udid swiftui-preview-dynamic--live-simulator-udid))
    (when (file-exists-p output-path)
      (delete-file output-path))
    (swiftui-preview-dynamic--simctl-async
     (list "io" udid "screenshot" output-path)
     (lambda (_)
       (when (file-exists-p output-path)
         (swiftui-preview-dynamic--crop-image output-path)
         (when (fboundp 'swiftui-preview--display-image)
           (swiftui-preview--display-image output-path))
         (message "Live preview refreshed"))))))

;;;###autoload
(defun swiftui-preview-dynamic-stop-live ()
  "Stop the live preview app and clean up."
  (interactive)
  ;; Cancel refresh timer if running
  (when (and swiftui-preview-dynamic--live-refresh-timer
             (timerp swiftui-preview-dynamic--live-refresh-timer))
    (cancel-timer swiftui-preview-dynamic--live-refresh-timer)
    (setq swiftui-preview-dynamic--live-refresh-timer nil))
  ;; Terminate app
  (when swiftui-preview-dynamic--live-simulator-udid
    (swiftui-preview-dynamic--simctl-async
     (list "terminate"
           swiftui-preview-dynamic--live-simulator-udid
           swiftui-preview-dynamic--live-bundle-id)
     (lambda (_)
       (message "Live preview stopped"))))
  ;; Clear state
  (setq swiftui-preview-dynamic--live-simulator-udid nil
        swiftui-preview-dynamic--live-app-path nil))

;;;###autoload
(defun swiftui-preview-dynamic-toggle-live-mode ()
  "Toggle between snapshot and live preview mode."
  (interactive)
  (setq swiftui-preview-dynamic-live-mode
        (not swiftui-preview-dynamic-live-mode))
  ;; Stop live preview when switching to snapshot mode
  (unless swiftui-preview-dynamic-live-mode
    (when swiftui-preview-dynamic--live-simulator-udid
      (swiftui-preview-dynamic-stop-live)))
  (message "SwiftUI Preview mode: %s"
           (if swiftui-preview-dynamic-live-mode
               "LIVE (interactive)" "SNAPSHOT")))

(provide 'swiftui-preview-dynamic)
;;; swiftui-preview-dynamic.el ends here
