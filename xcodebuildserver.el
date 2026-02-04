;;; xcodebuildserver.el --- Automated xcode-build-server setup -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.7.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, xcode, lsp

;;; Commentary:

;; Automated setup and configuration for xcode-build-server LSP integration

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'swift-async)

;; Optional dependency
(defvar mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

(defun xcodebuildserver-safe-mode-line-update (&rest args)
  "Safely call mode-line-hud:update if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:update args)))

(defun xcodebuildserver-safe-mode-line-notification (&rest args)
  "Safely call mode-line-hud:notification if available."
  (when mode-line-hud-available-p
    (apply #'mode-line-hud:notification args)))

(defgroup xcodebuildserver nil
  "Xcodebuildserver."
  :tag "xcodebuidserver"
  :group 'xcodebuildserver)

(defvar xcodebuildserver--debug nil
  "Debug mode for xcodebuildserver.")

(defun xcodebuildserver--get-build-root (root workspace scheme)
  "Get the build_root path for the project.
ROOT is the project root, WORKSPACE is the workspace file, SCHEME is the build scheme.
Returns the build root path (either .build/ or DerivedData) for BSP configuration.

First tries to use saved settings from swift-project-settings (fast).
Falls back to running xcodebuild if settings not available."
  (let ((build-dir nil))
    ;; Try to get from saved settings first (fast path)
    (when (fboundp 'swift-project-settings-load)
      (let ((settings (swift-project-settings-load root scheme)))
        (when settings
          (setq build-dir (plist-get settings :target-build-dir)))))
    
    ;; Fallback to xcodebuild if not in settings
    ;; Use -derivedDataPath .build to match actual build location
    (unless build-dir
      (let* ((default-directory root)
             (command (format "xcodebuild -showBuildSettings %s -scheme %s -derivedDataPath .build 2>/dev/null | grep -E '^ *BUILD_DIR = ' | head -1 | sed 's/.*= //'"
                              workspace (shell-quote-argument scheme))))
        (setq build-dir (string-trim (or (swift-async-run-sync command :timeout 15) "")))))
    
    ;; Extract build root from build-dir
    ;; BUILD_DIR is like: /path/to/project/.build/Build/Products
    ;; or: .../DerivedData/ProjectName-xxx/Build/Products
    ;; We need the parent of "Build" directory
    (when (and build-dir (not (string-empty-p build-dir)))
      (cond
       ;; Local .build directory
       ((string-match-p "/\\.build/" build-dir)
        (when (string-match "\\(.*/\\.build\\)" build-dir)
          (match-string 1 build-dir)))
       
       ;; DerivedData directory
       ((string-match-p "DerivedData" build-dir)
        (let ((parts (split-string build-dir "/"))
              (result nil))
          (catch 'found
            (let ((i 0))
              (while (< i (length parts))
                (when (string= (nth i parts) "DerivedData")
                  (when (< (1+ i) (length parts))
                    (setq result (string-join (seq-take parts (+ i 2)) "/")))
                  (throw 'found t))
                (setq i (1+ i)))))
          (when (and result (not (string-prefix-p "/" result)))
            (setq result (concat "/" result)))
          result))
       
       ;; Unknown format - return parent of Build/Products
       (t
        (when (string-match "\\(.*\\)/Build/Products" build-dir)
          (match-string 1 build-dir)))))))

(defun xcodebuildserver--update-config-with-build-root (root build-root)
  "Update the buildServer.json in ROOT with BUILD-ROOT.
Returns t if successful, nil otherwise."
  (let ((config-file (expand-file-name "buildServer.json" root)))
    (when (and build-root (file-exists-p config-file))
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (config (json-read-file config-file)))
            ;; Add or update build_root
            (setf (alist-get 'build_root config) build-root)
            ;; Write back
            (with-temp-file config-file
              (insert (json-encode config)))
            (when xcodebuildserver--debug
              (message "Updated buildServer.json with build_root: %s" build-root))
            t)
        (error
         (when xcodebuildserver--debug
           (message "Failed to update buildServer.json: %s" (error-message-string err)))
         nil)))))

(defun xcodebuildserver-regenerate-configuration ()
  "Regenerate the xcode-build-server configuration for the current project.
This will recreate buildServer.json with the build_root property set."
  (interactive)
  (let* ((root (or (when (fboundp 'xcode-project-project-root)
                     (xcode-project-project-root))
                   default-directory))
         (workspace (when (fboundp 'xcode-project-get-workspace-or-project)
                      (xcode-project-get-workspace-or-project)))
         (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                   xcode-project--current-xcode-scheme))
         (config-file (expand-file-name "buildServer.json" root)))
    (message "Regenerating BSP: root=%s, workspace=%s, scheme=%s"
             root workspace scheme)
    (cond
     ((not root)
      (message "Cannot regenerate: no project root found"))
     ((not workspace)
      (message "Cannot regenerate: no workspace/project found in %s" root))
     ((not scheme)
      (message "Cannot regenerate: no scheme configured. Run swift-development-compile-app first."))
     (t
      ;; Delete existing config
      (when (file-exists-p config-file)
        (delete-file config-file)
        (message "Deleted existing buildServer.json"))
      ;; Regenerate
      (xcodebuildserver-check-configuration
       :root root
       :workspace workspace
       :scheme scheme)
      (xcodebuildserver-safe-mode-line-notification
       :message "Regenerating BSP configuration..."
       :seconds 2)))))

(cl-defun xcodebuildserver-check-configuration (&key root workspace scheme)
  "Check if there is a configuration in (as ROOT) (as WORKSPACE) (as SCHEME).
Generates buildServer.json and sets the build_root for cross-file LSP support."
  (when xcodebuildserver--debug
    (message "Checking configuration for: %s|%s" workspace scheme))

  (when (not (xcodebuildserver-does-configuration-file-exist root))
    (xcodebuildserver-safe-mode-line-notification
     :message (format "Generating BSP configuration for: %s|%s"
                           (propertize workspace 'face 'font-lock-builtin-face)
                           (propertize scheme 'face 'font-lock-negation-char-face))
     :seconds 2)

    (let* ((default-directory root)
           (command (format "xcode-build-server config %s -scheme %s"
                           workspace (shell-quote-argument scheme)))
           (proc-buffer (generate-new-buffer " *xcode-build-server-config*"))
           (proc (start-process-shell-command
                  "xcode-build-server-config"
                  proc-buffer
                  command)))
      (when xcodebuildserver--debug
        (message "Running: %s (in %s)" command root))
      ;; Set sentinel to add build_root after config is generated
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (eq (process-status process) 'exit)
           (let ((exit-code (process-exit-status process))
                 (output (when (buffer-live-p proc-buffer)
                           (with-current-buffer proc-buffer
                             (buffer-string)))))
             (if (= exit-code 0)
                 ;; Config generated successfully, now add build_root
                 (if (file-exists-p (expand-file-name "buildServer.json" root))
                     (let ((build-root (xcodebuildserver--get-build-root root workspace scheme)))
                       (when build-root
                         (xcodebuildserver--update-config-with-build-root root build-root))
                       (xcodebuildserver-safe-mode-line-notification
                        :message (format "BSP configured%s"
                                         (if build-root
                                             (format " with build_root: %s"
                                                     (file-name-nondirectory build-root))
                                           ""))
                        :seconds 2))
                   ;; Command succeeded but no file created
                   (message "xcode-build-server config succeeded but no buildServer.json created. Output: %s"
                            (or output "none")))
               ;; Command failed
               (message "xcode-build-server config failed (exit %d): %s"
                        exit-code (or output "no output"))))
           ;; Cleanup buffer
           (when (buffer-live-p proc-buffer)
             (kill-buffer proc-buffer))))))))

(defun xcodebuildserver-does-configuration-file-exist (root)
  "Check if configuration file exists in (as ROOT)."
  (file-exists-p (format "%s/%s" root "buildServer.json")))

(defun xcodebuildserver-config-has-build-root-p (root)
  "Check if the buildServer.json in ROOT has a build_root property."
  (let ((config-file (expand-file-name "buildServer.json" root)))
    (when (file-exists-p config-file)
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (config (json-read-file config-file)))
            (alist-get 'build_root config))
        (error nil)))))

(defun xcodebuildserver-ensure-build-root ()
  "Ensure the current project's buildServer.json has build_root set.
If the config exists but lacks build_root, this will add it."
  (interactive)
  (let* ((root (or (when (fboundp 'xcode-project-project-root)
                     (xcode-project-project-root))
                   default-directory))
         (workspace (when (fboundp 'xcode-project-get-workspace-or-project)
                      (xcode-project-get-workspace-or-project)))
         (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                   xcode-project--current-xcode-scheme))
         (config-file (expand-file-name "buildServer.json" root)))
    (cond
     ((not (file-exists-p config-file))
      (message "No buildServer.json found. Run xcodebuildserver-regenerate-configuration first."))
     ((xcodebuildserver-config-has-build-root-p root)
      (message "buildServer.json already has build_root configured"))
     ((not (and workspace scheme))
      (message "Cannot determine build_root: missing workspace or scheme"))
     (t
      (let ((build-root (xcodebuildserver--get-build-root root workspace scheme)))
        (if build-root
            (progn
              (xcodebuildserver--update-config-with-build-root root build-root)
              (message "Added build_root to buildServer.json: %s" build-root))
          (message "Could not determine build_root. Build the project first.")))))))

(provide 'xcodebuildserver)

;;; xcodebuildserver.el ends here
