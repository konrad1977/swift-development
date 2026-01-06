;;; xcode-project.el --- Xcode project management and build control -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, xcode

;;; Commentary:

;; Xcode project management, build control, and notification system

;;; Code:

(require 'project)
(require 'transient)
(require 'xcodebuildserver)
(require 'swift-project)
(require 'xcode-build-config)
(require 'swift-project-settings)  ; Persistent project settings
(require 'swift-notification)      ; Unified notification system
(require 'xcode-clean nil t)       ; Cleaning utilities (optional)
(require 'periphery nil t)
(require 'periphery-helper nil t)
(require 'swift-cache nil t)  ; Optional - graceful fallback if not available

;; Legacy compatibility - now using swift-notification
(defvar mode-line-hud-available-p swift-notification--mode-line-hud-available-p
  "Whether mode-line-hud is available. Deprecated: use swift-notification module.")

;; Notification backend configuration - now delegates to swift-notification
(defcustom xcode-project-notification-backend 'mode-line-hud
  "Backend to use for displaying build progress and notifications.
Deprecated: Set `swift-notification-backend' instead.
Options:
- \\='mode-line-hud: Use mode-line-hud package (visual mode line updates)
- \\='message: Use Emacs message with colored propertized text
- \\='custom: Use custom function set in `xcode-project-notification-function'"
  :type '(choice (const :tag "Mode Line HUD" mode-line-hud)
                 (const :tag "Minibuffer Messages" message)
                 (const :tag "Custom Function" custom))
  :group 'xcode-project-xcodebuild)

(defvar xcode-project-notification-function nil
  "Custom notification function to use when `xcode-project-notification-backend' is \\='custom.
Function should accept keyword arguments :message, :delay, :seconds, and :reset.")

(defun xcode-project-notify (&rest args)
  "Universal notification function that delegates to swift-notification.
Accepts keyword arguments:
  :message - The message to display
  :delay - Optional delay before showing (for mode-line-hud)
  :seconds - How long to show notification
  :reset - Whether to reset after showing
  :face - Face to apply to message (for message backend)
  :no-redisplay - If t, skip the automatic redisplay (default nil)

This function is kept for backwards compatibility.
Prefer using `swift-notification-send' directly in new code."
  ;; Sync backend setting if user changed xcode-project-notification-backend
  (unless (eq swift-notification-backend xcode-project-notification-backend)
    (setq swift-notification-backend xcode-project-notification-backend))
  (when (and (eq xcode-project-notification-backend 'custom)
             (not swift-notification-function)
             xcode-project-notification-function)
    (setq swift-notification-function xcode-project-notification-function))
  (apply #'swift-notification-send args))

(defun xcode-project-safe-mode-line-update (&rest args)
  "Safely call notification system with update semantics.
This is a compatibility wrapper - prefer using `xcode-project-notify' directly."
  (apply #'xcode-project-notify args))

(defun xcode-project-safe-mode-line-notification (&rest args)
  "Safely call notification system with notification semantics.
This is a compatibility wrapper - prefer using `xcode-project-notify' directly."
  (apply #'xcode-project-notify args))

(defvar-local xcode-project--current-project-root nil
  "Buffer-local project root path for multi-project support.")
(defvar-local xcode-project--previous-project-root nil
  "Buffer-local previous project root for detecting project changes.")
(defvar-local xcode-project--current-xcode-scheme nil
  "Buffer-local Xcode scheme for the current project.")
;; Removed: current-build-settings-json - now using swift-cache
(defvar-local xcode-project--current-buildconfiguration-json-data nil
  "Buffer-local build configuration JSON data.")
(defvar-local xcode-project--current-build-configuration nil
  "Buffer-local build configuration (Debug/Release).")
(defvar-local xcode-project--current-app-identifier nil
  "Buffer-local app bundle identifier.")
(defvar-local xcode-project--current-build-folder nil
  "Buffer-local build output folder path.")
(defvar-local xcode-project--current-is-xcode-project nil
  "Buffer-local flag indicating if current project is an Xcode project.")
(defvar-local xcode-project--current-local-device-id nil
  "Buffer-local device ID for physical device builds.")
(defvar-local xcode-project--current-errors-or-warnings nil
  "Buffer-local errors or warnings from last build.")
(defvar-local xcode-project--last-device-type nil
  "Buffer-local last device type used (:simulator or :device).")
;; Note: swift-development--device-choice is now defined in swift-development.el
;; with a defvaralias for backwards compatibility
(defvar xcode-project--cache-warmed-projects (make-hash-table :test 'equal)
  "Hash table tracking which projects have had their caches warmed.")

(defvar xcode-project--async-processes (make-hash-table :test 'equal)
  "Hash table tracking active async processes by key.")

;; Forward declaration to avoid byte-compile warnings
(defvar xcode-project-debug)

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

;;; Async command execution infrastructure

(cl-defun xcode-project-run-command-async (command callback &key cache-key cache-ttl error-callback)
  "Run shell COMMAND asynchronously and call CALLBACK with parsed JSON result.
CALLBACK receives the parsed JSON data (or nil if parsing fails).
ERROR-CALLBACK is called on process errors if provided.
If CACHE-KEY is provided, results are cached with CACHE-TTL (default 300s)."
  ;; Check cache first
  (when cache-key
    (let ((cached (swift-cache-get cache-key)))
      (when cached
        (when xcode-project-debug
          (message "xcode-project-run-command-async: Cache hit for %s" cache-key))
        (funcall callback cached)
        (cl-return-from xcode-project-run-command-async nil))))

  ;; Kill any existing process with same cache-key
  (when cache-key
    (let ((existing-proc (gethash cache-key xcode-project--async-processes)))
      (when (and existing-proc (process-live-p existing-proc))
        (when xcode-project-debug
          (message "xcode-project-run-command-async: Killing existing process for %s" cache-key))
        (delete-process existing-proc))))

  (let* ((output-buffer (generate-new-buffer " *xcode-async-output*"))
         (proc (make-process
                :name "xcode-async"
                :buffer output-buffer
                :command (list shell-file-name shell-command-switch command)
                :sentinel
                (lambda (process event)
                  (when xcode-project-debug
                    (message "xcode-project-run-command-async: Process event: %s" (string-trim event)))
                  (when (memq (process-status process) '(exit signal))
                    ;; Remove from active processes
                    (when cache-key
                      (remhash cache-key xcode-project--async-processes))
                    (if (and (eq (process-status process) 'exit)
                             (= (process-exit-status process) 0))
                        (let ((json-data nil))
                          (with-current-buffer output-buffer
                            (goto-char (point-min))
                            (condition-case err
                                (setq json-data (json-read))
                              (error
                               (when xcode-project-debug
                                 (message "xcode-project-run-command-async: JSON parse error: %s"
                                          (error-message-string err))))))
                          ;; Cache the result if cache-key provided
                          (when (and cache-key json-data)
                            (swift-cache-set cache-key json-data (or cache-ttl 300)))
                          (kill-buffer output-buffer)
                          (funcall callback json-data))
                      ;; Process failed
                      (kill-buffer output-buffer)
                      (if error-callback
                          (funcall error-callback event)
                        (funcall callback nil))))))))
    ;; Track the process
    (when cache-key
      (puthash cache-key proc xcode-project--async-processes))
    proc))

(defun xcode-project-warm-cache ()
  "Warm up xcode-project caches asynchronously.
This pre-fetches scheme list and build settings in the background
to avoid blocking when they're needed later."
  (interactive)
  (let ((project-root (xcode-project-project-root)))
    (when project-root
      ;; Check if already warming/warmed
      (let ((warm-key (format "%s::cache-warming" project-root)))
        (unless (gethash warm-key xcode-project--cache-warmed-projects)
          (puthash warm-key t xcode-project--cache-warmed-projects)
          (when xcode-project-debug
            (message "xcode-project-warm-cache: Starting async cache warm for %s" project-root))

          ;; 1. Fetch schemes asynchronously
          (xcode-project-get-scheme-list-async
           (lambda (schemes)
             (when xcode-project-debug
               (message "xcode-project-warm-cache: Schemes cached: %s" schemes))
             ;; 2. If we have a scheme, also warm build settings
             (when (and schemes xcode-project--current-xcode-scheme)
               (xcode-project-get-build-settings-json-async
                (lambda (_json)
                  (when xcode-project-debug
                    (message "xcode-project-warm-cache: Build settings cached")))
                :config nil)))))))))

(defun xcode-project-warm-cache-if-needed ()
  "Warm cache if not already done for current project.
Safe to call multiple times - will only warm once per project."
  (let* ((project-root (xcode-project-project-root))
         (warm-key (when project-root (format "%s::cache-warming" project-root))))
    (when (and warm-key (not (gethash warm-key xcode-project--cache-warmed-projects)))
      (xcode-project-warm-cache))))

(defgroup xcode-project-xcodebuild nil
  "REPL."
  :tag "xcode-project-xcodebuild"
  :group 'xcode-project)

(defcustom xcode-project-debug nil
  "Enable debug mode for xcode additions."
  :type 'boolean
  :group 'xcode-project-xcodebuild)
        
(defconst xcode-project-extensions
  '(
    :project ".*\\.xcodeproj$"
    :workspace ".*\\.xcworkspace$"
    )
  "File extensions for Xcode project files.")

;;;###autoload
(defcustom xcode-project-clean-build-ignore-list
  '("ModuleCache.noindex" "SourcePackages")
  "List of directories to ignore when cleaning build folder."
  :type '(repeat string)
  :group 'xcode-project)

(defun xcode-project-get-app-path ()
  "Get the path to Xcode.app/ (with trailing slash) using xcode-select."
  (let ((developer-path (string-trim (shell-command-to-string "xcode-select -p"))))
    (cond
     ;; Extract Xcode.app path from developer path and add trailing slash
     ((string-match "\\(.*/Xcode\\.app\\)" developer-path)
      (concat (match-string 1 developer-path) "/"))
     
     ;; Fallback to standard location if it exists
     ((file-exists-p "/Applications/Xcode.app")
      "/Applications/Xcode.app/")
     
     ;; Return nil if not found
     (t nil))))

(defun xcode-project-accessibility-inspector ()
  "Launch the Accessibility Inspector."
  (interactive)
  (let ((accessibility-inspector-path (concat (xcode-project-get-app-path) "Contents/Applications/Accessibility Inspector.app")))
    (if (file-exists-p accessibility-inspector-path)
        (start-process "Accessibility Inspector" nil "open" accessibility-inspector-path)
      (message "Accessibility Inspector not found at %s" accessibility-inspector-path))))

(defun xcode-project-instruments ()
  "Launch the Instruments application."
  (interactive)
  (let ((instruments-file-path (concat (xcode-project-get-app-path) "Contents/Applications/Instruments.app")))
    (if (file-exists-p instruments-file-path)
        (start-process "Instruments" nil "open" instruments-file-path)
      (message "Instruments not found at %s" instruments-file-path))))

(defun xcode-project-get-extension (type)
  "Get file extension for TYPE (:project or :workspace)."
  (plist-get xcode-project-extensions type))

(defconst xcodeproject-extension (xcode-project-get-extension :project)
  "Xcode project extensions.")

(defconst workspace-extension (xcode-project-get-extension :workspace)
  "Xcode workspace extensions.")

(defun xcode-project-filename-by-extension (extension directory)
  "Get filename (without extension) for first file matching EXTENSION in DIRECTORY."
  (when-let* ((files (directory-files directory t extension))
              (first-match (car files)))
    (file-name-sans-extension (file-name-nondirectory first-match))))

(defun xcode-project-directory-contains-p (extension directory)
  "Check if DIRECTORY contain files matching EXTENSION."
  (consp (directory-files directory nil extension)))

(defun xcode-project-project-directory-p (directory)
  "Check if xcodeproj file exists in DIRECTORY or immediate subdirectories."
  (or (xcode-project-directory-contains-p xcodeproject-extension directory)
      (cl-some (lambda (dir)
                 (let ((full-dir (expand-file-name dir directory)))
                   (and (file-directory-p full-dir)
                        (not (member dir '("." "..")))
                        (xcode-project-directory-contains-p xcodeproject-extension full-dir))))
               (directory-files directory))))

(defun xcode-project-workspace-directory-p (directory)
  "Check if xcworkspace file exists in DIRECTORY."
  (xcode-project-directory-contains-p workspace-extension directory))

(defun xcode-project-find-ancestor-or-self-directory (predicate &optional directory)
  "Find first ancestor directory (including DIRECTORY itself) where PREDICATE returns non-nil.
If DIRECTORY is nil, use `default-directory'."
  (let ((dir (expand-file-name (or directory default-directory))))
    (cond ((funcall predicate dir) dir)
          ((or (string-equal dir "/")
               (string-equal dir (directory-file-name dir)))
           nil)
          (t (xcode-project-find-ancestor-or-self-directory
              predicate
              (file-name-directory (directory-file-name dir)))))))

(defun xcode-project-find-xcode-project-directory (&optional directory)
  "Try to find xcode project in DIRECTORY or its subdirectories."
  (when-let* ((start-dir (or directory default-directory))
              (found-dir (xcode-project-find-ancestor-or-self-directory 'xcode-project-project-directory-p start-dir)))
    (if (xcode-project-directory-contains-p xcodeproject-extension found-dir)
        found-dir
      (cl-some (lambda (dir)
                 (let ((full-dir (expand-file-name dir found-dir)))
                   (and (file-directory-p full-dir)
                        (not (member dir '("." "..")))
                        (xcode-project-directory-contains-p xcodeproject-extension full-dir)
                        full-dir)))
               (directory-files found-dir)))))

(defun xcode-project-find-workspace-directory (&optional directory)
  "Try to find xcode workspace in DIRECTORY or its ancestors."
  (xcode-project-find-ancestor-or-self-directory 'xcode-project-workspace-directory-p directory))

(defun xcode-project-workspace-name ()
  "Get the workspace name in current or ancestor directories."
  (when-let* ((dir (xcode-project-find-workspace-directory)))
    (xcode-project-filename-by-extension workspace-extension dir)))

(defun xcode-project-project-name ()
  "Get the project name in current or ancestor directories."
  (when-let* ((dir (xcode-project-find-xcode-project-directory)))
    (xcode-project-filename-by-extension xcodeproject-extension dir)))

(defun xcode-project-list-xcscheme-files (folder)
  "List the names of '.xcscheme' files in the xcshareddata/xcshemes subfolder of FOLDER.
Falls back to xcuserdata schemes if no shared schemes are found."
  (let ((xcscheme-names '()))
    (setq folder (file-name-as-directory (expand-file-name folder)))
    ;; Set the schemes folder path
    (setq xcschemes-folder (concat folder "xcshareddata/xcschemes/"))

    (when xcode-project-debug
      (message "Searching for shared schemes in folder: %s" xcschemes-folder))

    ;; First try shared schemes
    (when (file-directory-p xcschemes-folder)
      (dolist (item (directory-files xcschemes-folder t "\\.xcscheme$"))
        (when (file-regular-p item)
          (let ((scheme-name (file-name-sans-extension (file-name-nondirectory item))))
            (when xcode-project-debug
              (message "Found shared scheme: %s" scheme-name))
            (push scheme-name xcscheme-names)))))

    ;; Fallback: If no shared schemes found, look in xcuserdata
    (when (and (null xcscheme-names) (file-directory-p folder))
      (when xcode-project-debug
        (message "No shared schemes found, searching xcuserdata..."))
      (let ((xcuserdata-folder (concat folder "xcuserdata/")))
        (when (file-directory-p xcuserdata-folder)
          ;; Find all user-specific scheme directories
          (dolist (user-dir (directory-files xcuserdata-folder t "^[^.].*\\.xcuserdatad$"))
            (let ((user-schemes-folder (concat user-dir "/xcschemes/")))
              (when (file-directory-p user-schemes-folder)
                (dolist (item (directory-files user-schemes-folder t "\\.xcscheme$"))
                  (when (file-regular-p item)
                    (let ((scheme-name (file-name-sans-extension (file-name-nondirectory item))))
                      (when xcode-project-debug
                        (message "Found user scheme: %s" scheme-name))
                      (push scheme-name xcscheme-names))))))))))

    (setq xcscheme-names (nreverse xcscheme-names))

    (when xcode-project-debug
      (message "Final scheme list: %s" xcscheme-names))

    xcscheme-names))

(defun xcode-project-list-scheme-files ()
  "List the names of '.xcscheme' files in the xcshareddata/xcschemes subfolder of the current Xcode project or workspace directory."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "scheme-files"))))
    ;; Use cache for scheme files
    (swift-cache-with cache-key 600  ; Cache for 10 minutes
      (let* ((default-directory (or project-root default-directory))
             (workspace-directory (xcode-project-find-workspace-directory))
             (workspace-name (xcode-project-workspace-name))
             (project-directory (xcode-project-find-xcode-project-directory))
             (project-name (xcode-project-project-name))
             (workspace-path (when workspace-directory
                              (concat (file-name-as-directory workspace-directory)
                                    workspace-name
                                    ".xcworkspace/")))
             (project-path (when project-directory
                            (concat (file-name-as-directory project-directory)
                                  project-name
                                  ".xcodeproj/"))))

        (when xcode-project-debug
          (message "xcode-project-list-scheme-files:
Workspace directory: %s
Workspace name: %s
Workspace path: %s
Project directory: %s
Project name: %s
Project path: %s"
                   workspace-directory workspace-name workspace-path
                   project-directory project-name project-path))

        ;; Try workspace first (preferred for CocoaPods projects), then project
        (let ((schemes (or (when workspace-path
                             (xcode-project-list-xcscheme-files workspace-path))
                           (when project-path
                             (xcode-project-list-xcscheme-files project-path)))))
          ;; Return schemes or empty list if none found
          ;; NOTE: xcodebuild -list fallback removed to avoid blocking Emacs
          ;; If schemes are not found via file detection, user should check project setup
          (or schemes
              (progn
                (when xcode-project-debug
                  (message "Warning: No scheme files found. Check xcshareddata/xcschemes/ exists."))
                nil)))))))

(defun xcode-project-run-command-and-get-json (command)
  "Run a shell COMMAND and return the JSON output.
Returns nil if command fails or produces invalid JSON."
  (let* ((json-output (shell-command-to-string command)))
    (when xcode-project-debug
      (message "Command: %s" command)
      (message "JSON output length: %d" (length json-output))
      (message "JSON output preview: %s" (substring json-output 0 (min 200 (length json-output)))))
    ;; Check if output is empty or whitespace only
    (if (or (string-empty-p json-output)
            (string-match-p "\\`[[:space:]]*\\'" json-output))
        (progn
          (when xcode-project-debug
            (message "Command returned empty output"))
          nil)
      (condition-case err
          (json-read-from-string json-output)
        (error
         (when xcode-project-debug
           (message "JSON parsing error: %s" (error-message-string err))
           (message "Full JSON output: %s" json-output))
         nil)))))

(defun xcode-project-get-schemes-from-xcodebuild ()
  "Get list of schemes using xcodebuild -list command as fallback.
DEPRECATED: This function blocks Emacs. Use `xcode-project-get-schemes-from-xcodebuild-async' instead."
  (when xcode-project-debug
    (message "Falling back to xcodebuild -list for scheme detection (BLOCKING)"))
  (condition-case err
      (let* ((project-dir (or (xcode-project-project-root) default-directory))
             (json-data (let ((default-directory project-dir))
                          (xcode-project-run-command-and-get-json "xcrun xcodebuild -list -json 2>/dev/null"))))
        (when json-data
          (xcode-project--parse-schemes-from-json json-data)))
    (error
     (when xcode-project-debug
       (message "Error running xcodebuild -list: %s" (error-message-string err)))
     nil)))

(defun xcode-project--parse-schemes-from-json (json-data)
  "Parse scheme list from JSON-DATA returned by xcodebuild -list."
  (when json-data
    (let-alist json-data
      (cond
       ;; Workspace case
       (.workspace.schemes
        (when xcode-project-debug
          (message "Found schemes via xcodebuild (workspace): %s" .workspace.schemes))
        (append .workspace.schemes nil))  ; Convert vector to list
       ;; Project case
       (.project.schemes
        (when xcode-project-debug
          (message "Found schemes via xcodebuild (project): %s" .project.schemes))
        (append .project.schemes nil))  ; Convert vector to list
       (t
        (when xcode-project-debug
          (message "No schemes found in xcodebuild output"))
        nil)))))

(defun xcode-project-get-schemes-from-xcodebuild-async (callback)
  "Get list of schemes using xcodebuild -list asynchronously.
CALLBACK is called with the list of schemes (or nil if none found)."
  (when xcode-project-debug
    (message "Fetching schemes via xcodebuild -list (async)"))
  (let* ((project-dir (or (xcode-project-project-root) default-directory))
         (cache-key (swift-cache-project-key project-dir "xcodebuild-schemes")))
    (xcode-project-run-command-async
     (format "cd %s && xcrun xcodebuild -list -json 2>/dev/null"
             (shell-quote-argument project-dir))
     (lambda (json-data)
       (let ((schemes (xcode-project--parse-schemes-from-json json-data)))
         (funcall callback schemes)))
     :cache-key cache-key
     :cache-ttl 600)))

(defun xcode-project--build-settings-cache-key (config sdk)
  "Generate cache key for build settings with CONFIG and SDK."
  (let ((project-root (xcode-project-project-root))
        (config-key (or config "default"))
        (sdk-key (or sdk "default")))
    (when (fboundp 'swift-cache-project-key)
      (swift-cache-project-key project-root
                               (format "build-settings-%s-%s-%s"
                                       (or xcode-project--current-xcode-scheme "default")
                                       config-key
                                       sdk-key)))))

(defun xcode-project--build-settings-command (config sdk)
  "Generate xcodebuild command for build settings with CONFIG and SDK."
  (let ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme ""))))
    (format "xcrun xcodebuild %s -scheme %s -showBuildSettings %s %s -skipPackagePluginValidation -skipMacroValidation -json 2>/dev/null"
            (xcode-project-get-workspace-or-project)
            (shell-quote-argument scheme-name)
            (if config (format "-configuration %s" (shell-quote-argument config)) "")
            (if sdk (format "-sdk %s" sdk) ""))))

(cl-defun xcode-project-get-build-settings-json (&key config sdk)
  "Get build settings from xcodebuild CONFIG and SDK.
If CONFIG is nil, lets the scheme choose its default configuration.
SDK: Optional SDK (e.g., iphonesimulator, iphoneos).
NOTE: This blocks Emacs on first call. Use async version for background loading."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (xcode-project--build-settings-cache-key config sdk)))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((default-directory (or project-root default-directory)))
            (xcode-project-run-command-and-get-json
             (xcode-project--build-settings-command config sdk))))
      ;; Fallback when swift-cache not available
      (let ((default-directory (or project-root default-directory)))
        (xcode-project-run-command-and-get-json
         (xcode-project--build-settings-command config sdk))))))

(cl-defun xcode-project-get-build-settings-json-async (callback &key config sdk)
  "Get build settings asynchronously with CONFIG and SDK.
CALLBACK is called with the parsed JSON result.
Results are cached for 30 minutes."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (xcode-project--build-settings-cache-key config sdk))
         (default-directory (or project-root default-directory)))
    (xcode-project-run-command-async
     (xcode-project--build-settings-command config sdk)
     callback
     :cache-key cache-key
     :cache-ttl 1800)))

(defun xcode-project-product-name ()
  "Get product name from build settings or derive from scheme/project name."
  (let* ((config (or xcode-project--current-build-configuration "Debug"))
         (json (xcode-project-get-build-settings-json :config config)))
    (or
     ;; First try: get from build settings
     (when (and json (> (length json) 0))
       (let-alist (seq-elt json 0)
         (xcode-project--clean-display-name .buildSettings.PRODUCT_NAME)))
     ;; Second try: derive from scheme name
     (when xcode-project--current-xcode-scheme
       (let ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" xcode-project--current-xcode-scheme)))
         (if (string-match "^\\([^(]+\\)\\s-*(.*)" scheme-name)
             ;; Scheme has format "Name (Config)" - extract Name
             (string-trim (match-string 1 scheme-name))
           ;; Use full scheme name
           scheme-name)))
     ;; Last resort: use project directory name
     (when-let ((project-root (xcode-project-project-root)))
       (file-name-nondirectory (directory-file-name project-root)))
     ;; Ultimate fallback
     "Unknown")))

(defun xcode-project--extract-bundle-id-from-json (json)
  "Extract bundle identifier from build settings JSON."
  (when (and json (> (length json) 0))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_BUNDLE_IDENTIFIER)))

(defun xcode-project-get-bundle-identifier (config)
  "Get bundle identifier using CONFIG.
If CONFIG is nil, uses the scheme's default configuration.
Uses fast methods (cache, Info.plist) first, triggers async fetch if needed.
NEVER blocks Emacs with synchronous xcodebuild calls."
  (let* ((cache-key (xcode-project--build-settings-cache-key config nil))
         ;; Try cache first
         (cached-json (when cache-key (swift-cache-get cache-key)))
         (bundle-id (xcode-project--extract-bundle-id-from-json cached-json)))
    (if (and bundle-id (not (string-empty-p bundle-id)))
        bundle-id
      ;; Try to read from Info.plist files (fast, no xcodebuild)
      (let ((plist-id (xcode-project--get-bundle-id-from-plist)))
        (if (and plist-id (not (string-empty-p plist-id)))
            plist-id
          ;; Trigger async fetch for next time, return fallback now
          (when xcode-project-debug
            (message "Bundle ID not cached, triggering async fetch"))
          (xcode-project-get-bundle-identifier-async
           config
           (lambda (id)
             (when (and id (not (string-equal id "com.example.app")))
               (setq xcode-project--current-app-identifier id)
               (when xcode-project-debug
                 (message "Async bundle ID fetch complete: %s" id)))))
          ;; Return fallback immediately
          "com.example.app")))))

(defun xcode-project--get-bundle-id-from-plist ()
  "Try to get bundle identifier from project files.
Checks xcconfig first (scheme-aware), then Info.plist, then pbxproj.
This is a fast method that doesn't require xcodebuild."
  (let* ((project-root (xcode-project-project-root))
         (scheme (or xcode-project--current-xcode-scheme ""))
         (scheme-base (if (string-match "^\\([^(]+\\)" scheme)
                          (string-trim (match-string 1 scheme))
                        scheme))
         ;; Extract environment from scheme, e.g., "Bruce (Staging)" -> "Staging"
         (scheme-env (when (string-match "(\\([^)]+\\))" scheme)
                       (match-string 1 scheme))))
    (when project-root
      (or
       ;; First: Try xcconfig file matching the scheme environment
       (when scheme-env
         (xcode-project--get-bundle-id-from-xcconfig project-root scheme-env))
       ;; Second: Try Info.plist files
       (let ((plist-files (directory-files-recursively
                           project-root
                           "Info\\.plist$"
                           nil
                           (lambda (dir)
                             ;; Skip build directories and Pods
                             (not (or (string-match-p "/\\.build/" dir)
                                      (string-match-p "/Build/" dir)
                                      (string-match-p "/DerivedData/" dir)
                                      (string-match-p "/Pods/" dir)
                                      (string-match-p "/Frameworks/" dir)))))))
         (cl-loop for plist in plist-files
                  for bundle-id = (xcode-project--read-bundle-id-from-plist plist)
                  when (and bundle-id
                            (or (string-match-p (regexp-quote scheme-base) plist)
                                (not (string-match-p "Tests" plist))))
                  return bundle-id))
       ;; Last: Fallback to pbxproj (not scheme-aware, will get first match)
       (xcode-project--get-bundle-id-from-pbxproj project-root)))))

(defun xcode-project--get-bundle-id-from-xcconfig (project-root env-name)
  "Try to get bundle identifier from xcconfig file for ENV-NAME in PROJECT-ROOT.
Looks for files like Staging.xcconfig, Production.xcconfig, etc."
  (let* ((config-dirs (list
                       (expand-file-name "Configs" project-root)
                       (expand-file-name (concat (xcode-project-project-name) "/Configs") project-root)
                       project-root))
         (config-file nil))
    ;; Find the xcconfig file
    (cl-loop for dir in config-dirs
             for file = (expand-file-name (concat env-name ".xcconfig") dir)
             when (file-exists-p file)
             do (setq config-file file)
             and return nil)
    ;; Read APP_BUNDLE_ID from the config file
    (when config-file
      (xcode-project--read-bundle-id-from-xcconfig config-file))))

(defun xcode-project--read-bundle-id-from-xcconfig (xcconfig-file)
  "Read APP_BUNDLE_ID from XCCONFIG-FILE."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents xcconfig-file)
        (goto-char (point-min))
        (when (re-search-forward "^APP_BUNDLE_ID\\s-*=\\s-*\\(.+\\)$" nil t)
          (let ((id (string-trim (match-string 1))))
            (unless (or (string-match-p "\\$(" id) (string-empty-p id))
              id))))
    (error nil)))

(defun xcode-project--get-bundle-id-from-pbxproj (project-root)
  "Try to get bundle identifier from pbxproj file in PROJECT-ROOT.
Note: This is not scheme-aware and will return the first match found."
  (let ((pbxproj-files (directory-files project-root t "\\.xcodeproj$")))
    (cl-loop for proj-dir in pbxproj-files
             for pbxproj = (expand-file-name "project.pbxproj" proj-dir)
             when (file-exists-p pbxproj)
             return (xcode-project--read-bundle-id-from-pbxproj pbxproj))))

(defun xcode-project--read-bundle-id-from-pbxproj (pbxproj-file)
  "Read PRODUCT_BUNDLE_IDENTIFIER from PBXPROJ-FILE."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents pbxproj-file)
        (goto-char (point-min))
        ;; First, try to find APP_BUNDLE_ID (custom variable used by some projects)
        (let ((found-id nil))
          (when (re-search-forward "APP_BUNDLE_ID\\s-*=\\s-*\\([^;]+\\);" nil t)
            (let ((id (string-trim (match-string 1))))
              (unless (or (string-match-p "\\$(" id) (string-empty-p id))
                (setq found-id id))))
          ;; If not found, look for direct PRODUCT_BUNDLE_IDENTIFIER
          (unless found-id
            (goto-char (point-min))
            (while (and (not found-id)
                        (re-search-forward "PRODUCT_BUNDLE_IDENTIFIER\\s-*=\\s-*\"?\\([^;\"]+\\)\"?;" nil t))
              (let ((id (string-trim (match-string 1))))
                ;; Skip variables and explicit test target bundle IDs
                ;; Note: Only skip IDs ending with Tests/Test, not containing "test" anywhere
                (unless (or (string-match-p "\\$(" id)
                            (string-match-p "Tests$" id)
                            (string-match-p "\\.Tests\\." id)
                            (string-match-p "UITests$" id))
                  (setq found-id id)))))
          found-id))
    (error nil)))

(defun xcode-project--read-bundle-id-from-plist (plist-file)
  "Read CFBundleIdentifier from PLIST-FILE."
  (when (file-exists-p plist-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents plist-file)
          (goto-char (point-min))
          ;; Look for CFBundleIdentifier
          (when (re-search-forward "<key>CFBundleIdentifier</key>\\s-*<string>\\([^<]+\\)</string>" nil t)
            (let ((id (match-string 1)))
              ;; Skip if it's a variable like $(PRODUCT_BUNDLE_IDENTIFIER)
              (unless (string-match-p "\\$(" id)
                id))))
      (error nil))))

(defun xcode-project-get-bundle-identifier-async (config callback)
  "Get bundle identifier asynchronously using CONFIG.
CALLBACK is called with the bundle identifier string."
  (xcode-project-get-build-settings-json-async
   (lambda (json)
     (let ((bundle-id (xcode-project--extract-bundle-id-from-json json)))
       (funcall callback (if (and bundle-id (not (string-empty-p bundle-id)))
                             bundle-id
                           "com.example.app"))))
   :config config))

(cl-defun xcode-project-build-menu (&key title list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) (cons item item)) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(defun xcode-project-parse-build-folder (directory)
  "Parse build folders from DIRECTORY.
Returns a list of folder names, excluding hidden folders."
  (when xcode-project-debug
    (message "parse-build-folder: Checking directory: %s" directory)
    (message "parse-build-folder: Directory exists: %s" (file-directory-p directory)))
  (if (file-directory-p directory)
      (let* ((all-files (directory-files directory nil "^[^.].*" t))
             (folders (cl-remove-if-not
                      (lambda (folder)
                        (let ((full-path (expand-file-name folder directory))
                              (is-dir (file-directory-p (expand-file-name folder directory)))
                              (not-dot (not (member folder '("." "..")))))
                          (when xcode-project-debug
                            (message "parse-build-folder: Checking %s -> dir:%s not-dot:%s" folder is-dir not-dot))
                          (and is-dir not-dot)))
                      all-files)))
        (when xcode-project-debug
          (message "parse-build-folder: All files: %s" all-files)
          (message "parse-build-folder: Filtered folders: %s" folders))
        folders)
    (progn
      (when xcode-project-debug
        (message "parse-build-folder: Directory %s does not exist" directory))
      nil)))


(defun xcode-project--handle-scheme-selection (schemes)
  "Handle selection from SCHEMES list.
Sets `xcode-project--current-xcode-scheme' and loads settings."
  (cond
   ;; No schemes found at all
   ((or (null schemes) (= (length schemes) 0))
    (xcode-project-notify
     :message (propertize "No shared schemes found! Please share schemes in Xcode." 'face 'error)
     :seconds 5
     :reset t)
    nil)

   ;; Exactly one scheme - use it automatically
   ((= (length schemes) 1)
    (let ((scheme (car schemes)))
      (when scheme
        (setq xcode-project--current-xcode-scheme scheme)
        (when (fboundp 'swift-project-settings-load-for-scheme)
          (swift-project-settings-load-for-scheme (xcode-project-project-root) scheme))
        (xcode-project-notify
         :message (format "Selected scheme: %s"
                          (propertize (xcode-project--clean-display-name scheme) 'face 'success))
         :seconds 2
         :reset t))
      scheme))

   ;; Multiple schemes - prompt user
   (t
    (xcode-project-notify :message "Choose scheme...")
    (let ((selected (xcode-project-build-menu :title "Choose scheme: " :list schemes)))
      (when selected
        (setq xcode-project--current-xcode-scheme selected)
        (when (fboundp 'swift-project-settings-load-for-scheme)
          (swift-project-settings-load-for-scheme (xcode-project-project-root) selected))
        (xcode-project-notify
         :message (format "Selected scheme: %s"
                          (propertize (xcode-project--clean-display-name selected) 'face 'success))
         :seconds 2
         :reset t))
      selected))))

(defun xcode-project-scheme ()
  "Get the xcode scheme if set otherwise prompt user.
This uses fast file-based detection. If no schemes found via files,
automatically falls back to `xcodebuild -list` to find auto-generated schemes."
  (unless xcode-project--current-xcode-scheme
    (when xcode-project-debug
      (message "xcode-project-scheme - Starting scheme detection..."))
    (xcode-project-notify :message "Loading schemes...")
    (let ((schemes (xcode-project-get-scheme-list)))
      (when xcode-project-debug
        (message "xcode-project-scheme - Found %d schemes from files: %s" (length schemes) schemes))
      (if schemes
          (xcode-project--handle-scheme-selection schemes)
        ;; No schemes from files - try xcodebuild -list as fallback
        (when xcode-project-debug
          (message "No scheme files found, falling back to xcodebuild -list..."))
        (xcode-project-notify :message "Fetching schemes from xcodebuild...")
        (let ((xcodebuild-schemes (xcode-project-get-schemes-from-xcodebuild)))
          (when xcode-project-debug
            (message "xcode-project-scheme - Found %d schemes from xcodebuild: %s"
                     (length xcodebuild-schemes) xcodebuild-schemes))
          (if xcodebuild-schemes
              (xcode-project--handle-scheme-selection xcodebuild-schemes)
            (xcode-project-notify
             :message (propertize "No schemes found! Share schemes in Xcode." 'face 'error)
             :seconds 3
             :reset t))))))
  (if (not xcode-project--current-xcode-scheme)
      (error "No scheme selected. Check Xcode: Product > Scheme > Manage Schemes")
    (shell-quote-argument xcode-project--current-xcode-scheme)))

(defun xcode-project-fetch-schemes ()
  "Fetch available schemes asynchronously and prompt for selection.
Use this when scheme files are not found in the project."
  (interactive)
  (xcode-project-notify :message "Fetching schemes from xcodebuild...")
  (xcode-project-get-scheme-list-async
   (lambda (schemes)
     (if schemes
         (progn
           (xcode-project--handle-scheme-selection schemes)
           (when xcode-project--current-xcode-scheme
             (message "Scheme set to: %s" xcode-project--current-xcode-scheme)))
       (xcode-project-notify
        :message (propertize "No schemes found! Share schemes in Xcode." 'face 'error)
        :seconds 5
        :reset t)
       (message "No schemes found. In Xcode: Product > Scheme > Manage Schemes, check 'Shared'")))))

(defun xcode-project-scheme-display-name ()
  "Get the scheme name for display purposes (without shell escaping).
This should be used for user-facing messages and UI, not for shell commands."
  (xcode-project-scheme)  ; Ensure scheme is loaded
  (if (not xcode-project--current-xcode-scheme)
      (error "No scheme selected")
    (xcode-project--clean-display-name xcode-project--current-xcode-scheme)))

(defun xcode-project-fetch-or-load-build-configuration ()
  "Get the build configuration from the scheme file."
  (unless xcode-project--current-build-configuration
    (xcode-project-notify :message "Fetching build configuration..." :seconds 2)
    ;; Ensure scheme is loaded first
    (xcode-project-scheme)
    (let* ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme "")))
           (project-root (xcode-project-project-root))
           (xcodeproj-dirs (directory-files project-root t "\\.xcodeproj$"))
           (xcodeproj-dir (car xcodeproj-dirs))
           (scheme-file (when xcodeproj-dir
                         (format "%s/xcshareddata/xcschemes/%s.xcscheme"
                                xcodeproj-dir scheme-name))))
      (if (and scheme-file (file-exists-p scheme-file))
          (with-temp-buffer
            (insert-file-contents scheme-file)
            (goto-char (point-min))
            ;; Look for LaunchAction buildConfiguration first, then fallback to TestAction
            (if (re-search-forward "<LaunchAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                (setq xcode-project--current-build-configuration (match-string 1))
              (goto-char (point-min))
              (if (re-search-forward "<TestAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                  (setq xcode-project--current-build-configuration (match-string 1))
                (setq xcode-project--current-build-configuration "Debug"))))
        (setq xcode-project--current-build-configuration "Debug"))

      ;; Save build-configuration to settings asynchronously
      (when (and (fboundp 'swift-project-settings-capture-from-variables)
                 (fboundp 'xcode-project-project-root))
        (let ((current-buf (current-buffer))
              (project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf root)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (swift-project-settings-capture-from-variables root))))
                                 current-buf project-root))))

      ;; Only show notification if we successfully loaded a build config
      (when xcode-project--current-build-configuration
        (xcode-project-notify
         :message (format "Build config: %s"
                          (propertize xcode-project--current-build-configuration 'face 'font-lock-keyword-face))
         :seconds 2
         :reset t))))
  xcode-project--current-build-configuration)

(defun xcode-project-fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless xcode-project--current-app-identifier
    (xcode-project-notify :message "Fetching app identifier..." :seconds 2)
    ;; Ensure scheme is loaded first
    (xcode-project-scheme)
    (let ((config (xcode-project-fetch-or-load-build-configuration)))
      (when xcode-project-debug
        (message "xcode-project-fetch-or-load-app-identifier - scheme: %s, config: %s"
                 xcode-project--current-xcode-scheme config))
      ;; Use nil config to let scheme choose its own configuration
      (setq xcode-project--current-app-identifier (xcode-project-get-bundle-identifier nil))
      (when xcode-project-debug
        (message "xcode-project-fetch-or-load-app-identifier - bundle ID: %s"
                 xcode-project--current-app-identifier))

      ;; Save app-identifier to settings asynchronously
      (when (and (fboundp 'swift-project-settings-capture-from-variables)
                 (fboundp 'xcode-project-project-root))
        (let ((current-buf (current-buffer))
              (project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf root)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (swift-project-settings-capture-from-variables root))))
                                 current-buf project-root))))

      (xcode-project-notify
       :message (format "App ID: %s"
                        (propertize xcode-project--current-app-identifier 'face 'font-lock-string-face))
       :seconds 2
       :reset t)))
  xcode-project--current-app-identifier)

(defun xcode-project-get-scheme-list ()
  "Get list of project schemes from xcscheme files.
Does NOT fall back to xcodebuild -list to avoid blocking Emacs.
Use `xcode-project-get-scheme-list-async' if you need xcodebuild fallback."
  (or (xcode-project-list-scheme-files)
      ;; Don't use xcodebuild -list as it blocks Emacs
      ;; Instead, return nil and let async functions handle the fallback
      (progn
        (when xcode-project-debug
          (message "No scheme files found. Use xcode-project-fetch-schemes to load via xcodebuild."))
        nil)))

(defun xcode-project-get-scheme-list-async (callback)
  "Get list of project schemes asynchronously.
First tries xcscheme files (fast), then falls back to xcodebuild -list (async).
CALLBACK is called with the list of schemes."
  (let ((file-schemes (xcode-project-list-scheme-files)))
    (if file-schemes
        ;; Found schemes via file detection - call callback immediately
        (funcall callback file-schemes)
      ;; Fallback to xcodebuild -list async
      (when xcode-project-debug
        (message "No scheme files found, trying xcodebuild -list async..."))
      (xcode-project-get-schemes-from-xcodebuild-async callback))))

(cl-defun xcode-project-build-folder (&key (device-type :device))
  "Get build folder using fast auto-detection (no xcodebuild blocking).
Falls back to async xcodebuild fetch if auto-detection fails."
  (when (or (not xcode-project--current-build-folder)
            (not (eq device-type xcode-project--last-device-type)))
    (let* ((scheme (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme "")))
           (config (xcode-project-fetch-or-load-build-configuration))
           (target-suffix (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
           (cache-key (when (fboundp 'swift-cache-project-key)
                        (swift-cache-project-key
                         (xcode-project-project-root)
                         (format "build-folder-%s-%s-%s" scheme config target-suffix)))))

      ;; Try cached result first
      (let ((cached-folder (when (and cache-key (fboundp 'swift-cache-get))
                             (swift-cache-get cache-key))))
        (if (and cached-folder (file-exists-p cached-folder))
            (setq xcode-project--current-build-folder cached-folder)
          ;; Try fast auto-detection first (no xcodebuild call)
          (let ((base-build-products-dir (xcode-project-get-build-products-directory)))
            (when base-build-products-dir
              (let ((detected (xcode-project-auto-detect-build-folder
                               :build-products-dir base-build-products-dir
                               :scheme scheme
                               :config config
                               :device-type device-type
                               :target-suffix target-suffix)))
                (when detected
                  (setq xcode-project--current-build-folder
                        (if (file-name-absolute-p detected)
                            detected
                          (concat base-build-products-dir detected "/"))))))
            ;; If auto-detection failed and we have no folder, try async fetch
            (unless xcode-project--current-build-folder
              (when xcode-project-debug
                (message "Auto-detection failed, triggering async build settings fetch"))
              ;; Trigger async fetch for next time
              (xcode-project-build-folder-async
               (lambda (folder)
                 (when (and folder (not xcode-project--current-build-folder))
                   (setq xcode-project--current-build-folder folder)
                   (when xcode-project-debug
                     (message "Async build folder fetch complete: %s" folder))))
               :device-type device-type)))))

      ;; Cache the result if we found something
      (when (and cache-key (fboundp 'swift-cache-set) xcode-project--current-build-folder)
        (swift-cache-set cache-key xcode-project--current-build-folder 1800))

      (setq xcode-project--last-device-type device-type)

      ;; Save build-folder to settings asynchronously
      (when (and xcode-project--current-build-folder
                 (fboundp 'swift-project-settings-capture-from-variables)
                 (fboundp 'xcode-project-project-root))
        (let ((current-buf (current-buffer))
              (project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf root)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (swift-project-settings-capture-from-variables root))))
                                 current-buf project-root))))

      (when xcode-project-debug
        (message "xcode-project-build-folder: scheme=%s config=%s device=%s folder=%s"
                 scheme config device-type xcode-project--current-build-folder))))
  xcode-project--current-build-folder)

(cl-defun xcode-project-build-folder-async (callback &key (device-type :device))
  "Get build folder asynchronously using xcodebuild.
CALLBACK is called with the folder path when complete."
  (let* ((scheme (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme "")))
         (config (or xcode-project--current-build-configuration "Debug"))
         (sdk (if (eq device-type :simulator) "iphonesimulator" "iphoneos"))
         (target-suffix sdk))
    (xcode-project-get-build-settings-json-async
     (lambda (json)
       (let* ((xcode-built-products-dir (when (and json (> (length json) 0))
                                          (let-alist (seq-elt json 0)
                                            .buildSettings.BUILT_PRODUCTS_DIR)))
              (base-build-products-dir (xcode-project-get-build-products-directory))
              (result-folder
               (if base-build-products-dir
                   (let ((config-folder-name (when xcode-built-products-dir
                                               (file-name-nondirectory
                                                (directory-file-name xcode-built-products-dir)))))
                     (if (and config-folder-name
                              (file-directory-p (concat base-build-products-dir config-folder-name)))
                         (file-name-as-directory (concat base-build-products-dir config-folder-name))
                       ;; Try auto-detect as fallback
                       (let ((detected (xcode-project-auto-detect-build-folder
                                       :build-products-dir base-build-products-dir
                                       :scheme scheme
                                       :config config
                                       :device-type device-type
                                       :target-suffix target-suffix)))
                         (when detected
                           (if (file-name-absolute-p detected)
                               detected
                             (concat base-build-products-dir detected "/"))))))
                 ;; Use xcodebuild path directly
                 (when (and xcode-built-products-dir (file-directory-p xcode-built-products-dir))
                   (file-name-as-directory xcode-built-products-dir)))))
         ;; Cache the result
         (when result-folder
           (let ((cache-key (when (fboundp 'swift-cache-project-key)
                             (swift-cache-project-key
                              (xcode-project-project-root)
                              (format "build-folder-%s-%s-%s" scheme config target-suffix)))))
             (when (and cache-key (fboundp 'swift-cache-set))
               (swift-cache-set cache-key result-folder 1800))))
         (funcall callback result-folder)))
     :config config :sdk sdk)))

(defun xcode-project-get-build-products-directory ()
  "Get the base build products directory path."
  (let* ((project-root (file-name-as-directory (xcode-project-project-root)))
         (local-build-dir (concat project-root ".build/Build/Products/")))
    (if (file-exists-p local-build-dir)
        local-build-dir
      ;; Fallback to derived data path
      (let ((derived-path (xcode-project-derived-data-path)))
        (when derived-path
          (cond
           ;; If derived path ends with .build, append Build/Products
           ((string-suffix-p ".build" derived-path)
            (concat derived-path "/Build/Products/"))
           ;; Otherwise use the path as-is with Build/Products
           (t (concat (file-name-as-directory derived-path) "Build/Products/"))))))))

(cl-defun xcode-project-auto-detect-build-folder (&key build-products-dir scheme config device-type target-suffix)
  "Intelligently auto-detect the correct build folder.
Returns nil if build-products-dir is nil or doesn't exist."
  (when (and build-products-dir (file-directory-p build-products-dir))
    (let* ((default-directory build-products-dir)
           (all-folders (xcode-project-parse-build-folder default-directory)))

      (when xcode-project-debug
        (message "Auto-detecting build folder: scheme=%s config=%s target=%s" scheme config target-suffix)
        (message "Available folders: %s" all-folders))

      (or
       ;; Strategy 1: Look for exact match with scheme-config-platform
       (xcode-project-find-exact-build-folder all-folders scheme config target-suffix)

       ;; Strategy 2: Look for config-platform match
       (xcode-project-find-config-platform-folder all-folders config target-suffix)

       ;; Strategy 3: Look for platform-only match
       (xcode-project-find-platform-folder all-folders target-suffix)

       ;; Strategy 4: Look for any folder containing the scheme name
       (xcode-project-find-scheme-folder all-folders scheme)

       ;; Strategy 5: Interactive fallback
       (xcode-project-interactive-build-folder-selection all-folders target-suffix)))))

(defun xcode-project-find-exact-build-folder (folders scheme config target-suffix)
  "Find build folder matching scheme-config-platform pattern."
  (let* ((scheme-base (if (and scheme (string-match "^\\([^(]+\\)" scheme))
                         (string-trim (match-string 1 scheme))
                       scheme))
         ;; Extract suffix from scheme, e.g., "Bruce (Staging)" -> "Staging"
         (scheme-suffix (when (and scheme (string-match "(\\([^)]+\\))" scheme))
                         (match-string 1 scheme)))
         ;; IMPORTANT: Extract suffix from config as well, e.g., "Debug (Production)" -> "Production"
         ;; This is needed because scheme name might be "Bruce (Prod-debug)" but config is "Debug (Production)"
         (config-base (if (and config (string-match "^\\([^(]+\\)" config))
                         (string-trim (match-string 1 config))
                       config))
         (config-suffix (when (and config (string-match "(\\([^)]+\\))" config))
                         (match-string 1 config)))
         (patterns (list
                   ;; Pattern 1: Debug (Production)-iphonesimulator (using config suffix - most specific!)
                   ;; Use regexp-quote to escape special regex characters like parentheses
                   (when (and config-suffix config-base)
                     (regexp-quote (format "%s (%s)-%s" config-base config-suffix target-suffix)))
                   ;; Pattern 1b: Debug (Staging)-iphonesimulator (using scheme suffix as fallback)
                   (when (and scheme-suffix config-base)
                     (regexp-quote (format "%s (%s)-%s" config-base scheme-suffix target-suffix)))
                   ;; Pattern 1c: Any folder containing (Production)-iphonesimulator (when config-base is nil)
                   (when (and config-suffix (not config-base))
                     (format "(%s)-%s" (regexp-quote config-suffix) (regexp-quote target-suffix)))
                   ;; Pattern 2: MyApp-Debug-iphonesimulator
                   (when (and scheme-base config-base)
                     (regexp-quote (format "%s-%s-%s" scheme-base config-base target-suffix)))
                   ;; Pattern 3: MyApp_Debug-iphonesimulator
                   (when (and scheme-base config-base)
                     (regexp-quote (format "%s_%s-%s" scheme-base config-base target-suffix)))
                   ;; Pattern 4: Debug-iphonesimulator (least specific - check last!)
                   (when config-base
                     (format "^%s-%s$" (regexp-quote config-base) (regexp-quote target-suffix))))))
    (cl-some (lambda (pattern)
              (when pattern  ; Skip nil patterns
                (cl-find-if (lambda (folder)
                             (string-match-p pattern folder))
                           folders)))
            patterns)))

(defun xcode-project-find-config-platform-folder (folders config target-suffix)
  "Find build folder matching config-platform pattern.
Only matches if there's exactly one match to avoid ambiguity."
  (when config
    ;; Extract base config if it has suffix, e.g., "Debug (Production)" -> "Debug"
    (let* ((config-base (if (string-match "^\\([^(]+\\)" config)
                           (string-trim (match-string 1 config))
                         config))
           (pattern (format "%s.*%s" (regexp-quote config-base) (regexp-quote target-suffix)))
           (matches (seq-filter (lambda (folder)
                                 (string-match-p pattern folder))
                               folders)))
      ;; Only return a match if there's exactly one - avoid ambiguity
      (when (= (length matches) 1)
        (car matches)))))

(defun xcode-project-find-platform-folder (folders target-suffix)
  "Find any build folder matching the platform."
  (cl-find-if (lambda (folder) 
               (string-match-p target-suffix folder))
             folders))

(defun xcode-project-find-scheme-folder (folders scheme)
  "Find build folder containing the scheme name."
  (when (and scheme (not (string-empty-p scheme)))
    (cl-find-if (lambda (folder) 
                 (string-match-p (regexp-quote scheme) folder))
               folders)))

(defun xcode-project-interactive-build-folder-selection (folders target-suffix)
  "Let user interactively select build folder with smart defaults."
  (let ((platform-folders (seq-filter (lambda (folder) 
                                        (string-match-p target-suffix folder)) 
                                      folders)))
    (cond
     ;; Only one platform match - use it
     ((= (length platform-folders) 1)
      (car platform-folders))
     ;; Multiple platform matches - let user choose
     ((> (length platform-folders) 1)
      (xcode-project-build-menu
       :title (format "Choose %s build folder" 
                     (if (string= target-suffix "iphonesimulator") "Simulator" "Device"))
       :list platform-folders))
     ;; No platform matches - show all folders
     (t
      (xcode-project-build-menu
       :title "Choose build folder"
       :list folders)))))

;;;###autoload
(defun xcode-project-debug-build-folder-detection ()
  "Debug build folder detection process."
  (interactive)
  (let ((xcode-project-debug t))
    (message "=== Build Folder Detection Debug ===")
    (message "Project root: %s" (xcode-project-project-root))
    (message "Current scheme: %s" xcode-project--current-xcode-scheme)
    (message "Build configuration: %s" (xcode-project-fetch-or-load-build-configuration))
    (message "Build products dir: %s" (xcode-project-get-build-products-directory))
    (let ((build-products-dir (xcode-project-get-build-products-directory)))
      (when (file-exists-p build-products-dir)
        (message "Available build folders:")
        (dolist (folder (xcode-project-parse-build-folder build-products-dir))
          (message "  - %s" folder))))
    (message "Simulator folder: %s" (xcode-project-build-folder :device-type :simulator))
    (message "Device folder: %s" (xcode-project-build-folder :device-type :device))))

;;;###autoload  
(defun xcode-project-clear-build-folder-cache ()
  "Clear cached build folder selections."
  (interactive)
  (setq xcode-project--current-build-folder nil
        xcode-project--last-device-type nil)
  (when (fboundp 'swift-cache-invalidate-pattern)
    (swift-cache-invalidate-pattern "build-folder-"))
  (message "Build folder cache cleared"))

(defun xcode-project-setup-xcodebuildserver ()
  "Setup xcodebuild server."
  (xcodebuildserver-check-configuration
   :root (xcode-project-project-root)
   :workspace (xcode-project-get-workspace-or-project)
   :scheme (shell-quote-argument (xcode-project-scheme))))

(defun xcode-project-get-workspace-or-project ()
  "Check if there is workspace or project."
  (let ((workspace (xcode-project-workspace-name))
        (projectname (xcode-project-project-name)))
    (if workspace
        (format "-workspace %s.xcworkspace" (shell-quote-argument workspace))
      (format "-project %s.xcodeproj" (shell-quote-argument projectname)))))

(defun xcode-project-get-configuration-list ()
  "Get list of project configurations."
  (let* ((default-directory (xcode-project-project-root))
         (json (swift-development-get-buildconfiguration-json))
         (project (assoc 'project json))
         (result (cdr (assoc 'configurations project))))
    result))

(defun xcode-project-get-buildconfiguration-json ()
  "Return a cached version or load the build configuration."
  (unless xcode-project--current-buildconfiguration-json-data
    (let ((project-dir (or (xcode-project-project-root) default-directory)))
      (xcode-project-safe-mode-line-update :message "Fetching build configuration")
      (setq xcode-project--current-buildconfiguration-json-data 
            (let ((default-directory project-dir))
              (xcode-project-run-command-and-get-json (concat xcodebuild-list-config-command " 2>/dev/null"))))))
  xcode-project--current-buildconfiguration-json-data)

(defun xcode-project-get-target-list ()
  "Get list of project targets."
  (let* ((default-directory (xcode-project-project-root))
         (json (xcode-project-get-buildconfiguration-json))
         (project (assoc 'project json))
         (targets (cdr (assoc 'targets project))))
    targets))

(defun xcode-project-is-xcodeproject ()
  "Check if it's an Xcode project."
  (unless xcode-project--current-is-xcode-project
    (when-let* ((root (xcode-project-project-root)))
      (setq xcode-project--current-is-xcode-project
            (directory-files root nil "\\(?:\\.xcworkspace\\|\\.xcodeproj\\)$" t 1))))
  xcode-project--current-is-xcode-project)

(defun xcode-project-setup-current-project (project)
  "Check if we have a new project (as PROJECT).  If true reset settings."
  ;; Store the previous project root for comparison
  (setq xcode-project--previous-project-root xcode-project--current-project-root)

  ;; Normalize project path for comparison
  (let* ((normalized-project (file-truename (expand-file-name project)))
         (current-root (if xcode-project--current-project-root
                          (file-truename (expand-file-name
                                         (if (listp xcode-project--current-project-root)
                                             (car xcode-project--current-project-root)
                                           xcode-project--current-project-root)))
                        nil))
         (project-changed (not (and current-root
                                   (string= current-root normalized-project)))))

    (when xcode-project-debug
      (message "[DEBUG] Project restoration check:")
      (message "[DEBUG]   - current-root: %s" current-root)
      (message "[DEBUG]   - normalized-project: %s" normalized-project)
      (message "[DEBUG]   - xcode-project--current-project-root: %s" xcode-project--current-project-root)
      (message "[DEBUG]   - project-changed: %s" project-changed))

    (when (or (not xcode-project--current-project-root) project-changed)
      (when xcode-project-debug
        (message "[DEBUG] Project changed from '%s' to '%s'" current-root normalized-project))

      ;; Reset all cached values when project changes
      (xcode-project-reset)
      (setq default-directory project)
      (setq xcode-project--current-project-root normalized-project))

    ;; Always try to restore settings when project is setup (not just when it changes)
    ;; For buffer-local variables: restore if scheme is not yet set in this buffer
    ;; This allows each buffer to have its own project context
    (when (and (fboundp 'swift-project-settings-load)
               (fboundp 'swift-project-settings-load-for-scheme)
               (not xcode-project--current-xcode-scheme))
      (when xcode-project-debug
        (message "[DEBUG] Attempting to restore project settings to buffer-local vars..."))

      ;; First, check base settings for last-scheme
      (let* ((base-settings (swift-project-settings-load normalized-project nil))
             (last-scheme (when base-settings (plist-get base-settings :last-scheme))))

        (when last-scheme
          (when xcode-project-debug
            (message "[Settings] Found last-scheme: %s" last-scheme))

          ;; Set the scheme
          (setq xcode-project--current-xcode-scheme last-scheme)

          ;; Load scheme-specific settings
          (let ((settings (swift-project-settings-load-for-scheme normalized-project last-scheme)))
            (when settings
              (when xcode-project-debug
                (message "[Settings] Restored settings for scheme: %s (buffer-local)" last-scheme))

              ;; Restore app-identifier from settings
              (when-let ((app-id (plist-get settings :app-identifier)))
                (setq xcode-project--current-app-identifier app-id)
                (when xcode-project-debug
                  (message "[Settings] Restored app-identifier: %s" app-id)))

              ;; Restore build-config from settings
              (when-let ((build-config (plist-get settings :build-config)))
                (setq xcode-project--current-build-configuration build-config)
                (when xcode-project-debug
                  (message "[Settings] Restored build-config: %s" build-config)))

              ;; Restore build-folder from settings
              (when-let ((build-folder (plist-get settings :build-folder)))
                (setq xcode-project--current-build-folder build-folder)
                (when xcode-project-debug
                  (message "[Settings] Restored build-folder: %s" build-folder)))

              ;; Restore simulator selection
              (when (boundp 'ios-simulator--current-simulator-name)
                (when-let ((device-name (plist-get settings :device-name)))
                  (setq ios-simulator--current-simulator-name device-name)
                  (when xcode-project-debug
                    (message "[Settings] Restored simulator name: %s" device-name))))

              (when (boundp 'ios-simulator--current-simulator-id)
                (when-let ((device-id (plist-get settings :device-id)))
                  (setq ios-simulator--current-simulator-id device-id)
                  (when xcode-project-debug
                    (message "[Settings] Restored simulator ID: %s" device-id))))

              ;; Auto-launch simulator if enabled and we have device settings
              (when (and (boundp 'swift-development-auto-launch-simulator)
                         swift-development-auto-launch-simulator
                         (plist-get settings :device-id)
                         (fboundp 'ios-simulator-setup-simulator-dwim))
                (when xcode-project-debug
                  (message "[Settings] Auto-launching simulator: %s"
                           (plist-get settings :device-name)))
                (ios-simulator-setup-simulator-dwim (plist-get settings :device-id))))))))

    ;; Check cache warming independently of project-changed status
    ;; This handles cases where xcode-project--current-project-root was already set by another function
    (when xcode-project-debug
      (message "[DEBUG] Checking cache warming conditions...")
      (message "[DEBUG]   - swift-development-warm-build-cache defined?: %s" (fboundp 'swift-development-warm-build-cache))
      (message "[DEBUG]   - swift-development loaded?: %s" (featurep 'swift-development))
      (message "[DEBUG]   - normalized-project: %s" normalized-project)
      (message "[DEBUG]   - already in cache?: %s" (gethash normalized-project xcode-project--cache-warmed-projects)))

    (when (and (fboundp 'swift-development-warm-build-cache)
               (require 'swift-development nil t)
               (not (gethash normalized-project xcode-project--cache-warmed-projects)))
      (when xcode-project-debug
        (message "[DEBUG] All conditions met - warming caches now!"))

      ;; Always log to Messages buffer for visibility
      (message "Auto-warming build caches for %s..." (file-name-nondirectory normalized-project))

      ;; Show notification via configured backend (mode-line-hud, message, etc)
      (xcode-project-notify
       :message (format "Warming build caches for %s..."
                        (propertize (file-name-nondirectory normalized-project) 'face 'font-lock-constant-face)))

      (swift-development-warm-build-cache)
      ;; Mark this project as having warmed caches
      (puthash normalized-project t xcode-project--cache-warmed-projects))

    (when (and xcode-project-debug (or (not xcode-project--current-project-root) project-changed))
      (message "Set up new project: %s" normalized-project))))

(defun xcode-project-setup-project ()
  "Setup the current project."
  (xcode-project-notify :message "Setting up project...")
  (xcode-project-setup-current-project (swift-project-root))
  (xcode-project-notify
   :message (propertize "Project ready" 'face 'success)
   :seconds 2
   :reset t))

;;;###autoload
(defun xcode-project-show-project-info ()
  "Display current buffer-local project information for debugging.
Shows that multi-project support is enabled via buffer-local variables."
  (interactive)
  (message "Buffer: %s\nProject root: %s\nPrevious root: %s\nScheme: %s\nBuild config: %s\nApp identifier: %s\nBuild folder: %s\nDevice choice: %s\n\n(All values are buffer-local for multi-project support)"
           (buffer-name)
           (or xcode-project--current-project-root "nil")
           (or xcode-project--previous-project-root "nil")
           (or xcode-project--current-xcode-scheme "nil")
           (or xcode-project--current-build-configuration "nil")
           (or xcode-project--current-app-identifier "nil")
           (or xcode-project--current-build-folder "nil")
           (or (and swift-development--device-choice "Physical Device") "Simulator")))

(defun xcode-project-project-root ()
  "Get the project root as a path string."
  (setq xcode-project--current-project-root (swift-project-root)))

(cl-defun xcode-project-device-or-simulator-menu (&key title)
"Build device or simulator menu (as TITLE)."
(defconst deviceList '(("Simulator" nil)
                        ("Physical device" t)))
(progn
  (let* ((choices (seq-map (lambda (item) item) deviceList))
          (choice (completing-read title choices)))
    (car (cdr (assoc choice choices))))))

(defun xcode-addition-ask-for-device-or-simulator ()
  "Show menu for running on simulator or device."
  (interactive)
  (when (and (ios-device-connected-device-id) (not swift-development--device-choice))
    (setq swift-development--device-choice
          (xcode-project-device-or-simulator-menu :title "Run on simulator or device?"))))

(defun xcode-project-run-in-simulator ()
  "Return t if app should run in simulator, nil for physical device."
  (if (null swift-development--device-choice)
      t  ; Default to simulator if not set
    (not swift-development--device-choice)))

;;;###autoload
(defun xcode-project-reset ()
  "Reset the current project root and device choice.
Also clears all persistent cache files (.swift-development/).
Prompts to choose scheme first, then simulator."
  (interactive)
  ;; First, reset simulator state (but don't choose new one yet)
  (when (fboundp 'ios-simulator-reset)
    (ios-simulator-reset nil))  ; Pass nil - don't choose yet
  (when (fboundp 'periphery-kill-buffer)
    (periphery-kill-buffer))

  ;; Clear persistent cache files before resetting variables
  (when (and xcode-project--current-project-root
             (fboundp 'swift-project-settings-clear-all-cache))
    (swift-project-settings-clear-all-cache xcode-project--current-project-root))

  ;; Clear swift-cache for build folders and other cached data
  (when (fboundp 'swift-cache-invalidate-pattern)
    (swift-cache-invalidate-pattern "build-folder-")
    (swift-cache-invalidate-pattern "build-settings-")
    (swift-cache-invalidate-pattern "scheme-files"))

  (setq swift-development--device-choice nil
        xcode-project--current-local-device-id nil
        xcode-project--current-is-xcode-project nil
        xcode-project--current-build-folder nil
        xcode-project--current-app-identifier nil
        xcode-project--current-build-configuration nil
        xcode-project--previous-project-root xcode-project--current-project-root ; Store before resetting
        xcode-project--current-project-root nil
        xcode-project--current-xcode-scheme nil
        xcode-project--current-buildconfiguration-json-data nil
        xcode-project--current-errors-or-warnings nil
        swift-development--device-choice nil
        xcode-project--last-device-type nil)  ; Reset device choice

  ;; NOTE: We intentionally DO NOT reset swift-development--last-build-succeeded here
  ;; to allow each project to maintain its own build status across project switches.
  ;; Use swift-development-reset-build-status to manually reset if needed.
  (swift-project-reset-root)
  (xcode-project-safe-mode-line-update :message "Resetting configuration")
  (message "Xcode configuration reset - scheme cache cleared")

  ;; Now prompt for scheme selection first
  ;; This ensures scheme is set before simulator selection
  (when (y-or-n-p "Choose a new scheme? ")
    (xcode-project-scheme)
    ;; After scheme is selected, prompt for simulator
    (when (y-or-n-p "Choose a new simulator? ")
      (when (fboundp 'ios-simulator-choose-simulator)
        (ios-simulator-choose-simulator)))))

;;;###autoload
(defun xcode-project-start-debugging ()
  "Start debugging immediately without confirmation.
Automatically builds the app if sources have changed."
  (interactive)
  (condition-case err
      (progn
        ;; Ensure app is built before debugging
        (when (fboundp 'swift-development-needs-rebuild-p)
          (if (swift-development-needs-rebuild-p)
              (progn
                (message "Changes detected, building before debugging...")
                (swift-development-compile-app)
                ;; Wait a moment for build to complete
                ;; TODO: Better synchronization with build completion
                (sit-for 1))
            (message "App up-to-date, launching debugger...")))
        (xcode-project-setup-dape))
    (error
     (message "Error starting debugger: %s" (error-message-string err))
     (message "Please ensure a scheme is selected and the project is properly configured")
     (signal (car err) (cdr err)))))

(defun xcode-project-setup-dape ()
  "Setup and start dape for iOS debugging.
Automatically detects whether to debug on simulator or physical device
based on `swift-development--device-choice'."
  (interactive)
  (require 'dape)
  ;; Ensure scheme and project root are loaded first
  (xcode-project-scheme)
  (xcode-project-project-root)

  (let* ((is-device swift-development--device-choice)
         (app-bundle-id (or (xcode-project-fetch-or-load-app-identifier)
                            (error "Failed to get app bundle identifier")))
         (project-cwd (or (project-root (project-current))
                          default-directory))
         (codelldb-path (file-name-concat dape-adapter-dir
                                          "codelldb"
                                          "extension"
                                          "adapter"
                                          "codelldb"))
         (lldb-path "/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Versions/A/LLDB"))

    (if is-device
        ;; Physical device debugging
        (xcode-project-setup-dape-device app-bundle-id project-cwd codelldb-path lldb-path)
      ;; Simulator debugging
      (xcode-project-setup-dape-simulator app-bundle-id project-cwd codelldb-path lldb-path))))

(defun xcode-project-setup-dape-simulator (app-bundle-id project-cwd codelldb-path lldb-path)
  "Setup dape for simulator debugging.
APP-BUNDLE-ID is the app's bundle identifier.
PROJECT-CWD is the project working directory.
CODELLDB-PATH is path to the CodeLLDB adapter.
LLDB-PATH is path to LLDB framework."
  (let ((simulator-id (or (ignore-errors (ios-simulator-simulator-identifier))
                          (error "Failed to get simulator ID"))))
    (add-to-list 'dape-configs
                 `(ios-simulator
                   modes (swift-mode swift-ts-mode)
                   command-cwd ,project-cwd
                   command ,codelldb-path
                   command-args ("--port" :autoport
                                 "--settings" "{\"sourceLanguages\":[\"swift\"]}"
                                 "--liblldb" ,lldb-path)
                   port :autoport
                   simulator-id ,simulator-id
                   app-bundle-id ,app-bundle-id
                   fn (dape-config-autoport
                       ,(lambda (config)
                          (with-temp-buffer
                            (let* ((command
                                    (format "xcrun simctl launch --wait-for-debugger --terminate-running-process %S %S --console-pty"
                                            (plist-get config 'simulator-id)
                                            (plist-get config 'app-bundle-id)))
                                   (code (call-process-shell-command command nil (current-buffer))))
                              (dape--message (format "* Running: %S *" command))
                              (dape--message (buffer-string))
                              (save-match-data
                                (if (and (zerop code)
                                         (progn (goto-char (point-min))
                                                (search-forward-regexp "[[:digit:]]+" nil t)))
                                    (plist-put config :pid (string-to-number (match-string 0)))
                                  (dape--message (format "* Running: %S *" command))
                                  (dape--message (format "Failed to start simulator:\n%s" (buffer-string)))
                                  (user-error "Failed to start simulator")))))
                          config))
                   :type "lldb"
                   :request "attach"
                   :cwd "."))
    ;; Start dape with simulator configuration
    (message "Starting debugger on simulator: %s" simulator-id)
    (let ((config (copy-tree (cdr (assq 'ios-simulator dape-configs)))))
      (if config
          (dape config)
        (error "Failed to setup simulator dape configuration")))))

(defun xcode-project-setup-dape-device (app-bundle-id project-cwd codelldb-path lldb-path)
  "Setup dape for physical device debugging.
APP-BUNDLE-ID is the app's bundle identifier.
PROJECT-CWD is the project working directory.
CODELLDB-PATH is path to the CodeLLDB adapter (unused for device, kept for signature).
LLDB-PATH is path to LLDB framework (unused for device, kept for signature)."
  (require 'ios-device)
  (let* ((device-id (or ios-device--current-device-id
                        (ios-device-identifier)
                        (error "Failed to get device ID")))
         (device-name (or (ios-device-get-device-name device-id) "Unknown Device"))
         ;; Use Xcode's lldb-dap for device debugging (requires Xcode 16+)
         (lldb-dap-path "/Applications/Xcode.app/Contents/Developer/usr/bin/lldb-dap"))

    ;; Check if lldb-dap exists
    (unless (file-executable-p lldb-dap-path)
      (error "lldb-dap not found at %s. Device debugging requires Xcode 16+" lldb-dap-path))

    ;; First, terminate any existing instance
    (message "Terminating any existing app instance on %s..." device-name)
    (shell-command-to-string
     (format "xcrun devicectl device process terminate --device %s %s 2>/dev/null"
             (shell-quote-argument device-id)
             (shell-quote-argument app-bundle-id)))

    ;; Launch app in stopped state and get PID
    (message "Launching app on %s for debugging..." device-name)
    (let ((pid (ios-device-launch-for-debug :device-id device-id
                                            :app-identifier app-bundle-id)))
      (unless pid
        (error "Failed to launch app on device. Make sure the app is installed"))

      (message "App launched with PID %s, attaching debugger via lldb-dap..." pid)

      ;; Setup dape config for device using lldb-dap with device commands
      ;; lldb-dap is Xcode's native DAP server that supports device debugging
      ;; For iOS 17+ we need:
      ;; 1. platform select remote-ios (in initCommands)
      ;; 2. device select <uuid> (in attachCommands)
      ;; 3. device process attach -c --pid <pid> (in attachCommands)
      ;; The -c flag continues execution after attach
      (add-to-list 'dape-configs
                   `(ios-device
                     modes (swift-mode swift-ts-mode)
                     command-cwd ,project-cwd
                     command ,lldb-dap-path
                     :type "lldb"
                     :request "attach"
                     :initCommands ["platform select remote-ios"]
                     :attachCommands [,(format "device select %s" device-id)
                                      ,(format "device process attach -c --pid %d" pid)]
                     :cwd ,project-cwd))

      ;; Start dape with device configuration
      (let ((config (copy-tree (cdr (assq 'ios-device dape-configs)))))
        (if config
            (dape config)
          (error "Failed to setup device dape configuration"))))))


(defun xcode-project-clean-build-folder ()
  "Clean app build folder, Swift package caches, and optionally Xcode derived data."
  (interactive)
  (let ((root (xcode-project-project-root)))
    (unless root
      (error "Not in an Xcode project"))
    
    ;; Clean .build folder
    (xcode-project-clean-build-folder-with
     :root root
     :build-folder (expand-file-name ".build" root)
     :project-name (xcode-project-product-name)
     :ignore-list xcode-project-clean-build-ignore-list)
    
    ;; Clean Swift package caches
    (xcode-project-clean-swift-package-caches)
    
    ;; Optionally clean derived data for this project
    (when (yes-or-no-p "Also clean Xcode derived data for this project?")
      (xcode-project-clean-project-derived-data))))

(defun xcode-project-clean-swift-package-caches ()
  "Clean Swift package manager caches."
  (let ((package-cache-dir (expand-file-name "~/Library/Caches/org.swift.packages"))
        (cloned-sources-dir (expand-file-name "~/Library/Caches/org.swift.cloned-sources")))

    (when (file-exists-p package-cache-dir)
      (message "Cleaning Swift package cache...")
      (start-process "clean-swift-cache" nil "rm" "-rf" package-cache-dir))

    (when (file-exists-p cloned-sources-dir)
      (message "Cleaning Swift cloned sources...")
      (start-process "clean-swift-sources" nil "rm" "-rf" cloned-sources-dir))))

(defun xcode-project-clean-project-derived-data ()
  "Clean Xcode derived data for the current project."
  (let* ((project-name (xcode-project-product-name))
         (derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (project-pattern (concat project-name "-")))

    (message "Cleaning derived data for %s..." project-name)
    (start-process "clean-derived-data" nil "sh" "-c"
                   (format "rm -rf %s/%s*" derived-data-dir project-pattern))))

(defun xcode-project-deep-clean ()
  "Perform a deep clean: build folder, Swift package caches, and all derived data."
  (interactive)
  (let ((root (xcode-project-project-root)))
    (unless root
      (error "Not in an Xcode project"))
    
    (when (yes-or-no-p "Perform deep clean? This will delete .build, Swift caches, and ALL derived data.")
      ;; Clean .build folder
      (xcode-project-clean-build-folder-with
       :root root
       :build-folder (expand-file-name ".build" root)
       :project-name (xcode-project-product-name)
       :ignore-list xcode-project-clean-build-ignore-list)
      
      ;; Clean Swift package caches
      (xcode-project-clean-swift-package-caches)
      
      ;; Clean ALL derived data (not just project-specific)
      (let ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData")))
        (when (file-exists-p derived-data-dir)
          (message "Cleaning all derived data...")
          (start-process "clean-all-derived-data" nil "sh" "-c"
                         (format "find %s -mindepth 1 -maxdepth 1 ! -name 'ModuleCache' -exec rm -rf {} +" derived-data-dir)))))))

(cl-defun xcode-project-clean-build-folder-with (&key root build-folder project-name ignore-list)
  "Clean build folder with ROOT, BUILD-FOLDER, PROJECT-NAME asynchronously.
IGNORE-LIST is a list of folder names to ignore during cleaning."
  (let ((display-name (or project-name "project")))
    (when xcode-project-debug
      (message "Cleaning build folder: %s for %s" build-folder display-name))
    (let ((default-directory build-folder))
      (if (file-directory-p default-directory)
          (progn
            (xcode-project-safe-mode-line-update
             :message (format "Cleaning build folder for %s"
                              (propertize display-name 'face 'warning)))
            (async-start
             `(lambda ()
                ,(async-inject-variables "default-directory")
                (defun delete-directory-contents (directory ignore-list)
                  "Delete contents of DIRECTORY, ignoring folders in IGNORE-LIST.
Returns a list of errors encountered during deletion."
                  (let ((errors nil))
                    (dolist (file (directory-files directory t))
                      (let ((file-name (file-name-nondirectory file)))
                        (unless (or (member file-name '("." ".."))
                                    (member file-name ignore-list))
                          (condition-case err
                              (if (file-directory-p file)
                                  (delete-directory file t)
                                (delete-file file))
                            (file-error
                             (push (format "%s: %s" file (error-message-string err)) errors))))))
                    errors))
                (condition-case err
                    (let ((deletion-errors (delete-directory-contents ,default-directory ',ignore-list)))
                      (if deletion-errors
                          (format "completed with %d errors" (length deletion-errors))
                        "successfully"))
                  (error (format "Error during cleaning: %s" (error-message-string err)))))
             `(lambda (result)
                (xcode-project-safe-mode-line-notification
                 :message (format "Cleaning %s %s"
                                  (propertize ,display-name 'face 'warning)
                                  result)
                 :seconds 3
                 :reset t))))
        (xcode-project-safe-mode-line-notification
         :message (propertize "Build folder is empty or does not exist." 'face 'warning)
         :seconds 3
         :reset t)))))

(defun xcode-project-open-in-xcode ()
  "Open project in xcode."
  (interactive)
  (if-let* ((default-directory (xcode-project-project-root))
           (command "xed ."))
      (inhibit-sentinel-messages #'call-process-shell-command command)))

(defun xcode-project--clean-display-name (str)
  "Remove escape sequences and quotes from xcodebuild output names.
Handles escaped parentheses like \\( and \\), quotes, and other escaped characters."
  (when str
    (thread-last str
      ;; Remove backslash-escaped characters (e.g., \( becomes (, \" becomes ")
      (replace-regexp-in-string "\\\\\\(.\\)" "\\1")
      ;; Remove any remaining standalone quotes
      (replace-regexp-in-string "\"" ""))))

(cl-defun xcode-project-parse-compile-lines-output (&key input)
  "Parse compile output and print unique matched lines using separate message calls.
   Also prints compiler messages for C++ errors, warnings, and notes."
  (when xcode-project-debug
    (message "Parsing compile output: %s" input))
  (let ((seen-messages (make-hash-table :test 'equal))
        ;; Match both file-based errors (file:line:col: severity:) and build-level warnings (warning:/note:)
        (error-regex "^\\(?:.+:[0-9]+:[0-9]+: \\)?\\(error\\|warning\\|note\\): .+$"))
    (dolist (line (split-string input "\n"))
      (cond
       ;; Check for .compile.lock error and clear it
       ((string-match xcode-build-config-compile-lock-pattern line)
        (let ((msg "Clearing compile lock..."))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages)
            ;; Clear the lock file
            (let ((lock-file (concat (xcode-project-project-root) "/.compile.lock")))
              (when (file-exists-p lock-file)
                (delete-file lock-file)
                (message "Cleared .compile.lock file"))))))
       ;; Resolving packages
       ((string-match "Resolve Package Graph" line)
        (let ((msg "Resolving packages..."))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'success)))
            (puthash msg t seen-messages))))
       ;; Fetching packages
       ((string-match "Fetching from \\(https://github.com/[^/]+/\\([^/ ]+\\)\\)" line)
        (let* ((url (match-string 1 line))
               (package-name (match-string 2 line))
               ;; Clean up package name (remove .git suffix if present)
               (clean-name (if (string-suffix-p ".git" package-name)
                              (substring package-name 0 -4)
                            package-name))
               (msg (format "Fetching package: %s" clean-name)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-string-face)))
            (puthash msg t seen-messages))))
       ;; Package resolution details
       ((string-match "Resolved source packages:" line)
        (let ((msg "Package resolution complete"))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'success)))
            (puthash msg t seen-messages))))
       ;; Building specific targets
       ((string-match "Build target \\([^ ]+\\)" line)
        (let* ((target-raw (match-string 1 line))
               (target (xcode-project--clean-display-name target-raw))
               (msg (format "Building target: %s" target)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-builtin-face)))
            (puthash msg t seen-messages))))
       ;; Build phase script execution (SwiftGen, SwiftLint, etc.)
       ((string-match "PhaseScriptExecution.*\"\\([^\"]+\\)\"" line)
        (let* ((script-name-raw (match-string 1 line))
               (script-name (xcode-project--clean-display-name script-name-raw))
               (msg (format "Running script: %s" script-name)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-function-name-face)))
            (puthash msg t seen-messages))))
       ;; Asset catalog compilation
       ((string-match "CompileAssetCatalog.*/\\([^/[:space:]]+\\.xcassets\\)" line)
        (let* ((catalog-raw (match-string 1 line))
               (catalog (xcode-project--clean-display-name catalog-raw))
               (msg (format "Compiling assets: %s" catalog)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-variable-name-face)))
            (puthash msg t seen-messages))))
       ;; Info.plist processing
       ((string-match "ProcessInfoPlistFile.*/\\([^/[:space:]]+\\.plist\\)" line)
        (let* ((plist-raw (match-string 1 line))
               (plist (xcode-project--clean-display-name plist-raw))
               (msg (format "Processing: %s" plist)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-doc-face)))
            (puthash msg t seen-messages))))
       ;; Copy Swift libraries
       ((string-match "CopySwiftLibs" line)
        (let ((msg "Copying Swift libraries"))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-preprocessor-face)))
            (puthash msg t seen-messages))))
       ;; Copy resources/files
       ((string-match "\\bCopy\\b.*/\\([^/[:space:]]+\\)" line)
        (let* ((filename-raw (match-string 1 line))
               (filename (xcode-project--clean-display-name filename-raw))
               ;; Only show meaningful copy operations (not very short names or weird patterns)
               (is-meaningful (and filename
                                  (>= (length filename) 3)
                                  (not (string-match-p "^[0-9]+$" filename)))))
          (when is-meaningful
            (let ((msg (format "Copying: %s" filename)))
              (unless (gethash msg seen-messages)
                (xcode-project-safe-mode-line-update :message
                                                       (format "  %s" (propertize msg 'face 'font-lock-comment-face)))
                (puthash msg t seen-messages))))))
       ;; Create build directories
       ((or (string-match "CreateBuildDirectory" line)
            (string-match "^MkDir " line))
        (let ((msg "Creating build directories"))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-comment-face)))
            (puthash msg t seen-messages))))
       ;; Touch files (final build step marker)
       ((string-match "^Touch .*/\\([^/[:space:]]+\\.app\\)" line)
        (let* ((app-raw (match-string 1 line))
               (app (xcode-project--clean-display-name app-raw))
               (msg (format "Finalizing: %s" app)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'success)))
            (puthash msg t seen-messages))))
       ;; Linking
       ((string-match "\\bLd\\b.*/\\([^/[:space:]]+\\)\\(?: normal\\| \\)" line)
        (let* ((filename-raw (match-string 1 line))
               ;; Strip escape sequences and quotes
               (filename (xcode-project--clean-display-name filename-raw))
               ;; Filter out very short names (< 2 chars), argument flags, and paths with extensions
               (is-meaningful (and filename
                                  (>= (length filename) 2)
                                  (not (string-prefix-p "-" filename))
                                  (not (string-match-p "\\." filename))
                                  (not (string-match-p "^[a-z]$" filename)))))
          (when xcode-project-debug
            (message "[DEBUG Linking] raw='%s' clean='%s' is-meaningful=%s" filename-raw filename is-meaningful))
          (when is-meaningful
            (let ((msg (format "Linking: %s" filename)))
              (unless (gethash msg seen-messages)
                (xcode-project-safe-mode-line-update :message
                                                       (format "  %s" (propertize msg 'face 'font-lock-keyword-face)))
                (puthash msg t seen-messages))))))
       ;; Code signing
       ((string-match "CodeSign \\(.+\\)/\\([^/[:space:]]+\\.app\\)" line)
        (let* ((app-raw (match-string 2 line))
               (app (xcode-project--clean-display-name app-raw))
               (msg (format "Signing: %s" app)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-constant-face)))
            (puthash msg t seen-messages))))
       ;; C compilation
       ((string-match "CompileC.*/\\([^/[:space:]]+\\.[cm]\\)\\b" line)
        (let* ((file-raw (match-string 1 line))
               (file (xcode-project--clean-display-name file-raw))
               (msg (format "Compiling C: %s" file)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match "CompileSwiftModule \\([^ ]+\\)" line)
        (let* ((module-raw (match-string 1 line))
               (module (xcode-project--clean-display-name module-raw))
               (msg (format "Compiling Swift: %s" module)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ;; Swift files
       ((string-match "CompileSwift.*\\([^/]+\\.swift\\)" line)
        (let* ((file-raw (match-string 1 line))
               (file (xcode-project--clean-display-name file-raw))
               (msg (format "Swift: %s" file)))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-type-face)))
            (puthash msg t seen-messages))))
       ;; Errors, warnings, notes
       ((string-match error-regex line)
        (setq xcode-project--current-errors-or-warnings (concat line "\n" xcode-project--current-errors-or-warnings))
        (periphery-run-parser xcode-project--current-errors-or-warnings))))))

(defun xcode-project-derived-data-path ()
  "Get the actual DerivedData path by running xcodebuild -showBuildSettings.
Returns nil if build settings cannot be retrieved."
  (let* ((default-directory (or (xcode-project-project-root) default-directory))
         (config (or xcode-project--current-build-configuration "Debug"))
         (json (xcode-project-get-build-settings-json :config config)))
    (if (and json (> (length json) 0))
        (let-alist (seq-elt json 0)
          (or .buildSettings.BUILD_DIR
              .buildSettings.SYMROOT
              (concat (xcode-project-project-root) "/.build")))
      ;; Fallback when build settings unavailable
      (let ((project-root (xcode-project-project-root)))
        (when project-root
          (concat project-root "/.build"))))))

(defun xcode-project-target-build-directory (&optional configuration)
  "Get the build directory from xcodebuild -showBuildSettings output.
CONFIGURATION is the build configuration (Debug/Release)."
  (let* ((config (or configuration xcode-project--current-build-configuration "Debug"))
         (json (xcode-project-get-build-settings-json :config config)))
    (let-alist (seq-elt json 0)
      .buildSettings.TARGET_BUILD_DIR)))

(defun xcode-project-open-build-folder ()
  "Open build folder."
  (interactive)
    (let ((default-directory (xcode-project-derived-data-path)))
        (if (file-directory-p default-directory)
            (dired default-directory)
          (message "No build folder found"))))

;; Note: xcode-project-toggle-device-choice is now defined in swift-development.el
;; with a defalias for backwards compatibility

(defun xcode-project-show-current-configuration ()
  "Display the current Xcode project configuration."
  (interactive)
  (let ((config-message
         (format "Current Configuration:
Project Root: %s
Scheme: %s
Build Configuration: %s
App Identifier: %s
Build Folder: %s
Running on: %s
Simulator ID: %s
Debug Mode: %s"
                 xcode-project--current-project-root
                 xcode-project--current-xcode-scheme
                 xcode-project--current-build-configuration
                 xcode-project--current-app-identifier
                 xcode-project--current-build-folder
                 (if swift-development--device-choice "Physical Device" "Simulator")
                 (ios-simulator-simulator-identifier)
                 (if xcode-project-debug "Enabled" "Disabled"))))
    (with-current-buffer (get-buffer-create "*Xcode Configuration*")
      (erase-buffer)
      (insert config-message)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun xcode-project-toggle-debug ()
  "Toggle debug mode for xcode-additions."
  (interactive)
  (setq xcode-project-debug (not xcode-project-debug))
  (message "Xcode Additions debug mode %s"
           (if xcode-project-debug "enabled" "disabled")))

;;;###autoload
(defun xcode-project-cache-diagnostics ()
  "Show comprehensive cache diagnostics for current project."
  (interactive)
  (let* ((project-root (xcode-project-project-root))
         (normalized-root (file-truename (expand-file-name project-root)))
         (is-warmed (gethash normalized-root xcode-project--cache-warmed-projects))
         (cache-stats (when (fboundp 'swift-cache-stats)
                       (swift-cache-stats))))
    (with-current-buffer (get-buffer-create "*Swift Cache Diagnostics*")
      (erase-buffer)
      (insert (format "=== Swift Cache Diagnostics ===\n\n"))
      (insert (format "Project Root: %s\n" project-root))
      (insert (format "Normalized Root: %s\n" normalized-root))
      (insert (format "Cache Auto-Warmed: %s\n\n" (if is-warmed "Yes" "No")))

      ;; Show warmed projects
      (insert "=== Auto-Warmed Projects ===\n")
      (if (> (hash-table-count xcode-project--cache-warmed-projects) 0)
          (maphash (lambda (key _value)
                    (insert (format "  - %s\n" (file-name-nondirectory key))))
                  xcode-project--cache-warmed-projects)
        (insert "  (none)\n"))
      (insert "\n")

      ;; Show cache statistics if available
      (when cache-stats
        (insert "=== Swift Cache Statistics ===\n")
        (insert (format "Total Entries: %d\n" (plist-get cache-stats :total)))
        (insert (format "Expired Entries: %d\n" (plist-get cache-stats :expired)))
        (insert (format "Approximate Size: %d bytes\n\n" (plist-get cache-stats :size))))

      ;; Show current project cache keys
      (when (fboundp 'swift-cache-get)
        (insert "=== Current Project Cache Status ===\n")
        (let ((build-cache-key (when (fboundp 'swift-cache-project-key)
                                (swift-cache-project-key project-root "build-cache-warmed")))
              (scheme-files-key (when (fboundp 'swift-cache-project-key)
                                 (swift-cache-project-key project-root "scheme-files"))))
          (when build-cache-key
            (insert (format "Build Cache Warmed: %s\n"
                           (if (swift-cache-get build-cache-key) "Yes" "No"))))
          (when scheme-files-key
            (insert (format "Scheme Files Cached: %s\n"
                           (if (swift-cache-get scheme-files-key) "Yes" "No"))))))

      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

;; Build Process Interrupt Functions

;;;###autoload
(defun xcode-project-interrupt-build ()
  "Interrupt the currently active xcodebuild process.
Also checks for and handles compile.lock errors that can halt builds."
  (interactive)
  (require 'swift-development)
  
  ;; First check for compile.lock errors
  (let ((has-lock-error nil))
    (dolist (buffer-name '("*Swift Build*" "*compilation*" "*xcodebuild*"))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq has-lock-error t)
              (message "Detected compile.lock error in %s - this can cause hanging builds" buffer-name))))))
    
    (when has-lock-error
      (message "Compile.lock error detected - will clean derived data after interrupting build")))
  
  (cond
   ;; Check for active swift-additions build process
   ((and (boundp 'swift-development--active-build-process)
         swift-development--active-build-process
         (process-live-p swift-development--active-build-process))
    (message "Interrupting active build process...")
    (interrupt-process swift-development--active-build-process)
    ;; Give it a moment, then kill if still alive
    (run-with-timer 2 nil
                    (lambda ()
                      (when (and swift-development--active-build-process
                                 (process-live-p swift-development--active-build-process))
                        (kill-process swift-development--active-build-process)
                        (message "Build process killed"))))
    (setq swift-development--active-build-process nil)
    (when (boundp 'swift-development--active-build-buffer)
      (setq swift-development--active-build-buffer nil))
    (when (and (boundp 'swift-development--build-progress-spinner)
               swift-development--build-progress-spinner)
      (spinner-stop swift-development--build-progress-spinner)))
   
   ;; Check for compilation buffer with active process
   ((get-buffer "*Swift Build*")
    (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
      (if (and proc (process-live-p proc))
          (progn
            (message "Interrupting build in *Swift Build* buffer...")
            (interrupt-process proc)
            (run-with-timer 2 nil
                            (lambda ()
                              (when (and proc (process-live-p proc))
                                (kill-process proc)
                                (message "Build process killed")))))
        (message "No active build process found in *Swift Build* buffer"))))
   
   ;; Fall back to killing any xcodebuild processes
   (t
    (let ((killed-count (xcode-project-kill-all-xcodebuild-processes)))
      (if (> killed-count 0)
          (message "Killed %d xcodebuild process(es)" killed-count)
        (message "No active build processes found"))))))

;;;###autoload
(defun xcode-project-kill-all-xcodebuild-processes ()
  "Kill all xcodebuild processes system-wide.
Returns the number of processes killed."
  (interactive)
  (let ((killed-count 0))
    ;; Kill all xcodebuild processes
    (dolist (pid (split-string 
                  (shell-command-to-string "pgrep -f xcodebuild") "\n" t))
      (when (string-match-p "^[0-9]+$" pid)
        (condition-case nil
            (progn
              (call-process "kill" nil nil nil "-TERM" pid)
              (cl-incf killed-count)
              (message "Sent TERM signal to xcodebuild process %s" pid))
          (error nil))))
    
    ;; If we killed any, wait a bit then force kill any survivors
    (when (> killed-count 0)
      (run-with-timer 3 nil
                      (lambda ()
                        (dolist (pid (split-string 
                                      (shell-command-to-string "pgrep -f xcodebuild") "\n" t))
                          (when (string-match-p "^[0-9]+$" pid)
                            (condition-case nil
                                (progn
                                  (call-process "kill" nil nil nil "-KILL" pid)
                                  (message "Force killed stubborn xcodebuild process %s" pid))
                              (error nil)))))))
    
    (when (called-interactively-p 'interactive)
      (if (> killed-count 0)
          (message "Killed %d xcodebuild process(es)" killed-count)
        (message "No xcodebuild processes found")))
    killed-count))

;;;###autoload
(defun xcode-project-check-compile-lock-error ()
  "Check if build output contains the compile.lock error that can halt builds."
  (interactive)
  (let ((found-error nil)
        (buffers-to-check '("*Swift Build*" "*compilation*" "*xcodebuild*")))
    (dolist (buffer-name buffers-to-check)
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq found-error t)
              (message "Found compile.lock error in %s buffer - this can halt builds!" buffer-name))))))
    (if found-error
        (when (yes-or-no-p "Compile.lock error detected.  Kill build processes and clean derived data?")
          (xcode-project-interrupt-build)
          (message "Interrupted build. Consider running clean derived data."))
      (message "No compile.lock errors found in build buffers"))))

;;;###autoload
(defun xcode-project-build-status ()
  "Show status of current build process."
  (interactive)
  (require 'swift-development)
  ;; First check for compile.lock errors
  (let ((has-lock-error nil))
    (dolist (buffer-name '("*Swift Build*" "*compilation*" "*xcodebuild*"))
      (when (get-buffer buffer-name)
        (with-current-buffer buffer-name
          (save-excursion
            (goto-char (point-min))
            (when (search-forward ".compile.lock exists! parse already run" nil t)
              (setq has-lock-error t))))))
    
    (cond
     ;; Show lock error warning if found
     (has-lock-error
      (message "⚠️  Build may be stuck due to compile.lock error - use `xcode-project-interrupt-build` to fix"))
     
     ((and (boundp 'swift-development--active-build-process)
           swift-development--active-build-process
           (process-live-p swift-development--active-build-process))
      (message "Build active: %s (PID: %s) Buffer: %s"
               (process-name swift-development--active-build-process)
               (process-id swift-development--active-build-process)
               (or (and (boundp 'swift-development--active-build-buffer)
                        swift-development--active-build-buffer) 
                   "Unknown")))
     
     ((get-buffer "*Swift Build*")
      (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
        (if (and proc (process-live-p proc))
            (message "Build active in *Swift Build* buffer (PID: %s)" (process-id proc))
          (message "No active build process (buffer exists but no process)"))))
     
     (t (message "No active build process")))))

;;; Transient Menu

;;;###autoload
(transient-define-prefix xcode-project-transient ()
  "Xcode Project actions."
  ["Project Info"
   [("i" "Show project info" xcode-project-show-project-info)
    ("D" "Debug build folder" xcode-project-debug-build-folder-detection)
    ("s" "Build status" xcode-project-build-status)]]
  ["Build Control"
   [("k" "Interrupt build" xcode-project-interrupt-build)
    ("K" "Kill all xcodebuild" xcode-project-kill-all-xcodebuild-processes)
    ("l" "Check compile lock" xcode-project-check-compile-lock-error)]
   [("d" "Start debugging" xcode-project-start-debugging)
    ("g" "Toggle debug mode" xcode-project-toggle-debug)]]
  ["Cache Management"
   [("c" "Clear build folder cache" xcode-project-clear-build-folder-cache)
    ("C" "Cache diagnostics" xcode-project-cache-diagnostics)]
   [("r" "Reset project state" xcode-project-reset)]]
  [("q" "Quit" transient-quit-one)])

(provide 'xcode-project)
;;; xcode-project.el ends here
