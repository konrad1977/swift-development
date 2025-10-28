;;; xcode-project.el --- package for compiling and running swift apps in  -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(require 'project)
(require 'xcodebuildserver)
(require 'swift-project)
(require 'xcode-build-config)
(require 'swift-project-settings)  ; Persistent project settings
(require 'periphery nil t)
(require 'periphery-helper nil t)
(require 'swift-cache nil t)  ; Optional - graceful fallback if not available

;; Optional dependencies
(defvar mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

;; Notification backend configuration
(defcustom xcode-project-notification-backend 'mode-line-hud
  "Backend to use for displaying build progress and notifications.
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
  "Universal notification function that delegates to configured backend.
Accepts keyword arguments:
  :message - The message to display
  :delay - Optional delay before showing (for mode-line-hud)
  :seconds - How long to show notification
  :reset - Whether to reset after showing
  :face - Face to apply to message (for message backend)
  :no-redisplay - If t, skip the automatic redisplay (default nil)"
  (let ((message-text (plist-get args :message))
        (delay (plist-get args :delay))
        (seconds (plist-get args :seconds))
        (reset (plist-get args :reset))
        (face (plist-get args :face))
        (no-redisplay (plist-get args :no-redisplay)))

    (pcase xcode-project-notification-backend
      ('mode-line-hud
       (when mode-line-hud-available-p
         (cond
          ;; Notification style (with seconds and reset)
          ((and seconds reset)
           (mode-line-hud:notification :message message-text :seconds seconds :reset reset))
          ;; Update with delay
          (delay
           (mode-line-hud:updateWith :message message-text :delay delay))
          ;; Simple update
          (t
           (mode-line-hud:update :message message-text)))))

      ('message
       ;; Use minibuffer messages with optional color
       (if face
           (message "%s" (propertize message-text 'face face))
         (message "%s" message-text)))

      ('custom
       ;; Call custom function if configured
       (when (functionp xcode-project-notification-function)
         (apply xcode-project-notification-function args)))

      (_
       ;; Fallback to message if backend unknown
       (message "%s" message-text)))

    ;; Force display update so notification is visible before any blocking operation
    ;; This is especially important before shell commands, file I/O, or user input
    (unless no-redisplay
      (redisplay t))))

(defun xcode-project-safe-mode-line-update (&rest args)
  "Safely call notification system with update semantics.
This is a compatibility wrapper - prefer using `xcode-project-notify' directly."
  (apply #'xcode-project-notify args))

(defun xcode-project-safe-mode-line-notification (&rest args)
  "Safely call notification system with notification semantics.
This is a compatibility wrapper - prefer using `xcode-project-notify' directly."
  (apply #'xcode-project-notify args))

(defvar xcode-project--current-project-root nil)
(defvar xcode-project--previous-project-root nil)
(defvar xcode-project--current-xcode-scheme nil)
;; Removed: current-build-settings-json - now using swift-cache
(defvar xcode-project--current-buildconfiguration-json-data nil)
(defvar xcode-project--current-build-configuration nil)
(defvar xcode-project--current-app-identifier nil)
(defvar xcode-project--current-build-folder nil)
(defvar xcode-project--current-is-xcode-project nil)
(defvar xcode-project--current-local-device-id nil)
(defvar xcode-project--current-errors-or-warnings nil)
(defvar xcode-project--last-device-type nil)
(defvar xcode-project--device-choice nil
  "Stores the user's choice of device (simulator or physical device).")
(defvar xcode-project--cache-warmed-projects (make-hash-table :test 'equal)
  "Hash table tracking which projects have had their caches warmed.")

(defvar xcode-project--settings-restored-projects (make-hash-table :test 'equal)
  "Hash table tracking which projects have had their settings restored.")

(defconst xcodebuild-list-config-command "xcrun xcodebuild -list -json")

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
  "Run a shell COMMAND and return the JSON output."
  (let* ((json-output (shell-command-to-string command)))
    (when xcode-project-debug
      (message "Command: %s" command)
      (message "JSON output length: %d" (length json-output))
      (message "JSON output preview: %s" (substring json-output 0 (min 200 (length json-output)))))
    (condition-case err
        (json-read-from-string json-output)
      (error
       (when xcode-project-debug
         (message "JSON parsing error: %s" (error-message-string err))
         (message "Full JSON output: %s" json-output))
       (error "JSON parsing failed: %s" (error-message-string err))))))

(defun xcode-project-get-schemes-from-xcodebuild ()
  "Get list of schemes using xcodebuild -list command as fallback."
  (when xcode-project-debug
    (message "Falling back to xcodebuild -list for scheme detection"))
  (condition-case err
      (let* ((project-dir (or (xcode-project-project-root) default-directory))
             (json-data (let ((default-directory project-dir))
                          (xcode-project-run-command-and-get-json "xcrun xcodebuild -list -json 2>/dev/null"))))
        (when json-data
          (let-alist json-data
            (cond
             ;; Workspace case
             (.workspace.schemes
              (when xcode-project-debug
                (message "Found schemes via xcodebuild (workspace): %s" .workspace.schemes))
              .workspace.schemes)
             ;; Project case  
             (.project.schemes
              (when xcode-project-debug
                (message "Found schemes via xcodebuild (project): %s" .project.schemes))
              .project.schemes)
             (t
              (when xcode-project-debug
                (message "No schemes found in xcodebuild output"))
              nil)))))
    (error
     (when xcode-project-debug
       (message "Error running xcodebuild -list: %s" (error-message-string err)))
     nil)))

(cl-defun xcode-project-get-build-settings-json (&key (config "Debug"))
  "Get build settings from xcodebuild CONFIG."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root 
                                              (format "build-settings-%s-%s" 
                                                      (or xcode-project--current-xcode-scheme "default")
                                                      config)))))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((project-dir (or project-root default-directory)))
            (let ((default-directory project-dir)
                  (scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme ""))))
              (xcode-project-run-command-and-get-json 
               (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -configuration %s -json 2>/dev/null" 
                       (xcode-project-get-workspace-or-project)
                       (shell-quote-argument scheme-name)
                       (shell-quote-argument config))))))
      ;; Fallback when swift-cache not available
      (let ((project-dir (or project-root default-directory)))
        (let ((default-directory project-dir)
              (scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme ""))))
          (xcode-project-run-command-and-get-json 
           (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -configuration %s -json 2>/dev/null" 
                   (xcode-project-get-workspace-or-project)
                   (shell-quote-argument scheme-name)
                   (shell-quote-argument config))))))))

(defun xcode-project-product-name ()
  "Get product name."
  (let* ((config (or xcode-project--current-build-configuration "Debug"))
         (json (xcode-project-get-build-settings-json :config config)))
    (let-alist (seq-elt json 0)
      .buildSettings.PRODUCT_NAME)))

(defun xcode-project-get-bundle-identifier (config)
  "Get bundle identifier (as CONFIG)."
  (condition-case err
      (let ((json (xcode-project-get-build-settings-json :config config)))
        (let-alist (seq-elt json 0)
          .buildSettings.PRODUCT_BUNDLE_IDENTIFIER))
    (error
     ;; Fallback: use xcodebuild directly without JSON
     (let* ((scheme-name (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme "")))
            (workspace-or-project (xcode-project-get-workspace-or-project))
            (cmd (format "xcodebuild -showBuildSettings %s -scheme %s -configuration %s 2>/dev/null | grep '^    PRODUCT_BUNDLE_IDENTIFIER' | head -1 | sed 's/.*= //' | tr -d ' '"
                        workspace-or-project
                        (shell-quote-argument scheme-name)
                        (shell-quote-argument config)))
            (output (string-trim (shell-command-to-string cmd))))
       (when xcode-project-debug
         (message "Fallback command: %s" cmd)
         (message "Fallback output: %s" output))
       (if (string-empty-p output)
           (progn
             (when xcode-project-debug
               (message "No bundle identifier found, using generic fallback"))
             "com.example.app")  ; Generic fallback
         output)))))

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


(defun xcode-project-scheme ()
  "Get the xcode scheme if set otherwise prompt user."
  (unless xcode-project--current-xcode-scheme
    (when xcode-project-debug
      (message "xcode-project-scheme - Starting scheme detection..."))
    (xcode-project-notify :message "Loading schemes...")
    (let ((schemes (xcode-project-get-scheme-list)))
      (when xcode-project-debug
        (message "xcode-project-scheme - Found %d schemes: %s" (length schemes) schemes))
      (cond
       ;; No schemes found at all
       ((or (null schemes) (= (length schemes) 0))
        (xcode-project-notify
         :message (propertize "No shared schemes found! Please share schemes in Xcode." 'face 'error)
         :seconds 5
         :reset t)
        (error "No schemes found. In Xcode: Product > Scheme > Manage Schemes, check 'Shared' for your scheme"))

       ;; Exactly one scheme - use it automatically
       ((= (length schemes) 1)
        (let ((scheme (car schemes)))
          (when scheme  ; Double-check it's not nil
            (setq xcode-project--current-xcode-scheme scheme)
            (xcode-project-notify
             :message (format "Selected scheme: %s"
                              (propertize scheme 'face 'success))
             :seconds 2
             :reset t))))

       ;; Multiple schemes - prompt user
       (t
        (xcode-project-notify :message "Choose scheme...")
        (setq xcode-project--current-xcode-scheme
              (xcode-project-build-menu :title "Choose scheme: " :list schemes))
        ;; Only show notification if a scheme was actually selected
        (when xcode-project--current-xcode-scheme
          (xcode-project-notify
           :message (format "Selected scheme: %s"
                            (propertize xcode-project--current-xcode-scheme 'face 'success))
           :seconds 2
           :reset t))))

      ;; Save settings asynchronously to avoid blocking during scheme selection
      (when (and xcode-project--current-xcode-scheme
                 (fboundp 'swift-project-settings-capture-from-variables))
        (run-with-idle-timer 0.1 nil
                             (lambda (root)
                               (swift-project-settings-capture-from-variables root))
                             (xcode-project-project-root)))))
  (if (not xcode-project--current-xcode-scheme)
      (error "No scheme selected")
    (shell-quote-argument xcode-project--current-xcode-scheme)))

(defun xcode-project-fetch-or-load-build-configuration ()
  "Get the build configuration from the scheme file."
  (unless xcode-project--current-build-configuration
    ;; Ensure scheme is loaded first
    (xcode-project-scheme)
    (xcode-project-notify :message "Fetching build configuration...")
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
        (let ((project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (root)
                                   (swift-project-settings-capture-from-variables root))
                                 project-root))))

      (xcode-project-notify
       :message (format "Build config: %s"
                        (propertize xcode-project--current-build-configuration 'face 'font-lock-keyword-face))
       :seconds 2
       :reset t)))
  xcode-project--current-build-configuration)

(defun xcode-project-fetch-or-load-app-identifier ()
  "Get the app identifier for the current configiration."
  (unless xcode-project--current-app-identifier
    ;; Ensure scheme is loaded first
    (xcode-project-scheme)
    (xcode-project-notify :message "Fetching app identifier...")
    (let ((config (xcode-project-fetch-or-load-build-configuration)))
      (when xcode-project-debug
        (message "xcode-project-fetch-or-load-app-identifier - scheme: %s, config: %s"
                 xcode-project--current-xcode-scheme config))
      (setq xcode-project--current-app-identifier (xcode-project-get-bundle-identifier config))
      (when xcode-project-debug
        (message "xcode-project-fetch-or-load-app-identifier - bundle ID: %s"
                 xcode-project--current-app-identifier))

      ;; Save app-identifier to settings asynchronously
      (when (and (fboundp 'swift-project-settings-capture-from-variables)
                 (fboundp 'xcode-project-project-root))
        (let ((project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (root)
                                   (swift-project-settings-capture-from-variables root))
                                 project-root))))

      (xcode-project-notify
       :message (format "App ID: %s"
                        (propertize xcode-project--current-app-identifier 'face 'font-lock-string-face))
       :seconds 2
       :reset t)))
  xcode-project--current-app-identifier)

(defun xcode-project-get-scheme-list ()
  "Get list of project schemes from xcscheme files or xcodebuild -list as fallback."
  (or (xcode-project-list-scheme-files)
      ;; Fallback to xcodebuild -list if no scheme files found
      (progn
        (when xcode-project-debug
          (message "No scheme files found, trying xcodebuild -list..."))
        (xcode-project-get-schemes-from-xcodebuild))))

(cl-defun xcode-project-build-folder (&key (device-type :device))
  "Get build folder. Auto-detect based on scheme, configuration, and device type."
  (when (or (not xcode-project--current-build-folder)
            (not (eq device-type xcode-project--last-device-type)))
    (let* ((scheme (replace-regexp-in-string "^['\"]\\|['\"]$" "" (or xcode-project--current-xcode-scheme "")))
           (config (xcode-project-fetch-or-load-build-configuration))
           (build-products-dir (xcode-project-get-build-products-directory))
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
          ;; Auto-detect build folder
          (setq xcode-project--current-build-folder 
                (xcode-project-auto-detect-build-folder 
                 :build-products-dir build-products-dir
                 :scheme scheme
                 :config config
                 :device-type device-type
                 :target-suffix target-suffix))
          
          ;; Format the full path
          (when xcode-project--current-build-folder
            (setq xcode-project--current-build-folder 
                  (if (file-name-absolute-p xcode-project--current-build-folder)
                      xcode-project--current-build-folder
                    (concat build-products-dir xcode-project--current-build-folder "/"))))
          
          ;; Cache the result
          (when (and cache-key (fboundp 'swift-cache-set) xcode-project--current-build-folder)
            (swift-cache-set cache-key xcode-project--current-build-folder 1800))))  ; Cache for 30 minutes

      (setq xcode-project--last-device-type device-type)

      ;; Save build-folder to settings asynchronously
      (when (and xcode-project--current-build-folder
                 (fboundp 'swift-project-settings-capture-from-variables)
                 (fboundp 'xcode-project-project-root))
        (let ((project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (root)
                                   (swift-project-settings-capture-from-variables root))
                                 project-root))))

      (when xcode-project-debug
        (message "xcode-project-build-folder: scheme=%s config=%s device=%s folder=%s"
                 scheme config device-type xcode-project--current-build-folder))))
  xcode-project--current-build-folder)

(defun xcode-project-get-build-products-directory ()
  "Get the base build products directory path."
  (let* ((project-root (file-name-as-directory (xcode-project-project-root)))
         (local-build-dir (concat project-root ".build/Build/Products/")))
    (if (file-exists-p local-build-dir)
        local-build-dir
      ;; Fallback to derived data path  
      (let ((derived-path (xcode-project-derived-data-path)))
        (cond
         ;; If derived path ends with .build, append Build/Products
         ((string-suffix-p ".build" derived-path)
          (concat derived-path "/Build/Products/"))
         ;; Otherwise use the path as-is with Build/Products
         (t (concat (file-name-as-directory derived-path) "Build/Products/")))))))

(cl-defun xcode-project-auto-detect-build-folder (&key build-products-dir scheme config device-type target-suffix)
  "Intelligently auto-detect the correct build folder."
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
     (xcode-project-interactive-build-folder-selection all-folders target-suffix))))

(defun xcode-project-find-exact-build-folder (folders scheme config target-suffix)
  "Find build folder matching scheme-config-platform pattern."
  (let ((patterns (list
                   ;; Pattern: Debug-iphonesimulator, Release-iphoneos, etc.
                   (format "%s-%s" config target-suffix)
                   ;; Pattern: MyApp_Debug-iphonesimulator
                   (format "%s_%s-%s" scheme config target-suffix)
                   ;; Pattern: MyApp-Debug-iphonesimulator  
                   (format "%s-%s-%s" scheme config target-suffix))))
    (cl-some (lambda (pattern)
              (cl-find-if (lambda (folder) 
                           (string-match-p (regexp-quote pattern) folder))
                         folders))
            patterns)))

(defun xcode-project-find-config-platform-folder (folders config target-suffix)
  "Find build folder matching config-platform pattern."
  (let ((pattern (format "%s.*%s" config target-suffix)))
    (cl-find-if (lambda (folder) 
                 (string-match-p pattern folder))
               folders)))

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
    ;; This handles cases where xcode-project--current-project-root was set by other code
    (when (and (fboundp 'swift-project-settings-restore-to-variables)
               (not (gethash normalized-project xcode-project--settings-restored-projects)))
      (when xcode-project-debug
        (message "[DEBUG] Attempting to restore project settings..."))
      (let ((settings (swift-project-settings-restore-to-variables normalized-project)))
        (when settings
          ;; Mark this project as restored so we don't restore again
          (puthash normalized-project t xcode-project--settings-restored-projects)
          (when xcode-project-debug
            (message "[Settings] Restored settings for project: %s"
                     (file-name-nondirectory normalized-project)))

          ;; Auto-launch simulator if enabled and we have device settings
          (when (and (boundp 'swift-development-auto-launch-simulator)
                     swift-development-auto-launch-simulator
                     (plist-get settings :device-id)
                     (fboundp 'ios-simulator-setup-simulator-dwim))
            (when xcode-project-debug
              (message "[Settings] Auto-launching simulator: %s"
                       (plist-get settings :device-name)))
            (ios-simulator-setup-simulator-dwim (plist-get settings :device-id))))))

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
  (xcode-project-setup-current-project (xcode-project-project-root))
  (xcode-project-notify
   :message (propertize "Project ready" 'face 'success)
   :seconds 2
   :reset t))

;;;###autoload
(defun xcode-project-show-project-info ()
  "Display current project information for debugging."
  (interactive)
  (message "Current project root: %s\nPrevious project root: %s\nApp identifier: %s\nBuild folder: %s\nScheme: %s"
           (or xcode-project--current-project-root "nil")
           (or xcode-project--previous-project-root "nil") 
           (or xcode-project--current-app-identifier "nil")
           (or xcode-project--current-build-folder "nil")
           (or xcode-project--current-xcode-scheme "nil")))

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
  (when (and (ios-device-connected-device-id) (not xcode-project--device-choice))
    (setq xcode-project--device-choice
          (xcode-project-device-or-simulator-menu :title "Run on simulator or device?"))
    (setq current-run-on-device xcode-project--device-choice)))

(defun xcode-project-run-in-simulator ()
  "Return t if app should run in simulator, nil for physical device."
  (if (null xcode-project--device-choice)
      t  ; Default to simulator if not set
    (not xcode-project--device-choice)))

;;;###autoload
(defun xcode-project-reset ()
  "Reset the current project root and device choice.
Also clears all persistent cache files (.swift-development/)."
  (interactive)
  (when (fboundp 'ios-simulator-reset)
    (ios-simulator-reset))
  (when (fboundp 'periphery-kill-buffer)
    (periphery-kill-buffer))

  ;; Clear persistent cache files before resetting variables
  (when (and xcode-project--current-project-root
             (fboundp 'swift-project-settings-clear-all-cache))
    (swift-project-settings-clear-all-cache xcode-project--current-project-root))

  (setq current-run-on-device nil
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
        xcode-project--device-choice nil
        xcode-project--last-device-type nil)  ; Reset device choice

  ;; Clear the restored projects hash to allow restoration after reset
  (clrhash xcode-project--settings-restored-projects)

  ;; NOTE: We intentionally DO NOT reset swift-development--last-build-succeeded here
  ;; to allow each project to maintain its own build status across project switches.
  ;; Use swift-development-reset-build-status to manually reset if needed.
  (swift-project-reset-root)
  (xcode-project-safe-mode-line-update :message "Resetting configuration")
  (message "Xcode configuration reset - scheme cache cleared"))

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

(defun xcode-project-setup-dape()
  "Setup dape."
  (interactive)
  (require 'dape)
  ;; Ensure scheme and project root are loaded first
  (xcode-project-scheme)
  (xcode-project-project-root)
  (add-to-list 'dape-configs
               `(ios
                 modes (swift-mode)
                 command-cwd ,(or (project-root (project-current))
                                  default-directory)
                 command ,(file-name-concat dape-adapter-dir
                                            "codelldb"
                                            "extension"
                                            "adapter"
                                            "codelldb")
                 command-args ("--port" :autoport
                               "--settings" "{\"sourceLanguages\":[\"swift\"]}"
                               "--liblldb" "/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Versions/A/LLDB")
                 port :autoport
                 simulator-id ,(or (ignore-errors (ios-simulator-simulator-identifier))
                                   (error "Failed to get simulator ID"))
                 app-bundle-id ,(or (xcode-project-fetch-or-load-app-identifier)
                                    (error "Failed to get app bundle identifier"))
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
  ;; Start dape directly with the ios configuration
  (let ((config (copy-tree (cdr (assq 'ios dape-configs)))))
    (if config
        (dape config)
      (error "Failed to setup dape configuration"))))


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
      (async-start
       `(lambda ()
          (delete-directory ,package-cache-dir t)
          "Swift package cache cleaned")
       (lambda (result)
         (message "%s" result))))
    
    (when (file-exists-p cloned-sources-dir)
      (message "Cleaning Swift cloned sources...")
      (async-start
       `(lambda ()
          (delete-directory ,cloned-sources-dir t)
          "Swift cloned sources cleaned")
       (lambda (result)
         (message "%s" result))))))

(defun xcode-project-clean-project-derived-data ()
  "Clean Xcode derived data for the current project."
  (let* ((project-name (xcode-project-product-name))
         (derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (project-pattern (concat "^" (regexp-quote project-name) "-")))
    
    (when (file-exists-p derived-data-dir)
      (message "Cleaning derived data for %s..." project-name)
      (async-start
       `(lambda ()
          (let ((cleaned-count 0))
            (dolist (dir (directory-files ,derived-data-dir t ,project-pattern))
              (when (file-directory-p dir)
                (delete-directory dir t)
                (setq cleaned-count (1+ cleaned-count))))
            (format "Cleaned %d derived data folder(s) for %s" cleaned-count ,project-name)))
       (lambda (result)
         (message "%s" result))))))

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
          (async-start
           `(lambda ()
              (dolist (dir (directory-files ,derived-data-dir t "^[^.]"))
                (when (and (file-directory-p dir)
                           (not (string-match-p "ModuleCache" dir)))
                  (delete-directory dir t)))
              "All derived data cleaned")
           (lambda (result)
             (message "%s" result))))))))

(cl-defun xcode-project-clean-build-folder-with (&key root build-folder project-name ignore-list)
  "Clean build folder with ROOT, BUILD-FOLDER, PROJECT-NAME asynchronously.
IGNORE-LIST is a list of folder names to ignore during cleaning."
  (when xcode-project-debug
    (message "Cleaning build folder: %s for %s" build-folder project-name))
  (let ((default-directory build-folder))
    (if (file-directory-p default-directory)
        (progn
          (xcode-project-safe-mode-line-update
           :message (format "Cleaning build folder for %s"
                            (propertize project-name 'face 'warning)))
          (async-start
           `(lambda ()
              ,(async-inject-variables "default-directory")
              (defun delete-directory-contents (directory ignore-list)
                "Delete contents of DIRECTORY, ignoring folders in IGNORE-LIST."
                (dolist (file (directory-files directory t))
                  (let ((file-name (file-name-nondirectory file)))
                    (unless (or (member file-name '("." ".."))
                                (member file-name ignore-list))
                      (if (file-directory-p file)
                          (progn
                            (delete-directory file t t))
                        (delete-file file t))))))
              (condition-case err
                  (progn
                    (delete-directory-contents ,default-directory ',ignore-list) "successfully")
                (error (format "Error during cleaning: %s" (error-message-string err)))))
           `(lambda (result)
              (xcode-project-safe-mode-line-notification
               :message (format "Cleaning %s %s"
                                (propertize ,project-name 'face 'warning)
                                result)
               :seconds 3
               :reset t))))
      (xcode-project-safe-mode-line-notification
       :message (propertize "Build folder is empty or does not exist." 'face 'warning)
       :seconds 3
       :reset t))))

(defun xcode-project-open-in-xcode ()
  "Open project in xcode."
  (interactive)
  (if-let* ((default-directory (xcode-project-project-root))
           (command "xed ."))
      (inhibit-sentinel-messages #'call-process-shell-command command)))

(cl-defun xcode-project-parse-compile-lines-output (&key input)
  "Parse compile output and print unique matched lines using separate message calls.
   Also prints compiler messages for C++ errors, warnings, and notes."
  (when xcode-project-debug
    (message "Parsing compile output: %s" input))
  (let ((seen-messages (make-hash-table :test 'equal))
        (error-regex "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(error\\|warning\\|note\\): .+$"))
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
        (let ((msg (format "Building target: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-builtin-face)))
            (puthash msg t seen-messages))))
       ;; Linking
       ((string-match "\\bLd\\b.*/\\([^/[:space:]]+\\)\\(?: normal\\| \\)" line)
        (let* ((filename (match-string 1 line))
               ;; Filter out very short names (< 2 chars), argument flags, and paths with extensions
               (is-meaningful (and filename
                                  (>= (length filename) 2)
                                  (not (string-prefix-p "-" filename))
                                  (not (string-match-p "\\." filename))
                                  (not (string-match-p "^[a-z]$" filename)))))
          (when is-meaningful
            (let ((msg (format "Linking: %s" filename)))
              (unless (gethash msg seen-messages)
                (xcode-project-safe-mode-line-update :message
                                                       (format "  %s" (propertize msg 'face 'font-lock-keyword-face)))
                (puthash msg t seen-messages))))))
       ;; Code signing
       ((string-match "CodeSign \\(.+\\)/\\([^/[:space:]]+\\.app\\)" line)
        (let ((msg (format "Signing: %s" (match-string 2 line))))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-constant-face)))
            (puthash msg t seen-messages))))
       ;; C compilation
       ((string-match "CompileC.*/\\([^/[:space:]]+\\.[cm]\\)\\b" line)
        (let ((msg (format "Compiling C: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ((string-match "CompileSwiftModule \\([^ ]+\\)" line)
        (let ((msg (format "Compiling Swift: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'warning)))
            (puthash msg t seen-messages))))
       ;; Swift files
       ((string-match "CompileSwift.*\\([^/]+\\.swift\\)" line)
        (let ((msg (format "Swift: %s" (match-string 1 line))))
          (unless (gethash msg seen-messages)
            (xcode-project-safe-mode-line-update :message
                                                   (format "  %s" (propertize msg 'face 'font-lock-type-face)))
            (puthash msg t seen-messages))))
       ;; Errors, warnings, notes
       ((string-match error-regex line)
        (setq xcode-project--current-errors-or-warnings (concat line "\n" xcode-project--current-errors-or-warnings))
        (periphery-run-parser xcode-project--current-errors-or-warnings))))))

(defun xcode-project-derived-data-path ()
  "Get the actual DerivedData path by running xcodebuild -showBuildSettings."
  (let* ((default-directory (or (xcode-project-project-root) default-directory))
         (config (or xcode-project--current-build-configuration "Debug"))
         (json (xcode-project-get-build-settings-json :config config)))
    (when json
      (let-alist (seq-elt json 0)
        (or .buildSettings.BUILD_DIR
            .buildSettings.SYMROOT
            (concat (xcode-project-project-root) "/.build"))))))

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

(defun xcode-project-toggle-device-choice ()
  "Toggle between simulator and physical device."
  (interactive)
  (setq xcode-project--device-choice (not xcode-project--device-choice))
  (setq current-run-on-device xcode-project--device-choice)
  (message "Now running on %s" (if xcode-project--device-choice "physical device" "simulator")))

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
                 (if xcode-project--device-choice "Physical Device" "Simulator")
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

(provide 'xcode-project)
;;; xcode-project.el ends here
