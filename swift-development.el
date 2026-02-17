;;; swift-development.el --- Package for compiling and running Swift apps in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, xcode, development
;; URL: https://github.com/konrad1977/swift-development

;;; Commentary:

;; Package for building and running iOS/macOS apps from Emacs

;;; Code:

(require 'cl-lib)
(require 'compile) ;; For compilation-mode when not using periphery
(require 'transient)
(require 'ios-device nil t)
(require 'ios-simulator nil t)
(require 'swift-cache nil t) ;; Unified caching system
(require 'swift-error-proxy nil t) ;; Unified error parsing proxy
(require 'swift-file-watcher nil t) ;; File change detection for instant rebuild checks
(require 'swift-macro-manager nil t) ;; Swift macro approval management
(require 'swift-package-manager nil t) ;; SPM dependency management
(require 'swift-test-explorer nil t) ;; Test explorer and test running
(require 'swift-project-settings nil t) ;; Persistent project settings
(require 'swiftui-preview nil t) ;; SwiftUI preview support
(require 'xcode-build-config nil t) ;; Build configuration and command construction
(require 'xcode-project nil t) ;; For notification system
(require 'swift-notification nil t) ;; Unified notification system with progress bars
(require 'swift-incremental-build nil t) ;; Incremental build pipeline
(require 'swift-refactor nil t) ;; Code refactoring tools
(require 'xcode-archive nil t) ;; Archive, export, and TestFlight distribution

;; Forward declarations for optional dependencies
;; Spinner (visual progress indicators)
(defvar spinner-current)
(declare-function spinner-start "spinner" (type))
(declare-function spinner-stop "spinner" (&optional spinner))

;; Periphery (error parsing and display) - accessed via swift-error-proxy
(declare-function periphery-kill-buffer "periphery" ())
(declare-function periphery-clear "periphery" ())
(declare-function periphery-helper:filter-keep-beginning-paths "periphery-helper" (text))

;; Async shell commands (custom keyword-based interface from periphery)
(declare-function async-shell-command-to-string "periphery" (&rest args))
(declare-function async-start-command-to-string "periphery" (&rest args))
(declare-function message-with-color "periphery" (&rest args))

;; Swift macro manager
(declare-function spm-macro-check-and-offer-approval "swift-macro-manager" (build-output))

;; Incremental build
(declare-function swift-incremental-build-ready-p "swift-incremental-build")
(declare-function swift-incremental-build-compile-and-run "swift-incremental-build")
(declare-function swift-incremental-build-compile "swift-incremental-build")
(declare-function swift-incremental-build-extract-from-build-output "swift-incremental-build")

;; Swift test explorer
(declare-function swift-test-explorer-show "swift-test-explorer")
(declare-function swift-test-run-at-point "swift-test-explorer")
(declare-function swift-test-run-class "swift-test-explorer")
(declare-function swift-test-run-all "swift-test-explorer")
(declare-function swift-test-transient "swift-test-explorer")

;; Transient menus from other modules
(declare-function ios-simulator-transient "ios-simulator")
(declare-function ios-device-transient "ios-device")
(declare-function spm-transient "swift-package-manager")
(declare-function swiftui-preview-transient "swiftui-preview")
(declare-function xcode-project-transient "xcode-project")
(declare-function swift-refactor-transient "swift-refactor")
(declare-function periphery-transient "periphery")
(declare-function xcode-archive-transient "xcode-archive")

;; Provide fallbacks when periphery is not available
(unless (fboundp 'message-with-color)
  (cl-defun message-with-color (&key tag text attributes)
    "Fallback for message-with-color when periphery is not loaded."
    (ignore attributes)  ; Suppress unused variable warning
    (message "%s %s" (or tag "") (or text ""))))

;; Declare periphery functions - they will be loaded on demand
(declare-function periphery-toggle-buffer "periphery" nil)
(declare-function periphery-transient "periphery" nil)

;; Error proxy (unified error parsing)
(declare-function swift-error-proxy-parse-output "swift-error-proxy")
(declare-function swift-error-proxy-parse-test-output "swift-error-proxy")
(declare-function swift-error-proxy-has-errors-p "swift-error-proxy")
(declare-function swift-error-proxy-has-warnings-p "swift-error-proxy")
(declare-function swift-error-proxy-clear "swift-error-proxy")
(declare-function swift-error-proxy-kill-buffer "swift-error-proxy")
(declare-function swift-error-proxy-effective-backend "swift-error-proxy")
(defvar swift-error-proxy-backend)

(defun swift-development--periphery-toggle-buffer ()
  "Toggle periphery buffer, or show message if not available."
  (interactive)
  (if (fboundp 'periphery-toggle-buffer)
      (call-interactively #'periphery-toggle-buffer)
    (message "Periphery package is not installed")))

(defun swift-development--periphery-transient ()
  "Open periphery transient menu, or show message if not available."
  (interactive)
  (if (fboundp 'periphery-transient)
      (call-interactively #'periphery-transient)
    (message "Periphery package is not installed")))

(defgroup swift-additions nil
  "Swift development tools and utilities for Emacs."
  :group 'programming
  :prefix "swift-additions")

(defcustom swift-development-debug nil
  "Enable debug mode for swift additions."
  :type 'boolean
  :group 'swift-development)

(defcustom swift-development-use-periphery t
  "Whether to use periphery for error parsing.
Deprecated: use `swift-error-proxy-backend' instead.
Kept for backward compatibility -- synced by `swift-development-toggle-periphery-mode'."
  :type 'boolean
  :group 'swift-development)
(make-obsolete-variable 'swift-development-use-periphery
                        'swift-error-proxy-backend "0.7.0")

(defcustom swift-development-analysis-mode 'fast
  "Level of post-build analysis to perform.
- 'fast: Quick analysis, async periphery with truncation (recommended)
- 'full: Complete analysis, may be slower for large builds
- 'minimal: Basic success check only, fastest option
- 'disabled: Skip all analysis except build success detection"
  :type '(choice (const :tag "Fast (async with truncation)" fast)
                 (const :tag "Full (complete analysis)" full)
                 (const :tag "Minimal (basic check only)" minimal)
                 (const :tag "Disabled (success check only)" disabled))
  :group 'swift-development)

(defcustom swift-development-watched-extensions
  '("swift" "m" "mm" "h" "c" "cpp" "storyboard" "xib" "xcassets")
  "List of file extensions to watch for changes that trigger rebuilds.
Only files with these extensions will be checked for modifications.
Default includes source code and UI resources that affect the app bundle."
  :type '(repeat string)
  :group 'swift-development)

(defcustom swift-development-cache-ttl 300
  "Time-to-live for build cache in seconds.
Cache is automatically invalidated when files are saved in Emacs.
This TTL provides a safety fallback for external changes (git, Xcode, etc).
Default is 300 seconds (5 minutes).
Set to 0 to disable caching (not recommended for large projects)."
  :type 'integer
  :group 'swift-development)

(defcustom swift-development-ignore-paths
  '("*Tests/*" "*/Tests.swift" "*UITests/*" "*/.build/*" "*/DerivedData/*" "*/.git/*")
  "List of path patterns to ignore when checking if rebuild is needed.
Test files and build artifacts are ignored by default.
Patterns can use wildcards (* and ?). Examples:
- \"*Tests/*\" - ignores files in any Tests directory
- \"*/Generated/*\" - ignores files in Generated directories
- \"*Pods/*\" - ignores CocoaPods dependencies
- \"*/.build/*\" - ignores Xcode build artifacts
- \"*/DerivedData/*\" - ignores DerivedData"
  :type '(repeat string)
  :group 'swift-development)

(defcustom swift-development-auto-launch-simulator t
  "Automatically launch simulator when entering a project with saved settings.
When non-nil, the simulator will start automatically when opening a Swift project
that has saved settings in .swift-development/settings.
Set to nil to disable automatic simulator launching."
  :type 'boolean
  :group 'swift-development)

;; Internal variables
(defvar swift-development--build-progress-spinner nil)
(defvar swift-development--current-build-progress 0
  "Current build progress percentage (0-100). Only increases, never decreases.")
(defvar swift-development--active-build-process nil
  "Currently active build process, if any.")
(defvar swift-development--active-build-buffer nil
  "Buffer name for the active build process.")
(defvar swift-development--compilation-time nil)
(defvar swift-development--last-build-succeeded nil
  "Track if the last build succeeded.
nil = unknown/never built, t = success, \\='failed = failed.")
(defvar swift-development--current-build-command nil
  "Current build command being used.")
(defvar swift-development--device-choice 'unset
  "Whether to run on physical device (t) or simulator (nil).
Value is \\='unset if no choice has been made yet.")

(defvar swift-development--force-next-build nil
  "When non-nil, force build regardless of file watcher status.
Set to t after scheme change or reset, cleared after successful build.")

(defvar swift-development--force-full-build nil
  "When non-nil, the next build uses xcodebuild (skipping incremental).
Automatically reset to nil after one build.")

(defvar swift-development--build-context nil
  "Captured build context for the current build session.
Plist with :root, :build-folder, :app-id, :sim-id, :scheme, :device-id.")

(defvar run-once-compiled nil
  "When non-nil, run the app after successful build.")

(defvar swift-development-force-package-resolution nil
  "When non-nil, force Swift package resolution on next build.")

(defun swift-development--capture-build-context (&optional for-device)
  "Capture current build context for use in callbacks.
When FOR-DEVICE is non-nil, captures device-specific context."
  (setq swift-development--build-context
        (if for-device
            (list :root (xcode-project-project-root)
                  :build-folder (xcode-project-build-folder :device-type :device)
                  :app-id (xcode-project-fetch-or-load-app-identifier)
                  :device-id (when (fboundp 'ios-device-udid) (ios-device-udid))
                  :scheme xcode-project--current-xcode-scheme)
          (list :root (xcode-project-project-root)
                :build-folder (xcode-project-build-folder :device-type :simulator)
                :app-id (xcode-project-fetch-or-load-app-identifier)
                :sim-id (ios-simulator-simulator-identifier)
                :scheme xcode-project--current-xcode-scheme)))
  (when swift-development-debug
    (message "[Build Context] Captured: %S" swift-development--build-context))
  swift-development--build-context)

(defun swift-development-log-debug (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when swift-development-debug
    (apply #'message (concat "[Swift Debug] " format-string) args)))

(defun swift-development-cleanup ()
  "Cleanup resources and state."
  (when swift-development--build-progress-spinner
    (spinner-stop swift-development--build-progress-spinner))
  (setq swift-development--current-build-command nil
        swift-development--compilation-time nil
        swift-development-force-package-resolution nil))  ; Reset force flag after build

(defun swift-development-handle-build-error (error-message)
  "Handle build ERROR-MESSAGE and display appropriate feedback.
Also checks for Swift macro approval errors and offers to approve them."
  (when swift-development-debug
    (message "Build error: %s" error-message))
  (swift-development-cleanup)
  
  ;; Check for macro approval errors first - offer to approve and rebuild
  (if (and (fboundp 'spm-macro-check-and-offer-approval)
           (spm-macro-check-and-offer-approval error-message))
      ;; User approved macros - trigger rebuild
      (swift-development-compile-app)
    ;; No macro errors (or user declined to approve) - show normal error output
    (when (fboundp 'xcode-project-notify)
      (xcode-project-notify
       :message (format "Build failed: %s"
                        (propertize (truncate-string-to-width error-message 50) 'face 'error))))
    (swift-error-proxy-parse-output error-message)))

(define-obsolete-function-alias 'swift-development-show-errors-in-compilation-mode
  #'ignore "0.7.0" "Use `swift-error-proxy-parse-output' instead.")

(defun swift-development-reset ()
  "Reset build settings and clear all cached state.
This also invalidates the build status to force a rebuild on next compile."
  (interactive)
  ;; Reset device-choice BEFORE xcode-project-reset so it can be set fresh
  (setq swift-development--device-choice 'unset)
  (ios-device-reset)
  (xcode-build-config-reset)
  (xcode-project-reset)
  (setq swift-development--build-progress-spinner nil
        swift-development--compilation-time nil
        run-once-compiled nil
        swift-development--last-build-succeeded nil)  ; Force rebuild next time
  ;; Also clear the build folder cache
  (setq xcode-project--current-build-folder nil
        xcode-project--last-device-type nil)
  ;; Reset file watcher
  (when (fboundp 'swift-file-watcher-stop)
    (swift-file-watcher-stop))
  ;; Restart file watcher if we're in a project
  (let ((project-root (swift-project-root nil t)))
    (when (and project-root (fboundp 'swift-file-watcher-start))
      (run-with-idle-timer 0.5 nil
                           (lambda ()
                             (swift-file-watcher-start project-root)))))
  (swift-notification-send :message "Build settings reset - next build will run unconditionally" :seconds 3))

(defun swift-development-toggle-device-choice ()
  "Toggle between running on simulator and physical device.
When switching to physical device, prompts for device selection."
  (interactive)
  ;; If unset, default to simulator (nil), then toggle will set to device (t)
  (setq swift-development--device-choice
        (if (eq swift-development--device-choice 'unset)
            t  ; First toggle: unset -> device
          (not swift-development--device-choice)))
  ;; Clear cached build folder since it's device-type dependent
  (setq xcode-project--current-build-folder nil
        xcode-project--last-device-type nil)
  ;; When switching to physical device, prompt for device selection
  (when (eq swift-development--device-choice t)
    (ios-device-reset)
    (ios-device-select-device))
  ;; Save settings
  (when (and (fboundp 'swift-project-settings-capture-from-variables)
             (fboundp 'xcode-project-project-root))
    (let ((root (xcode-project-project-root)))
      (when root
        (swift-project-settings-capture-from-variables root))))
  (swift-notification-send
   :message (format "Now running on %s"
                    (if (eq swift-development--device-choice t) "physical device" "simulator"))
   :seconds 3))

;; Legacy alias for backwards compatibility
(defalias 'xcode-project-toggle-device-choice 'swift-development-toggle-device-choice)

(defun swift-development--compilation-time ()
  "Get the time of the compilation."
  (if-let* ((end-time (current-time))
            (start-time swift-development--compilation-time))
      (format "%.1f" (float-time (time-subtract end-time start-time)))
    "N/A"))

(defun swift-development--post-build-common ()
  "Common post-build steps: notify, save settings, refresh build info.
Returns the build context for further use."
  (let* ((ctx swift-development--build-context)
         (scheme-raw (or (plist-get ctx :scheme) "Unknown"))
         (scheme-name (if (fboundp 'xcode-project--clean-display-name)
                          (xcode-project--clean-display-name scheme-raw)
                        scheme-raw)))
    ;; Notify build completion
    (when (fboundp 'xcode-project-notify)
      (xcode-project-notify
       :message (format "Built %s in %s seconds"
                        (propertize scheme-name 'face 'font-lock-builtin-face)
                        (propertize (swift-development--compilation-time) 'face 'warning))))

    ;; Update progress bar
    (when (fboundp 'swift-notification-progress-update)
      (swift-notification-progress-update 'swift-build :percent 65 :message "Installing..."))

    ;; Save last-modified file to settings
    (let* ((project-root (or (plist-get ctx :root) (xcode-project-project-root)))
           (scheme (plist-get ctx :scheme))
           (last-modified (swift-development-get-last-modified-file))
           (for-device (eq swift-development--device-choice t))
           (sdk (if for-device "iphoneos" "iphonesimulator")))
      (when (and project-root last-modified (fboundp 'swift-project-settings-update))
        (swift-project-settings-update project-root :last-modified-file last-modified))
      ;; Refresh build settings from xcodebuild (async)
      (when (and project-root scheme
                 (fboundp 'swift-project-settings-fetch-build-info))
        (swift-project-settings-fetch-build-info project-root scheme sdk nil)))
    ctx))

(defun swift-development-run-app-on-device-after-build ()
  "Run app on device after build using captured context."
  (swift-development--post-build-common)
  ;; Use captured context for installation
  (let* ((ctx swift-development--build-context)
         (project-root (or (plist-get ctx :root) (xcode-project-project-root)))
         (scheme (plist-get ctx :scheme))
         (app-id (or (plist-get ctx :app-id) (xcode-project-fetch-or-load-app-identifier)))
         (build-folder (plist-get ctx :build-folder))
         (saved-folder (when (fboundp 'swift-project-settings-get-build-dir)
                         (swift-project-settings-get-build-dir project-root scheme "iphoneos"))))
    (let ((folder-to-use (or (and saved-folder (file-directory-p saved-folder) saved-folder)
                             (and build-folder (file-directory-p build-folder) build-folder))))
      (if folder-to-use
          (ios-device-install-app
           :buildfolder folder-to-use
           :appIdentifier app-id)
        (swift-notification-progress-update 'swift-build :percent 65 :message "Locating build...")
        (xcode-project-build-folder-async
         (lambda (folder)
           (if folder
               (progn
                 (when swift-development-debug
                   (message "[Install] Got build folder from xcodebuild: %s" folder))
                 (ios-device-install-app
                  :buildfolder folder
                  :appIdentifier app-id))
             (swift-notification-progress-cancel 'swift-build)
             (message "Error: Could not determine build folder after build")))
         :device-type :device)))))

(defun swift-development-run-app-after-build ()
  "Run app in simulator after build using captured context."
  (swift-development--post-build-common)
  (swift-development-cleanup)
  ;; Use captured context for installation
  (let* ((ctx swift-development--build-context)
         (root (or (plist-get ctx :root) (xcode-project-project-root)))
         (scheme (plist-get ctx :scheme))
         (sim-id (or (plist-get ctx :sim-id) (ios-simulator-simulator-identifier)))
         (app-id (or (plist-get ctx :app-id) (xcode-project-fetch-or-load-app-identifier)))
         (build-folder (plist-get ctx :build-folder))
         (saved-folder (when (fboundp 'swift-project-settings-get-build-dir)
                         (swift-project-settings-get-build-dir root scheme "iphonesimulator"))))
    (let ((folder-to-use (or (and saved-folder (file-directory-p saved-folder) saved-folder)
                             (and build-folder (file-directory-p build-folder) build-folder))))
      (if folder-to-use
          (ios-simulator-install-and-run-app
           :rootfolder root
           :build-folder folder-to-use
           :simulatorId sim-id
           :appIdentifier app-id)
        (swift-notification-progress-update 'swift-build :percent 65 :message "Locating build...")
        (xcode-project-build-folder-async
         (lambda (folder)
           (if folder
               (progn
                 (when swift-development-debug
                   (message "[Install] Got build folder from xcodebuild: %s" folder))
                 (ios-simulator-install-and-run-app
                  :rootfolder root
                  :build-folder folder
                  :simulatorId sim-id
                  :appIdentifier app-id))
             (swift-notification-progress-cancel 'swift-build)
             (message "Error: Could not determine build folder after build")))
         :device-type :simulator)))))

(defun swift-development-check-if-build-was-successful (input-text)
  "Check if INPUT-TEXT indicates a successful build.
  
  Optimized to check only the last part of the output where xcodebuild
  writes its final status, avoiding false positives from build output."
  (when swift-development-debug 
    (message "Checking build success with output length: %d" (length input-text)))
  
  ;; xcodebuild always puts "BUILD SUCCEEDED" or "BUILD FAILED" near the end
  ;; Check only the last 5000 characters for performance and accuracy
  (let* ((text-length (length input-text))
         (check-region (if (> text-length 5000)
                          (substring input-text (- text-length 5000))
                        input-text))
         ;; Look for explicit build status markers
         (has-success nil)
         (has-failure nil))
    
    ;; Check for BUILD SUCCEEDED - this is the definitive success marker
    (when (string-match-p xcode-build-config-build-succeeded-pattern check-region)
      (setq has-success t))

    ;; Check for explicit failure markers
    (when (or (string-match-p xcode-build-config-build-failed-pattern check-region)
              (string-match-p xcode-build-config-build-interrupted-pattern check-region)
              ;; Only check for xcodebuild-specific errors
              (string-match-p xcode-build-config-xcodebuild-error-pattern check-region)
              (string-match-p "^Command failed with exit code" check-region)
              (string-match-p "^The following build commands failed:" check-region))
      (setq has-failure t))
    
    (when swift-development-debug 
      (message "Build check - Success: %s, Failure: %s (checked last %d chars)" 
               has-success has-failure (length check-region)))
    
    ;; Build is successful only if we found BUILD SUCCEEDED and no failures
    (and has-success (not has-failure))))

(defun swift-development-check-for-errors (output callback)
  "Run error checking on OUTPUT, then call CALLBACK if build successful.
Analysis level controlled by `swift-development-analysis-mode`."
  (swift-development-log-debug "Checking for errors in output length: %d chars" (length output))
  
  ;; Always check if build was successful
  (condition-case err
      (when (swift-development-check-if-build-was-successful output)
        (funcall callback))
    (error
     (swift-development-handle-build-error (error-message-string err))))
  
  ;; Run analysis based on configured mode
  (pcase swift-development-analysis-mode
    ('disabled
     ;; Skip all analysis
     nil)
    ('minimal
     ;; Only basic error detection, no UI updates
     (let ((swift-error-proxy-backend 'minimal))
       (swift-error-proxy-parse-output output)))
    ('fast
     ;; Async analysis with truncation (default)
     (swift-error-proxy-parse-output output t))
    ('full
     ;; Full synchronous analysis (may be slow)
     (swift-error-proxy-parse-output output))))

(define-obsolete-function-alias 'swift-development-run-minimal-analysis
  #'ignore "0.7.0" "Use `swift-error-proxy-parse-output' with minimal backend instead.")

(define-obsolete-function-alias 'swift-development-run-periphery-async
  #'ignore "0.7.0" "Use `swift-error-proxy-parse-output' with ASYNC parameter instead.")

(define-obsolete-function-alias 'swift-development-truncate-output-intelligently
  #'ignore "0.7.0" "Use `swift-error-proxy--truncate-output' instead.")

(defun swift-development-run-xcode-build-server-parse (output)
  "Run xcode-build-server parse on OUTPUT asynchronously, avoiding .compile conflicts."
  (when (executable-find "xcode-build-server")
    (let* ((project-root (xcode-project-project-root))
           (compile-lock (expand-file-name ".compile.lock" project-root)))
      ;; Only clean up lock file if it exists (not the .compile file since we want to append)
      (when (file-exists-p compile-lock)
        (delete-file compile-lock))
      
      ;; Wait a moment to ensure build processes have finished writing
      ;; Capture project-root now to avoid issues when default-directory changes
      (let ((captured-root project-root))
        (run-with-timer 1.0 nil
                        (lambda ()
                          (let ((temp-file (make-temp-file "xcodebuild-output-" nil ".log")))
                            ;; Write output to temp file
                            (with-temp-file temp-file
                              (insert output))
                            ;; Run xcode-build-server parse with captured project root
                            (let ((proc (start-process
                                         "xcode-build-server-parse"
                                         nil
                                         "sh" "-c"
                                         (format "cd %s && cat %s | xcode-build-server parse -a"
                                                 (shell-quote-argument captured-root)
                                                 (shell-quote-argument temp-file)))))
                              (set-process-sentinel
                               proc
                               (lambda (_p _e)
                                 ;; Always cleanup temp file when process finishes
                                 (when (file-exists-p temp-file)
                                   (condition-case nil
                                       (delete-file temp-file)
                                     (file-error nil)))))))))))))

(defun swift-development-successful-build ()
  "Show that the build was successful and save last-modified file."
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (format "Successful build %s"
                      (propertize (xcode-project-scheme-display-name) 'face 'font-lock-builtin-face))))

  ;; Mark file watcher as built (instant, clears dirty flag)
  (when (fboundp 'swift-file-watcher-mark-built)
    (swift-file-watcher-mark-built))

  ;; Save last-modified file to settings (fast, synchronous)
  (let* ((project-root (xcode-project-project-root))
         (scheme xcode-project--current-xcode-scheme)
         (last-modified (swift-development-get-last-modified-file)))
    (when (and project-root last-modified
               (fboundp 'swift-project-settings-update))
      (swift-project-settings-update project-root :last-modified-file last-modified)
      (when swift-development-debug
        (message "[Build Success] Saved last-modified: %s at %s"
                 (cdr last-modified) (car last-modified))))

    ;; Refresh build settings from xcodebuild (async, updates deployment-target etc.)
    (when (and project-root scheme
               (fboundp 'swift-project-settings-fetch-build-info))
      (let ((sdk (if (eq swift-development--device-choice t) "iphoneos" "iphonesimulator")))
        (swift-project-settings-fetch-build-info
         project-root scheme sdk
         (lambda (settings)
           (when (and settings swift-development-debug)
             (message "[Build Success] Settings refreshed: target=%s module=%s"
                      (plist-get settings :deployment-target)
                      (plist-get settings :swift-module-name)))))))))

(cl-defun swift-development-compile-with-progress (&key command callback update-callback)
  "Run compilation COMMAND with progress indicator and CALLBACK/UPDATE-CALLBACK in background.
Returns a cons cell (PROCESS . LOG-BUFFER) where LOG-BUFFER accumulates the build output."
  (if (eq (swift-error-proxy-effective-backend) 'compilation)
      ;; Use compilation-mode directly
      (progn
        ;; Clean up any existing build process first
        (when (and swift-development--active-build-process
                   (process-live-p swift-development--active-build-process))
          (message "Killing existing build process before starting new one")
          (delete-process swift-development--active-build-process)
          (setq swift-development--active-build-process nil))
        (setq swift-development--compilation-time (current-time))
        (let ((compilation-buffer-name-function (lambda (_) "*Swift Build*"))
              (compilation-scroll-output t)
              (compilation-auto-jump-to-first-error nil))
          (compile command)
          ;; Set up a sentinel to handle completion
          (let ((proc (get-buffer-process (get-buffer "*Swift Build*"))))
            (when proc
              ;; Track active build process
              (setq swift-development--active-build-process proc)
              (setq swift-development--active-build-buffer "*Swift Build*")
              (set-process-sentinel 
               proc 
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   ;; Clear active process tracking
                   (setq swift-development--active-build-process nil)
                   (setq swift-development--active-build-buffer nil)
                   (let ((exit-status (process-exit-status process)))
                     (if (= exit-status 0)
                         (progn
                           (setq swift-development--last-build-succeeded t)
                           ;; Clear force flag after successful build
                           (setq swift-development--force-next-build nil)
                           (when (and callback (functionp callback))
                             (funcall callback "Build succeeded")))
                       ;; Build failed - do NOT call callback to prevent installation
                       (progn
                         (setq swift-development--last-build-succeeded 'failed)
                         ;; Extract actual error from build buffer
                         (let* ((build-buf (get-buffer "*Swift Build*"))
                                (error-msg (if (and build-buf (buffer-live-p build-buf))
                                               (with-current-buffer build-buf
                                                 (save-excursion
                                                   (goto-char (point-max))
                                                   ;; Look for actual xcodebuild errors
                                                   (if (re-search-backward xcode-build-config-simple-error-pattern nil t)
                                                       (buffer-substring-no-properties
                                                        (line-beginning-position)
                                                        (min (+ (point) 500) (point-max)))
                                                     (format "Build failed with exit status %s" exit-status))))
                                             (format "Build failed with exit status %s" exit-status))))
                           (swift-development-handle-build-error error-msg))
                         ;; Reset run-once-compiled to prevent installation
                         (setq run-once-compiled nil)))))))))
          nil))
    ;; Async implementation with make-process
    (progn
      ;; Clean up any existing build process first
      (when (and swift-development--active-build-process
                 (process-live-p swift-development--active-build-process))
        (message "Killing existing build process before starting new one")
        (delete-process swift-development--active-build-process)
        (setq swift-development--active-build-process nil))
      (spinner-start 'progress-bar-filled)
      (setq swift-development--build-progress-spinner spinner-current
            swift-development--compilation-time (current-time))

      ;; Update progress bar (already started in swift-development-compile)
      (when (fboundp 'swift-notification-progress-update)
        (swift-notification-progress-update 'swift-build
                                            :percent 15
                                            :title "Building"
                                            :message (or (xcode-project-scheme) "Project")))

      ;; Create or get the output buffer for real-time display
      (let* ((output-buffer-name "*Swift Build Output*")
             (output-buffer (get-buffer-create output-buffer-name))
             (log-buffer (generate-new-buffer " *xcodebuild-log*")))
        
        ;; Clear and prepare output buffer
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Building with xcodebuild...\n")
            (insert (format "Command: %s\n\n" command))))
        
        ;; Don't display buffer automatically - user can switch to it manually if needed
        
        (let ((process (make-process
                        :name "xcodebuild-background"
                        :buffer log-buffer
                        ;; Use exec to ensure the shell waits for the command to complete
                        :command (list shell-file-name shell-command-switch (concat "exec " command))
                        :noquery t   ; Detach from Emacs process list
                        :sentinel (let ((captured-default-directory default-directory))
                                    (lambda (proc event)
                                      (when (memq (process-status proc) '(exit signal))
                                        ;; Restore default-directory for all operations in sentinel
                                        (let ((default-directory captured-default-directory))
                                          ;; Clear active process tracking
                                          (setq swift-development--active-build-process nil)
                                          (setq swift-development--active-build-buffer nil)
                                          (spinner-stop swift-development--build-progress-spinner)
                                          (let* ((exit-status (process-exit-status proc))
                                                 (output (with-current-buffer log-buffer
                                                           (buffer-string))))
                                            (when swift-development-debug
                                              (message "Build process exited with status: %d, event: %s"
                                                       exit-status event))
                                            ;; Check both exit status and output
                                            (if (and (= exit-status 0)
                                                     (swift-development-check-if-build-was-successful output))
                                                (progn
                                                  (setq swift-development--last-build-succeeded t)
                                                  ;; Clear force flag after successful build
                                                  (setq swift-development--force-next-build nil)
                                                  (when (buffer-live-p output-buffer)
                                                    (with-current-buffer output-buffer
                                                      (let ((inhibit-read-only t))
                                                        (goto-char (point-max))
                                                        (insert "\nBUILD SUCCEEDED\n"))))
                                                  ;; Update progress to 60% (build done, ready for install)
                                                  (setq swift-development--current-build-progress 60)
                                                  ;; Use run-with-timer to avoid "call-process recursively"
                                                  (run-with-timer
                                                   0 nil
                                                   (lambda ()
                                                     (when (fboundp 'swift-notification-progress-update)
                                                       (swift-notification-progress-update
                                                        'swift-build :percent 60 :message "Build complete"))))
                                                  ;; Run xcode-build-server parse asynchronously
                                                  (swift-development-run-xcode-build-server-parse output)
                                                  ;; Extract incremental build commands for next time
                                                  (when (fboundp 'swift-incremental-build-extract-from-build-output)
                                                    (run-with-timer
                                                     0.5 nil
                                                     #'swift-incremental-build-extract-from-build-output output))
                                                  (when (and callback (functionp callback))
                                                    (funcall callback output))
                                                  (swift-development-cleanup))
                                              (progn
                                                (setq swift-development--last-build-succeeded 'failed)
                                                ;; Cancel progress bar on failure (use timer to avoid recursive call-process)
                                                (run-with-timer
                                                 0 nil
                                                 (lambda ()
                                                   (when (fboundp 'swift-notification-progress-cancel)
                                                     (swift-notification-progress-cancel 'swift-build))))
                                                (when (buffer-live-p output-buffer)
                                                  (with-current-buffer output-buffer
                                                    (let ((inhibit-read-only t))
                                                      (goto-char (point-max))
                                                      (insert (format "\nBUILD FAILED (exit status: %d)\n" exit-status)))))
                                                ;; Run xcode-build-server parse for errors too
                                                (swift-development-run-xcode-build-server-parse output)
                                                (swift-development-handle-build-error output)))
                                            (when (buffer-live-p log-buffer)
                                              (kill-buffer log-buffer))))))))))

          ;; Configure process handling
          (set-process-query-on-exit-flag process nil)

          ;; Track active build process
          (setq swift-development--active-build-process process)
          (setq swift-development--active-build-buffer (buffer-name log-buffer))

          ;; Start async output processing - show output in real-time
          (set-process-filter process
                              (lambda (_proc string)
                                ;; Save to log buffer
                                (with-current-buffer log-buffer
                                  (goto-char (point-max))
                                  (insert string))
                                ;; Display output to visible buffer in real-time
                                (with-current-buffer output-buffer
                                  (let ((inhibit-read-only t))
                                    (goto-char (point-max))
                                    (insert string)
                                    ;; Auto-scroll to bottom
                                    (let ((windows (get-buffer-window-list output-buffer nil t)))
                                      (dolist (window windows)
                                        (with-selected-window window
                                          (goto-char (point-max))
                                          (recenter -1))))))
                                ;; Update progress bar based on build phase (only if going forward)
                                ;; Use run-with-timer to avoid "call-process recursively" in filter
                                (let ((new-progress nil)
                                      (new-message nil))
                                  (cond
                                   ((string-match-p "Compiling\\|CompileC\\|CompileSwift" string)
                                    (setq new-progress 15 new-message "Compiling..."))
                                   ((string-match-p "Linking\\|Ld " string)
                                    (setq new-progress 40 new-message "Linking..."))
                                   ((string-match-p "CodeSign\\|Signing" string)
                                    (setq new-progress 50 new-message "Signing..."))
                                   ((string-match-p "Touch\\|CpResource\\|ProcessInfoPlist" string)
                                    (setq new-progress 55 new-message "Finishing...")))
                                  ;; Only update if new progress is higher than current
                                  (when (and new-progress
                                             (> new-progress swift-development--current-build-progress))
                                    (setq swift-development--current-build-progress new-progress)
                                    (run-with-timer
                                     0 nil
                                     (lambda ()
                                       (when (fboundp 'swift-notification-progress-update)
                                         (swift-notification-progress-update 'swift-build
                                                                             :percent new-progress
                                                                             :message new-message))))))
                                ;; Call update callback if provided
                                (when (and update-callback (functionp update-callback))
                                  (funcall update-callback string))))

          (swift-development-log-debug "Running command: %s" command)
          (cons process log-buffer))))))

(defcustom swift-development-use-async-rebuild-check t
  "Deprecated: kept for backwards compatibility.
Rebuild detection is now always synchronous (fast enough to not block)."
  :type 'boolean
  :group 'swift-development)

(defun swift-development--run-up-to-date-app (run)
  "Run an already-built app when build is up-to-date.
RUN should be non-nil to actually launch the app."
  (when run
    (condition-case err
        (let* ((for-device (eq swift-development--device-choice t))
               (device-type (if for-device :device :simulator))
               (root (xcode-project-project-root))
               (build-folder (xcode-project-build-folder :device-type device-type))
               (app-id (xcode-project-fetch-or-load-app-identifier)))
          (if for-device
              ;; Physical device path
              (progn
                (when swift-development-debug
                  (message "Running on physical device (build up-to-date)"))
                (ios-device-install-app
                 :buildfolder build-folder
                 :appIdentifier app-id))
            ;; Simulator path
            (let* ((sim-id (ios-simulator-simulator-identifier))
                   (app-name (when (fboundp 'ios-simulator-get-app-name-fast)
                               (ios-simulator-get-app-name-fast build-folder))))
              (when swift-development-debug
                (message "Run up-to-date: root=%s build-folder=%s sim-id=%s app-id=%s app-name=%s"
                         root build-folder sim-id app-id app-name))
              ;; Check for nil values before running - if missing, trigger build
              (if (or (not root) (not build-folder) (not sim-id) (not app-id) (not app-name))
                  (progn
                    (when swift-development-debug
                      (message "Missing values - triggering build instead of run"))
                    (swift-development--do-compile :run t))
                ;; All values present - run directly
                (swift-error-proxy-kill-buffer)
                (when (fboundp 'ios-simulator-kill-buffer)
                  (ios-simulator-kill-buffer))
                (require 'ios-simulator)
                (ios-simulator-install-and-run-app
                 :rootfolder root
                 :build-folder build-folder
                 :appIdentifier app-id
                 :simulatorId sim-id)))))
      (error
       (message "Error running app: %s" (error-message-string err))
       (message "Try running with prefix arg to force rebuild: C-u C-c C-c")))))

(cl-defun swift-development-compile (&key run force)
  "Build project using xcodebuild (as RUN).
If FORCE is non-nil, always recompile even if sources are unchanged."

   ;; Clear error list immediately at build start
  (swift-error-proxy-clear)

  ;; Show immediate feedback - start progress bar right away
  (setq swift-development--current-build-progress 0)
  (when (fboundp 'swift-notification-progress-start)
    (swift-notification-progress-start
     :id 'swift-build
     :title "Preparing"
     :message "Starting build..."
     :percent 0))

  (if (xcode-project-is-xcodeproject)
      (progn
        ;; Setup current project first to ensure settings are loaded
        ;; Handles project switching - fast when project hasn't changed
        (when (fboundp 'xcode-project-setup-current-project)
          (xcode-project-setup-current-project (swift-project-root)))

        ;; Start simulator early (async, non-blocking) so it's ready when build completes
        (when (and run (not (eq swift-development--device-choice t)))
          (require 'ios-simulator)
          (let ((sim-id (ios-simulator-simulator-identifier)))
            (when sim-id
              (ios-simulator-setup-simulator-dwim sim-id))))

        ;; Check if rebuild is needed (fast synchronous check)
        ;; force-full-build bypasses this check too
        (if (and (not force)
                 (not swift-development--force-full-build)
                 (not (swift-development-needs-rebuild-p)))
            (progn
              ;; Finish progress bar - build was skipped
              (when (fboundp 'swift-notification-progress-finish)
                (swift-notification-progress-finish 'swift-build "Build up-to-date"))
              (swift-notification-send
               :message (propertize "Build: skipped (sources unchanged)" 'face 'success)
               :seconds 3)
              ;; Run the already-built app if requested
              (swift-development--run-up-to-date-app run))
          ;; Sources changed or force - try incremental first, fallback to full
          (if (and (not force)
                   (not swift-development--force-full-build)
                   (not (eq swift-development--device-choice t)) ;; simulator only
                   (fboundp 'swift-incremental-build-ready-p)
                   (swift-incremental-build-ready-p))
              (progn
                ;; Cancel the swift-build progress (incremental has its own)
                (when (fboundp 'swift-notification-progress-cancel)
                  (swift-notification-progress-cancel 'swift-build))
                (swift-notification-send
                 :message (propertize "Build: incremental (bypassing xcodebuild)" 'face 'warning)
                 :seconds 3)
                (if run
                    (swift-incremental-build-compile-and-run)
                  (swift-incremental-build-compile)))
            ;; Full xcodebuild
            (setq swift-development--force-full-build nil)
            (swift-notification-send
              :message (propertize "Build: full xcodebuild" 'face 'font-lock-keyword-face)
              :seconds 3)
            (swift-development--do-compile :run run))))
    (if (swift-development-is-a-swift-package-base-project)
        (swift-development-build-swift-package)
      (progn
        ;; Cancel progress bar - not a valid project
        (when (fboundp 'swift-notification-progress-cancel)
          (swift-notification-progress-cancel 'swift-build))
        (swift-notification-send :message "Not xcodeproject nor swift package" :seconds 3)))))

(cl-defun swift-development--do-compile (&key run)
  "Internal function to perform the actual compilation.
RUN specifies whether to run after building."
  ;; Save modified buffers before building
  (save-some-buffers t)
  ;; Update progress bar - starting compilation
  (when (fboundp 'swift-notification-progress-update)
    (swift-notification-progress-update 'swift-build
                                        :percent 10
                                        :title "Building"
                                        :message "Compiling..."))
  (swift-error-proxy-kill-buffer)
  (ios-simulator-kill-buffer)
  ;; Only ask for device/simulator if not already set
  (when (eq swift-development--device-choice 'unset)
    (xcode-addition-ask-for-device-or-simulator))
  (if (not (xcode-project-run-in-simulator))
      (swift-development-compile-for-device :run run)
    (swift-development-compile-for-simulator :run run)))

(defun swift-development--find-bridging-header-fast (dir)
  "Find bridging header in DIR using fast shallow search.
Only checks root directory and immediate subdirectories to avoid
expensive recursive search.  Returns the first matching file or nil."
  (let ((pattern ".*-Bridging-Header\\.h$")
        (result nil))
    ;; First check root directory
    (setq result (car (directory-files dir t pattern)))
    ;; If not found, check immediate subdirectories (but not deeper)
    (unless result
      (dolist (subdir (directory-files dir t "^[^.]" t) result)
        (when (and (file-directory-p subdir)
                   (not result)
                   ;; Skip common deep directories
                   (not (member (file-name-nondirectory subdir)
                               '("Pods" "Carthage" ".build" "DerivedData" 
                                 "node_modules" ".git" "build" "Build"))))
          (setq result (car (directory-files subdir t pattern))))))
    result))

(cl-defun swift-development-warm-build-cache ()
  "Warm up build caches and precompile common modules."
  (interactive)
  (cl-block swift-development-warm-build-cache
    (let* ((project-root (xcode-project-project-root))
           (cache-key (if (fboundp 'swift-cache-project-key)
                         (swift-cache-project-key project-root "build-cache-warmed")
                       nil)))
      ;; Check if already warmed for this project
      (when (and cache-key (fboundp 'swift-cache-get))
        (when (swift-cache-get cache-key)
          (when (fboundp 'xcode-project-notify)
            (xcode-project-notify
             :message (propertize "Build caches already warmed (cached)" 'face 'success)
             :seconds 2
             :reset t))
          (cl-return-from swift-development-warm-build-cache)))
    
    (let ((default-directory project-root)
          (cache-dir (expand-file-name "~/Library/Caches/org.swift.swiftpm/ModuleCache")))
      
      ;; Ensure cache directories exist
      (dolist (dir (list cache-dir
                         (expand-file-name "~/Library/Caches/org.swift.packages")
                         (expand-file-name "~/Library/Caches/org.swift.cloned-sources")
                         (expand-file-name "~/Library/Developer/Xcode/DerivedData/ModuleCache")))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      
      ;; Precompile common system frameworks asynchronously
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (propertize "Warming build caches..." 'face 'font-lock-builtin-face)))
      (dolist (framework '("Foundation" "UIKit" "SwiftUI" "Combine" "CoreData" "CoreGraphics"))
        (start-process-shell-command
         (format "cache-%s" framework)
         nil
         (format "xcrun swiftc -emit-module -module-name %s -sdk $(xcrun --sdk iphonesimulator --show-sdk-path) -target arm64-apple-ios15.0-simulator -O -whole-module-optimization /dev/null 2>/dev/null || true" framework)))

      ;; Precompile bridging headers if they exist
      ;; Only check common locations (root and immediate subdirs) to avoid slow recursive search
      (let ((bridging-header (swift-development--find-bridging-header-fast default-directory)))
        (when bridging-header
          (start-process-shell-command
           "cache-bridging"
           nil
           (format "xcrun clang -x objective-c-header -arch arm64 -isysroot $(xcrun --sdk iphonesimulator --show-sdk-path) -c %s -o /tmp/bridging.pch 2>/dev/null || true" bridging-header))))

      ;; Mark as warmed in cache
      (when (and cache-key (fboundp 'swift-cache-set))
        (swift-cache-set cache-key t 7200))  ; Cache for 2 hours

      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (propertize "Build cache warming initiated in background" 'face 'success)
         :seconds 2
         :reset t))))))

(defun swift-development-precompile-common-headers ()
  "Precompile common headers to speed up subsequent builds."
  (when swift-development-debug
    (message "Precompiling common headers..."))
  
  (let ((default-directory (xcode-project-project-root))
        (headers-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData/SharedPrecompiledHeaders")))
    
    ;; Create headers directory if it doesn't exist
    (unless (file-exists-p headers-dir)
      (make-directory headers-dir t))
    
    ;; Find common Swift/Objective-C headers with better optimization
    (dolist (framework '("Foundation" "UIKit" "SwiftUI" "Combine"))
      (let ((cmd (format "xcrun clang -x objective-c-header -arch arm64 -O2 -fmodules -fcxx-modules -isysroot $(xcrun --sdk iphonesimulator --show-sdk-path) -I$(xcrun --sdk iphonesimulator --show-sdk-path)/System/Library/Frameworks/%s.framework/Headers -o %s/%s.pch /dev/null 2>/dev/null || true"
                        framework headers-dir framework)))
        (when swift-development-debug
          (message "Running: %s" cmd))
        (start-process-shell-command (format "precompile-%s" framework) nil cmd)))))

(cl-defun swift-development--compile-target (&key run for-device)
  "Internal compile function for both simulator and device targets.
RUN: run app after build.  FOR-DEVICE: t for physical device, nil for simulator."
  (swift-development-cleanup)
  (xcode-build-config-setup-build-environment :for-device for-device)
  (xcode-build-config-enable-build-caching)
  (setq swift-development--current-build-command nil)
  ;; Clear build folder cache to ensure fresh detection
  (setq xcode-project--current-build-folder nil
        xcode-project--last-device-type nil)
  (xcode-project-setup-project)
  (setq run-once-compiled run)

  ;; Capture build context before async build starts
  (swift-development--capture-build-context for-device)

  (let* ((build-command (if for-device
                            (xcode-build-config-build-app-command
                             :device-id (ios-device-udid))
                          (xcode-build-config-build-app-command
                           :sim-id (ios-simulator-simulator-identifier))))
         (default-directory (xcode-project-project-root))
         (target-name (if for-device
                          (propertize "Physical Device" 'face 'font-lock-negation-char-face)
                        (propertize (swift-development-format-simulator-name
                                    (ios-simulator-simulator-name))
                                   'face 'font-lock-negation-char-face)))
         (run-callback (if for-device
                           #'swift-development-run-app-on-device-after-build
                         #'swift-development-run-app-after-build)))

    (when swift-development-debug
      (swift-development-log-debug "Build-folder: %s" (xcode-project-derived-data-path)))

    (when (fboundp 'xcode-project-notify)
      (xcode-project-notify
       :message (format "Building: %s|%s"
                        (propertize (xcode-project-scheme-display-name)
                                   'face 'font-lock-builtin-face)
                        target-name)
       :seconds 3))

    (xcode-project-setup-xcodebuildserver)

    (if (not (eq (swift-error-proxy-effective-backend) 'compilation))
        (swift-development-compile-with-progress
         :command build-command
         :callback (lambda (text)
                     (if run-once-compiled
                         (swift-development-check-for-errors text run-callback)
                       (swift-development-check-for-errors text #'swift-development-successful-build)))
         :update-callback (lambda (text)
                            (xcode-project-parse-compile-lines-output :input text)))
      (swift-development-compile-with-progress
       :command build-command
       :callback (lambda (_text)
                   (if run-once-compiled
                       (funcall run-callback)
                     (swift-development-successful-build)))
       :update-callback nil))))

(cl-defun swift-development-compile-for-simulator (&key run)
  "Compile app for simulator with optional RUN after completion."
  (swift-development--compile-target :run run :for-device nil))

(defun swift-development-compile-for-device (&key run)
  "Compile and optionally RUN on device."
  (swift-development--compile-target :run run :for-device t))

(defcustom swift-development-analyze-max-diagnostics 200
  "Maximum number of diagnostics to show from static analysis.
Errors are always included; warnings are truncated after this limit."
  :type 'integer
  :group 'swift-development)

(defun swift-development--extract-diagnostics (output)
  "Extract unique actionable diagnostic lines from build OUTPUT.
Keeps only file-level errors and warnings (file:line:col: severity:),
plus build-level errors.  Skips notes (context-only, not actionable)
and build-system noise.  Deduplicates to avoid repeating per-target.
Limits total results to `swift-development-analyze-max-diagnostics'
to prevent Emacs from freezing on large projects."
  (let ((lines (split-string output "\n"))
        ;; Only file-level errors/warnings + build-level errors (no notes)
        (diagnostic-regex
         "^\\(?:.+:[0-9]+:[0-9]+: \\(?:error\\|warning\\):\\|^xcodebuild: error:\\|^ld: \\|^Undefined symbol\\|The following build commands failed:\\|\\*\\* \\(?:BUILD\\|ANALYZE\\) FAILED \\*\\*\\)")
        (seen (make-hash-table :test 'equal))
        (result nil)
        (count 0)
        (total-unique 0)
        (max-items swift-development-analyze-max-diagnostics))
    (dolist (line lines)
      (when (and line
                 (not (string-empty-p line))
                 (string-match-p diagnostic-regex line)
                 (not (gethash line seen)))
        (puthash line t seen)
        (cl-incf total-unique)
        ;; Always include errors, limit warnings
        (when (or (< count max-items)
                  (string-match-p "error:" line))
          (push line result)
          (cl-incf count))))
    (when (> total-unique max-items)
      (message "Analysis: showing %d of %d diagnostics (limit: %d)"
               count total-unique max-items))
    (string-join (nreverse result) "\n")))

;;;###autoload
(defun swift-development-analyze-app ()
  "Run static analysis on the current project using xcodebuild analyze.
Finds potential bugs, memory leaks, and logic errors.
Results are displayed through the error proxy (same format as build errors/warnings)."
  (interactive)
  (unless (xcode-project-is-xcodeproject)
    (user-error "Static analysis requires an Xcode project"))
  (save-some-buffers t)
  (swift-error-proxy-clear)
  ;; Clear accumulated errors from previous builds
  (setq xcode-project--current-errors-or-warnings nil)
  (setq xcode-project--seen-diagnostics nil)
  ;; Setup project if needed
  (xcode-project-setup-project)
  (let* ((sim-id (when (xcode-project-run-in-simulator)
                   (ios-simulator-simulator-identifier)))
         (device-id (unless sim-id
                      (when (fboundp 'ios-device-udid) (ios-device-udid))))
         (analyze-command (xcode-build-config-analyze-command
                           :sim-id sim-id
                           :device-id device-id))
         (default-directory (xcode-project-project-root)))
    ;; Start progress notification
    (when (fboundp 'swift-notification-progress-start)
      (swift-notification-progress-start
       :id 'swift-analyze
       :title "Analyzing"
       :message "Running static analysis..."
       :percent 0))
    (swift-notification-send :message "Starting static analysis..." :seconds 2)
    ;; Run analysis with progress
    (swift-development-compile-with-progress
     :command analyze-command
     :callback (lambda (text)
                 (when (fboundp 'swift-notification-progress-finish)
                   (swift-notification-progress-finish 'swift-analyze "Analysis complete"))
                 (swift-development-cleanup)
                 ;; Clear live-parsed results, then re-parse with :analyze context
                 ;; to show [ ANALYZER ] badges instead of [ WARNING ]
                 (swift-error-proxy-clear)
                 ;; Filter to only diagnostic lines before parsing
                 ;; (full output can be 100KB+ of build log, freezes Emacs)
                 (let ((diagnostics (swift-development--extract-diagnostics text)))
                   (if (swift-error-proxy-has-errors-p diagnostics)
                       (progn
                         (swift-notification-send
                          :message "Analysis found issues" :seconds 3)
                         (swift-error-proxy-parse-output diagnostics nil :analyze))
                     (if (swift-error-proxy-has-warnings-p diagnostics)
                         (progn
                           (swift-notification-send
                            :message "Analysis complete with warnings" :seconds 3)
                           (swift-error-proxy-parse-output diagnostics nil :analyze))
                       (swift-notification-send
                        :message "Analysis complete - no issues found" :seconds 3)))))
     :update-callback (lambda (text)
                         ;; Show progress with live error parsing (uses default compiler parsers
                         ;; for real-time feedback; final callback re-parses with :analyze context)
                         (xcode-project-parse-compile-lines-output
                          :input text)))))

;; ============================================================================
;; Smart Build Detection - Skip compilation if sources unchanged
;; ============================================================================

;; ============================================================================
;; Source File Discovery
;; ============================================================================

(defvar swift-development--fd-available 'unknown
  "Cache whether fd is available.  One of `unknown', t, or nil.")

(defun swift-development--fd-available-p ()
  "Return non-nil if `fd' is available on the system."
  (when (eq swift-development--fd-available 'unknown)
    (setq swift-development--fd-available
          (eq 0 (condition-case nil
                    (call-process "fd" nil nil nil "--version")
                  (error 1)))))
  swift-development--fd-available)

(defun swift-development--build-fd-command ()
  "Build an fd command that finds source files respecting watched extensions.
fd automatically respects .gitignore so .build/, DerivedData/, .git/ are excluded."
  (let ((ext-args (mapconcat (lambda (ext) (format "-e %s" ext))
                             swift-development-watched-extensions
                             " "))
        ;; Convert glob ignore patterns to fd --exclude flags
        ;; fd patterns don't need the leading */
        (exclude-args (mapconcat
                       (lambda (pattern)
                         (format "--exclude '%s'" pattern))
                       swift-development-ignore-paths
                       " ")))
    (format "fd -t f %s %s" ext-args exclude-args)))

(defun swift-development--build-find-patterns ()
  "Build find command patterns for watched file extensions."
  (mapconcat (lambda (ext)
               (format "-name '*.%s'" ext))
             swift-development-watched-extensions
             " -o "))

(defun swift-development--build-find-excludes ()
  "Build find command exclusion patterns from `swift-development-ignore-paths'."
  (mapconcat (lambda (pattern)
               (format "-not -path '%s'" pattern))
             (append '("*/.*" "*/DerivedData/*")
                     swift-development-ignore-paths)
             " "))

(defun swift-development--build-source-file-cmd ()
  "Build command to list all source files.  Prefers fd over find."
  (if (swift-development--fd-available-p)
      (swift-development--build-fd-command)
    (let ((patterns (swift-development--build-find-patterns))
          (excludes (swift-development--build-find-excludes)))
      (format "find . \\( %s \\) %s -type f" patterns excludes))))

(defun swift-development-find-all-source-files ()
  "Find all source files in project (cached for 60 seconds).
Includes files matching `swift-development-watched-extensions'.
Excludes paths matching patterns in `swift-development-ignore-paths'.
Uses fd when available for significantly faster results."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "source-files")))
         (cmd (swift-development--build-source-file-cmd)))
    (swift-cache-with cache-key swift-development-cache-ttl
      (let ((default-directory project-root)
            (output (swift-async-run-sync cmd :timeout 30)))
        (split-string (or output "") "\n" t)))))

(defun swift-development-find-all-source-files-async (callback)
  "Find all source files in project asynchronously.
Calls CALLBACK with list of files when complete.  Does not block Emacs.
Includes files matching `swift-development-watched-extensions'.
Excludes paths matching patterns in `swift-development-ignore-paths'.
Uses fd when available for significantly faster results."
  (let* ((project-root (xcode-project-project-root))
         (cache-key (when (fboundp 'swift-cache-project-key)
                      (swift-cache-project-key project-root "source-files")))
         (cmd (swift-development--build-source-file-cmd)))

    ;; Check cache first
    (if (and cache-key (fboundp 'swift-cache-get))
        (let ((cached (swift-cache-get cache-key)))
          (if cached
              (progn
                (when swift-development-debug
                  (message "Using cached source file list (%d files)" (length cached)))
                (funcall callback cached))
            ;; Cache miss - fetch async
            (swift-development--find-files-async project-root cmd cache-key callback)))
      ;; No cache available - fetch async
      (swift-development--find-files-async project-root cmd nil callback))))

(defun swift-development--find-files-async (project-root find-cmd cache-key callback)
  "Run find command asynchronously and call CALLBACK with results.
PROJECT-ROOT is the root directory, FIND-CMD is the find command.
CACHE-KEY if non-nil will be used to cache results."
  (let ((default-directory project-root))
    (swift-async-run
     find-cmd
     (lambda (output)
       (let ((files (split-string (or output "") "\n" t)))
         (when cache-key
           (swift-cache-set cache-key files 60))
         (when swift-development-debug
           (message "Found %d source files asynchronously" (length files)))
         (funcall callback files)))
     :timeout 30)))


(defun swift-development-get-built-app-path ()
  "Get path to the built .app bundle."
  (let* ((build-folder (xcode-project-build-folder
                       :device-type (if (xcode-project-run-in-simulator)
                                        :simulator
                                      :device)))
         (product-name (xcode-project-product-name))
         (app-path (when (and build-folder product-name)
                    (expand-file-name (format "%s.app" product-name) build-folder))))
    (when (and app-path (file-exists-p app-path))
      app-path)))

(defun swift-development-get-last-modified-file ()
  "Get the most recently modified source file in project.
Returns (timestamp . filepath) or nil if no files found.
Uses fd when available (~60ms) with fallback to find (~5s)."
  (let* ((project-root (xcode-project-project-root))
         (default-directory project-root)
         ;; Use fd | xargs stat for ~100x speedup over find -exec stat
         ;; fd respects .gitignore automatically, excluding .build/, DerivedData/, .git/
         (cmd (if (swift-development--fd-available-p)
                  (format "%s -0 | xargs -0 stat -f '%%m %%N' | sort -rn | head -1"
                          (swift-development--build-fd-command))
                (let ((patterns (mapconcat (lambda (ext) (format "-name \"*.%s\"" ext))
                                          swift-development-watched-extensions
                                          " -o "))
                      (ignore-patterns (when swift-development-ignore-paths
                                         (mapconcat (lambda (pattern)
                                                      (format "! -path \"%s\"" pattern))
                                                    swift-development-ignore-paths
                                                    " "))))
                  (format "find . \\( %s \\) %s -exec stat -f \"%%m %%N\" {} + 2>/dev/null | sort -rn | head -1"
                          patterns (or ignore-patterns "")))))
         (output (string-trim (or (swift-async-run-sync cmd :timeout 10) ""))))

    (when swift-development-debug
      (message "[Last-Modified] Command: %s" cmd)
      (message "[Last-Modified] Output: %s" output))

    (when (and output (not (string-empty-p output)))
      (if (string-match "^\\([0-9]+\\) \\(.+\\)$" output)
          (let ((result (cons (match-string 1 output) (match-string 2 output))))
            (when swift-development-debug
              (message "[Last-Modified] Found: %s at %s" (cdr result) (car result)))
            result)
        (when swift-development-debug
          (message "[Last-Modified] Failed to parse output"))
        nil))))

(defun swift-development-needs-rebuild-p ()
  "Return t if rebuild is needed, nil if app is up-to-date.
Checks in order:
1. Force flag (set after scheme change/reset)
2. Build folder exists on disk
3. .app bundle exists
4. Last build status
5. File watcher / last-modified timestamps"
  (let* ((build-folder (xcode-project-build-folder 
                        :device-type (if (eq swift-development--device-choice t) :device :simulator)))
         (app-path (swift-development-get-built-app-path))
         (project-root (xcode-project-project-root))
         (needs-rebuild nil))

    ;; Priority checks - these always force a build
    (cond
     ;; Check 1: Force flag is set (after reset/scheme change)
     (swift-development--force-next-build
      (setq needs-rebuild t)
      (when swift-development-debug
        (message "[Rebuild Check] Force flag set - build needed")))

     ;; Check 2: Build folder doesn't exist on disk
     ((or (not build-folder) (not (file-directory-p build-folder)))
      (setq needs-rebuild t)
      (when swift-development-debug
        (message "[Rebuild Check] Build folder missing (%s) - build needed" 
                 (or build-folder "nil"))))

     ;; Check 3: No .app bundle exists
     ((not app-path)
      (setq needs-rebuild t)
      (when swift-development-debug
        (message "[Rebuild Check] No .app exists - build needed")))

     ;; Check 4: Last build failed
     ((eq swift-development--last-build-succeeded 'failed)
      (setq needs-rebuild t)
      (when swift-development-debug
        (message "[Rebuild Check] Last build failed - build needed")))

     ;; Check 5: File watcher or timestamp check
     (t
      (if (and (fboundp 'swift-file-watcher-active-p)
               (swift-file-watcher-active-p))
          (progn
            (setq needs-rebuild (swift-file-watcher-needs-rebuild-p))
            (when swift-development-debug
              (message "[Rebuild Check] File watcher: %s"
                       (if needs-rebuild "changes detected" "up-to-date"))))
        ;; Fallback: Compare last modified file with saved value
        (let* ((current-last-modified (swift-development-get-last-modified-file))
               (saved-last-modified (when (fboundp 'swift-project-settings-get)
                                     (swift-project-settings-get project-root :last-modified-file))))
          (when swift-development-debug
            (message "[Rebuild Check] Timestamp mode"))
          (when (or (not saved-last-modified)
                    (not (equal current-last-modified saved-last-modified)))
            (setq needs-rebuild t)
            (when swift-development-debug
              (message "[Rebuild Check] File changed - build needed")))))))

    needs-rebuild))

(defun swift-development-needs-rebuild-async-p (callback)
  "Check if rebuild is needed and call CALLBACK with result.
Now synchronous since last-modified check is so fast (0.1-0.5s).
Kept async signature for backward compatibility."
  ;; Just call the synchronous version and invoke callback immediately
  ;; The new method is fast enough that async is not needed
  (let ((needs-rebuild (swift-development-needs-rebuild-p)))
    (funcall callback needs-rebuild)))

(defun swift-development-ensure-built (&optional force)
  "Ensure app is built.  Build only if needed unless FORCE is non-nil.
Returns t if build was performed, nil if skipped.
BLOCKING: May freeze Emacs on large projects. Consider using -async version."
  (interactive "P")
  (if (or force (swift-development-needs-rebuild-p))
      (progn
        (when force
          (message "Forcing rebuild..."))
        (when (and (not force) swift-development-debug)
          (message "Changes detected, building..."))
        (swift-development-compile-and-run)
        t)
    (progn
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (propertize "Build up-to-date, skipping compilation" 'face 'success)
         :seconds 2
         :reset t))
      nil)))

;;;###autoload
(defun swift-development-ensure-built-async (&optional force)
  "Ensure app is built asynchronously. Build only if needed unless FORCE is non-nil.
Non-blocking version suitable for large projects.
Shows message while checking, builds if needed."
  (interactive "P")
  (if force
      (progn
        (message "Forcing rebuild...")
        (swift-development-compile-and-run))
    (progn
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (propertize "Checking build cache..." 'face 'font-lock-keyword-face)
         :seconds 2))
      (swift-development-needs-rebuild-async-p
       (lambda (needs-rebuild)
         (if needs-rebuild
             (progn
               (when swift-development-debug
                 (message "Changes detected, building..."))
               (swift-development-compile-and-run))
           (when (fboundp 'xcode-project-notify)
             (xcode-project-notify
              :message (propertize "Build up-to-date, skipping compilation" 'face 'success)
              :seconds 2
              :reset t))))))))

;;;###autoload
(defun swift-development-reset-build-status ()
  "Reset the build status tracking.
This forces the next build to run regardless of file timestamps."
  (interactive)
  (setq swift-development--last-build-succeeded nil)
  (swift-notification-send :message "Build status reset - next build will run unconditionally" :seconds 3))

;;;###autoload
(defun swift-development-clear-hash-cache ()
  "Clear all cache files for the current project.
Clears settings, device-cache, and file-cache from .swift-development/."
  (interactive)
  ;; Clear persistent cache files
  (when (and (fboundp 'xcode-project-project-root)
             (fboundp 'swift-project-settings-clear-all-cache))
    (let ((project-root (xcode-project-project-root)))
      (when project-root
        (swift-project-settings-clear-all-cache project-root))))

  (swift-notification-send :message "All cache files cleared for current project" :seconds 3))

;;;###autoload
(defun swift-development-build-status ()
  "Show current build status and whether rebuild is needed."
  (interactive)
  (let* ((app-path (swift-development-get-built-app-path))
         (app-mtime (when app-path
                     (file-attribute-modification-time (file-attributes app-path))))
         (needs-rebuild (swift-development-needs-rebuild-p))
         (project-root (xcode-project-project-root))
         (last-modified (swift-development-get-last-modified-file))
         (saved-last-modified (when (fboundp 'swift-project-settings-get)
                               (swift-project-settings-get project-root :last-modified-file))))

    (with-current-buffer (get-buffer-create "*Swift Build Status*")
      (erase-buffer)
      (insert "Swift Build Status\n")
      (insert "==================\n\n")

      (insert (format "Project: %s\n" (xcode-project-project-root)))
      (insert (format "Scheme: %s\n" (or xcode-project--current-xcode-scheme "Not set")))
      (insert (format "Target: %s\n\n"
                     (if (xcode-project-run-in-simulator) "Simulator" "Device")))

      (if app-path
          (progn
            (insert (format "Built App: %s\n" app-path))
            (insert (format "Last Built: %s\n\n"
                           (if app-mtime
                               (format-time-string "%Y-%m-%d %H:%M:%S" app-mtime)
                             "Unknown"))))
        (insert "Built App: Not found\n\n"))

      (insert "Last Modified File (Current):\n")
      (if last-modified
          (insert (format "  %s (timestamp: %s)\n\n" (cdr last-modified) (car last-modified)))
        (insert "  Not found\n\n"))

      (insert "Last Modified File (Saved):\n")
      (if saved-last-modified
          (insert (format "  %s (timestamp: %s)\n\n" (cdr saved-last-modified) (car saved-last-modified)))
        (insert "  Not saved yet\n\n"))

      (insert (format "Last Build Result: %s\n"
                     (cond ((eq swift-development--last-build-succeeded t)
                            (propertize "Success" 'face 'success))
                           ((eq swift-development--last-build-succeeded 'failed)
                            (propertize "Failed" 'face 'error))
                           (t "Unknown"))))

      (insert (format "Status: %s\n"
                     (if needs-rebuild
                         (propertize "Rebuild Needed" 'face 'warning)
                       (propertize "Up-to-date" 'face 'success))))

      (when needs-rebuild
        (insert "\nReason: ")
        (cond
         ((not app-path)
          (insert "No built app found"))
         ((eq swift-development--last-build-succeeded 'failed)
          (insert (propertize "Last build failed" 'face 'error)))
         ((and last-modified app-mtime (> (car last-modified) (float-time app-mtime)))
          (insert (format "Source files modified (%s)" (cdr last-modified))))
         (t
          (insert "Files changed since last build"))))

      (display-buffer (current-buffer)))))

;;;###autoload
(defun swift-development-show-last-build-errors ()
  "Show the last 50 lines of the build output, focusing on errors."
  (interactive)
  (if-let* ((build-buffer (get-buffer "*Swift Build*")))
      (with-current-buffer (get-buffer-create "*Swift Build Errors*")
        (erase-buffer)
        (insert "Last Build Errors\n")
        (insert "=================\n\n")
        (insert (format "Build Status: %s\n\n"
                       (cond ((eq swift-development--last-build-succeeded t)
                              (propertize "Success" 'face 'success))
                             ((eq swift-development--last-build-succeeded 'failed)
                              (propertize "Failed" 'face 'error))
                             (t "Unknown"))))

        ;; Extract and show actual errors first
        (let ((errors (with-current-buffer build-buffer
                        (save-excursion
                          (goto-char (point-min))
                          (let ((error-lines '()))
                            (while (re-search-forward xcode-build-config-error-location-pattern nil t)
                              (push (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (min (+ (line-end-position) 200) (point-max)))
                                   error-lines))
                            (nreverse error-lines))))))
          (when errors
            (insert "=== ERRORS FOUND ===\n")
            (dolist (err errors)
              (insert (propertize err 'face 'error))
              (insert "\n\n"))))

        (insert "\n\nLast 50 lines of build output:\n")
        (insert "-------------------------------\n")
        (insert (with-current-buffer build-buffer
                  (save-excursion
                    (goto-char (point-max))
                    (forward-line -50)
                    (buffer-substring-no-properties (point) (point-max)))))
        (compilation-mode)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
    (swift-notification-send :message "No *Swift Build* buffer found. Run a build first." :seconds 3)))

;; ============================================================================

(defun swift-development-is-a-swift-package-base-project ()
  "Check if project is a swift package based."
  (let ((default-directory (swift-development-get-project-root)))
    (file-exists-p "Package.swift")))

(defun swift-development-get-scheme-configuration ()
  "Get the build configuration from the current scheme.
Returns the configuration name or 'Debug' as fallback."
  (or xcode-build-config-default-configuration  ; Use custom if set
      (condition-case nil
          (let* ((scheme-name (xcode-project-scheme))
                 (workspace-or-project (xcode-project-get-workspace-or-project))
                 (project-root (xcode-project-project-root))
                 (scheme-file nil))
            ;; Try to find the scheme file
            (let ((scheme-paths (list
                                 (format "%s/.swiftpm/xcode/xcshareddata/xcschemes/%s.xcscheme" 
                                         project-root scheme-name)
                                 (format "%s/xcshareddata/xcschemes/%s.xcscheme" 
                                         project-root scheme-name))))
              ;; Also check in workspace/project directories
              (when (string-match "-workspace" workspace-or-project)
                (let ((workspace-name (replace-regexp-in-string ".*\\(\\w+\\)\\.xcworkspace.*" "\\1" workspace-or-project)))
                  (push (format "%s/%s.xcworkspace/xcshareddata/xcschemes/%s.xcscheme"
                                project-root workspace-name scheme-name) 
                        scheme-paths)))
              (when (string-match "-project" workspace-or-project)
                (let ((project-name (replace-regexp-in-string ".*\\(\\w+\\)\\.xcodeproj.*" "\\1" workspace-or-project)))
                  (push (format "%s/%s.xcodeproj/xcshareddata/xcschemes/%s.xcscheme"
                                project-root project-name scheme-name) 
                        scheme-paths)))
              
              ;; Find the first existing scheme file
              (setq scheme-file (cl-find-if #'file-exists-p scheme-paths)))
            
            ;; Parse the scheme file to find the configuration
            (if scheme-file
                (with-temp-buffer
                  (insert-file-contents scheme-file)
                  ;; Look for buildConfiguration in the LaunchAction
                  (if (re-search-forward "LaunchAction.*?buildConfiguration *= *\"\\([^\"]+\\)\"" nil t)
                      (let ((config (match-string 1)))
                        (swift-development-log-debug "Detected configuration from scheme: %s" config)
                        config)
                    ;; Fallback to Debug if not found in LaunchAction
                    "Debug"))
              ;; If no scheme file found, try to detect from available configurations
              (swift-development-detect-available-configuration)))
        (error "Debug"))))  ; Ultimate fallback

(defun swift-development-detect-available-configuration ()
  "Try to detect a reasonable configuration from available build settings."
  (condition-case nil
      (let* ((project-root (xcode-project-project-root))
             (cmd (format "cd '%s' && xcodebuild -list -json 2>/dev/null | grep -A 10 configurations"
                          project-root))
             (output (or (swift-async-run-sync cmd :timeout 10) "")))
        ;; Look for common development configurations
        (cond
         ((string-match "Release (Development)" output) "Release (Development)")
         ((string-match "Debug (Development)" output) "Debug (Development)")  
         ((string-match "Development" output) "Development")
         ((string-match "Debug" output) "Debug")
         (t "Debug")))
    (error "Debug")))


(defun swift-development-uses-swift-packages-p ()
  "Check if the current project uses Swift Package Manager."
  (let* ((project-root (swift-development-get-project-root))
         (cache-key (if (fboundp 'swift-cache-project-key)
                       (swift-cache-project-key project-root "uses-swift-packages")
                     nil)))
    (if (and cache-key (fboundp 'swift-cache-with))
        (swift-cache-with cache-key 1800  ; Cache for 30 minutes
          (let ((default-directory project-root))
            (or (file-exists-p "Package.swift")
                (file-exists-p "Package.resolved")
                ;; Check if Xcode project has package dependencies
                (and (xcode-project-is-xcodeproject)
                     (swift-development-project-has-package-dependencies-p)))))
      ;; Fallback without caching
      (let ((default-directory project-root))
        (or (file-exists-p "Package.swift")
            (file-exists-p "Package.resolved")
            ;; Check if Xcode project has package dependencies
            (and (xcode-project-is-xcodeproject)
                 (swift-development-project-has-package-dependencies-p)))))))

(defun swift-development-project-has-package-dependencies-p ()
  "Check if the Xcode project has Swift Package dependencies."
  (let ((default-directory (swift-development-get-project-root)))
    (or
     ;; Check for Package.resolved in project
     (file-exists-p "Package.resolved")
     ;; Check for SourcePackages directory (created by Xcode for SPM)
     (file-exists-p ".swiftpm/xcode/package.xcworkspace")
     ;; Look for project.pbxproj references to package dependencies
     (when-let* ((project-files (directory-files-recursively 
                                default-directory 
                                "project\\.pbxproj$" t)))
       (cl-some (lambda (file)
                  (with-temp-buffer
                    (insert-file-contents file)
                    (or (search-forward "XCRemoteSwiftPackageReference" nil t)
                        (search-forward "packageProductDependencies" nil t)
                        (search-forward "swift-package" nil t))))
                project-files)))))

(defun swift-development-get-project-root ()
  "Get the project root directory."
  (cond
   ;; If periphery helper is available, use it for project root
   ((fboundp 'periphery-helper:project-root-dir)
    (periphery-helper:project-root-dir))
   ;; Try xcode-additions if available
   ((fboundp 'xcode-project-project-root)
    (xcode-project-project-root))
   ;; Try vc-root-dir for git projects
   ((vc-root-dir)
    (vc-root-dir))
   ;; Fallback to default-directory
   (t default-directory)))

(defun swift-development-check-for-spm-build-errors (text)
  "Check for Swift package build errors in TEXT."
  (when swift-development-debug (message text))
  ;; Cancel progress notification
  (when (fboundp 'swift-notification-progress-cancel)
    (swift-notification-progress-cancel 'spm-build))
  (let ((has-errors (swift-error-proxy-has-errors-p text))
        (has-warnings (swift-error-proxy-has-warnings-p text)))
    (cond
     ;; Build failed with errors
     (has-errors
      (swift-notification-send :message "Build failed with errors" :seconds 3)
      (swift-error-proxy-parse-output text))
     ;; Build succeeded with warnings - run anyway
     (has-warnings
      (swift-notification-send :message "Build succeeded (with warnings)" :seconds 2)
      (swift-error-proxy-parse-output text)
      (swift-development-run-async-swift-package))
     ;; Build succeeded
     (t
      (swift-notification-send :message "Build succeeded - running..." :seconds 2)
      (swift-development-run-async-swift-package)))))

(defun swift-development-run-async-swift-package ()
  "Run async swift package and hide the normal output."
  (if (fboundp 'inhibit-sentinel-messages)
      (inhibit-sentinel-messages #'async-shell-command
                                 "swift run"
                                 "*Swift Package*")
    ;; Fallback when periphery is not available
    (async-shell-command "swift run" "*Swift Package*")))

(defun swift-development-build-swift-package ()
  "Build swift package module."
  (interactive)
  (let* ((default-directory (swift-development-get-project-root))
         (package-name (file-name-nondirectory (directory-file-name default-directory))))
    ;; Start progress notification
    (when (fboundp 'swift-notification-progress-start)
      (swift-notification-progress-start
       :id 'spm-build
       :title (format "Building %s" package-name)
       :message "Compiling Swift package..."
       :percent 0))
    (if (fboundp 'async-shell-command-to-string)
        (progn
          ;; Start async build
          (async-shell-command-to-string
           :process-name "swift-package-build"
           :command "swift build"
           :callback #'swift-development-check-for-spm-build-errors)
          ;; Update progress to show we're working
          (when (fboundp 'swift-notification-progress-update)
            (swift-notification-progress-update 'spm-build
                                                :percent 25
                                                :message "Building...")))
      ;; Fallback: use compilation-mode for building
      (let ((compilation-buffer-name-function (lambda (_) "*Swift Package Build*"))
            (compilation-scroll-output t))
        (swift-notification-send :message (format "Building %s..." package-name) :seconds 2)
        (compile "swift build")))))

(defun swift-development-test-swift-package-from-file ()
  "Test swift package module from current file location."
  (interactive)
  (swift-development--test-swift-package-impl :root (swift-development-detect-package-root)))

(defun swift-development-test-swift-package ()
  "Test swift package module from project root."
  (interactive)
  (swift-development--test-swift-package-impl :root (swift-development-get-project-root)))

(cl-defun swift-development--test-swift-package-impl (&key root)
  "Test package in ROOT."
  (let ((default-directory root)
        (package-name (file-name-nondirectory (directory-file-name root))))
    (progn
      (spinner-start 'progress-bar-filled)
      (setq swift-development--build-progress-spinner spinner-current)
      (if (fboundp 'async-start-command-to-string)
          (async-start-command-to-string
           :command "swift test"
           :callback (lambda (text)
                       (spinner-stop swift-development--build-progress-spinner)
                       (let ((filtered (if (fboundp 'periphery-helper:filter-keep-beginning-paths)
                                           (periphery-helper:filter-keep-beginning-paths text)
                                         text)))
                         (swift-error-proxy-parse-test-output
                          filtered
                          (lambda ()
                            (message-with-color
                             :tag "[All tests passed]"
                             :text ""
                             :attributes 'success)))))
           :debug swift-development-debug)
        ;; Fallback: use compilation-mode for testing
        (spinner-stop swift-development--build-progress-spinner)
        (let ((compilation-buffer-name-function (lambda (_) "*Swift Test*"))
              (compilation-scroll-output t))
          (compile "swift test")))
      (message-with-color
       :tag (format "[Testing '%s'-package]" package-name)
       :text "Please wait. Patience is a virtue!"
       :attributes 'warning))))

(defun swift-development-detect-package-root ()
  "Detects the root directory of the Swift package based on the current buffer."
  (let ((buffer-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (locate-dominating-file buffer-dir "Package.swift")))

;;;###autoload
(defun swift-development-toggle-periphery-mode ()
  "Toggle between periphery and compilation mode for error display.
Cycles through `swift-error-proxy-backend' settings."
  (interactive)
  (let ((new-backend (pcase swift-error-proxy-backend
                       ('periphery 'compilation)
                       ('compilation 'minimal)
                       (_ 'periphery))))
    (setq swift-error-proxy-backend new-backend)
    ;; Keep legacy variable in sync
    (setq swift-development-use-periphery (eq new-backend 'periphery))
    (message "Swift error display mode: %s"
             (pcase new-backend
               ('periphery "Periphery (rich UI)")
               ('compilation "Compilation mode")
               ('minimal "Minimal (counts only)")))
    ;; Kill existing buffers to ensure fresh start with new mode
    (swift-error-proxy-kill-buffer)
    (dolist (name '("*Swift Build*" "*Swift Test*" "*Swift Package Build*"))
      (when-let* ((buf (get-buffer name)))
        (kill-buffer buf)))))

;;;###autoload
(defun swift-development-show-build-output ()
  "Show the Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (if buf
        (display-buffer buf '((display-buffer-reuse-window
                               display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 15)))
      (message "No build output available yet"))))

;;;###autoload
(defun swift-development-hide-build-output ()
  "Hide the Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (when buf
      (delete-windows-on buf))))

;;;###autoload
(defun swift-development-enable-build-cache-sharing ()
  "Enable build cache sharing between builds for maximum speed.
This configures the build to share compiled modules and objects."
  (interactive)
  (let* ((project-root (xcode-project-project-root))
         (build-cache-dir (expand-file-name ".build/ModuleCache" project-root))
         (shared-pch-dir (expand-file-name ".build/SharedPrecompiledHeaders" project-root)))
    ;; Create cache directories
    (unless (file-exists-p build-cache-dir)
      (make-directory build-cache-dir t))
    (unless (file-exists-p shared-pch-dir)
      (make-directory shared-pch-dir t))
    
    ;; Set environment for shared caching
    (setenv "SWIFT_MODULE_CACHE_PATH" build-cache-dir)
    (setenv "CLANG_MODULE_CACHE_PATH" build-cache-dir)
    (setenv "SHARED_PRECOMPS_DIR" shared-pch-dir)
    
    ;; Enable all caching mechanisms
    (setenv "SWIFT_ENABLE_BATCH_MODE" "YES")
    (setenv "SWIFT_USE_INTEGRATED_DRIVER" "YES")  ; New Swift driver for better caching
    (setenv "SWIFT_ENABLE_EXPLICIT_MODULES" "YES")  ; Explicit module builds
    
    (message "Build cache sharing enabled. Subsequent builds will be much faster!")))

;;;###autoload
(defun swift-development-quick-rebuild ()
  "Perform a quick rebuild using all available optimizations.
This is the fastest way to rebuild after small changes."
  (interactive)
  ;; Set to maximum speed mode but respect periphery setting
  (let ((xcode-build-config-skip-package-resolution 'always)
        (swift-development-analysis-mode 'minimal))  ; Use minimal instead of disabled
    (swift-development-enable-build-cache-sharing)
    (swift-development-compile :run t)))

;;;###autoload  
(defun swift-development-toggle-build-output ()
  "Toggle visibility of Swift build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (if (and buf (get-buffer-window buf))
        (swift-development-hide-build-output)
      (swift-development-show-build-output))))

;;;###autoload
(defun swift-development-compile-and-run ()
  "Compile and run app."
  (interactive)
  (swift-development-compile :run t))

(defun swift-development-format-scheme-name (scheme-name)
  "Format SCHEME-NAME for display in mode-line.
  Removes all backslashes and formats nicely."
  (when scheme-name
    ;; Simply remove ALL backslashes from the name
    (let ((cleaned (replace-regexp-in-string "\\\\" "" scheme-name)))
      
      ;; Now try to extract the parts and format nicely
      (cond
       ;; Pattern 1: "Name (Config)" - parentheses without dash
       ((string-match "^\\([^(]+\\)[[:space:]]*(\\([^)]+\\))" cleaned)
        (format "%s-%s" 
                (string-trim (match-string 1 cleaned))
                (string-trim (match-string 2 cleaned))))
       ;; Pattern 2: "Name - (Config)" - with dash and parentheses
       ((string-match "^\\([^-]+\\)[[:space:]]*-[[:space:]]*(\\([^)]+\\))" cleaned)
        (format "%s-%s" 
                (string-trim (match-string 1 cleaned))
                (string-trim (match-string 2 cleaned))))
       ;; Pattern 3: Just return cleaned version without backslashes
       (t (string-trim cleaned))))))

;;;###autoload
(defun swift-development-test-scheme-formatting ()
  "Test the scheme name formatting with various inputs."
  (interactive)
  (let ((test-cases '("Bruce - (Development)"
                      "Bruce - \\(Development)"
                      "Bruce - \\(Development\\)"
                      "MyApp - (Release)"
                      "MyApp - \\(Release\\)")))
    (dolist (test test-cases)
      (message "Input: '%s' → Output: '%s'" 
               test 
               (swift-development-format-scheme-name test)))))

;;;###autoload
(defun swift-development-debug-current-scheme ()
  "Debug the current scheme name to see exactly what we're dealing with."
  (interactive)
  (let* ((scheme (xcode-project-scheme))
         (swift-development-debug t))
    (message "=== Debugging Current Scheme ===")
    (message "Raw scheme from xcode-project- %S" scheme)
    (message "Length: %d characters" (length scheme))
    (message "Character breakdown:")
    (dotimes (i (length scheme))
      (let ((char (aref scheme i)))
        (message "  Position %d: '%c' (code: %d, hex: %x)" 
                 i char char char)))
    (message "Formatted result: %s" (swift-development-format-scheme-name scheme))
    (message "=== End Debug ==")))

(defun swift-development-format-simulator-name (simulator-name)
  "Format SIMULATOR-NAME for better display.
  Ensures proper capitalization for iPhone/iPad."
  (if simulator-name
      ;; Fix common simulator name issues
      (replace-regexp-in-string "iphone" "iPhone"
                               (replace-regexp-in-string "ipad" "iPad" 
                                                        simulator-name t) t)
    ""))

;;;###autoload
(defun swift-development-force-full-build ()
  "Force the next build to use xcodebuild (skip incremental).
The flag is automatically cleared after one build."
  (interactive)
  (setq swift-development--force-full-build t)
  (message "Next build will use full xcodebuild"))

;;;###autoload
(defun swift-development-compile-app ()
  "Compile app."
  (interactive)
  (swift-development-compile :run nil))

;;;###autoload
(defun swift-development-run()
    "Rerun already compiled and installed app."
    (interactive)
  (swift-error-proxy-kill-buffer)
  (ios-simulator-kill-buffer)

    (if (xcode-project-run-in-simulator)
        (ios-simulator-install-and-run-app
         :rootfolder (xcode-project-project-root)
         :build-folder (xcode-project-build-folder :device-type :simulator)
         :simulatorId (ios-simulator-simulator-identifier)
         :appIdentifier (xcode-project-fetch-or-load-app-identifier))
      (ios-device-install-app
       :buildfolder (xcode-project-build-folder :device-type :device)
       :appIdentifier (xcode-project-fetch-or-load-app-identifier))))

;;;###autoload
(defun swift-development-test-module-silent ()
  "Test module."
  (interactive)
  (save-some-buffers t)
  (swift-error-proxy-kill-buffer)
  (ios-simulator-kill-buffer)
  (swift-development-test-swift-package))

;;;###autoload
(defun swift-development-clear-derived-data ()
  "Clear Xcode's DerivedData folder to fix stubborn build issues."
  (interactive)
  (let ((derived-data-path (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
        (project-build-path (expand-file-name ".build" (xcode-project-project-root))))
    
    ;; First clear project-specific build folder
    (when (file-directory-p project-build-path)
      (message "Clearing project build folder at %s..." project-build-path)
      (dolist (file (directory-files project-build-path t "^[^.]"))
        (condition-case err
            (if (file-directory-p file)
                (delete-directory file t)
              (delete-file file))
          (file-error (message "Could not delete %s: %s" file (error-message-string err))))))

    ;; Then ask about clearing all derived data
    (when (file-directory-p derived-data-path)
      (when (yes-or-no-p "Clear all Xcode derived data. This will force a full rebuild?")
        (message "Clearing derived data...")
        (dolist (file (directory-files derived-data-path t "^[^.]"))
          (unless (string-match-p "ModuleCache$" file)
            (condition-case err
                (if (file-directory-p file)
                    (delete-directory file t)
                  (delete-file file))
              (file-error (message "Could not delete %s: %s" file (error-message-string err))))))))))

;;;###autoload
(defun swift-development-optimize-build-system ()
  "Perform various optimizations to speed up the build system."
  (interactive)
  (message "Optimizing build system...")
  
  ;; Kill Xcode-related processes that might be locking files
  (dolist (process '("com.apple.dt.Xcode" "IBCocoaTouchImageCatalogTool" 
                     "IBCocoaTouchTool" "XCPreviewAgent" "SourceKitService"))
    (call-process "pkill" nil nil nil "-f" process))
  
  ;; Clear module caches
  (let ((module-cache (expand-file-name "~/Library/Developer/Xcode/DerivedData/ModuleCache")))
    (when (file-directory-p module-cache)
      (condition-case err
          (delete-directory module-cache t)
        (file-error (message "Could not clear module cache: %s" (error-message-string err))))
      (message "Module cache cleared")))
  
  ;; Stop Swift Package Manager daemons
  (call-process "pkill" nil nil nil "-f" "swift-package")
  
  ;; Optimize disk I/O by moving derived data to RAM disk if available
  (when (file-exists-p "/Volumes/RAMDisk")
    (let ((ram-derived-data "/Volumes/RAMDisk/DerivedData"))
      (unless (file-exists-p ram-derived-data)
        (make-directory ram-derived-data t))
      (message "RAM disk detected - using it for derived data")))
  
  ;; Precompile common headers
  (swift-development-precompile-common-headers)
  
  ;; Generate optimized xcconfig
  (xcode-build-config-generate-fast-build-xcconfig)
  
  ;; Clean SPM package cache
  (let ((spm-cache-dir (expand-file-name "~/.swiftpm/cache")))
    (when (file-directory-p spm-cache-dir)
      (message "Cleaning Swift Package Manager cache...")
      (condition-case err
          (delete-directory spm-cache-dir t)
        (file-error (message "Could not clean SPM cache: %s" (error-message-string err))))))
  
  (message "Build system optimized. Next build should be faster."))

;;;###autoload
(defun swift-development-enable-turbo-mode ()
  "Enable maximum build speed optimizations (may reduce debugging capability)."
  (interactive)
  (setq xcode-build-config-use-thin-lto nil  ; Thin LTO can actually slow down incremental builds
        xcode-build-config-enable-timing-summary t
        xcode-build-config-other-swift-flags
        '("-no-whole-module-optimization" "-DDEBUG"))  ; Keep it simple and working
  (swift-development-reset)  ; Reset cached build commands
  (message "Turbo mode enabled. Next build will use speed optimizations."))

;;;###autoload
(defun swift-development-enable-balanced-mode ()
  "Enable balanced build speed with some debugging capability."
  (interactive)
  (setq xcode-build-config-use-thin-lto nil
        xcode-build-config-enable-timing-summary t
        xcode-build-config-other-swift-flags
        '("-no-whole-module-optimization" "-DDEBUG"))  ; Remove problematic flags
  (swift-development-reset)  ; Reset cached build commands
  (message "Balanced mode enabled. Next build will balance speed and debugging."))

;;;###autoload
(defun swift-development-benchmark-build ()
  "Run a benchmark build to measure compilation performance."
  (interactive)
  (let ((old-debug swift-development-debug))
    (setq swift-development-debug t)
    (message "Starting benchmark build at %s..." (format-time-string "%H:%M:%S"))
    (swift-development-compile-app)
    (setq swift-development-debug old-debug)
    (message "Benchmark started. Check build times in output.")))

;;;###autoload
(defun swift-development-fix-dependency-issues ()
  "Fix common dependency issues with Swift packages and CocoaPods (supports hybrid projects)."
  (interactive)
  (let ((default-directory (xcode-project-project-root))
        (uses-pods (xcode-build-config-uses-cocoapods-p))
        (uses-spm (swift-development-uses-swift-packages-p)))
    (message "Fixing dependency issues...")
    
    ;; Handle CocoaPods dependencies if present
    (when uses-pods
      (message "Detected CocoaPods, applying CocoaPods fixes...")
      
      ;; Clean CocoaPods cache and reinstall
      (when (file-exists-p "Podfile.lock")
        (message "Cleaning CocoaPods cache...")
        (async-shell-command "pod cache clean --all"))
      
      ;; Remove Pods directory and reinstall
      (when (file-exists-p "Pods")
        (message "Removing Pods directory...")
        (delete-directory "Pods" t))
      
      ;; Reinstall pods
      (message "Reinstalling CocoaPods dependencies...")
      (async-shell-command-to-string 
       :command "pod install --repo-update"
       :callback (lambda (_output)
                   (message "CocoaPods dependencies updated."))))
    
    ;; Handle Swift Package Manager dependencies if present
    (when uses-spm
      (message "Detected Swift Package Manager, applying SPM fixes...")
      
      ;; Remove Package.resolved to force re-resolution
      (when (file-exists-p "Package.resolved")
        (delete-file "Package.resolved")
        (message "Removed Package.resolved"))
      
      ;; Clean SPM cache
      (let ((spm-cache (expand-file-name "~/.swiftpm/cache")))
        (when (file-directory-p spm-cache)
          (condition-case err
              (delete-directory spm-cache t)
            (file-error (message "Could not clean SPM cache: %s" (error-message-string err))))))
      
      ;; Clean project build folder
      (when (file-exists-p ".build")
        (message "Cleaning .build folder...")
        (delete-directory ".build" t))
      
      ;; Reset package dependencies (force resolution)
      (let ((swift-development-force-package-resolution t))
        (async-shell-command-to-string 
         :command "xcodebuild -resolvePackageDependencies"
         :callback (lambda (_output)
                     (message "Swift Package dependencies resolved.")))))
    
    ;; Always clean derived data regardless of dependency manager
    (let* ((project-name (or (xcode-project-workspace-name) (xcode-project-project-name)))
           (derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData/")))
      ;; Find and delete matching directories safely
      (when (and project-name (file-directory-p derived-data-dir))
        (dolist (dir (directory-files derived-data-dir t (concat "^" (regexp-quote project-name))))
          (when (file-directory-p dir)
            (condition-case err
                (delete-directory dir t)
              (file-error (message "Could not delete %s: %s" dir (error-message-string err))))))))
    
    ;; Update xcconfig with architecture settings
    (xcode-build-config-generate-fast-build-xcconfig)
    
    (cond 
     ((and uses-pods uses-spm)
      (message "Hybrid project detected. Fixed both CocoaPods and Swift Package dependencies."))
     (uses-pods
      (message "CocoaPods dependency issues fixed."))
     (uses-spm
      (message "Swift Package dependency issues fixed."))
     (t
      (message "No package managers detected, but cleaned derived data.")))
    
    (message "Ready to build.")))

(defun swift-development-diagnose ()
  "Display diagnostic information about the current Swift development environment."
  (interactive)
  (with-help-window "*Swift Diagnostics*"
    (let ((standard-output (get-buffer-create "*Swift Diagnostics*")))
      (princ "Swift Development Environment Diagnostics\n")
      (princ "=====================================\n\n")

      ;; Project information
      (princ "Project Information:\n")
      (princ "-------------------\n")
      (princ (format "Project Root: %s\n" (xcode-project-project-root)))
      (princ (format "Project Type: %s\n"
                    (cond ((xcode-project-is-xcodeproject) "Xcode Project")
                          ((swift-development-is-a-swift-package-base-project) "Swift Package")
                          (t "Unknown"))))
      (princ (format "Uses CocoaPods: %s\n" (if (xcode-build-config-uses-cocoapods-p) "Yes" "No")))
      (princ (format "Uses Swift Packages: %s\n" (if (swift-development-uses-swift-packages-p) "Yes" "No")))
      (let ((uses-pods (xcode-build-config-uses-cocoapods-p))
            (uses-spm (swift-development-uses-swift-packages-p)))
        (when (and uses-pods uses-spm)
          (princ "Project Type: Hybrid (CocoaPods + Swift Packages)\n"))
        (when uses-pods
          (princ (format "Workspace Name: %s\n" (or (xcode-project-workspace-name) "Not found")))
          (princ (format "Podfile exists: %s\n" (if (file-exists-p "Podfile") "Yes" "No")))
          (princ (format "Podfile.lock exists: %s\n" (if (file-exists-p "Podfile.lock") "Yes" "No")))
          (princ (format "Pods directory exists: %s\n" (if (file-exists-p "Pods") "Yes" "No"))))
        (when uses-spm
          (princ (format "Package.swift exists: %s\n" (if (file-exists-p "Package.swift") "Yes" "No")))
          (princ (format "Package.resolved exists: %s\n" (if (file-exists-p "Package.resolved") "Yes" "No")))
          (princ (format "Has package dependencies in Xcode project: %s\n" 
                        (if (swift-development-project-has-package-dependencies-p) "Yes" "No")))))
      (when (xcode-project-is-xcodeproject)
        (princ (format "Scheme: %s\n" (xcode-project-scheme)))
        (princ (format "Derived Data Path: %s\n" (xcode-project-derived-data-path))))

      ;; Build configuration
      (princ "\nBuild Configuration:\n")
      (princ "------------------\n")
      (princ (format "Configuration Override: %s\n" 
                    (if xcode-build-config-default-configuration 
                        xcode-build-config-default-configuration 
                        "None (using scheme's default)")))
      (princ (format "Will include -configuration flag: %s\n" 
                    (if xcode-build-config-default-configuration "Yes" "No")))
      (princ (format "Build System: %s\n" xcode-build-config-modern-build-system))
      (princ (format "Additional Build Flags: %s\n"
                    (if xcode-build-config-additional-build-flags
                        (mapconcat #'identity xcode-build-config-additional-build-flags " ")
                      "None")))

      ;; Environment
      (princ "\nEnvironment:\n")
      (princ "------------\n")
      (princ (format "x86 Environment: %s\n"
                    (if xcode-build-config--current-environment-x86 "Yes" "No")))
      (princ (format "Number of CPU Cores: %s\n" (xcode-build-config-get-optimal-jobs)))

      ;; Device/Simulator
      (princ "\nDevice Configuration:\n")
      (princ "-------------------\n")
      (if (xcode-project-run-in-simulator)
          (progn
            (princ "Running in Simulator:\n")
            (princ (format "Simulator ID: %s\n" (ios-simulator-simulator-identifier)))
            (princ (format "Simulator Name: %s\n" (ios-simulator-simulator-name))))
        (princ "Running on Physical Device\n"))

      ;; Current build state
      (princ "\nCurrent Build State:\n")
      (princ "------------------\n")
      (when swift-development--current-build-command
        (princ "Current Build Command:\n")
        (princ "------------------\n")
        (princ swift-development--current-build-command)
        (princ "\n"))
      (when swift-development--compilation-time
        (princ (format "\nLast Compilation Time: %s seconds\n"
                      (swift-development--compilation-time))))

      ;; Debug settings
      (princ "\nDebug Settings:\n")
      (princ "--------------\n")
      (princ (format "Debug Mode: %s\n" (if swift-development-debug "Enabled" "Disabled")))

      ;; File system checks
      (princ "\nFile System Checks:\n")
      (princ "-----------------\n")
      (let ((build-dir (xcode-project-build-folder
                        :device-type (if (eq swift-development--device-choice t) :device :simulator))))
        (princ (format "Build Directory Exists: %s\n"
                      (if (and build-dir (file-exists-p build-dir)) "Yes" "No")))
        (when build-dir
          (princ (format "Build Directory Path: %s\n" build-dir))))

      ;; Swift toolchain
      (princ "\nSwift Toolchain:\n")
      (princ "---------------\n")
      (let ((swift-version (or (swift-async-run-sync "swift --version" :timeout 5) "Unknown")))
        (princ swift-version))

      ;; System info
      (princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Emacs Version: %s\n" emacs-version))
      (princ (format "System Type: %s\n" system-type))

      ;(princ "\nSystem Information:\n")
      (princ "------------------\n")
      (princ (format "Build command: %s\n"
                     (if (eq swift-development--device-choice t)
                         (xcode-build-config-build-app-command
                          :device-id (when (fboundp 'ios-device-udid) (ios-device-udid))
                          :derived-path (xcode-project-derived-data-path))
                       (xcode-build-config-build-app-command
                        :sim-id (ios-simulator-simulator-identifier)
                        :derived-path (xcode-project-derived-data-path)))))

      (princ "\nRecommendations:\n")
      (princ "---------------\n")
      (unless (xcode-project-is-xcodeproject)
        (unless (swift-development-is-a-swift-package-base-project)
          (princ "WARNING: Neither Xcode project nor Swift package detected\n")))
      (unless swift-development--current-build-command
        (princ "NOTE: No build command has been generated yet\n"))
      (when swift-development-debug
        (princ "NOTE: Debug mode is enabled - this may affect performance\n")))))

(defvar swift-development--auto-warm-timer nil
  "Timer for deferred auto-warm cache operation.")

(defvar swift-development--file-watcher-timer nil
  "Timer for deferred file watcher startup.")

;;;###autoload
(defun swift-development-auto-warm-cache-on-file-open ()
  "Automatically warm build cache when opening a Swift file in an Xcode project.
Uses idle timer to avoid blocking file open.
Also starts file watcher for instant rebuild detection."
  (when swift-development-debug
    (message "[DEBUG] Auto-warm hook triggered"))
  (when (and (or (derived-mode-p 'swift-mode)
                 (derived-mode-p 'swift-ts-mode))
             (buffer-file-name))
    ;; Cancel any pending timer to avoid duplicate work
    (when swift-development--auto-warm-timer
      (cancel-timer swift-development--auto-warm-timer))
    (when swift-development--file-watcher-timer
      (cancel-timer swift-development--file-watcher-timer))
    ;; Defer the heavy work to after Emacs is idle
    (let ((buf (current-buffer)))
      (setq swift-development--auto-warm-timer
            (run-with-idle-timer
             0.5 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (and (xcode-project-is-xcodeproject)
                              (or (derived-mode-p 'swift-mode)
                                  (derived-mode-p 'swift-ts-mode)))
                     (when swift-development-debug
                       (message "[DEBUG] Deferred: Conditions met - calling xcode-project-setup-current-project"))
                     (let ((project-root (swift-project-root nil t)))
                       (when project-root
                         (when swift-development-debug
                           (message "[DEBUG] Deferred: Project root found: %s" project-root))
                         (xcode-project-setup-current-project project-root)))))))))
      ;; Start file watcher after a longer delay (1s) to let other setup complete
      (setq swift-development--file-watcher-timer
            (run-with-idle-timer
             1.0 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((project-root (swift-project-root nil t)))
                     (when (and project-root
                                (fboundp 'swift-file-watcher-start)
                                (fboundp 'swift-file-watcher-active-p)
                                (not (swift-file-watcher-active-p)))
                       (when swift-development-debug
                         (message "[DEBUG] Starting file watcher for: %s" project-root))
                        (swift-file-watcher-start project-root)))))))))))

;;;###autoload
(defun swift-development-test-auto-warm ()
  "Test the automatic cache warming for current project."
  (interactive)
  (let ((project-root (swift-project-root nil t)))
    (if project-root
        (progn
          (message "Testing cache warming for project: %s" project-root)
          (xcode-project-setup-current-project project-root))
      (message "No Xcode project found in current directory"))))

;;;###autoload
(defun swift-development-diagnose-auto-warm ()
  "Diagnose why auto-warming might not be triggering."
  (interactive)
  (with-current-buffer (get-buffer-create "*Auto-Warm Diagnostics*")
    (erase-buffer)
    (insert "=== Auto-Warm Cache Diagnostics ===\n\n")

    ;; Check major mode
    (insert (format "Current major mode: %s\n" major-mode))
    (insert (format "Is swift-mode?: %s\n" (derived-mode-p 'swift-mode)))
    (insert (format "Is swift-ts-mode?: %s\n" (derived-mode-p 'swift-ts-mode)))
    (insert (format "swift-development-mode active?: %s\n\n"
                    (if (bound-and-true-p swift-development-mode) "YES" "NO")))

    ;; Check hooks
    (insert "=== Hook Status ===\n")
    (insert (format "swift-development-mode-hook: %s\n"
                    (if (member 'swift-development-auto-warm-cache-on-file-open swift-development-mode-hook)
                        "✓ REGISTERED"
                      "✗ NOT REGISTERED")))
    (insert (format "swift-mode-hook activates swift-development-mode?: %s\n"
                    (if (member 'swift-development-mode-enable swift-mode-hook)
                        "✓ YES"
                      "✗ NO")))
    (insert (format "swift-ts-mode-hook activates swift-development-mode?: %s\n\n"
                    (if (member 'swift-development-mode-enable swift-ts-mode-hook)
                        "✓ YES"
                      "✗ NO")))

    ;; Check project
    (insert "=== Project Status ===\n")
    (let ((project-root (xcode-project-project-root)))
      (insert (format "Project root: %s\n" (or project-root "nil")))
      (insert (format "Is Xcode project?: %s\n" (xcode-project-is-xcodeproject)))

      (when project-root
        (let* ((normalized-root (file-truename (expand-file-name project-root)))
               (is-warmed (gethash normalized-root xcode-project--cache-warmed-projects)))
          (insert (format "Normalized root: %s\n" normalized-root))
          (insert (format "Already warmed this session?: %s\n\n" (if is-warmed "YES" "NO"))))))

    ;; Check cache
    (insert "=== Cache Status ===\n")
    (when (fboundp 'swift-cache-project-key)
      (let* ((project-root (xcode-project-project-root))
             (cache-key (swift-cache-project-key project-root "build-cache-warmed")))
        (insert (format "Cache key: %s\n" cache-key))
        (when (fboundp 'swift-cache-get)
          (insert (format "Persistent cache warmed?: %s\n"
                         (if (swift-cache-get cache-key) "YES" "NO"))))))

    (insert "\n=== Recommendation ===\n")
    (unless (member 'swift-development-auto-warm-cache-on-file-open swift-development-mode-hook)
      (insert "⚠ Hook not registered! Try reloading swift-development.el\n"))
    (unless (bound-and-true-p swift-development-mode)
      (insert "⚠ swift-development-mode not active! Ensure it's enabled in your Swift buffer.\n"))

    (display-buffer (current-buffer))))

;; Add hook to automatically setup project when opening Swift files
;; Use swift-development-mode-hook for unified hook across swift-mode and swift-ts-mode
;;;###autoload
(add-hook 'swift-development-mode-hook 'swift-development-auto-warm-cache-on-file-open)

;; Update project signature when source files are saved (incremental)
;;;###autoload
;; Note: We no longer need an after-save-hook since the new last-modified
;; system automatically detects changes on next build check

;; Performance and Analysis Mode Controls

;;;###autoload
(defun swift-development-toggle-analysis-mode ()
  "Cycle through analysis modes for performance tuning.
Modes: fast -> minimal -> disabled -> full -> fast"
  (interactive)
  (setq swift-development-analysis-mode
        (pcase swift-development-analysis-mode
          ('fast 'minimal)
          ('minimal 'disabled) 
          ('disabled 'full)
          ('full 'fast)
          (_ 'fast)))  ; fallback
  (message "Swift analysis mode: %s" swift-development-analysis-mode))

;;;###autoload
(defun swift-development-set-fast-mode ()
  "Set analysis to fast mode for optimal performance/features balance."
  (interactive)
  (setq swift-development-analysis-mode 'fast)
  (message "Swift analysis set to fast mode (recommended)"))

;;;###autoload
(defun swift-development-set-minimal-mode ()
  "Set analysis to minimal mode for fastest builds."
  (interactive)
  (setq swift-development-analysis-mode 'minimal)
  (message "Swift analysis set to minimal mode (fastest)"))

;;;###autoload
(defun swift-development-run-on-additional-simulator ()
  "Run current app on an additional simulator (e.g., iOS 18 and iOS 26 simultaneously)."
  (interactive)
  (require 'ios-simulator)
  (ios-simulator-run-on-additional-simulator))

;;;###autoload
(defun swift-development-toggle-debug ()
  "Toggle debug mode for all swift-development packages.
Toggles: swift-development-debug, ios-simulator-debug, xcode-project-debug,
swift-project-debug, swift-cache-debug, and ios-device-debug."
  (interactive)
  (let* ((current-state (or swift-development-debug
                            ios-simulator-debug
                            xcode-project-debug))
         (new-state (not current-state)))
    ;; Toggle all debug variables
    (setq swift-development-debug new-state
          ios-simulator-debug new-state
          xcode-project-debug new-state
          swift-project-debug new-state
          swift-cache-debug new-state)
    ;; ios-device-debug if available
    (when (boundp 'ios-device-debug)
      (setq ios-device-debug new-state))
    ;; Report status
    (message "Swift Development debug mode: %s"
             (if new-state
                 (propertize "ENABLED" 'face 'success)
               (propertize "DISABLED" 'face 'font-lock-comment-face)))
    new-state))

;; Load the minor mode for unified keybindings
(require 'swift-development-mode nil t)

;;; Transient Menu

(defun swift-development--transient-status ()
  "Return current status for transient header."
  (let* ((scheme (ignore-errors (xcode-project-scheme-display-name)))
         (simulator (ignore-errors ios-simulator--current-simulator-name))
         (booted (ignore-errors (ios-simulator-get-all-booted-simulators)))
         (booted-names (mapcar #'car booted)))
    (concat
     (format "Scheme: %s" (propertize (or scheme "Not selected") 'face 'font-lock-constant-face))
     " | "
     (format "Simulator: %s" (propertize (or simulator "Not selected") 'face 'font-lock-type-face))
     (when booted-names
       (format " | Booted: %s" (propertize (string-join booted-names ", ") 'face 'success))))))

(defun swift-development-filter-errors ()
  "Open periphery filter menu to filter build errors."
  (interactive)
  (if (fboundp 'periphery-filter-menu)
      (periphery-filter-menu)
    (message "Periphery not loaded.")))

(defun swift-development-toggle-build-output-buffer ()
  "Toggle visibility of the build output buffer."
  (interactive)
  (let ((buf (get-buffer "*Swift Build Output*")))
    (if buf
        (if (get-buffer-window buf)
            (delete-window (get-buffer-window buf))
          (display-buffer buf))
      (message "No build output buffer exists yet."))))

;;;###autoload
(defun swift-development-toggle-continue-after-errors ()
  "Toggle continue building after errors.
When enabled, xcodebuild will compile as many files as possible
even when some files have errors, showing all errors at once."
  (interactive)
  (setq xcode-build-config-continue-building-after-errors
        (not xcode-build-config-continue-building-after-errors))
  ;; Clear cached build command since flags changed
  (when (boundp 'xcode-build-config--build-command-cache)
    (clrhash xcode-build-config--build-command-cache))
  (message "Continue building after errors: %s"
           (if xcode-build-config-continue-building-after-errors
               (propertize "ENABLED" 'face 'success)
             (propertize "DISABLED" 'face 'font-lock-comment-face))))

(defun swift-development--settings-status ()
  "Return current settings status for transient header."
  (concat
   (format "Analysis: %s"
           (propertize (symbol-name swift-development-analysis-mode) 'face 'font-lock-constant-face))
   " | "
   (format "Continue after errors: %s"
           (if xcode-build-config-continue-building-after-errors
               (propertize "ON" 'face 'success)
             (propertize "OFF" 'face 'font-lock-comment-face)))
   " | "
   (format "Debug: %s"
           (if swift-development-debug
               (propertize "ON" 'face 'warning)
             (propertize "OFF" 'face 'font-lock-comment-face)))))

;;;###autoload
(transient-define-prefix swift-development-settings-transient ()
  "Swift Development - Settings Menu."
  [:description swift-development--settings-status]
  ["Build Behavior"
   [("e" "Toggle continue after errors" swift-development-toggle-continue-after-errors)
    ("p" "Toggle package resolution" swift-development-toggle-package-resolution)
    ("P" "Toggle periphery mode" swift-development-toggle-periphery-mode)]]
  ["Analysis Mode"
   [("1" "Fast analysis (recommended)" swift-development-set-fast-mode)
    ("2" "Minimal analysis (fastest)" swift-development-set-minimal-mode)
    ("3" "Full analysis" (lambda () (interactive)
                           (setq swift-development-analysis-mode 'full)
                           (message "Swift analysis set to full mode")))
    ("4" "Disabled" (lambda () (interactive)
                      (setq swift-development-analysis-mode 'disabled)
                      (message "Swift analysis disabled")))]]
  ["Build Modes"
   [("t" "Turbo mode" swift-development-enable-turbo-mode)
    ("b" "Balanced mode" swift-development-enable-balanced-mode)]]
  ["Debug & Cache"
   [("d" "Toggle debug" swift-development-toggle-debug)
    ("c" "Clear hash cache" swift-development-clear-hash-cache)
    ("C" "Clear build command cache" (lambda () (interactive)
                                        (when (boundp 'xcode-build-config--build-command-cache)
                                          (clrhash xcode-build-config--build-command-cache))
                                        (message "Build command cache cleared")))
    ("X" "Clear DerivedData" swift-development-clear-derived-data)]]
  [("q" "Quit" transient-quit-one)])

;;;###autoload
(transient-define-prefix swift-development-transient ()
  "Swift Development - Main Menu."
  [:description swift-development--transient-status]
  ["Build & Run"
   [("c" "Compile" swift-development-compile-app)
    ("r" "Compile & Run" swift-development-compile-and-run)
    ("q" "Quick rebuild" swift-development-quick-rebuild)]
   [("b" "Build SPM package" swift-development-build-swift-package)
    ("a" "Static Analysis" swift-development-analyze-app)]]
  ["Build Modes"
   [("1" "Turbo mode" swift-development-enable-turbo-mode)
    ("2" "Balanced mode" swift-development-enable-balanced-mode)
    ("3" "Fast analysis" swift-development-set-fast-mode)
    ("4" "Minimal analysis" swift-development-set-minimal-mode)]]
  ["Cache & Status"
   [("w" "Warm build cache" swift-development-warm-build-cache)
    ("h" "Enable cache sharing" swift-development-enable-build-cache-sharing)
    ("B" "Build status" swift-development-build-status)
    ("e" "Show last errors" swift-development-show-last-build-errors)]
   [("C" "Clear hash cache" swift-development-clear-hash-cache)
    ("X" "Clear DerivedData" swift-development-clear-derived-data)
    ("R" "Reset all" swift-development-reset)]]
  ["Windows"
   [    ("W p" "Toggle Periphery" swift-development--periphery-toggle-buffer)
    ("W o" "Toggle Build Output" swift-development-toggle-build-output-buffer)
    ("W s" "Toggle Simulator Log" ios-simulator-toggle-buffer)]]
  ["Packages"
   [("N" "Create package..." spm-create-package)
    ("P" "Resolve packages" spm-resolve)
    ("L" "List dependencies" spm-list-dependencies)
    ("U" "Update all packages" spm-update-all)]]
  ["Sub-Menus"
   [("s" "Simulator..." ios-simulator-transient)
    ("d" "Device..." ios-device-transient)
    ("p" "SPM UI..." spm-transient)
    ("t" "Tests..." swift-test-transient)]
   [("v" "Preview..." swiftui-preview-transient)
    ("x" "Xcode Project..." xcode-project-transient)
    ("f" "Refactor..." swift-refactor-transient)
    ("F" "Distribute..." xcode-archive-transient)
    ("E" "Errors (Periphery)..." swift-development--periphery-transient)
    ("S" "Settings..." swift-development-settings-transient)]]
  ["Quick Settings"
   [("D" "Toggle debug" swift-development-toggle-debug)
    ("A" "Toggle analysis" swift-development-toggle-analysis-mode)
    ("T" "Toggle device/sim" swift-development-toggle-device-choice)
    ("O" "Toggle build output" swift-development-toggle-build-output)
    ("e" "Toggle continue after errors" swift-development-toggle-continue-after-errors)]
   [("M-s" "Select build scheme" xcode-project-select-scheme)
    ("M-t" "Select test scheme" xcode-project-select-test-scheme)]]
  [("Q" "Quit" transient-quit-one)])

(provide 'swift-development)
;;; swift-development.el ends here

