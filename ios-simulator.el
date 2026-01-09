;;; ios-simulator.el --- iOS Simulator management and control -*- lexical-binding: t -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, simulator

;;; Commentary:

;; This package provides iOS Simulator management, log viewing, and device control

;;; Code:

(require 'nerd-icons nil t)
(require 'json)
(require 'cl-lib)
(require 'ansi-color)
(require 'transient)
(require 'swift-cache nil t)

(with-eval-after-load 'periphery-helper
 (require 'periphery-helper))

(require 'mode-line-hud nil t)
(require 'xcode-project nil t)
(require 'swift-project nil t)
(require 'swift-project-settings nil t)
(require 'swift-notification nil t)

(with-eval-after-load 'json
  (require 'json))

(defcustom ios-simulator-debug nil
  "Enable debug mode for iOS simulator."
  :type 'boolean
  :group 'ios-simulator)

(defgroup ios-simulator nil
  "IOS-SIMULATOR."
  :tag "ios-simulator"
  :group 'applications)

(defcustom ios-simulator-default-language "sv-SE"
  "Default language for the simulator."
  :type 'string
  :group 'ios-simulator)

(defcustom ios-simulator-boot-timeout 60
  "Timeout in seconds for simulator boot operations.
If the simulator doesn't boot within this time, an error is signaled."
  :type 'integer
  :group 'ios-simulator)

(defface ios-simulator-background-face
  `((t (:inherit default :height 150)))
  "Buffer background color."
  :group 'ios-simulator)

;; Log output faces
(defface ios-simulator-error-face
  '((t (:inherit error :weight bold)))
  "Face for error messages in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-warning-face
  '((t (:inherit warning :weight bold)))
  "Face for warning messages in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-debug-face
  '((t (:inherit font-lock-comment-face)))
  "Face for debug messages in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-info-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for info messages in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-thread-face
  '((t (:inherit font-lock-constant-face)))
  "Face for thread identifiers in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-address-face
  '((t (:inherit font-lock-string-face)))
  "Face for memory addresses in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-stackframe-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for stack frame numbers in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-summary-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for summary lines in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-separator-face
  '((t (:inherit font-lock-comment-face :weight bold)))
  "Face for separator lines in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-url-face
  '((t (:inherit link)))
  "Face for URLs in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-http-error-face
  '((t (:inherit error)))
  "Face for HTTP error status codes (4xx, 5xx) in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-http-success-face
  '((t (:inherit success)))
  "Face for HTTP success status codes (2xx) in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-objc-error-face
  '((t (:inherit error)))
  "Face for Objective-C runtime errors in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-update-face
  '((t (:inherit font-lock-doc-face)))
  "Face for SDK update notices in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-version-face
  '((t (:inherit font-lock-constant-face)))
  "Face for version numbers in simulator output."
  :group 'ios-simulator)

(defface ios-simulator-filepath-face
  '((t (:inherit font-lock-string-face :underline t)))
  "Face for file paths in simulator output."
  :group 'ios-simulator)

(defcustom ios-simulator-colorize-output t
  "Whether to colorize simulator log output.
When non-nil, apply syntax highlighting to log messages."
  :type 'boolean
  :group 'ios-simulator)

(defvar ios-simulator-log-font-lock-keywords
  `(;; Fatal errors and signals
    ("DEADLYSIGNAL\\|SEGV\\|SIGABRT\\|SIGKILL\\|SIGSEGV" . 'ios-simulator-error-face)
    ;; iOS log format: 2025-12-05 10:58:48.588 App[pid:tid] [Category] ERROR - message
    ("^[0-9-]+ [0-9:.]+.*\\[.*\\] ERROR +-.*$" . 'ios-simulator-error-face)
    ("^[0-9-]+ [0-9:.]+.*\\[.*\\] WARNING +-.*$" . 'ios-simulator-warning-face)
    ;; Objective-C runtime errors: *** -[ClassName method]: message
    ("^.*\\*\\*\\* -\\[.+\\]:.*$" . 'ios-simulator-objc-error-face)
    ("^.*\\*\\*\\* +\\[.+\\]:.*$" . 'ios-simulator-objc-error-face)
    ;; NSError/Cocoa errors: Error Domain=NSCocoaErrorDomain Code=N
    ("Error Domain=\\w+ Code=[0-9]+" . 'ios-simulator-error-face)
    ("NSUnderlyingError" . 'ios-simulator-error-face)
    ("NSPOSIXErrorDomain" . 'ios-simulator-error-face)
    ;; Generic ERROR/WARNING patterns
    ("^.*\\bERROR[:.].*$" . 'ios-simulator-error-face)
    ("^==\\([0-9]+\\)==ERROR:.*$" . 'ios-simulator-error-face)
    ;; ThreadSanitizer/AddressSanitizer headers
    ("^\\(WARNING\\|ERROR\\): \\(ThreadSanitizer\\|AddressSanitizer\\|LeakSanitizer\\):.*$"
     . 'ios-simulator-error-face)
    ;; Warning patterns
    ("^.*\\bWARNING[:.].*$" . 'ios-simulator-warning-face)
    ;; Summary lines
    ("^SUMMARY:.*$" . 'ios-simulator-summary-face)
    ;; Separator lines (multiple = signs)
    ("^=+$" . 'ios-simulator-separator-face)
    ;; iOS log timestamp and process info (highlight the category in brackets)
    ("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9:.]+\\)" 1 'font-lock-comment-face)
    ("\\[\\([A-Za-z0-9_]+\\)\\]" 1 'ios-simulator-info-face)
    ;; Debug info from system frameworks
    ("^CoreData:.*$" . 'ios-simulator-debug-face)
    ("^\\w+: debug:.*$" . 'ios-simulator-debug-face)
    ;; Stack frame numbers
    ("^[[:space:]]*#[0-9]+" . 'ios-simulator-stackframe-face)
    ;; Thread identifiers
    ("\\b\\(Thread\\|thread\\) T?[0-9]+" . 'ios-simulator-thread-face)
    ("\\btid=[0-9]+" . 'ios-simulator-thread-face)
    ;; Memory addresses
    ("0x[0-9a-fA-F]+" . 'ios-simulator-address-face)
    ;; Process IDs
    ("\\bpid=[0-9]+" . 'ios-simulator-thread-face)
    ;; Location info (file:line patterns in stack traces)
    ("<null> ([^)]+:[^)]+)" . 'font-lock-type-face)
    ;; URLs (http/https)
    ("https?://[^ \t\n\r\"'>)]*" . 'ios-simulator-url-face)
    ;; HTTP status codes - errors (4xx, 5xx)
    ("\\b[45][0-9][0-9]\\b" . 'ios-simulator-http-error-face)
    ;; HTTP status codes - success (2xx)
    ("\\b2[0-9][0-9]\\b" . 'ios-simulator-http-success-face)
    ;; Log levels [lvl=N]
    ("\\[lvl=[0-9]+\\]" . 'ios-simulator-warning-face)
    ;; SDK/library update notices
    ("New version.+available:.*$" . 'ios-simulator-update-face)
    ;; Version numbers (e.g., 9.2.0.0, 10.6.0.0)
    ("\\b[0-9]+\\.[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?\\b" . 'ios-simulator-version-face)
    ;; File paths (absolute paths starting with / or ~)
    ("\\(?:NSFilePath=\\|NSURL=\\)?\\(\\(?:file://\\)?\\(?:/[^ \t\n,}]+\\|~/[^ \t\n,}]+\\)\\)" 1 'ios-simulator-filepath-face)
    ;; Common iOS/macOS paths
    ("/Users/[^/ \t\n,}]+/Library/Developer/[^ \t\n,}]+" . 'ios-simulator-filepath-face)
    ("/Applications/[^ \t\n,}]+\\.app[^ \t\n,}]*" . 'ios-simulator-filepath-face))
  "Font lock keywords for iOS simulator log output.")

(defconst ios-simulator-buffer-name "*iOS Simulator*"
  "Name of the buffer.")

(defconst list-simulators-command
  "xcrun simctl list devices available -j"
  "List available simulators.")

(defconst ios-simulator-get-booted-simulator-command
  "xcrun simctl list devices | grep -m 1 \"(Booted)\" | grep -E -o -i \"([0-9a-f]{8}-([0-9a-f]{4}-){3}[0-9a-f]{12})\""
  "Get booted simulator id if any.")

(defvar-local ios-simulator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i t") #'ios-simulator-terminate-current-app)
    (define-key map (kbd "C-c i l") #'ios-simulator-change-language)
    (define-key map (kbd "C-c i c") #'ios-simulator-appcontainer)
    (define-key map (kbd "C-c i n") #'ios-simulator-send-notification)
    map)
  "Keymap for `ios-simulator-mode'.")

;;;###autoload
(define-minor-mode ios-simulator-mode
  "Minor mode for iOS simulator integration."
  :init-value nil
  :lighter " iOS-Sim"
  :keymap ios-simulator-mode-map
  :group 'ios-simulator
  (if ios-simulator-mode
      (ios-simulator-mode-setup)
    (ios-simulator-mode-teardown)))


(defvar ios-simulator--installation-process nil
  "Process object for the current app installation.")

(defvar-local ios-simulator--current-simulator-name nil)
(defvar-local current-simulator-id nil)
(defvar-local xcode-project--current-app-identifier nil)
(defvar-local current-app-name nil)
;; Removed unused variable: use-rosetta (was always nil)

;; Multi-simulator support
(defvar ios-simulator--active-simulators (make-hash-table :test 'equal)
  "Hash table mapping simulator IDs to their info (name, app, buffer).")
(defvar ios-simulator--simulator-buffers (make-hash-table :test 'equal)
  "Hash table mapping simulator IDs to their buffer names.")
(defvar ios-simulator--target-simulators nil
  "List of simulator IDs to launch app on. When non-nil, app will launch on all these simulators.")

;; Legacy cache variables - kept for compatibility but now use swift-cache
(defvar ios-simulator--cached-devices nil
  "Deprecated: Now using swift-cache. Cached list of available simulator devices.")
(defvar ios-simulator--cache-timestamp nil
  "Deprecated: Now using swift-cache. Timestamp when devices were last cached.")
(defconst ios-simulator--cache-ttl 300
  "Deprecated: Now using swift-cache. Cache TTL in seconds (5 minutes).")

(defvar ios-simulator-ready-hook nil
  "Hook run when the simulator is ready for app installation.")

(defvar-local current-root-folder-simulator nil)

(defvar-local current-language-selection ios-simulator-default-language
  "Current language selection for the simulator.")


(defun ios-simulator-mode-setup ()
  "Setup the iOS simulator mode."
  (add-hook 'kill-buffer-hook #'ios-simulator-cleanup nil t))

(defun ios-simulator-mode-teardown ()
  "Teardown the iOS simulator mode."
  (remove-hook 'kill-buffer-hook #'ios-simulator-cleanup t))

(defun ios-simulator-cleanup ()
  "Clean up simulator resources."
  (when (and ios-simulator--installation-process
             (process-live-p ios-simulator--installation-process))
    (delete-process ios-simulator--installation-process))
  (setq ios-simulator--installation-process nil))

(defun ios-simulator-cleanup-global-state ()
  "Clean up global hash tables to prevent memory leaks.
This is called on `kill-emacs-hook' to ensure proper cleanup."
  (interactive)
  (when (hash-table-p ios-simulator--active-simulators)
    (clrhash ios-simulator--active-simulators))
  (when (hash-table-p ios-simulator--simulator-buffers)
    (clrhash ios-simulator--simulator-buffers))
  ;; Clear legacy cache variables
  (setq ios-simulator--cached-devices nil
        ios-simulator--cache-timestamp nil
        ios-simulator--target-simulators nil)
  (swift-notification-send :message "iOS Simulator global state cleaned up" :seconds 2))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'ios-simulator-cleanup-global-state)

(defun ios-simulator-cleanup-stale-entries ()
  "Remove entries for simulators that no longer exist.
This helps prevent memory leaks from accumulated hash table entries."
  (interactive)
  (let ((booted-ids (mapcar #'cdr (ios-simulator-get-all-booted-simulators)))
        (removed-count 0))
    ;; Clean up active simulators hash table
    (when (hash-table-p ios-simulator--active-simulators)
      (let ((keys-to-remove '()))
        (maphash (lambda (k _v)
                   (unless (member k booted-ids)
                     (push k keys-to-remove)))
                 ios-simulator--active-simulators)
        (dolist (k keys-to-remove)
          (remhash k ios-simulator--active-simulators)
          (cl-incf removed-count))))
    ;; Clean up simulator buffers hash table
    (when (hash-table-p ios-simulator--simulator-buffers)
      (let ((keys-to-remove '()))
        (maphash (lambda (k v)
                   (unless (and (member k booted-ids)
                                (buffer-live-p (get-buffer v)))
                     (push k keys-to-remove)))
                 ios-simulator--simulator-buffers)
        (dolist (k keys-to-remove)
          (remhash k ios-simulator--simulator-buffers)
          (cl-incf removed-count))))
    (when (and (> removed-count 0) ios-simulator-debug)
      (message "Cleaned up %d stale simulator entries" removed-count))
    removed-count))

(defun ios-simulator-reset (&optional choose-new-simulator)
  "Reset current settings.
With prefix argument CHOOSE-NEW-SIMULATOR, also select a new simulator."
  (interactive "P")
  (setq ios-simulator--current-simulator-name nil
        current-simulator-id nil
        xcode-project--current-app-identifier nil
        current-app-name nil
        current-language-selection ios-simulator-default-language
        current-root-folder-simulator nil)
  (ios-simulator-kill-buffer)
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (propertize "Simulator reset" 'face 'success)
     :seconds 2
     :reset t))
  (when (or choose-new-simulator
            (when (called-interactively-p 'any)
              (y-or-n-p "Choose a new simulator? ")))
    (ios-simulator-choose-simulator)))

(defun ios-simulator-current-sdk-version ()
  "Get the current simulator sdk-version."
  (string-trim (shell-command-to-string "xcrun --sdk iphonesimulator --show-sdk-version")))

;;;###autoload
(defun ios-simulator-sdk-path ()
  "Get the current simulator sdk-path."
  (string-trim (shell-command-to-string "xcrun --show-sdk-path --sdk iphonesimulator")))

(defun ios-simulator-current-arch ()
  "Get the current arch."
  (string-trim (shell-command-to-string "clang -print-target-triple")))

(defun ios-simulator-shut-down-all ()
  "Shut down all simulators."
  (interactive)
  (call-process-shell-command "xcrun simctl shutdown all")
  (swift-notification-send :message "All simulators shut down" :seconds 2))

(defun ios-simulator-target ()
  "Get the current simulator sdk."
  (let* ((target-components (split-string (ios-simulator-current-arch) "-"))
         (arch (nth 0 target-components))
         (vendor (nth 1 target-components))
         (version (ios-simulator-current-sdk-version)))
    (format "%s-%s-ios%s-simulator" arch vendor version)))

(cl-defun ios-simulator-install-and-run-app (&key rootfolder &key build-folder &key simulatorId &key appIdentifier &key terminate-first)
  "Install app in simulator with ROOTFOLDER BUILD-FOLDER SIMULATORID, APPIDENTIFIER BUFFER.
If TERMINATE-FIRST is non-nil, terminate existing app instance before installing.
If ios-simulator--target-simulators is set, launches on all specified simulators."
  (when ios-simulator-debug
    (message "Install-and-run root: %s build:%s" rootfolder build-folder))

  ;; Get the list of simulators to launch on
  (let* ((default-directory rootfolder)
         (primary-simulator-id (or simulatorId (ios-simulator-simulator-identifier)))
         (target-simulator-ids (if ios-simulator--target-simulators
                                   ios-simulator--target-simulators
                                 (list primary-simulator-id)))
         (applicationName (ios-simulator-get-app-name-fast build-folder)))

    (when ios-simulator-debug
      (message "Installing app: %s on %d simulator(s)"
               (or applicationName "Unknown")
               (length target-simulator-ids)))

    (setq current-simulator-id primary-simulator-id
          xcode-project--current-app-identifier appIdentifier
          current-root-folder-simulator rootfolder
          current-app-name applicationName)

    ;; Launch on all target simulators
    (dolist (simulator-id target-simulator-ids)
      (let* ((simulatorName (ios-simulator-simulator-name-from :id simulator-id))
             (buffer (if (string= simulator-id primary-simulator-id)
                        (get-buffer-create ios-simulator-buffer-name)
                      (ios-simulator-get-or-create-buffer-for-simulator simulator-id simulatorName))))

        (when ios-simulator-debug
          (message "Installing app: %s for simulator: %s (ID: %s)"
                   (or applicationName "Unknown")
                   (or simulatorName "Unknown")
                   (or simulator-id "Unknown")))

        ;; Store simulator info if not primary
        (unless (string= simulator-id primary-simulator-id)
          (puthash simulator-id
                   (list :name simulatorName
                         :app-identifier appIdentifier
                         :app-name applicationName
                         :buffer buffer)
                   ios-simulator--active-simulators))

        ;; Setup and boot simulator (only for additional simulators, primary is already booted)
        (unless (string= simulator-id primary-simulator-id)
          (ios-simulator-setup-simulator-dwim simulator-id))

        ;; Create closures that capture the current values
        (let ((current-sim-id simulator-id)
              (current-sim-name simulatorName)
              (current-buffer buffer)
              (current-buffer-name (buffer-name buffer))
              (current-app-name applicationName)
              (current-app-id appIdentifier)
              (xcode-project--current-build-folder build-folder)
              (current-terminate-first terminate-first))
          (if current-terminate-first
              ;; Terminate app asynchronously
              (make-process
               :name (format "terminate-app-%s" current-sim-id)
               :command (list "xcrun" "simctl" "terminate" current-sim-id current-app-id)
               :noquery t
               :sentinel (lambda (proc event)
                           ;; Install immediately after termination
                           (ios-simulator-install-app
                            :simulatorID current-sim-id
                            :build-folder xcode-project--current-build-folder
                            :appname current-app-name
                            :callback (lambda ()
                                        (when ios-simulator-debug
                                          (message "App installation completed on %s, launching app" current-sim-name))
                                        ;; Ensure buffer is still alive, recreate if needed
                                        (let ((launch-buffer (if (buffer-live-p current-buffer)
                                                                current-buffer
                                                              (get-buffer-create current-buffer-name))))
                                          (ios-simulator-launch-app
                                           :appIdentifier current-app-id
                                           :applicationName current-app-name
                                           :simulatorName current-sim-name
                                           :simulatorID current-sim-id
                                           :buffer launch-buffer
                                           :terminate-running current-terminate-first)))))))
            ;; Install directly without terminating
            (ios-simulator-install-app
             :simulatorID current-sim-id
             :build-folder xcode-project--current-build-folder
             :appname current-app-name
             :callback (lambda ()
                         (when ios-simulator-debug
                           (message "App installation completed on %s, launching app" current-sim-name))
                         ;; Ensure buffer is still alive, recreate if needed
                         (let ((launch-buffer (if (buffer-live-p current-buffer)
                                                 current-buffer
                                               (get-buffer-create current-buffer-name))))
                           (ios-simulator-launch-app
                            :appIdentifier current-app-id
                            :applicationName current-app-name
                            :simulatorName current-sim-name
                            :simulatorID current-sim-id
                            :buffer launch-buffer
                            :terminate-running current-terminate-first)))))))))

(cl-defun ios-simulator-install-app (&key simulatorID &key build-folder &key appname &key callback)
  "Install app (as SIMULATORID and BUILD-FOLDER APPNAME) and call CALLBACK when done."
  (let* ((folder build-folder)
         ;; Remove shell quotes if present to get clean path for file operations
         (install-path (if (and folder (string-match "^['\"]\\(.*\\)['\"]$" folder))
                          (match-string 1 folder)
                        folder))
         (app-path (format "%s%s.app" install-path appname))
         (command (list "xcrun" "simctl" "install" simulatorID app-path)))
    (when ios-simulator-debug
      (message "Installing app: %s" app-path)
      (message "App path exists: %s" (file-exists-p app-path)))

    (when (fboundp 'xcode-project-notify)
      (xcode-project-notify
       :message (format "Installing %s..." (propertize appname 'face 'font-lock-constant-face))))

    ;; Update progress bar
    (when (fboundp 'swift-notification-progress-update)
      (swift-notification-progress-update 'swift-build :percent 75 :message "Installing app..."))

    ;; Direct process creation without buffer
    (setq ios-simulator--installation-process
          (make-process
           :name "ios-simulator-install"
           :command command
           :noquery t
           :sentinel (lambda (process event)
                       (condition-case install-err
                           (progn
                             (when ios-simulator-debug
                               (message "Installation process event: %s" event))
                             ;; Handle all process exit events (finished, exited abnormally, etc.)
                             (when (memq (process-status process) '(exit signal))
                               (if (= 0 (process-exit-status process))
                                   (progn
                                     (when (fboundp 'xcode-project-notify)
                                       (xcode-project-notify
                                        :message (format "%s installed"
                                                         (propertize appname 'face 'success))
                                        :seconds 2
                                        :reset t))
                                     ;; Update progress bar - installation complete
                                     (when (fboundp 'swift-notification-progress-update)
                                       (swift-notification-progress-update 'swift-build :percent 85 :message "Installed"))
                                     (when callback (funcall callback)))
                                 (progn
                                   (when (fboundp 'xcode-project-notify)
                                     (xcode-project-notify
                                      :message (format "Installation failed: %s"
                                                       (propertize appname 'face 'error))
                                      :seconds 3
                                      :reset t))
                                   ;; Cancel progress bar on failure
                                   (when (fboundp 'swift-notification-progress-cancel)
                                     (swift-notification-progress-cancel 'swift-build))
                                   (message "App installation failed with exit code: %d"
                                            (process-exit-status process))))))
                         (error
                          (message "Error in installation sentinel: %s" (error-message-string install-err)))))))))


(defun ios-simulator-get-app-name-fast (build-folder)
  "Get app name quickly by looking for .app files in BUILD-FOLDER."
  (when build-folder
    (let* ((folder (if (string-match "^['\"]\\(.*\\)['\"]$" build-folder)
                      (match-string 1 build-folder)
                    build-folder))
           (app-files (directory-files folder nil "\\.app$")))
      (when app-files
        (file-name-sans-extension (car app-files))))))

(defun ios-simulator-kill-buffer ()
  "Kill the ios-simulator buffer."
  (when (get-buffer ios-simulator-buffer-name)
    (kill-buffer ios-simulator-buffer-name)))

(defun ios-simulator-setup-simulator-dwim (id)
  "Setup simulator dwim (as ID). Non-blocking asynchronous boot."
  (when ios-simulator-debug
    (message "Setting up simulator with id %s" id))
  ;; Try to boot directly - if Simulator.app is not running, boot will fail
  ;; and we'll start the app as fallback. This avoids blocking ps command.
  ;; NOTE: Don't show "Preparing simulator..." here - it runs too often (on file switch)
  ;; Only show notifications when simulator actually needs action
  (let ((proc (make-process
               :name "boot-simulator-dwim"
               :command (list "xcrun" "simctl" "boot" id)
               :noquery t
               :stderr (get-buffer-create " *simctl-boot-stderr*")
               :sentinel (lambda (proc event)
                           (when (string= event "finished\n")
                             (let ((exit-code (process-exit-status proc)))
                               (cond
                                ;; Successfully booted (exit code 0) - show notification
                                ((= exit-code 0)
                                 (when ios-simulator-debug
                                   (message "Simulator %s booted successfully" id))
                                 ;; Activate Simulator.app
                                 (make-process
                                  :name "activate-simulator"
                                  :command (list "osascript" "-e" "tell application \"Simulator\" to activate")
                                  :noquery t)
                                 (when (fboundp 'xcode-project-notify)
                                   (xcode-project-notify
                                    :message (propertize "Simulator ready" 'face 'success)
                                    :seconds 2
                                    :reset t)))
                                ;; Already booted (exit code 149) - just activate, no notification
                                ((= exit-code 149)
                                 (when ios-simulator-debug
                                   (message "Simulator %s already booted" id))
                                 ;; Still activate Simulator.app to bring it to front
                                 (make-process
                                  :name "activate-simulator"
                                  :command (list "osascript" "-e" "tell application \"Simulator\" to activate")
                                  :noquery t))
                                ;; Simulator.app not running, start it
                                (t
                                 (when ios-simulator-debug
                                   (message "Simulator.app not running, starting it for %s" id))
                                 (ios-simulator-start-simulator-with-id id)))))))))
    (set-process-query-on-exit-flag proc nil)))

(cl-defun ios-simulator-simulator-name (&key callback)
  "Fetches simulator name. If CALLBACK provided, run asynchronously."
  (if callback
      ;; Asynchronous version
      (if ios-simulator--current-simulator-name
          (funcall callback ios-simulator--current-simulator-name)
        (ios-simulator-simulator-name-from 
         :id current-simulator-id
         :callback (lambda (simulator-name)
                     (if simulator-name
                         (setq ios-simulator--current-simulator-name simulator-name)
                       (setq ios-simulator--current-simulator-name "Simulator (unknown)"))
                     (funcall callback ios-simulator--current-simulator-name))))
    ;; Synchronous version (fallback)
    (unless ios-simulator--current-simulator-name
      (let ((simulator-name (ios-simulator-simulator-name-from :id current-simulator-id)))
        (if simulator-name
            (setq ios-simulator--current-simulator-name simulator-name)
          (setq ios-simulator--current-simulator-name "Simulator (unknown)"))))
    ios-simulator--current-simulator-name))

(defun ios-simulator-boot-simulator-with-id (id)
  "Simulator app is running.  Boot simulator (as ID)."
  (when ios-simulator-debug
    (message "Booting simulator with id %s" id))
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify :message "Booting simulator..."))
  (make-process
   :name "boot-simulator"
   :command (list "sh" "-c" (ios-simulator-boot-command :id id))
   :sentinel (lambda (proc event)
               (when (string= event "finished\n")
                 ;; Activate Simulator.app after booting
                 (make-process
                  :name "activate-simulator"
                  :command (list "osascript" "-e" "tell application \"Simulator\" to activate")
                  :noquery t)
                 (when (fboundp 'xcode-project-notify)
                   (xcode-project-notify
                    :message (propertize "Simulator ready" 'face 'success)
                    :seconds 2
                    :reset t))))))

;;;###autoload
(defun ios-simulator-boot ()
  "Boot a simulator interactively.
Prompts for simulator selection and boots it."
  (interactive)
  (let ((sim-id (ios-simulator-get-or-choose-simulator)))
    (when sim-id
      (ios-simulator-boot-simulator-with-id sim-id))))

(defun ios-simulator-start-simulator-with-id (id)
  "Launch a specific simulator with (as ID)."
  (when ios-simulator-debug
    (message "Starting simulator with id: %s" id))
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (format "Starting simulator with id %s..." (propertize id 'face 'font-lock-constant-face))))
  (make-process
   :name "start-simulator"
   :command (list "sh" "-c" (format "open -a simulator --args -CurrentDeviceUDID %s" id))
   :sentinel (lambda (proc event)
               (when (string= event "finished\n")
                 ;; Wait a bit for Simulator.app to start, then activate it
                 (run-with-timer 0.5 nil
                                 (lambda ()
                                   (make-process
                                    :name "activate-simulator"
                                    :command (list "osascript" "-e" "tell application \"Simulator\" to activate")
                                    :noquery t)))))))

(cl-defun ios-simulator-boot-command (&key id)
  "Boot simulator (as ID)."
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (format "No simulator running. Booting device id: %s..." (propertize id 'face 'font-lock-constant-face))))
  (format "xcrun simctl boot %s" (shell-quote-argument id)))

(cl-defun ios-simulator-is-simulator-app-running (&key callback)
  "Check if simulator is running.
If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "check-simulator"
       :command (list "sh" "-c" "ps ax | grep -v grep | grep Simulator.app")
       :noquery t
       :buffer " *check-simulator-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (let ((buf (process-buffer proc)))
                       (when (buffer-live-p buf)
                         (let ((output (with-current-buffer buf (buffer-string))))
                           (kill-buffer buf)
                           (funcall callback (not (string= "" output)))))))))
    ;; Synchronous fallback
    (let ((output (shell-command-to-string "ps ax | grep -v grep | grep Simulator.app")))
      (not (string= "" output)))))

(cl-defun ios-simulator-simulator-name-from (&key id &key callback)
  "Get simulator name (as ID).
If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "simulator-name"
       :command (list "sh" "-c" (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" (shell-quote-argument id)))
       :noquery t
       :buffer " *simulator-name-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (let ((buf (process-buffer proc)))
                       (when (buffer-live-p buf)
                         (let ((result (string-trim (with-current-buffer buf (buffer-string)))))
                           (kill-buffer buf)
                           (funcall callback result)))))))
    ;; Synchronous fallback
    (string-trim
     (shell-command-to-string (format "xcrun simctl list devices | grep %s | awk -F \"(\" '{ print $1 }'" (shell-quote-argument id))))))

(defun ios-simulator-available-simulators ()
  "List available simulators with iOS version displayed."
  (let* ((devices (ios-simulator-fetch-available-simulators))
         (items (seq-map
                 (lambda (device)
                   (let* ((name (cdr (assoc 'name device)))
                          (ios-version (cdr (assoc 'iosVersion device)))
                          ;; Add iOS version only if name doesn't already contain version info
                          (display-name 
                           (cond
                            ;; If name already contains version info, keep it as is
                            ((string-match-p "([0-9]+\\.[0-9]+)" name) name)
                            ((string-match-p "(iOS [0-9]+\\.[0-9]+)" name) name)
                            ;; If we have ios-version and name doesn't show it, add it
                            ((and ios-version)
                             (format "%s (iOS %s)" name ios-version))
                            ;; Default: use name as is
                            (t name))))
                     (cons display-name
                           (cdr (assoc 'udid device))))) devices)))
    items))

(defun ios-simulator-available-ios-versions ()
  "Get list of available iOS versions."
  (let* ((json (ios-simulator-run-command-and-get-json-simple list-simulators-command))
         (devices (when json (cdr (assoc 'devices json))))
         (versions '()))
    (unless devices
      (when ios-simulator-debug
        (message "No devices found in simulator list")))
    (dolist (runtime-entry devices)
      (let* ((runtime-key (car runtime-entry))
             (runtime-devices (cdr runtime-entry))
             (runtime-string (if (symbolp runtime-key)
                                 (symbol-name runtime-key)
                               runtime-key))
             (ios-version (when (and runtime-string
                                     (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                           (format "%s.%s"
                                   (match-string 1 runtime-string)
                                   (match-string 2 runtime-string)))))
        ;; Only add version if there are available devices
        (when (and ios-version
                   (seq-some (lambda (device) (cdr (assoc 'isAvailable device)))
                            runtime-devices))
          (unless (member ios-version versions)
            (push ios-version versions)))))
    (sort versions 'version<)))

(defun ios-simulator-available-ios-versions-async (callback)
  "Get list of available iOS versions asynchronously.
Calls CALLBACK with version list."
  (ios-simulator-run-command-and-get-json-async
   list-simulators-command
   (lambda (json)
     (when json
       (let* ((devices (cdr (assoc 'devices json)))
              (versions '()))
         (dolist (runtime-entry devices)
           (let* ((runtime-key (car runtime-entry))
                  (runtime-devices (cdr runtime-entry))
                  (runtime-string (if (symbolp runtime-key)
                                      (symbol-name runtime-key)
                                    runtime-key))
                  (ios-version (when (and runtime-string
                                          (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                (format "%s.%s"
                                        (match-string 1 runtime-string)
                                        (match-string 2 runtime-string)))))
             ;; Only add version if there are available devices
             (when (and ios-version
                        (seq-some (lambda (device) (cdr (assoc 'isAvailable device)))
                                 runtime-devices))
               (unless (member ios-version versions)
                 (push ios-version versions)))))
         (funcall callback (sort versions 'version<)))))))

(defun ios-simulator-devices-for-ios-version (ios-version)
  "Get available devices for a specific IOS-VERSION."
  (let* ((json (ios-simulator-run-command-and-get-json-simple list-simulators-command))
         (devices (when json (cdr (assoc 'devices json))))
         (matching-devices '()))
    (unless devices
      (when ios-simulator-debug
        (message "No devices found for iOS version %s" ios-version)))
    (dolist (runtime-entry devices)
      (let* ((runtime-key (car runtime-entry))
             (runtime-devices (cdr runtime-entry))
             (runtime-string (if (symbolp runtime-key)
                                 (symbol-name runtime-key)
                               runtime-key))
             (runtime-ios-version (when (and runtime-string
                                            (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                   (format "%s.%s" 
                                           (match-string 1 runtime-string)
                                           (match-string 2 runtime-string)))))
        (when (string= runtime-ios-version ios-version)
          (dolist (device (append runtime-devices nil))
            (when (and device
                       (listp device)
                       (cdr (assoc 'isAvailable device)))
              (push (cons (cdr (assoc 'name device))
                         (cdr (assoc 'udid device))) matching-devices))))))
    (nreverse matching-devices)))

(cl-defun ios-simulator-build-language-menu (&key title)
  "Build language menu (as TITLE)."
  (defconst languageList '(
                           ("🇦🇪 Arabic (United Arab Emirates)" "ar-AE")
                           ("🇦🇷 Arabic (Saudi Arabia)" "ar-SA")
                           ("🇦🇺 English (Australia)" "en-AU")
                           ("🇧🇪 Dutch (Belgium)" "nl-BE")
                           ("🇧🇭 Indonesian (Indonesia)" "id-ID")
                           ("🇧🇷 Portuguese (Brazil)" "pt-BR")
                           ("🇨🇳 Chinese (Simplified)" "zh-CN")
                           ("🇩🇪 German (Germany)" "de-DE")
                           ("🇪🇸 Spanish (Spain)" "es-ES")
                           ("🇫🇷 French (France)" "fr-FR")
                           ("🇬🇧 English (UK)" "en-UK")
                           ("🇮🇳 Hindi (India)" "hi-IN")
                           ("🇮🇹 Italian (Italy)" "it-IT")
                           ("🇯🇵 Japanese (Japan)" "ja-JP")
                           ("🇰🇷 Korean (Korea)" "ko-KR")
                           ("🇳🇱 Dutch (Netherlands)" "nl-NL")
                           ("🇳🇴 Norwegian (Bokmål)" "nb-NO")
                           ("🇵🇱 Polish (Poland)" "pl-PL")
                           ("🇷🇺 Russian (Russia)" "ru-RU")
                           ("🇸🇦 Arabic (Saudi Arabia)" "ar-EG")
                           ("🇸🇪 Swedish (Sweden)" "sv-SE")
                           ("🇹🇷 Turkish (Turkey)" "tr-TR")
                           ("🇹🇼 Chinese (Traditional)" "zh-TW")
 ("🇩🇰 Danish (Denmark)" "da-DK")
                           ))
    (progn
    (let* ((choices (seq-map (lambda (item) item) languageList))
           (choice (completing-read title choices)))
      (car (cdr (assoc choice choices))))))

(cl-defun ios-simulator-build-selection-menu (&key title &key list)
  "Builds a widget menu from (as TITLE as LIST)."
  (if (<= (length list) 1)
      (elt list 0)
    (progn
      (let* ((choices (seq-map (lambda (item) item) list))
             (choice (completing-read title choices)))
        (cdr (assoc choice choices))))))

(cl-defun ios-simulator-simulator-identifier (&key callback)
  "Get the booted simulator id or fetch a suitable one. If CALLBACK provided, run asynchronously."
  ;; Validate current-simulator-id if it exists
  (when (and current-simulator-id
             (not (ios-simulator-device-exists-p current-simulator-id)))
    (when ios-simulator-debug
      (message "Cached simulator ID %s no longer exists, clearing it" current-simulator-id))
    (when (fboundp 'xcode-project-notify)
      (xcode-project-notify
       :message "Saved simulator no longer available, please select a new one"
       :seconds 3))
    (setq current-simulator-id nil))

  (if callback
      ;; Asynchronous version
      (if current-simulator-id
          (funcall callback current-simulator-id)
        (let ((device-id (ios-simulator-get-or-choose-simulator)))
          (when ios-simulator-debug
            (message "Selected simulator ID: %s" device-id))
          (funcall callback device-id)))
    ;; Synchronous version (fallback)
    (if current-simulator-id
        current-simulator-id
      (let ((device-id (ios-simulator-get-or-choose-simulator)))
        (when ios-simulator-debug
          (message "Selected simulator ID: %s" device-id))
        device-id))))

(defun ios-simulator-get-or-choose-simulator ()
  "Get booted simulator or let user choose one.
If target simulators are configured, ensures primary is set correctly.
If exactly one simulator is booted, use it automatically.
If multiple simulators are booted, let user choose which is the main one."
  ;; Pre-load cache in background if not already loaded (non-blocking)
  (when (and (not ios-simulator--cached-devices)
             (not (and (fboundp 'swift-cache-get)
                       (swift-cache-get "ios-simulator-available-devices"))))
    (ios-simulator-preload-cache))

  (let ((booted-simulators (ios-simulator-get-all-booted-simulators)))
    ;; Validate current-simulator-id if it exists
    (when (and current-simulator-id
               (not (ios-simulator-device-exists-p current-simulator-id)))
      (when ios-simulator-debug
        (message "Saved simulator ID %s no longer exists, clearing it" current-simulator-id))
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (format "Saved simulator no longer available, please select a new one")
         :seconds 3))
      (setq current-simulator-id nil))

    (if (and current-simulator-id
             ;; Only use cached ID if a simulator is actually booted
             (or booted-simulators
                 ;; Or if this is a fresh start, boot it
                 (progn
                   (ios-simulator-setup-simulator-dwim current-simulator-id)
                   t)))
        (progn
          (ios-simulator-setup-language)
          ;; If we have target simulators configured, make sure current is in the list
          (when (and ios-simulator--target-simulators
                     (not (member current-simulator-id ios-simulator--target-simulators)))
            (push current-simulator-id ios-simulator--target-simulators))
          current-simulator-id)
      ;; No current ID or no booted simulators - choose one
      (cond
       ;; No simulators booted - let user choose and boot one
       ((null booted-simulators)
        (ios-simulator-choose-simulator))

       ;; Exactly one simulator booted - use it automatically
       ((= (length booted-simulators) 1)
        (let ((booted-id (cdar booted-simulators))
              (booted-name (caar booted-simulators)))
          (when ios-simulator-debug
            (message "Auto-selecting booted simulator: %s" booted-name))
          (setq current-simulator-id booted-id)
          (ios-simulator-setup-language)
          ;; Already booted, no need to setup/boot again
          (when (fboundp 'xcode-project-notify)
            (xcode-project-notify
             :message (format "Using booted simulator: %s"
                              (propertize booted-name 'face 'font-lock-function-name-face))
             :seconds 2
             :reset t))
          booted-id))

       ;; Multiple simulators booted - let user choose the main one
       (t
        (let* ((choice (completing-read
                       (format "Multiple simulators running. Choose main simulator (%d booted): "
                               (length booted-simulators))
                       booted-simulators nil t))
               (chosen-id (cdr (assoc choice booted-simulators))))
          (when ios-simulator-debug
            (message "User selected main simulator: %s (ID: %s)" choice chosen-id))
          (setq current-simulator-id chosen-id)
          (ios-simulator-setup-language)
          ;; Already booted, no need to setup/boot again
          (when (fboundp 'xcode-project-notify)
            (xcode-project-notify
             :message (format "Using simulator: %s"
                              (propertize choice 'face 'font-lock-function-name-face))
             :seconds 2
             :reset t))
          chosen-id))))))

;;;###autoload
(defun ios-simulator-choose-simulator ()
  "Choose a simulator using two-step process: iOS version first, then device.
Uses cached simulator data when available to improve performance."
  (interactive)
  ;; Show message immediately so user knows something is happening
  (message "Loading simulators...")
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify :message "Loading available simulators..."))
  (redisplay t) ; Force display update

  ;; Use cached data from ios-simulator-fetch-available-simulators
  ;; This function uses swift-cache which will block on first call but is fast when cached
  (let* ((all-devices (ios-simulator-fetch-available-simulators))
         (ios-versions '())
         (devices-by-version (make-hash-table :test 'equal)))

    ;; Build version list and devices-by-version map from cached data
    (dolist (device all-devices)
      (let ((ios-version (cdr (assoc 'iosVersion device)))
            (device-name (cdr (assoc 'name device)))
            (device-udid (cdr (assoc 'udid device))))
        (when ios-version
          (unless (member ios-version ios-versions)
            (push ios-version ios-versions))
          ;; Store device for this version
          (let ((current-devices (gethash ios-version devices-by-version '())))
            (push (cons device-name device-udid) current-devices)
            (puthash ios-version current-devices devices-by-version)))))

    ;; Reverse the device lists to maintain order
    (maphash (lambda (version devices)
               (puthash version (nreverse devices) devices-by-version))
             devices-by-version)

    (setq ios-versions (sort ios-versions 'version<))

    ;; Show prompts
    (let* ((chosen-ios-version (completing-read "Choose iOS version: " ios-versions nil t))
           (devices-for-version (gethash chosen-ios-version devices-by-version))
           (device-choice (if (= (length devices-for-version) 1)
                              ;; If only one device, use it automatically
                              (car devices-for-version)
                            ;; If multiple devices, let user choose
                            (let* ((choices devices-for-version)
                                   (choice (completing-read
                                           (format "Choose device for iOS %s: " chosen-ios-version)
                                           choices nil t)))
                              (assoc choice choices))))
           (device-id (cdr device-choice)))

      (when ios-simulator-debug
        (message "Selected iOS %s with device: %s (ID: %s)"
                 chosen-ios-version (car device-choice) device-id))
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (format "Selected: %s"
                          (propertize (car device-choice) 'face 'success))
         :seconds 2
         :reset t))
      (setq current-simulator-id device-id
            ios-simulator--current-simulator-name (car device-choice))

      ;; Save the new selection to settings asynchronously
      (when (and (fboundp 'xcode-project-project-root)
                 (fboundp 'swift-project-settings-capture-from-variables))
        (let ((current-buf (current-buffer))
              (project-root (xcode-project-project-root)))
          (when project-root
            (run-with-idle-timer 0.1 nil
                                 (lambda (buf root)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (swift-project-settings-capture-from-variables root))))
                                 current-buf project-root))))

      (ios-simulator-setup-language)
      (ios-simulator-setup-simulator-dwim device-id)
      device-id)))

(defun ios-simulator-device-exists-p (device-id)
  "Check if DEVICE-ID exists in available simulators.
Returns t if the device exists, nil otherwise."
  (when device-id
    (let* ((json-data (ios-simulator-run-command-and-get-json-simple
                       "xcrun simctl list devices available -j"))
           (devices-dict (when json-data (cdr (assoc 'devices json-data))))
           (found nil))
      (when devices-dict
        (dolist (runtime-entry devices-dict)
          (let ((devices-list (cdr runtime-entry)))
            (when (vectorp devices-list)
              (mapc (lambda (device)
                      (let ((udid (cdr (assoc 'udid device))))
                        (when (string= udid device-id)
                          (setq found t))))
                    (append devices-list nil))))))
      found)))

(defun ios-simulator-get-all-booted-simulators ()
  "Get list of all currently booted simulators.
Returns list of (name . id) pairs."
  (when ios-simulator-debug
    (message "Checking for booted simulators..."))
  (let* ((json-data (ios-simulator-run-command-and-get-json-simple
                     "xcrun simctl list devices available -j"))
         (devices-dict (when json-data (cdr (assoc 'devices json-data))))
         (simulators '()))
    (when devices-dict
      ;; Iterate through all runtimes
      (dolist (runtime-entry devices-dict)
        (let ((devices-list (cdr runtime-entry)))
          (when (vectorp devices-list)
            ;; Convert vector to list and check each device
            (mapc (lambda (device)
                    (let ((state (cdr (assoc 'state device)))
                          (name (cdr (assoc 'name device)))
                          (udid (cdr (assoc 'udid device))))
                      (when (and state (string= state "Booted"))
                        (when ios-simulator-debug
                          (message "Found booted simulator: %s (%s)" name udid))
                        (push (cons name udid) simulators))))
                  (append devices-list nil))))))
    (when ios-simulator-debug
      (message "Total booted simulators found: %d" (length simulators)))
    (nreverse simulators)))

;;;###autoload
(defun ios-simulator-list-booted ()
  "List all currently booted simulators."
  (interactive)
  (let ((booted (ios-simulator-get-all-booted-simulators)))
    (if (null booted)
        (swift-notification-send :message "No booted simulators" :seconds 2)
      (with-current-buffer (get-buffer-create "*Booted Simulators*")
        (erase-buffer)
        (insert "Booted Simulators:\n")
        (insert (make-string 50 ?─) "\n\n")
        (dolist (sim booted)
          (insert (format "  %s\n    ID: %s\n\n"
                          (propertize (car sim) 'face 'font-lock-function-name-face)
                          (cdr sim))))
        (goto-char (point-min)))
      (display-buffer "*Booted Simulators*")
      (swift-notification-send :message (format "%d booted simulator(s)" (length booted)) :seconds 2))))

(cl-defun ios-simulator-booted-simulator (&key callback)
  "Get booted simulator if any. If CALLBACK provided, run asynchronously."
  (if callback
      (make-process
       :name "booted-simulator"
       :command (list "sh" "-c" ios-simulator-get-booted-simulator-command)
       :noquery t
       :buffer " *booted-simulator-temp*"
       :sentinel (lambda (proc event)
                   (when (string= event "finished\n")
                     (let ((buf (process-buffer proc)))
                       (when (buffer-live-p buf)
                         (let ((device-id (string-trim (with-current-buffer buf (buffer-string)))))
                           (kill-buffer buf)
                           (funcall callback (if (string= "" device-id) nil device-id))))))))
    ;; Synchronous fallback
    (let ((device-id (shell-command-to-string ios-simulator-get-booted-simulator-command)))
      (if (not (string= "" device-id))
          (string-trim device-id)
        nil))))

(defun ios-simulator-terminate-current-app ()
  "Terminate the current app running in simulator."
  (interactive)
  (if xcode-project--current-app-identifier
      (ios-simulator-terminate-app-with :appIdentifier xcode-project--current-app-identifier)))

(defun ios-simulator-change-language ()
  "Reset current language for simulator."
  (interactive)
  (setq current-language-selection (ios-simulator-build-language-menu :title "Choose simulator language")))

(defun ios-simulator-setup-language ()
  "Setup language if it isnt set."
  (unless current-language-selection
    ;; Use default language instead of showing a blocking prompt
    (setq current-language-selection ios-simulator-default-language)))

(cl-defun ios-simulator-launch-app (&key appIdentifier &key applicationName &key simulatorName &key simulatorID &key buffer &key terminate-running)
  "Launch app (as APPIDENTIFIER APPLICATIONNAME SIMULATORNAME SIMULATORID) and display output in BUFFER.
If TERMINATE-RUNNING is non-nil, terminate any running instance before launching."
  ;; Run immediately without idle timer
  (ios-simulator-setup-language)
  ;; Show notification for app launch
  (when (and (fboundp 'xcode-project-notify) applicationName)
    (let ((safe-sim-name (or simulatorName "Simulator")))
      (xcode-project-notify
       :message (format "%s|%s"
                        (propertize applicationName 'face 'font-lock-constant-face)
                        (propertize safe-sim-name 'face 'font-lock-function-name-face))
       :delay 2.0
       :reset t)))

  ;; Finish progress bar - app is launching
  (when (fboundp 'swift-notification-progress-finish)
    (swift-notification-progress-finish 'swift-build "Running!"))

  (let ((command (append (list "xcrun" "simctl" "launch" "--console-pty"
                               (or simulatorID "booted")
                               appIdentifier)
                         (when terminate-running (list "--terminate-running-process"))
                         (list "-AppleLanguages" (format "(%s)" current-language-selection)))))
    ;; Prepare the buffer
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 5
                  right-fringe-width 5
                  buffer-face-mode-face 'ios-simulator-background-face
                  window-point-insertion-type t
                  kill-buffer-query-functions nil)
      (buffer-face-mode 1)
      (read-only-mode -1)
      (visual-line-mode 1)
      ;; Enable syntax highlighting for log output
      (when ios-simulator-colorize-output
        (setq-local font-lock-defaults '(ios-simulator-log-font-lock-keywords t))
        (font-lock-mode 1))
      ;; Enable ios-simulator-mode for the buffer
      (ios-simulator-mode 1))

    (display-buffer buffer '(display-buffer-pop-up-window))

    (let ((process (make-process
                       :name "ios-simulator-launch"
                       :command command
                       :buffer buffer
                       :noquery t
                       :filter (lambda (proc string)
                             (when (buffer-live-p (process-buffer proc))
                               (with-current-buffer (process-buffer proc)
                                 (let ((inhibit-read-only t)
                                       (at-end (= (point) (point-max)))
                                       (start-pos (point-max)))
                                   (save-excursion
                                     (goto-char (point-max))
                                     ;; Apply ANSI color codes and remove control-M
                                     (let ((cleaned-string (ios-simulator-remove-control-m string)))
                                       (insert (if ios-simulator-colorize-output
                                                   (ansi-color-apply cleaned-string)
                                                 cleaned-string)))
                                     ;; Re-fontify the newly inserted region
                                     (when (and ios-simulator-colorize-output font-lock-mode)
                                       (font-lock-ensure start-pos (point-max))))
                                   (when at-end
                                     (goto-char (point-max))
                                     (dolist (window (get-buffer-window-list (current-buffer) nil t))
                                       (set-window-point window (point-max))))))))
                       :sentinel (lambda (process event)
                                     (when (and (string= event "finished\n")
                                        (buffer-live-p (process-buffer process)))
                               (with-current-buffer (process-buffer process)
                                 (let ((inhibit-read-only t))
                                   (goto-char (point-max))
                                   (insert "\n\nProcess finished\n"))
                                 (read-only-mode 1)))))))
      ;; Don't ask when killing the process
      (set-process-query-on-exit-flag process nil))))

(defun ios-simulator-run-command-and-get-json (command)
  "Run a shell COMMAND and return the JSON output as a string.
Note: Prefer `ios-simulator-parse-json-safe' for better error handling."
  (condition-case err
      (let* ((json-output (shell-command-to-string command))
             (json-data (json-read-from-string json-output)))
        json-data)
    (error
     (when ios-simulator-debug
       (message "JSON parse error in command '%s': %s" command (error-message-string err)))
     nil)))

(defun ios-simulator-json-get (json &rest keys)
  "Safely get nested value from JSON using KEYS.
Returns nil if any key in path doesn't exist or JSON is nil."
  (when json
    (let ((value json))
      (while (and keys value)
        (setq value (cdr (assoc (pop keys) value))))
      value)))

(defun ios-simulator-parse-json-safe (command &optional error-message)
  "Run COMMAND and parse JSON with proper error handling.
Returns a cons cell: (success . data) on success, or (nil . error-string) on failure.
ERROR-MESSAGE is an optional custom error message."
  (condition-case err
      (let ((json (ios-simulator-run-command-and-get-json-simple command)))
        (if json
            (cons t json)
          (cons nil (or error-message "Empty JSON response from command"))))
    (error
     (cons nil (format "JSON parse error: %s" (error-message-string err))))))

(defun ios-simulator-run-command-and-get-json-async (command callback)
  "Run a shell COMMAND asynchronously and call CALLBACK with parsed JSON data."
  (make-process
   :name "simctl-json"
   :command (list "sh" "-c" command)
   :noquery t
   :buffer " *simctl-json-temp*"
   :sentinel (lambda (proc event)
               (when (string= event "finished\n")
                 (let ((buf (process-buffer proc)))
                   (when (buffer-live-p buf)
                     (let ((json-data (condition-case err
                                          (json-read-from-string
                                           (with-current-buffer buf (buffer-string)))
                                        (error
                                         (message "Failed to parse JSON: %s" err)
                                         nil))))
                       (kill-buffer buf)
                       (when callback
                         (funcall callback json-data)))))))))

(defun ios-simulator-run-command-and-get-json-simple (command)
  "Run a shell COMMAND and return JSON. Simple synchronous version with timeout.
The command should be fast (<0.3s) but we add a 5s timeout as safety measure."
  (let* ((timeout-secs 5)
         (start-time (current-time))
         (json-output nil)
         (timed-out nil))
    (with-temp-buffer
      (let ((proc (start-process-shell-command
                   "simctl-json-sync" (current-buffer) command)))
        ;; Wait for process to finish or timeout
        (while (and (process-live-p proc)
                    (< (float-time (time-subtract (current-time) start-time)) timeout-secs))
          (accept-process-output proc 0.1))

        (if (process-live-p proc)
            (progn
              ;; Timeout - kill the process
              (delete-process proc)
              (setq timed-out t)
              (when ios-simulator-debug
                (message "Command timed out after %ds: %s" timeout-secs command)))
          ;; Process finished - get output
          (setq json-output (buffer-string)))))

    (if (and json-output (not timed-out) (> (length json-output) 0))
        (condition-case err
            (json-read-from-string json-output)
          (error
           (when ios-simulator-debug
             (message "Failed to parse JSON: %s" err))
           nil))
      nil)))

(cl-defun ios-simulator-terminate-app-with (&key appIdentifier)
  "Terminate runnings apps (as APPIDENTIFIER)."
  (setq xcode-project--current-app-identifier appIdentifier)
  ;; Run in an idle timer to avoid blocking
  (run-with-idle-timer 0 nil
    (lambda ()
      (ios-simulator-terminate-app :simulatorID current-simulator-id :appIdentifier appIdentifier))))

(cl-defun ios-simulator-terminate-app (&key simulatorID &key appIdentifier)
  "Terminate app (as APPIDENTIFIER as SIMULATORID)."
  (when ios-simulator-debug
    (message "%s %s" simulatorID appIdentifier))
  (inhibit-sentinel-messages #'call-process-shell-command
                             (string-trim
                              (concat
                               (if simulatorID
                                   (format "xcrun simctl terminate %s %s"
                                           (shell-quote-argument simulatorID)
                                           (shell-quote-argument appIdentifier))
                                 (format "xcrun simctl terminate booted %s"
                                         (shell-quote-argument appIdentifier)))))))

(defun ios-simulator-send-notification ()
  "Send a notification to the current simulator and app."
  (interactive)
  (let ((missing-items '()))
    ;; Check what's missing
    (unless current-simulator-id
      (push "simulator" missing-items))
    (unless xcode-project--current-app-identifier
      (push "app bundle ID" missing-items))
    ;; Report missing items with notification
    (when missing-items
      (let ((msg (format "Cannot send notification. Missing: %s. Build and run the app first."
                         (string-join (nreverse missing-items) ", "))))
        (when (fboundp 'xcode-project-notify)
          (xcode-project-notify
           :message (propertize msg 'face 'error)
           :seconds 4
           :reset t))
        (user-error "%s" msg))))
  ;; All good, proceed
  (let* ((text (read-string "Notification text: ")))
    (when (and text (not (string-empty-p text)))
      (let* ((payload (json-encode `((aps . ((alert . ,text) (sound . "default"))))))
             (temp-file (make-temp-file "ios-notification" nil ".json" payload))
             (sim-id current-simulator-id)
             (app-id xcode-project--current-app-identifier)
             (app-name (or current-app-name app-id)))
        (make-process
         :name "ios-notification-push"
         :command (list "xcrun" "simctl" "push" sim-id app-id temp-file)
         :noquery t
         :sentinel (lambda (proc event)
                     (unwind-protect
                         (when (string-match-p "finished" event)
                           (when (fboundp 'xcode-project-notify)
                             (xcode-project-notify
                              :message (format "Notification sent to %s"
                                               (propertize app-name 'face 'success))
                              :seconds 2
                              :reset t))
                           (message "Notification sent to %s" app-name))
                       ;; Always cleanup temp file
                       (when (file-exists-p temp-file)
                         (condition-case nil
                             (delete-file temp-file)
                           (file-error nil))))))))))

(defun ios-simulator-appcontainer ()
  "Get the app container of the current app (as SIMULATORID, APPIDENTIFIER)."
  (interactive)
  (if-let* ((identifier xcode-project--current-app-identifier)
           (id current-simulator-id)
           (command (shell-command-to-string (format "xcrun simctl get_app_container %s %s data"
                                                     (shell-quote-argument id)
                                                     (shell-quote-argument identifier)))))
      (async-shell-command (concat "open " (shell-quote-argument (string-trim command))))))

(defun ios-simulator-fetch-available-simulators ()
  "List available simulators, using persistent device-cache if valid."
  (let* ((project-root (when (fboundp 'xcode-project-project-root)
                         (xcode-project-project-root)))
         (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                   xcode-project--current-xcode-scheme))
         (project-file (when project-root
                         (let ((xcodeproj-files (directory-files project-root nil "\\.xcodeproj$")))
                           (car xcodeproj-files))))
         ;; Try to get cached devices if we have project context
         (cached-devices (when (and project-root scheme project-file
                                    (fboundp 'swift-project-settings-get-cached-devices))
                           (swift-project-settings-get-cached-devices
                            project-root scheme project-file))))

    (if cached-devices
        ;; Return cached devices
        (progn
          (when ios-simulator-debug
            (message "[Device Cache] Using %d cached devices for scheme: %s"
                     (length cached-devices) scheme))
          cached-devices)

      ;; Cache miss or invalid - fetch fresh devices
      (when ios-simulator-debug
        (message "[Device Cache] Fetching fresh device list..."))

      (let* ((json (ios-simulator-run-command-and-get-json-simple list-simulators-command))
             (devices (when json (cdr (assoc 'devices json))))
             (flattened-devices '()))

        (unless devices
          (when ios-simulator-debug
            (message "[Device Cache] Failed to fetch device list - empty response")))

        ;; Process each runtime and its devices
        (dolist (runtime-entry devices)
          (let* ((runtime-key (car runtime-entry))
                 (runtime-devices (cdr runtime-entry))
                 ;; Extract iOS version from runtime key
                 (runtime-string (if (symbolp runtime-key)
                                     (symbol-name runtime-key)
                                   runtime-key))
                 (ios-version (when (and runtime-string
                                         (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                               (format "%s.%s"
                                       (match-string 1 runtime-string)
                                       (match-string 2 runtime-string)))))
            ;; Process each device in this runtime
            (dolist (device (append runtime-devices nil))
              (when (and device
                         (listp device)
                         (cdr (assoc 'isAvailable device)))
                ;; Create a new device entry with iOS version
                (let ((device-with-version (copy-alist device)))
                  (when ios-version
                    (setcdr device-with-version
                            (cons (cons 'iosVersion ios-version)
                                  (cdr device-with-version))))
                  (push device-with-version flattened-devices))))))

        (setq flattened-devices (nreverse flattened-devices))

        ;; Cache the devices if we have project context
        (when (and project-root scheme project-file
                   (fboundp 'swift-project-settings-cache-devices))
          (swift-project-settings-cache-devices
           project-root flattened-devices scheme project-file)
          (when ios-simulator-debug
            (message "[Device Cache] Cached %d devices for scheme: %s"
                     (length flattened-devices) scheme)))

        flattened-devices))))

(defun ios-simulator-invalidate-cache ()
  "Force refresh of simulator device cache (both persistent and in-memory)."
  (interactive)
  ;; Invalidate persistent device-cache
  (let ((project-root (when (fboundp 'xcode-project-project-root)
                        (xcode-project-project-root))))
    (when (and project-root (fboundp 'swift-project-settings-invalidate-device-cache))
      (swift-project-settings-invalidate-device-cache project-root)))

  ;; Invalidate swift-cache if available
  (when (fboundp 'swift-cache-invalidate)
    (swift-cache-invalidate "ios-simulator-available-devices"))

  ;; Invalidate legacy cache variables
  (setq ios-simulator--cached-devices nil
        ios-simulator--cache-timestamp nil)

  (message "Simulator cache invalidated"))

(defvar ios-simulator--preload-timer nil
  "Timer for delayed cache preloading.")

(defun ios-simulator-preload-cache ()
  "Pre-load simulator device cache in the background to improve responsiveness.
This is called automatically when needed but can also be called manually."
  (interactive)
  ;; Cancel any existing timer
  (when ios-simulator--preload-timer
    (cancel-timer ios-simulator--preload-timer)
    (setq ios-simulator--preload-timer nil))

  ;; Check if cache is already valid
  (let ((cache-valid (or (and ios-simulator--cached-devices
                              ios-simulator--cache-timestamp
                              (< (- (float-time) ios-simulator--cache-timestamp)
                                 ios-simulator--cache-ttl))
                         (and (fboundp 'swift-cache-get)
                              (swift-cache-get "ios-simulator-available-devices")))))
    (if cache-valid
        (when ios-simulator-debug
          (message "[Auto-warming] Cache already valid, skipping preload"))
      (when ios-simulator-debug
        (message "[Auto-warming] Starting async cache preload..."))
      (ios-simulator-run-command-and-get-json-async
       list-simulators-command
       (lambda (json)
         (when json
           (let* ((devices (cdr (assoc 'devices json)))
                  (flattened-devices '()))
             ;; Process devices
             (dolist (runtime-entry devices)
               (let* ((runtime-key (car runtime-entry))
                      (runtime-devices (cdr runtime-entry))
                      (runtime-string (if (symbolp runtime-key)
                                          (symbol-name runtime-key)
                                        runtime-key))
                      (ios-version (when (and runtime-string
                                              (string-match "iOS-\\([0-9]+\\)-\\([0-9]+\\)" runtime-string))
                                    (format "%s.%s"
                                            (match-string 1 runtime-string)
                                            (match-string 2 runtime-string)))))
                 (dolist (device (append runtime-devices nil))
                   (when (and device
                              (listp device)
                              (cdr (assoc 'isAvailable device)))
                     (let ((device-with-version (copy-alist device)))
                       (when ios-version
                         (setcdr device-with-version
                                 (cons (cons 'iosVersion ios-version)
                                       (cdr device-with-version))))
                       (push device-with-version flattened-devices))))))
             ;; Store in cache
             (if (fboundp 'swift-cache-set)
                 (swift-cache-set "ios-simulator-available-devices" (nreverse flattened-devices) 300)
               ;; Fallback to legacy cache
               (setq ios-simulator--cached-devices (nreverse flattened-devices)
                     ios-simulator--cache-timestamp (float-time)))
             (when ios-simulator-debug
               (message "[Auto-warming] ✅ Cache pre-loaded with %d devices in background" (length flattened-devices))))))))))

(defun ios-simulator-maybe-preload-cache ()
  "Schedule cache preloading if in a Swift/iOS project.
Uses a timer to avoid blocking during file opening."
  (when ios-simulator-debug
    (message "[Auto-warming] Checking if cache preload needed for buffer: %s" (buffer-name)))
  ;; Only preload if we're likely in an iOS project
  (when (and (buffer-file-name)
             (or (string-match-p "\\.swift$" (buffer-file-name))
                 (string-match-p "\\.xcodeproj" (or default-directory ""))))
    (when ios-simulator-debug
      (message "[Auto-warming] ✓ Swift file detected, scheduling cache preload in 0.5s..."))
    ;; Schedule preload after 0.5 seconds to not interfere with file opening
    (setq ios-simulator--preload-timer
          (run-with-idle-timer 0.5 nil #'ios-simulator-preload-cache))))

(defun ios-simulator-toggle-buffer ()
  "Toggle visibility of the iOS Simulator buffer window."
  (interactive)
  (if-let* ((buffer (get-buffer ios-simulator-buffer-name)))
      (if (get-buffer-window buffer)
          (delete-window (get-buffer-window buffer))
        (display-buffer buffer))
    (message "Buffer %s does not exist" ios-simulator-buffer-name)))

(defun ios-simulator-remove-control-m (string)
  "Remove ^M characters from STRING."
  (replace-regexp-in-string "\r" "" string))

(defun ios-simulator-get-or-create-buffer-for-simulator (simulator-id simulator-name)
  "Get existing or create buffer for SIMULATOR-ID with SIMULATOR-NAME.
Reuses existing buffer if already created for this simulator."
  (let* ((buffer-name (format "*iOS Simulator - %s*" simulator-name))
         (existing-buffer (get-buffer buffer-name)))
    (puthash simulator-id buffer-name ios-simulator--simulator-buffers)
    (or existing-buffer (get-buffer-create buffer-name))))

(cl-defun ios-simulator-install-and-run-on-additional-simulator (&key rootfolder &key build-folder &key appIdentifier &key terminate-first)
  "Install and run app on an additional simulator.
ROOTFOLDER: Project root directory
BUILD-FOLDER: Path to build artifacts
APPIDENTIFIER: Bundle identifier
TERMINATE-FIRST: Whether to terminate existing app instance"
  (interactive)
  (when ios-simulator-debug
    (message "Installing on additional simulator - root: %s build:%s" rootfolder build-folder))

  ;; Let user choose a different simulator
  (let* ((additional-simulator-id (ios-simulator-choose-simulator))
         (simulator-name (ios-simulator-simulator-name-from :id additional-simulator-id))
         (buffer (ios-simulator-get-or-create-buffer-for-simulator additional-simulator-id simulator-name))
         (applicationName (ios-simulator-get-app-name-fast build-folder)))

    (when ios-simulator-debug
      (message "Installing app: %s for additional simulator: %s (ID: %s)"
               (or applicationName "Unknown")
               (or simulator-name "Unknown")
               (or additional-simulator-id "Unknown")))

    ;; Store simulator info
    (puthash additional-simulator-id
             (list :name simulator-name
                   :app-identifier appIdentifier
                   :app-name applicationName
                   :buffer buffer)
             ios-simulator--active-simulators)

    ;; Setup and boot the additional simulator
    (ios-simulator-setup-simulator-dwim additional-simulator-id)

    ;; Install and launch
    (if terminate-first
        (make-process
         :name "terminate-app-additional"
         :command (list "xcrun" "simctl" "terminate" additional-simulator-id appIdentifier)
         :noquery t
         :sentinel (lambda (proc event)
                     (ios-simulator-install-app
                      :simulatorID additional-simulator-id
                      :build-folder build-folder
                      :appname applicationName
                      :callback (lambda ()
                                  (when ios-simulator-debug
                                    (message "Additional simulator app installation completed"))
                                  (ios-simulator-launch-app
                                   :appIdentifier appIdentifier
                                   :applicationName applicationName
                                   :simulatorName simulator-name
                                   :simulatorID additional-simulator-id
                                   :buffer buffer
                                   :terminate-running terminate-first)))))
      (ios-simulator-install-app
       :simulatorID additional-simulator-id
       :build-folder build-folder
       :appname applicationName
       :callback (lambda ()
                   (when ios-simulator-debug
                     (message "Additional simulator app installation completed"))
                   (ios-simulator-launch-app
                    :appIdentifier appIdentifier
                    :applicationName applicationName
                    :simulatorName simulator-name
                    :simulatorID additional-simulator-id
                    :buffer buffer
                    :terminate-running terminate-first))))

    (message "Installing app on additional simulator: %s" simulator-name)
    additional-simulator-id))

;;;###autoload
(defun ios-simulator-add-target-simulator ()
  "Add a simulator to the target list. App will launch on all target simulators when built."
  (interactive)
  (let* ((additional-simulator-id (ios-simulator-choose-simulator))
         (simulator-name (ios-simulator-simulator-name-from :id additional-simulator-id)))

    ;; Initialize list if needed and ensure primary simulator is included
    (unless ios-simulator--target-simulators
      (setq ios-simulator--target-simulators (list (or current-simulator-id
                                                       (ios-simulator-simulator-identifier)))))

    ;; Add new simulator if not already in list
    (unless (member additional-simulator-id ios-simulator--target-simulators)
      (push additional-simulator-id ios-simulator--target-simulators)
      (message "Added simulator: %s. App will now launch on %d simulator(s)"
               simulator-name
               (length ios-simulator--target-simulators)))

    ;; Store info for management
    (puthash additional-simulator-id
             (list :name simulator-name
                   :app-identifier xcode-project--current-app-identifier
                   :app-name current-app-name)
             ios-simulator--active-simulators)))

;;;###autoload
(defun ios-simulator-remove-target-simulator ()
  "Remove a simulator from the target list."
  (interactive)
  (if (or (not ios-simulator--target-simulators)
          (<= (length ios-simulator--target-simulators) 1))
      (message "Cannot remove - need at least one simulator")
    (let* ((choices (mapcar (lambda (sim-id)
                             (cons (ios-simulator-simulator-name-from :id sim-id) sim-id))
                           ios-simulator--target-simulators))
           (selected-name (completing-read "Remove simulator: " choices nil t))
           (selected-id (cdr (assoc selected-name choices))))

      (setq ios-simulator--target-simulators
            (remove selected-id ios-simulator--target-simulators))
      (remhash selected-id ios-simulator--active-simulators)
      (remhash selected-id ios-simulator--simulator-buffers)

      (message "Removed simulator: %s. App will now launch on %d simulator(s)"
               selected-name
               (length ios-simulator--target-simulators)))))

;;;###autoload
(defun ios-simulator-list-target-simulators ()
  "List all target simulators where app will launch."
  (interactive)
  (if (not ios-simulator--target-simulators)
      (message "No target simulators configured. App will launch on default simulator only.")
    (with-current-buffer (get-buffer-create "*Target Simulators*")
      (erase-buffer)
      (insert (format "App will launch on %d simulator(s):\n\n"
                     (length ios-simulator--target-simulators)))
      (dolist (sim-id ios-simulator--target-simulators)
        (let ((name (ios-simulator-simulator-name-from :id sim-id)))
          (insert (format "- %s\n  ID: %s\n\n" name sim-id))))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ios-simulator-clear-target-simulators ()
  "Clear all target simulators and reset to single simulator mode."
  (interactive)
  (setq ios-simulator--target-simulators nil)
  (clrhash ios-simulator--active-simulators)
  (clrhash ios-simulator--simulator-buffers)
  (message "Cleared all target simulators. App will launch on default simulator only."))

;;;###autoload
(defun ios-simulator-run-on-additional-simulator ()
  "Run current app on an additional simulator while keeping the current one running."
  (interactive)
  (unless current-root-folder-simulator
    (error "No app currently installed. Please run the app first."))
  (unless xcode-project--current-build-folder
    (error "No build folder configured"))
  (unless xcode-project--current-app-identifier
    (error "No app identifier configured"))

  (ios-simulator-install-and-run-on-additional-simulator
   :rootfolder current-root-folder-simulator
   :build-folder xcode-project--current-build-folder
   :appIdentifier xcode-project--current-app-identifier
   :terminate-first nil))

;;;###autoload
(defun ios-simulator-list-active-simulators ()
  "List all active simulators with running apps."
  (interactive)
  (if (hash-table-empty-p ios-simulator--active-simulators)
      (message "No active simulators")
    (with-current-buffer (get-buffer-create "*Active Simulators*")
      (erase-buffer)
      (insert "Active Simulators:\n\n")
      (maphash (lambda (sim-id info)
                 (insert (format "Simulator: %s\n" (plist-get info :name)))
                 (insert (format "  ID: %s\n" sim-id))
                 (insert (format "  App: %s (%s)\n"
                               (plist-get info :app-name)
                               (plist-get info :app-identifier)))
                 (insert (format "  Buffer: %s\n\n"
                               (buffer-name (plist-get info :buffer)))))
               ios-simulator--active-simulators)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ios-simulator-terminate-app-on-simulator (simulator-id)
  "Terminate app on a specific SIMULATOR-ID."
  (interactive
   (list (completing-read "Select simulator: "
                         (let (sims)
                           (maphash (lambda (id info)
                                      (push (cons (plist-get info :name) id) sims))
                                    ios-simulator--active-simulators)
                           sims)
                         nil t)))
  (when-let* ((info (gethash simulator-id ios-simulator--active-simulators))
              (app-id (plist-get info :app-identifier)))
    (call-process-shell-command
     (format "xcrun simctl terminate %s %s"
             (shell-quote-argument simulator-id)
             (shell-quote-argument app-id)))
    (message "Terminated app on simulator: %s" (plist-get info :name))))

;;;###autoload
(defun ios-simulator-shutdown-simulator (simulator-id)
  "Shutdown a specific SIMULATOR-ID and remove from active list."
  (interactive
   (list (completing-read "Select simulator to shutdown: "
                         (let (sims)
                           (maphash (lambda (id info)
                                      (push (cons (plist-get info :name) id) sims))
                                    ios-simulator--active-simulators)
                           sims)
                         nil t)))
  (when-let* ((info (gethash simulator-id ios-simulator--active-simulators)))
    (call-process-shell-command (format "xcrun simctl shutdown %s" (shell-quote-argument simulator-id)))
    (remhash simulator-id ios-simulator--active-simulators)
    (remhash simulator-id ios-simulator--simulator-buffers)
    (message "Shutdown simulator: %s" (plist-get info :name))))

;;;###autoload
(defun ios-simulator-test-two-step-selection ()
  "Test the new two-step selection process."
  (interactive)
  (let ((ios-simulator-debug t))
    (message "Testing two-step simulator selection:")
    (message "Available iOS versions: %s" (string-join (ios-simulator-available-ios-versions) ", "))
    (message "Example: Devices for iOS 17.2:")
    (let ((devices (ios-simulator-devices-for-ios-version "17.2")))
      (dolist (device devices)
        (message "  - %s" (car device))))))

;;;###autoload
(defun ios-simulator-setup-hooks ()
  "Setup hooks for iOS simulator functionality."
  ;; Support both swift-mode and swift-ts-mode
  (add-hook 'swift-mode-hook #'ios-simulator-maybe-preload-cache)
  (add-hook 'swift-ts-mode-hook #'ios-simulator-maybe-preload-cache)
  (when ios-simulator-debug
    (message "iOS Simulator hooks installed for swift-mode and swift-ts-mode")))

;; Auto-setup hooks when this file is loaded
(ios-simulator-setup-hooks)

(defun ios-simulator-reset-selection ()
  "Reset the current simulator selection.
Useful when the saved simulator no longer exists."
  (interactive)
  (setq current-simulator-id nil
        ios-simulator--current-simulator-name nil)
  (message "Simulator selection cleared. You will be prompted to choose a new one on next build."))

;;; ============================================================================
;;; Simulator Control Functions
;;; ============================================================================

(defun ios-simulator-restart ()
  "Restart the Simulator.app completely.
This kills the Simulator app and restarts it with the current simulator."
  (interactive)
  (let ((sim-id (or current-simulator-id (ios-simulator-simulator-identifier))))
    (message "Restarting Simulator.app...")
    ;; Kill Simulator.app
    (call-process "killall" nil nil nil "Simulator")
    ;; Wait a moment for it to fully quit
    (run-with-timer 1.0 nil
                    (lambda ()
                      (when sim-id
                        (ios-simulator-boot-simulator-with-id sim-id))
                      (message "Simulator restarted")))))

(defun ios-simulator-shutdown ()
  "Shutdown the current simulator."
  (interactive)
  (let ((sim-id (or current-simulator-id "booted")))
    (message "Shutting down simulator...")
    (call-process-shell-command (format "xcrun simctl shutdown %s" sim-id))
    (message "Simulator shut down")))

(defun ios-simulator-erase ()
  "Erase all content and settings from the current simulator.
This is like a factory reset - removes all apps, data, and settings."
  (interactive)
  (let ((sim-id (or current-simulator-id (ios-simulator-simulator-identifier)))
        (sim-name (or ios-simulator--current-simulator-name "current simulator")))
    (when (yes-or-no-p (format "Erase ALL content from %s? This cannot be undone. " sim-name))
      (message "Erasing simulator...")
      ;; Must shutdown first
      (call-process-shell-command (format "xcrun simctl shutdown %s 2>/dev/null" sim-id))
      (sit-for 0.5)
      (call-process-shell-command (format "xcrun simctl erase %s" sim-id))
      (message "Simulator erased. Rebooting...")
      (ios-simulator-boot-simulator-with-id sim-id))))

(defun ios-simulator-open-url (url)
  "Open URL in the current simulator's default browser."
  (interactive "sURL to open: ")
  (let ((sim-id (or current-simulator-id "booted")))
    (call-process-shell-command
     (format "xcrun simctl openurl %s '%s'" sim-id url))
    (message "Opened %s in simulator" url)))

(defun ios-simulator-open-url-in-app (url)
  "Open URL in the current app (for deep links/universal links)."
  (interactive "sDeep link URL: ")
  (let ((sim-id (or current-simulator-id "booted")))
    (call-process-shell-command
     (format "xcrun simctl openurl %s '%s'" sim-id url))
    (message "Opened deep link: %s" url)))

;;; ============================================================================
;;; Screenshot and Video Recording
;;; ============================================================================

(defcustom ios-simulator-screenshot-directory "~/Desktop"
  "Default directory for simulator screenshots."
  :type 'directory
  :group 'ios-simulator)

(defcustom ios-simulator-video-directory "~/Desktop"
  "Default directory for simulator video recordings."
  :type 'directory
  :group 'ios-simulator)

(defvar ios-simulator--recording-process nil
  "Current video recording process.")

(defun ios-simulator-screenshot (&optional filename)
  "Take a screenshot of the current simulator.
If FILENAME is nil, generates a timestamped filename on Desktop."
  (interactive)
  (let* ((sim-id (or current-simulator-id "booted"))
         (default-name (format "simulator-%s.png"
                               (format-time-string "%Y%m%d-%H%M%S")))
         (file (or filename
                   (expand-file-name default-name ios-simulator-screenshot-directory))))
    (call-process-shell-command
     (format "xcrun simctl io %s screenshot '%s'" sim-id file))
    (message "Screenshot saved to %s" file)
    file))

(defun ios-simulator-screenshot-to-clipboard ()
  "Take a screenshot and copy it to clipboard."
  (interactive)
  (let* ((temp-file (make-temp-file "sim-screenshot" nil ".png")))
    (ios-simulator-screenshot temp-file)
    (call-process-shell-command
     (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as TIFF picture)'" temp-file))
    (delete-file temp-file)
    (message "Screenshot copied to clipboard")))

(defun ios-simulator-start-recording (&optional filename)
  "Start recording video from the current simulator.
If FILENAME is nil, generates a timestamped filename."
  (interactive)
  (if ios-simulator--recording-process
      (message "Already recording! Use ios-simulator-stop-recording to stop.")
    (let* ((sim-id (or current-simulator-id "booted"))
           (default-name (format "simulator-%s.mp4"
                                 (format-time-string "%Y%m%d-%H%M%S")))
           (file (or filename
                     (expand-file-name default-name ios-simulator-video-directory))))
      (setq ios-simulator--recording-process
            (start-process "simctl-record" nil
                           "xcrun" "simctl" "io" sim-id "recordVideo" file))
      (set-process-query-on-exit-flag ios-simulator--recording-process nil)
      (message "Recording started... Use ios-simulator-stop-recording to stop (saving to %s)" file))))

(defun ios-simulator-stop-recording ()
  "Stop the current video recording."
  (interactive)
  (if ios-simulator--recording-process
      (progn
        (interrupt-process ios-simulator--recording-process)
        (setq ios-simulator--recording-process nil)
        (message "Recording stopped and saved"))
    (message "No recording in progress")))

(defun ios-simulator-toggle-recording ()
  "Toggle video recording on/off."
  (interactive)
  (if ios-simulator--recording-process
      (ios-simulator-stop-recording)
    (ios-simulator-start-recording)))

;;; ============================================================================
;;; Location Simulation
;;; ============================================================================

(defvar ios-simulator-preset-locations
  '(("San Francisco" . (37.7749 . -122.4194))
    ("New York" . (40.7128 . -74.0060))
    ("London" . (51.5074 . -0.1278))
    ("Tokyo" . (35.6762 . 139.6503))
    ("Sydney" . (-33.8688 . 151.2093))
    ("Paris" . (48.8566 . 2.3522))
    ("Berlin" . (52.5200 . 13.4050))
    ("Stockholm" . (59.3293 . 18.0686))
    ("Apple Park" . (37.3349 . -122.0090))
    ("Googleplex" . (37.4220 . -122.0841)))
  "Preset locations for simulator GPS.")

(defun ios-simulator-set-location (latitude longitude)
  "Set simulator GPS location to LATITUDE and LONGITUDE."
  (interactive "nLatitude: \nnLongitude: ")
  (let ((sim-id (or current-simulator-id "booted")))
    (call-process-shell-command
     (format "xcrun simctl location %s set %f,%f" sim-id latitude longitude))
    (message "Location set to %f, %f" latitude longitude)))

(defun ios-simulator-set-location-preset ()
  "Set simulator location from a list of preset locations."
  (interactive)
  (let* ((choices (mapcar #'car ios-simulator-preset-locations))
         (choice (completing-read "Select location: " choices nil t))
         (coords (cdr (assoc choice ios-simulator-preset-locations))))
    (when coords
      (ios-simulator-set-location (car coords) (cdr coords))
      (message "Location set to %s" choice))))

(defun ios-simulator-clear-location ()
  "Clear/reset the simulated location."
  (interactive)
  (let ((sim-id (or current-simulator-id "booted")))
    (call-process-shell-command
     (format "xcrun simctl location %s clear" sim-id))
    (message "Location simulation cleared")))

;;; ============================================================================
;;; Status Bar Customization
;;; ============================================================================

(defun ios-simulator-status-bar-override (&optional time battery wifi cellular)
  "Override status bar appearance for clean screenshots.
TIME: time string (e.g., \"9:41\")
BATTERY: battery level 0-100 or \"charging\"
WIFI: wifi bars 0-3
CELLULAR: cellular bars 0-4"
  (interactive)
  (let* ((sim-id (or current-simulator-id "booted"))
         (time-str (or time "9:41"))
         (battery-level (or battery 100))
         (wifi-bars (or wifi 3))
         (cellular-bars (or cellular 4))
         (cmd (format "xcrun simctl status_bar %s override --time '%s' --batteryLevel %d --wifiBars %d --cellularBars %d"
                      sim-id time-str battery-level wifi-bars cellular-bars)))
    (call-process-shell-command cmd)
    (message "Status bar overridden")))

(defun ios-simulator-status-bar-apple-style ()
  "Set status bar to Apple's marketing style (9:41, full bars)."
  (interactive)
  (ios-simulator-status-bar-override "9:41" 100 3 4))

(defun ios-simulator-status-bar-clear ()
  "Clear status bar overrides and restore normal behavior."
  (interactive)
  (let ((sim-id (or current-simulator-id "booted")))
    (call-process-shell-command
     (format "xcrun simctl status_bar %s clear" sim-id))
    (message "Status bar restored to normal")))

;;; ============================================================================
;;; App Management
;;; ============================================================================

(defun ios-simulator-list-apps ()
  "List all apps installed on the current simulator."
  (interactive)
  (let* ((sim-id (or current-simulator-id "booted"))
         (output (shell-command-to-string
                  (format "xcrun simctl listapps %s 2>/dev/null" sim-id)))
         (buffer (get-buffer-create "*Simulator Apps*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Installed Apps on Simulator\n")
      (insert "===========================\n\n")
      ;; Parse the plist output to show app names and bundle IDs
      (let ((apps '()))
        (with-temp-buffer
          (insert output)
          (goto-char (point-min))
          (while (re-search-forward "CFBundleIdentifier.*?=.*?\"\\([^\"]+\\)\"" nil t)
            (push (match-string 1) apps)))
        (dolist (app (nreverse apps))
          (insert (format "  • %s\n" app)))))
    (display-buffer buffer)
    (message "Found %d apps" (with-current-buffer buffer
                               (count-lines (point-min) (point-max))))))

(defun ios-simulator-uninstall-app (bundle-id)
  "Uninstall app with BUNDLE-ID from the current simulator."
  (interactive
   (list (read-string "Bundle ID to uninstall: "
                      xcode-project--current-app-identifier)))
  (let ((sim-id (or current-simulator-id "booted")))
    (when (yes-or-no-p (format "Uninstall %s? " bundle-id))
      (call-process-shell-command
       (format "xcrun simctl uninstall %s %s" sim-id bundle-id))
      (message "Uninstalled %s" bundle-id))))

(defun ios-simulator-uninstall-current-app ()
  "Uninstall the current project's app from the simulator."
  (interactive)
  (if xcode-project--current-app-identifier
      (ios-simulator-uninstall-app xcode-project--current-app-identifier)
    (message "No current app identifier set")))

(defun ios-simulator-app-container (bundle-id &optional container-type)
  "Get the container path for BUNDLE-ID.
CONTAINER-TYPE can be: app, data, groups, or a specific group identifier."
  (interactive
   (list (read-string "Bundle ID: " xcode-project--current-app-identifier)
         (completing-read "Container type: " '("app" "data" "groups") nil t "data")))
  (let* ((sim-id (or current-simulator-id "booted"))
         (type (or container-type "data"))
         (path (string-trim
                (shell-command-to-string
                 (format "xcrun simctl get_app_container %s %s %s 2>/dev/null"
                         sim-id bundle-id type)))))
    (if (and path (not (string-empty-p path)) (file-exists-p path))
        (progn
          (message "Container path: %s" path)
          (when (called-interactively-p 'any)
            (kill-new path)
            (message "Path copied to clipboard: %s" path))
          path)
      (message "Could not find container for %s" bundle-id)
      nil)))

(defun ios-simulator-open-app-data ()
  "Open the current app's data container in Finder."
  (interactive)
  (when-let ((path (ios-simulator-app-container
                    xcode-project--current-app-identifier "data")))
    (call-process "open" nil nil nil path)))

(defun ios-simulator-open-app-bundle ()
  "Open the current app's bundle container in Finder."
  (interactive)
  (when-let ((path (ios-simulator-app-container
                    xcode-project--current-app-identifier "app")))
    (call-process "open" nil nil nil path)))

;;; ============================================================================
;;; Privacy & Permissions
;;; ============================================================================

(defvar ios-simulator-privacy-services
  '("all" "calendar" "contacts" "location" "location-always"
    "photos" "photos-add" "media-library" "microphone" "camera"
    "reminders" "siri" "speech-recognition" "health" "homekit"
    "motion" "bluetooth" "keychain" "focus")
  "Available privacy services for simctl privacy command.")

(defun ios-simulator-privacy-grant (service &optional bundle-id)
  "Grant privacy permission for SERVICE to BUNDLE-ID."
  (interactive
   (list (completing-read "Service: " ios-simulator-privacy-services nil t)
         (read-string "Bundle ID: " xcode-project--current-app-identifier)))
  (let ((sim-id (or current-simulator-id "booted"))
        (app-id (or bundle-id xcode-project--current-app-identifier)))
    (call-process-shell-command
     (format "xcrun simctl privacy %s grant %s %s" sim-id service app-id))
    (message "Granted %s permission to %s" service app-id)))

(defun ios-simulator-privacy-revoke (service &optional bundle-id)
  "Revoke privacy permission for SERVICE from BUNDLE-ID."
  (interactive
   (list (completing-read "Service: " ios-simulator-privacy-services nil t)
         (read-string "Bundle ID: " xcode-project--current-app-identifier)))
  (let ((sim-id (or current-simulator-id "booted"))
        (app-id (or bundle-id xcode-project--current-app-identifier)))
    (call-process-shell-command
     (format "xcrun simctl privacy %s revoke %s %s" sim-id service app-id))
    (message "Revoked %s permission from %s" service app-id)))

(defun ios-simulator-privacy-reset (service &optional bundle-id)
  "Reset privacy permission for SERVICE (will ask again)."
  (interactive
   (list (completing-read "Service: " ios-simulator-privacy-services nil t)
         (read-string "Bundle ID (empty for all apps): " "")))
  (let ((sim-id (or current-simulator-id "booted")))
    (if (and bundle-id (not (string-empty-p bundle-id)))
        (call-process-shell-command
         (format "xcrun simctl privacy %s reset %s %s" sim-id service bundle-id))
      (call-process-shell-command
       (format "xcrun simctl privacy %s reset %s" sim-id service)))
    (message "Reset %s permission" service)))

(defun ios-simulator-privacy-grant-all ()
  "Grant all privacy permissions to the current app."
  (interactive)
  (ios-simulator-privacy-grant "all" xcode-project--current-app-identifier))

;;; ============================================================================
;;; Clipboard Sync
;;; ============================================================================

(defun ios-simulator-paste-to-simulator (text)
  "Paste TEXT to the simulator's clipboard."
  (interactive "sText to paste to simulator: ")
  (let ((sim-id (or current-simulator-id "booted")))
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max)
                           "xcrun" nil nil nil
                           "simctl" "pbcopy" sim-id))
    (message "Text copied to simulator clipboard")))

(defun ios-simulator-paste-from-kill-ring ()
  "Paste the current kill-ring entry to simulator clipboard."
  (interactive)
  (ios-simulator-paste-to-simulator (current-kill 0)))

(defun ios-simulator-copy-from-simulator ()
  "Copy the simulator's clipboard to Emacs kill-ring."
  (interactive)
  (let* ((sim-id (or current-simulator-id "booted"))
         (text (shell-command-to-string
                (format "xcrun simctl pbpaste %s" sim-id))))
    (kill-new text)
    (message "Copied from simulator: %s" (truncate-string-to-width text 50))))

;;; ============================================================================
;;; Keymap and Menu
;;; ============================================================================

(defvar ios-simulator-command-map
  (let ((map (make-sparse-keymap)))
    ;; Simulator control
    (define-key map (kbd "r") #'ios-simulator-restart)
    (define-key map (kbd "q") #'ios-simulator-shutdown)
    (define-key map (kbd "e") #'ios-simulator-erase)
    (define-key map (kbd "c") #'ios-simulator-choose-simulator)
    ;; Screenshots/Recording
    (define-key map (kbd "s") #'ios-simulator-screenshot)
    (define-key map (kbd "S") #'ios-simulator-screenshot-to-clipboard)
    (define-key map (kbd "v") #'ios-simulator-toggle-recording)
    ;; Location
    (define-key map (kbd "l") #'ios-simulator-set-location-preset)
    (define-key map (kbd "L") #'ios-simulator-clear-location)
    ;; Status bar
    (define-key map (kbd "b") #'ios-simulator-status-bar-apple-style)
    (define-key map (kbd "B") #'ios-simulator-status-bar-clear)
    ;; Apps
    (define-key map (kbd "a") #'ios-simulator-list-apps)
    (define-key map (kbd "u") #'ios-simulator-uninstall-current-app)
    (define-key map (kbd "d") #'ios-simulator-open-app-data)
    ;; URLs
    (define-key map (kbd "o") #'ios-simulator-open-url)
    ;; Privacy
    (define-key map (kbd "p") #'ios-simulator-privacy-grant)
    (define-key map (kbd "P") #'ios-simulator-privacy-revoke)
    ;; Clipboard
    (define-key map (kbd "y") #'ios-simulator-paste-from-kill-ring)
    (define-key map (kbd "Y") #'ios-simulator-copy-from-simulator)
    ;; Push notification
    (define-key map (kbd "n") #'ios-simulator-send-notification)
    map)
  "Keymap for iOS Simulator commands.")

(defun ios-simulator-menu ()
  "Show an interactive menu of simulator commands."
  (interactive)
  (let ((choice (completing-read
                 "Simulator command: "
                 '(("restart - Restart Simulator.app" . ios-simulator-restart)
                   ("shutdown - Shutdown current simulator" . ios-simulator-shutdown)
                   ("erase - Factory reset simulator" . ios-simulator-erase)
                   ("choose - Select a different simulator" . ios-simulator-choose-simulator)
                   ("screenshot - Take screenshot" . ios-simulator-screenshot)
                   ("screenshot-clipboard - Screenshot to clipboard" . ios-simulator-screenshot-to-clipboard)
                   ("record-toggle - Start/stop video recording" . ios-simulator-toggle-recording)
                   ("location - Set GPS location" . ios-simulator-set-location-preset)
                   ("location-clear - Clear GPS simulation" . ios-simulator-clear-location)
                   ("statusbar-clean - Apple marketing style" . ios-simulator-status-bar-apple-style)
                   ("statusbar-clear - Restore normal status bar" . ios-simulator-status-bar-clear)
                   ("apps - List installed apps" . ios-simulator-list-apps)
                   ("uninstall - Uninstall current app" . ios-simulator-uninstall-current-app)
                   ("app-data - Open app data folder" . ios-simulator-open-app-data)
                   ("open-url - Open URL in simulator" . ios-simulator-open-url)
                   ("privacy-grant - Grant permission" . ios-simulator-privacy-grant)
                   ("privacy-revoke - Revoke permission" . ios-simulator-privacy-revoke)
                   ("privacy-grant-all - Grant all permissions" . ios-simulator-privacy-grant-all)
                   ("clipboard-paste - Paste to simulator" . ios-simulator-paste-from-kill-ring)
                   ("clipboard-copy - Copy from simulator" . ios-simulator-copy-from-simulator)
                   ("notification - Send push notification" . ios-simulator-send-notification))
                 nil t)))
    (when choice
      (let ((cmd (cdr (assoc choice
                             '(("restart - Restart Simulator.app" . ios-simulator-restart)
                               ("shutdown - Shutdown current simulator" . ios-simulator-shutdown)
                               ("erase - Factory reset simulator" . ios-simulator-erase)
                               ("choose - Select a different simulator" . ios-simulator-choose-simulator)
                               ("screenshot - Take screenshot" . ios-simulator-screenshot)
                               ("screenshot-clipboard - Screenshot to clipboard" . ios-simulator-screenshot-to-clipboard)
                               ("record-toggle - Start/stop video recording" . ios-simulator-toggle-recording)
                               ("location - Set GPS location" . ios-simulator-set-location-preset)
                               ("location-clear - Clear GPS simulation" . ios-simulator-clear-location)
                               ("statusbar-clean - Apple marketing style" . ios-simulator-status-bar-apple-style)
                               ("statusbar-clear - Restore normal status bar" . ios-simulator-status-bar-clear)
                               ("apps - List installed apps" . ios-simulator-list-apps)
                               ("uninstall - Uninstall current app" . ios-simulator-uninstall-current-app)
                               ("app-data - Open app data folder" . ios-simulator-open-app-data)
                               ("open-url - Open URL in simulator" . ios-simulator-open-url)
                               ("privacy-grant - Grant permission" . ios-simulator-privacy-grant)
                               ("privacy-revoke - Revoke permission" . ios-simulator-privacy-revoke)
                               ("privacy-grant-all - Grant all permissions" . ios-simulator-privacy-grant-all)
                               ("clipboard-paste - Paste to simulator" . ios-simulator-paste-from-kill-ring)
                               ("clipboard-copy - Copy from simulator" . ios-simulator-copy-from-simulator)
                               ("notification - Send push notification" . ios-simulator-send-notification))))))
        (when cmd
          (call-interactively cmd))))))

;;; Transient Menu

(defun ios-simulator--current-status ()
  "Get current simulator status for transient display."
  (let* ((name (or ios-simulator--current-simulator-name "Not selected"))
         (booted (ignore-errors (ios-simulator-get-all-booted-simulators)))
         (booted-names (mapcar #'car booted)))
    (concat
     (format "Selected: %s"
             (propertize name 'face 'font-lock-constant-face))
     " | "
     (if booted
         (format "Booted: %s"
                 (propertize (string-join booted-names ", ") 'face 'success))
       (propertize "No booted simulators" 'face 'font-lock-comment-face)))))

;;;###autoload
(transient-define-prefix ios-simulator-transient ()
  "iOS Simulator actions."
  [:description ios-simulator--current-status]
  ["Selection"
   [("c" "Choose simulator" ios-simulator-choose-simulator)
    ("r" "Reset selection" ios-simulator-reset)
    ("l" "List booted" ios-simulator-list-booted)]]
  ["Control"
   [("b" "Boot" ios-simulator-boot)
    ("s" "Shutdown" ios-simulator-shutdown-simulator)
    ("S" "Shutdown all" ios-simulator-shut-down-all)]
   [("L" "Change language" ios-simulator-change-language)
    ("a" "Terminate app" ios-simulator-terminate-current-app)]]
  ["Screenshots & Recording"
   [("p" "Screenshot" ios-simulator-screenshot)
    ("P" "Screenshot to clipboard" ios-simulator-screenshot-to-clipboard)
    ("v" "Toggle video recording" ios-simulator-toggle-recording)]]
  ["Location"
   [("g" "Set GPS location" ios-simulator-set-location-preset)
    ("G" "Clear GPS" ios-simulator-clear-location)]]
  ["Status Bar"
   [("1" "Apple style" ios-simulator-status-bar-apple-style)
    ("0" "Clear" ios-simulator-status-bar-clear)]]
  ["Apps & Data"
   [("i" "List apps" ios-simulator-list-apps)
    ("u" "Uninstall app" ios-simulator-uninstall-current-app)
    ("d" "App data folder" ios-simulator-open-app-data)
    ("o" "Open URL" ios-simulator-open-url)]]
  ["Privacy"
   [("+" "Grant permission" ios-simulator-privacy-grant)
    ("-" "Revoke permission" ios-simulator-privacy-revoke)
    ("*" "Grant all" ios-simulator-privacy-grant-all)]]
  ["Clipboard & Notifications"
   [("y" "Paste to sim" ios-simulator-paste-from-kill-ring)
    ("Y" "Copy from sim" ios-simulator-copy-from-simulator)
    ("n" "Send notification" ios-simulator-send-notification)]]
  [("q" "Quit" transient-quit-one)])

(provide 'ios-simulator)

;;; ios-simulator.el ends here

