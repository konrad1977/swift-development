;;; swift-project-settings.el --- Persistent project settings for swift-development -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, xcode, ios, project, persistence

;;; Commentary:

;; This module provides persistent storage for project-specific settings
;; across Emacs sessions. Settings are stored in separate files within
;; the .swift-development/ directory in the project root.
;;
;; Directory Structure (.swift-development/):
;; ├── settings       - Project settings (scheme, device, config, etc.)
;; ├── device-cache   - Cached simulator devices with validation
;; └── file-cache     - File hashes for rebuild detection (managed by swift-development.el)
;;
;; Settings File Format (.swift-development/settings):
;; (:scheme "Bruce (Development)"
;;  :test-scheme "KYCTests"              ; Separate scheme for testing
;;  :device-name "iPhone 15 Pro"
;;  :device-id "ABC-123"
;;  :platform "iOS Simulator"
;;  :build-config "Debug"
;;  :app-identifier "com.example.app"
;;  :project-file "Bruce.xcodeproj"
;;  :build-folder "/path/to/DerivedData/..."
;;  :last-updated "2025-10-25T10:30:00")
;;
;; Device Cache Format (.swift-development/device-cache):
;; (:devices ((...) (...) ...)
;;  :scheme "Bruce (Development)"
;;  :project-file "Bruce.xcodeproj"
;;  :timestamp "2025-10-25T10:30:00")
;;
;; Features:
;; - Save/load project settings (scheme, device, platform, etc.)
;; - Device cache with scheme/project validation
;; - Separate files prevent conflicts between different systems
;; - Emacs lisp format (readable, no JSON dependency)
;; - Automatic directory creation

;;; Code:

(defgroup swift-project-settings nil
  "Persistent project settings for swift-development."
  :group 'swift-development
  :prefix "swift-project-settings-")

(defvar swift-project-settings-debug nil
  "Enable debug logging for project settings.
WARNING: Enabling this may cause Emacs to freeze during file operations.")

;;; Directory and File Access

(defun swift-project-settings--directory (project-root)
  "Get the .swift-development directory path for PROJECT-ROOT."
  (expand-file-name ".swift-development" project-root))

(defun swift-project-settings--ensure-directory (project-root)
  "Ensure .swift-development directory exists for PROJECT-ROOT."
  (let ((dir (swift-project-settings--directory project-root)))
    (unless (file-exists-p dir)
      (make-directory dir t)
      (when swift-project-settings-debug
        (message "[Settings] Created directory: %s" dir)))
    dir))

(defun swift-project-settings--settings-file (project-root &optional scheme)
  "Get the settings file path for PROJECT-ROOT and optionally SCHEME.
If SCHEME is provided, returns path like 'settings-Dev', otherwise 'settings'."
  (let ((filename (if scheme
                      (format "settings-%s" (swift-project-settings--sanitize-scheme-name scheme))
                    "settings")))
    (expand-file-name filename (swift-project-settings--directory project-root))))

(defun swift-project-settings--sanitize-scheme-name (scheme)
  "Sanitize SCHEME name for use in filename.
Removes backslashes and quotes only."
  (let ((clean-name (replace-regexp-in-string "\\\\" "" scheme)))
    (replace-regexp-in-string "['\"]" "" clean-name)))

(defun swift-project-settings--device-cache-file (project-root)
  "Get the device-cache file path for PROJECT-ROOT."
  (expand-file-name "device-cache" (swift-project-settings--directory project-root)))

(defun swift-project-settings--read-file (file-path)
  "Read elisp data from FILE-PATH.
Returns data or nil if file doesn't exist."
  (when (file-exists-p file-path)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (message "[Settings] Error reading %s: %s"
                file-path (error-message-string err))
       nil))))

(defun swift-project-settings--write-file (file-path data)
  "Write DATA to FILE-PATH."
  (condition-case err
      (progn
        ;; Ensure parent directory exists
        (let ((dir (file-name-directory file-path)))
          (unless (file-exists-p dir)
            (make-directory dir t)))
        (with-temp-file file-path
          (prin1 data (current-buffer)))
        (when swift-project-settings-debug
          (message "[Settings] Wrote: %s" file-path)))
    (error
     (message "[Settings] Error writing %s: %s"
              file-path (error-message-string err)))))

;;; Settings Management

(defun swift-project-settings-save (project-root settings &optional scheme)
  "Save SETTINGS to .swift-development/ for PROJECT-ROOT and SCHEME.
SETTINGS should be a plist with keys like :scheme, :device-name, etc.
If SCHEME is provided, saves to 'settings-<scheme>', otherwise 'settings'."
  (swift-project-settings--ensure-directory project-root)

  ;; Use scheme from settings if not explicitly provided
  (let* ((target-scheme (or scheme (plist-get settings :scheme)))
         (settings-with-time (plist-put (copy-sequence settings)
                                        :last-updated
                                        (format-time-string "%Y-%m-%dT%H:%M:%S"))))

    (swift-project-settings--write-file
     (swift-project-settings--settings-file project-root target-scheme)
     settings-with-time)

    (when swift-project-settings-debug
      (message "[Settings] Saved settings for scheme: %s (file: settings-%s)"
               target-scheme
               (swift-project-settings--sanitize-scheme-name target-scheme)))

    settings-with-time))

(defun swift-project-settings-load (project-root &optional scheme)
  "Load settings from .swift-development/ for PROJECT-ROOT and SCHEME.
If SCHEME is provided, loads from 'settings-<scheme>', otherwise 'settings'.
Returns a plist or nil if no settings exist."
  (let ((settings (swift-project-settings--read-file
                   (swift-project-settings--settings-file project-root scheme))))
    (when (and settings swift-project-settings-debug)
      (message "[Settings] Loaded settings for scheme: %s (file: settings-%s)"
               scheme
               (if scheme (swift-project-settings--sanitize-scheme-name scheme) "default")))
    settings))

(defun swift-project-settings-update (project-root key value &optional scheme)
  "Update a single KEY with VALUE in settings for PROJECT-ROOT and SCHEME.
Loads existing settings, updates the key, and saves."
  (let* ((settings (or (swift-project-settings-load project-root scheme)
                       (list)))
         (updated (plist-put settings key value)))
    (swift-project-settings-save project-root updated scheme)))

(defun swift-project-settings-get (project-root key &optional default scheme)
  "Get KEY from settings for PROJECT-ROOT and SCHEME, returning DEFAULT if not found."
  (let ((settings (swift-project-settings-load project-root scheme)))
    (or (plist-get settings key) default)))

(defun swift-project-settings-clear (project-root)
  "Clear settings file in .swift-development/ for PROJECT-ROOT."
  (interactive (list (xcode-project-project-root)))
  (let ((settings-file (swift-project-settings--settings-file project-root)))
    (when (file-exists-p settings-file)
      (delete-file settings-file)
      (message "Cleared settings for project: %s"
               (file-name-nondirectory project-root)))))

(defun swift-project-settings-clear-all-cache (project-root)
  "Clear all cache files (settings, device-cache, file-cache) for PROJECT-ROOT.
Returns the number of files deleted."
  (let ((settings-dir (swift-project-settings--directory project-root))
        (deleted-count 0))
    (when (file-exists-p settings-dir)
      (let ((settings-file (expand-file-name "settings" settings-dir))
            (device-cache-file (expand-file-name "device-cache" settings-dir))
            (file-cache-file (expand-file-name "file-cache" settings-dir)))

        (when (file-exists-p settings-file)
          (delete-file settings-file)
          (setq deleted-count (1+ deleted-count))
          (when swift-project-settings-debug
            (message "[Settings] Deleted settings file")))

        (when (file-exists-p device-cache-file)
          (delete-file device-cache-file)
          (setq deleted-count (1+ deleted-count))
          (when swift-project-settings-debug
            (message "[Settings] Deleted device-cache file")))

        (when (file-exists-p file-cache-file)
          (delete-file file-cache-file)
          (setq deleted-count (1+ deleted-count))
          (when swift-project-settings-debug
            (message "[Settings] Deleted file-cache file")))))

    (when (> deleted-count 0)
      (message "Cleared %d cache file%s from .swift-development/"
               deleted-count (if (> deleted-count 1) "s" "")))
    deleted-count))

;;; Device Cache Management

(defun swift-project-settings-cache-devices (project-root devices scheme project-file)
  "Cache DEVICES to .swift-development/device-cache for PROJECT-ROOT, SCHEME, and PROJECT-FILE."
  (swift-project-settings--ensure-directory project-root)

  (let ((cache (list :devices devices
                     :scheme scheme
                     :project-file project-file
                     :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))))

    (swift-project-settings--write-file
     (swift-project-settings--device-cache-file project-root)
     cache)

    (when swift-project-settings-debug
      (message "[Settings] Cached %d devices for scheme: %s"
               (length devices) scheme))

    cache))

(defun swift-project-settings-get-cached-devices (project-root scheme project-file)
  "Get cached devices for PROJECT-ROOT if valid for SCHEME and PROJECT-FILE.
Returns device list or nil if cache is invalid."
  (let ((cache (swift-project-settings--read-file
                (swift-project-settings--device-cache-file project-root))))
    (when cache
      (let ((cached-scheme (plist-get cache :scheme))
            (cached-project (plist-get cache :project-file))
            (devices (plist-get cache :devices)))

        ;; Validate cache against current scheme and project
        (if (and (equal scheme cached-scheme)
                 (equal project-file cached-project))
            (progn
              (when swift-project-settings-debug
                (message "[Settings] Using cached devices (%d simulators)"
                         (length devices)))
              devices)
          (when swift-project-settings-debug
            (message "[Settings] Device cache invalid (scheme or project changed)"))
          nil)))))

(defun swift-project-settings-invalidate-device-cache (project-root)
  "Invalidate device cache by deleting .swift-development/device-cache for PROJECT-ROOT."
  (let ((cache-file (swift-project-settings--device-cache-file project-root)))
    (when (file-exists-p cache-file)
      (delete-file cache-file)
      (when swift-project-settings-debug
        (message "[Settings] Invalidated device cache")))))

;;; Integration Helpers

(defun swift-project-settings-restore-to-variables (project-root)
  "Restore settings from .swift-development to Emacs variables for PROJECT-ROOT.
This is called when opening a project to restore previous selections."
  (let* ((base-settings (swift-project-settings-load project-root))
         (last-scheme (when base-settings (plist-get base-settings :last-scheme)))
         (settings (if last-scheme
                      (swift-project-settings-load-for-scheme project-root last-scheme)
                    base-settings)))
    (when swift-project-settings-debug
      (message "[Settings] Attempting to restore settings from: %s"
               (swift-project-settings--settings-file project-root))
      (when last-scheme
        (message "[Settings] Found last-scheme: %s" last-scheme)))
    (if settings
        (progn
          (when swift-project-settings-debug
            (message "[Settings] Found settings: %S" settings))

          ;; Restore to xcode-project variables
          (when-let* ((scheme (or (plist-get settings :scheme) last-scheme)))
            (setq xcode-project--current-xcode-scheme scheme)
            (when swift-project-settings-debug
              (message "[Settings] Restored scheme: %s" scheme)))

          (when-let* ((build-config (plist-get settings :build-config)))
            (setq xcode-project--current-build-configuration build-config)
            (when swift-project-settings-debug
              (message "[Settings] Restored build-config: %s" build-config)))

          (when-let* ((app-id (plist-get settings :app-identifier)))
            (setq xcode-project--current-app-identifier app-id)
            (when swift-project-settings-debug
              (message "[Settings] Restored app-identifier: %s" app-id)))

          ;; NOTE: We intentionally do NOT restore build-folder from settings.
          ;; The build folder depends on the device type (simulator vs device)
          ;; and should be auto-detected by xcode-project-build-folder based on
          ;; the current device-type. Restoring a stale folder causes issues
          ;; when switching between simulator and physical device.

          ;; Restore device choice (physical device vs simulator)
          (when (boundp 'swift-development--device-choice)
            (when-let* ((platform (plist-get settings :platform)))
              (setq swift-development--device-choice
                    (string= platform "Physical Device"))
              (when swift-project-settings-debug
                (message "[Settings] Restored platform: %s (device-choice=%s)"
                         platform swift-development--device-choice))))

          ;; Restore simulator selection (if variables exist)
          (when (boundp 'ios-simulator--current-simulator-name)
            (when-let* ((device-name (plist-get settings :device-name)))
              (setq ios-simulator--current-simulator-name device-name)
              (when swift-project-settings-debug
                (message "[Settings] Restored device-name: %s" device-name))))

          (when (boundp 'current-simulator-id)
            (when-let* ((device-id (plist-get settings :device-id)))
              ;; Validate that the device still exists before restoring
              (if (and (fboundp 'ios-simulator-device-exists-p)
                       (ios-simulator-device-exists-p device-id))
                  (progn
                    (setq current-simulator-id device-id)
                    (when swift-project-settings-debug
                      (message "[Settings] Restored device-id: %s" device-id)))
                (progn
                  (when swift-project-settings-debug
                    (message "[Settings] Saved device-id %s no longer exists, skipping restore" device-id))
                  (setq current-simulator-id nil)))))

          ;; Return settings for further use
          settings)
      (when swift-project-settings-debug
        (message "[Settings] No settings found to restore"))
      nil)))

(defun swift-project-settings-load-for-scheme (project-root scheme)
  "Load and apply settings for SCHEME in PROJECT-ROOT.
This is called when a scheme is selected to load scheme-specific settings.
IMPORTANT: Clears build-related variables first to prevent leaking between schemes."
  (when scheme
    ;; CRITICAL: Clear build-related variables when switching schemes
    ;; This prevents values from one scheme leaking into another
    ;; By clearing these, we force fresh lookups which will use the correct
    ;; scheme-specific cache entries (cache keys include scheme name)
    (setq xcode-project--current-build-configuration nil
          xcode-project--current-app-identifier nil
          xcode-project--current-build-folder nil
          xcode-project--last-device-type nil)  ; Force rebuild folder detection

    (let ((settings (swift-project-settings-load project-root scheme)))
      (when swift-project-settings-debug
        (message "[Settings] Loading settings for scheme: %s (found: %s)" scheme (not (null settings))))

      ;; Apply settings to buffer-local variables if they exist
      (when settings
        (when-let* ((build-config (plist-get settings :build-config)))
          (setq xcode-project--current-build-configuration build-config))

        (when-let* ((app-id (plist-get settings :app-identifier)))
          (setq xcode-project--current-app-identifier app-id))

        ;; NOTE: Do NOT restore build-folder - let it be auto-detected based on device type

        ;; Restore device choice (physical device vs simulator)
        (when (boundp 'swift-development--device-choice)
          (when-let* ((platform (plist-get settings :platform)))
            (setq swift-development--device-choice
                  (string= platform "Physical Device"))))

        ;; Only restore device settings if user hasn't already selected a simulator
        ;; This prevents overwriting manual simulator selection after reset
        (unless (and (boundp 'current-simulator-id) current-simulator-id)
          (when (boundp 'ios-simulator--current-simulator-name)
            (when-let* ((device-name (plist-get settings :device-name)))
              (setq ios-simulator--current-simulator-name device-name)))

          (when (boundp 'current-simulator-id)
            (when-let* ((device-id (plist-get settings :device-id)))
              ;; Validate that the device still exists before restoring
              (if (and (fboundp 'ios-simulator-device-exists-p)
                       (ios-simulator-device-exists-p device-id))
                  (setq current-simulator-id device-id)
                (progn
                  (when swift-project-settings-debug
                    (message "[Settings] Saved device-id %s no longer exists, skipping restore" device-id))
                  (setq current-simulator-id nil)))))))

      ;; Always try to load build-config from scheme file if not set
      ;; This handles both: settings exist but lack build-config, or no settings at all
      (when (not xcode-project--current-build-configuration)
        (when swift-project-settings-debug
          (message "[Settings] Build config not set, fetching from scheme file for: %s" scheme))
        (let* ((xcodeproj-dirs (directory-files project-root t "\\.xcodeproj$"))
               (xcodeproj-dir (car xcodeproj-dirs))
               (scheme-file (when xcodeproj-dir
                              (format "%s/xcshareddata/xcschemes/%s.xcscheme"
                                      xcodeproj-dir scheme))))
          (when (and scheme-file (file-exists-p scheme-file))
            (with-temp-buffer
              (insert-file-contents scheme-file)
              (goto-char (point-min))
              (if (re-search-forward "<LaunchAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                  (setq xcode-project--current-build-configuration (match-string 1))
                (goto-char (point-min))
                (if (re-search-forward "<TestAction[^>]*buildConfiguration\\s-*=\\s-*\"\\([^\"]+\\)\"" nil t)
                    (setq xcode-project--current-build-configuration (match-string 1))
                  (setq xcode-project--current-build-configuration "Debug"))))
            (when swift-project-settings-debug
              (message "[Settings] Fetched build config from scheme file: %s" xcode-project--current-build-configuration)))))

      settings)))

(defun swift-project-settings-capture-from-variables (project-root)
  "Capture current Emacs variables and save as settings for PROJECT-ROOT.
Only updates fields that have non-nil values, preserving existing saved values.
Saves to scheme-specific file if a scheme is set.
Also saves the current scheme as 'last-scheme' in the base settings file.
IMPORTANT: Device-specific settings are ONLY saved to scheme-specific files."
  ;; Get current scheme from buffer-local variable
  (let* ((current-scheme xcode-project--current-xcode-scheme)
         ;; Load existing settings for this scheme
         (settings (or (swift-project-settings-load project-root current-scheme) (list))))

    ;; Update only non-nil values
    (when current-scheme
      (setq settings (plist-put settings :scheme current-scheme)))

    ;; Device-specific settings - ONLY save if we have a scheme
    ;; This prevents device info from being saved to the base settings file
    (when current-scheme
      (when (and (boundp 'ios-simulator--current-simulator-name)
                 ios-simulator--current-simulator-name)
        (setq settings (plist-put settings :device-name ios-simulator--current-simulator-name)))

      (when (and (boundp 'current-simulator-id)
                 current-simulator-id)
        (setq settings (plist-put settings :device-id current-simulator-id)))

      ;; Always save platform - not just when device-choice is t
      (setq settings (plist-put settings :platform
                                (if swift-development--device-choice
                                    "Physical Device"
                                  "iOS Simulator")))

      (when xcode-project--current-build-configuration
        (setq settings (plist-put settings :build-config xcode-project--current-build-configuration)))

      (when xcode-project--current-app-identifier
        (setq settings (plist-put settings :app-identifier xcode-project--current-app-identifier)))

      ;; NOTE: We intentionally do NOT save build-folder to settings.
      ;; Build folder is device-type dependent (iphoneos vs iphonesimulator)
      ;; and should be auto-detected by xcode-project-build-folder.
      )

    ;; Project file can be saved regardless of scheme
    (let ((project-file (swift-project-settings--get-project-file project-root)))
      (when project-file
        (setq settings (plist-put settings :project-file project-file))))

    (when swift-project-settings-debug
      (message "[Settings] Capturing settings for scheme '%s': %S" current-scheme settings))

    ;; Only save if we have a scheme - don't pollute base settings with device info
    (when current-scheme
      (swift-project-settings-save project-root settings current-scheme)

      ;; Also save the current scheme as "last-scheme" in base settings file
      ;; This allows us to remember which scheme was used last
      ;; IMPORTANT: Only save :last-scheme and :project-file to base, NOT device info
      (let ((base-settings (or (swift-project-settings-load project-root nil) (list))))
        ;; Remove device-related keys from base settings to keep it clean
        (setq base-settings (plist-put base-settings :last-scheme current-scheme))
        (setq base-settings (map-delete base-settings :device-name))
        (setq base-settings (map-delete base-settings :device-id))
        (setq base-settings (map-delete base-settings :platform))
        (setq base-settings (map-delete base-settings :build-config))
        (setq base-settings (map-delete base-settings :app-identifier))
        (setq base-settings (map-delete base-settings :build-folder))
        (let ((project-file (swift-project-settings--get-project-file project-root)))
          (when project-file
            (setq base-settings (plist-put base-settings :project-file project-file))))
        (swift-project-settings-save project-root base-settings nil)
        (when swift-project-settings-debug
          (message "[Settings] Saved last-scheme: %s (cleaned base settings)" current-scheme))))))

;;; Test Scheme Management

(defun swift-project-settings-test-scheme (project-root)
  "Get the saved test scheme for PROJECT-ROOT.
Returns nil if no test scheme has been saved."
  (let ((settings (swift-project-settings-load project-root nil)))
    (plist-get settings :test-scheme)))

(defun swift-project-settings-set-test-scheme (project-root scheme)
  "Set SCHEME as the test scheme for PROJECT-ROOT.
This is saved to the base settings file (not scheme-specific)."
  (let ((settings (or (swift-project-settings-load project-root nil) (list))))
    (setq settings (plist-put settings :test-scheme scheme))
    (swift-project-settings-save project-root settings nil)
    (when swift-project-settings-debug
      (message "[Settings] Saved test-scheme: %s" scheme))
    scheme))

;;; Diagnostics

(defun swift-project-settings-show-diagnostics ()
  "Show diagnostics for current project settings."
  (interactive)
  (let* ((project-root (xcode-project-project-root))
         (dir (swift-project-settings--directory project-root))
         (settings-file (swift-project-settings--settings-file project-root))
         (device-cache-file (swift-project-settings--device-cache-file project-root))
         (base-settings (swift-project-settings-load project-root))
         (last-scheme (when base-settings (plist-get base-settings :last-scheme)))
         (scheme-settings (when last-scheme
                           (swift-project-settings-load-for-scheme project-root last-scheme)))
         (settings (or scheme-settings base-settings))
         (device-cache (swift-project-settings--read-file device-cache-file)))

    (with-current-buffer (get-buffer-create "*Swift Project Settings*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Swift Project Settings Diagnostics ===\n\n")

        (insert (format "Project Root: %s\n" project-root))
        (insert (format "Settings Directory: %s\n" dir))
        (insert (format "Directory Exists: %s\n\n"
                       (if (file-exists-p dir) "YES" "NO")))

        (insert "=== Settings File ===\n")
        (insert (format "Path: %s\n" settings-file))
        (insert (format "Exists: %s\n" (if (file-exists-p settings-file) "YES" "NO")))
        (when last-scheme
          (insert (format "Last Scheme: %s\n" last-scheme))
          (insert (format "Scheme Settings File: %s\n"
                         (swift-project-settings--settings-file project-root last-scheme))))

        ;; Show test-scheme from base settings (separate from build scheme)
        (let ((test-scheme (plist-get base-settings :test-scheme)))
          (insert (format "\nTest Scheme: %s\n" (or test-scheme "(not set)"))))

        (if settings
            (progn
              (insert "\n=== Current Build Settings ===\n")
              (insert (format "Build Scheme: %s\n" (or (plist-get settings :scheme) last-scheme)))
              (insert (format "Device Name: %s\n" (plist-get settings :device-name)))
              (insert (format "Device ID: %s\n" (plist-get settings :device-id)))
              (insert (format "Platform: %s\n" (plist-get settings :platform)))
              (insert (format "Build Config: %s\n" (plist-get settings :build-config)))
              (insert (format "App Identifier: %s\n" (plist-get settings :app-identifier)))
              (insert (format "Project File: %s\n" (plist-get settings :project-file)))
              (insert (format "Last Updated: %s\n" (plist-get settings :last-updated))))
          (insert "\nNo settings loaded.\n"))

        (insert "\n=== Device Cache File ===\n")
        (insert (format "Path: %s\n" device-cache-file))
        (insert (format "Exists: %s\n" (if (file-exists-p device-cache-file) "YES" "NO")))

        (if device-cache
            (progn
              (insert "\n=== Device Cache ===\n")
              (insert (format "Scheme: %s\n" (plist-get device-cache :scheme)))
              (insert (format "Project File: %s\n" (plist-get device-cache :project-file)))
              (insert (format "Cached Devices: %d\n"
                             (length (plist-get device-cache :devices))))
              (insert (format "Timestamp: %s\n" (plist-get device-cache :timestamp))))
          (insert "\nNo device cache loaded.\n"))

        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

;;; Utility Functions

(defun swift-project-settings--get-project-file (project-root)
  "Get the .xcodeproj file for PROJECT-ROOT.
Returns relative filename or nil."
  (let ((xcodeproj-files (directory-files project-root nil "\\.xcodeproj$")))
    (car xcodeproj-files)))

;;; Build Info Fetching (from xcodebuild)

(defvar swift-project-settings--fetch-in-progress nil
  "Hash table tracking in-progress build info fetches per scheme.")

(cl-defun swift-project-settings-fetch-build-info (project-root scheme sdk callback)
  "Fetch build info from xcodebuild for PROJECT-ROOT, SCHEME and SDK.
SDK should be \"iphonesimulator\" or \"iphoneos\".
Calls CALLBACK with the updated settings plist when done.
Extracts: TARGET_BUILD_DIR, PRODUCT_NAME, PRODUCT_BUNDLE_IDENTIFIER.

This is the ONLY source of truth for build paths. No guessing."
  (unless swift-project-settings--fetch-in-progress
    (setq swift-project-settings--fetch-in-progress (make-hash-table :test 'equal)))
  
  (let ((fetch-key (format "%s-%s-%s" project-root scheme sdk)))
    ;; Prevent duplicate fetches
    (when (gethash fetch-key swift-project-settings--fetch-in-progress)
      (when swift-project-settings-debug
        (message "[Settings] Fetch already in progress for %s" scheme))
      (cl-return-from swift-project-settings-fetch-build-info nil))
    
    (puthash fetch-key t swift-project-settings--fetch-in-progress)
    
    (when swift-project-settings-debug
      (message "[Settings] Fetching build info for scheme: %s sdk: %s" scheme sdk))
    
    ;; Build xcodebuild command directly - NO CACHE, always fresh
    ;; IMPORTANT: Use -derivedDataPath .build to match actual build location
    (let* ((workspace-or-project (when (fboundp 'xcode-project-get-workspace-or-project)
                                   (xcode-project-get-workspace-or-project)))
           (command (format "xcrun xcodebuild %s -scheme %s -showBuildSettings -sdk %s -derivedDataPath .build -json 2>/dev/null"
                            (or workspace-or-project "")
                            (shell-quote-argument scheme)
                            sdk))
           (output-buffer (generate-new-buffer " *build-settings-fetch*")))
      
      (when swift-project-settings-debug
        (message "[Settings] Running: %s" command))
      
      (make-process
       :name "swift-settings-fetch"
       :buffer output-buffer
       :command (list shell-file-name shell-command-switch command)
       :sentinel
       (lambda (process _event)
         ;; Clear in-progress flag
         (remhash fetch-key swift-project-settings--fetch-in-progress)
         
         (when (eq (process-status process) 'exit)
           (if (= (process-exit-status process) 0)
               (let ((json-data nil))
                 (with-current-buffer output-buffer
                   (goto-char (point-min))
                   (condition-case nil
                       (setq json-data (json-read))
                     (error nil)))
                 (kill-buffer output-buffer)
                 
                 (if (and json-data (> (length json-data) 0))
                     (let* ((build-settings (let-alist (seq-elt json-data 0) .buildSettings))
                            (target-build-dir (alist-get 'BUILT_PRODUCTS_DIR build-settings))
                            (product-name (alist-get 'PRODUCT_NAME build-settings))
                            (bundle-id (alist-get 'PRODUCT_BUNDLE_IDENTIFIER build-settings))
                            (build-config (alist-get 'CONFIGURATION build-settings))
                            (app-path (when (and target-build-dir product-name)
                                        (expand-file-name 
                                         (concat product-name ".app")
                                         target-build-dir)))
                            ;; Load existing settings and merge
                            (existing (swift-project-settings-load project-root scheme))
                            (updated (copy-sequence (or existing (list)))))
                       
                       ;; Update with new build info
                       (setq updated (plist-put updated :scheme scheme))
                       (setq updated (plist-put updated :sdk sdk))
                       (when target-build-dir
                         (setq updated (plist-put updated :target-build-dir 
                                                  (file-name-as-directory target-build-dir))))
                       (when product-name
                         (setq updated (plist-put updated :product-name product-name)))
                       (when bundle-id
                         (setq updated (plist-put updated :bundle-id bundle-id)))
                       (when build-config
                         (setq updated (plist-put updated :build-config build-config)))
                       (when app-path
                         (setq updated (plist-put updated :app-path app-path)))
                       
                       ;; Save to disk
                       (swift-project-settings-save project-root updated scheme)
                       
                       (when swift-project-settings-debug
                         (message "[Settings] Saved: config=%s dir=%s" build-config target-build-dir))
                       
                       ;; Call callback with updated settings
                       (when callback
                         (funcall callback updated)))
                   
                    ;; JSON was empty/nil
                    (when swift-project-settings-debug
                      (message "[Settings] xcodebuild returned no data"))
                    (when callback
                      (funcall callback nil))))
              ;; Process failed
              (kill-buffer output-buffer)
              (when swift-project-settings-debug
                (message "[Settings] xcodebuild failed"))
              (when callback
                (funcall callback nil)))))))))

(defun swift-project-settings-get-build-dir (project-root scheme sdk)
  "Get cached build directory for PROJECT-ROOT, SCHEME and SDK.
Returns nil if not cached - use `swift-project-settings-fetch-build-info' to fetch."
  (let ((settings (swift-project-settings-load project-root scheme)))
    (when (and settings (equal sdk (plist-get settings :sdk)))
      (plist-get settings :target-build-dir))))

(defun swift-project-settings-get-app-path (project-root scheme sdk)
  "Get cached app path for PROJECT-ROOT, SCHEME and SDK.
Returns nil if not cached."
  (let ((settings (swift-project-settings-load project-root scheme)))
    (when (and settings (equal sdk (plist-get settings :sdk)))
      (plist-get settings :app-path))))

(defun swift-project-settings-get-bundle-id (project-root scheme)
  "Get cached bundle identifier for PROJECT-ROOT and SCHEME."
  (let ((settings (swift-project-settings-load project-root scheme)))
    (plist-get settings :bundle-id)))

(defun swift-project-settings-get-product-name (project-root scheme)
  "Get cached product name for PROJECT-ROOT and SCHEME."
  (let ((settings (swift-project-settings-load project-root scheme)))
    (plist-get settings :product-name)))

(provide 'swift-project-settings)
;;; swift-project-settings.el ends here
