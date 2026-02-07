;;; xcode-archive.el --- Archive, export, and distribute iOS apps to TestFlight -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, xcode, testflight, distribution
;; URL: https://github.com/konrad1977/swift-development

;;; Commentary:

;; This module provides functionality to archive, export, and upload
;; iOS/macOS apps to TestFlight from within Emacs.
;;
;; The distribution pipeline consists of three steps:
;; 1. Archive: xcodebuild archive (creates .xcarchive)
;; 2. Export: xcodebuild -exportArchive (creates .ipa)
;; 3. Upload: xcrun altool --upload-app (uploads to App Store Connect)
;;
;; Authentication uses App Store Connect API keys (.p8 files).
;; Signing uses automatic code signing by default.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'swift-notification nil t)
(require 'swift-project-settings nil t)

;; Forward declarations
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-get-workspace-or-project "xcode-project")
(declare-function xcode-project-scheme "xcode-project")
(declare-function xcode-project-scheme-display-name "xcode-project")
(declare-function xcode-project-is-xcodeproject "xcode-project")
(declare-function swift-project-root "swift-project")

;;; ============================================================================
;;; Customization Variables
;;; ============================================================================

(defgroup xcode-archive nil
  "Settings for archiving, exporting, and distributing iOS apps."
  :group 'programming
  :prefix "xcode-archive-")

(defcustom xcode-archive-development-team nil
  "Apple Developer Team ID (10-character string).
This is required for code signing during archive builds.
Find it at https://developer.apple.com/account under Membership."
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Team ID"))
  :group 'xcode-archive)

(defcustom xcode-archive-export-method 'app-store
  "Export method for the IPA.
- app-store: For TestFlight and App Store distribution
- ad-hoc: For ad-hoc distribution (direct install)
- development: For development devices
- enterprise: For enterprise distribution"
  :type '(choice (const :tag "App Store / TestFlight" app-store)
                 (const :tag "Ad Hoc" ad-hoc)
                 (const :tag "Development" development)
                 (const :tag "Enterprise" enterprise))
  :group 'xcode-archive)

(defcustom xcode-archive-upload-symbols t
  "Whether to upload dSYM symbol files with the build.
Enables symbolicated crash reports in App Store Connect."
  :type 'boolean
  :group 'xcode-archive)

(defcustom xcode-archive-api-key-id nil
  "App Store Connect API Key ID.
Create at https://appstoreconnect.apple.com/access/api"
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Key ID"))
  :group 'xcode-archive)

(defcustom xcode-archive-api-issuer-id nil
  "App Store Connect API Issuer ID (UUID).
Found at https://appstoreconnect.apple.com/access/api"
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Issuer ID"))
  :group 'xcode-archive)

(defcustom xcode-archive-api-key-path nil
  "Path to the App Store Connect API private key (.p8 file).
Downloaded when creating the API key. Can only be downloaded once."
  :type '(choice (const :tag "Not configured" nil)
                 (file :tag "Path to .p8 file"))
  :group 'xcode-archive)

(defcustom xcode-archive-configuration "Release"
  "Build configuration to use for archive builds."
  :type '(choice (const :tag "Release" "Release")
                 (const :tag "Debug" "Debug")
                 (string :tag "Custom configuration"))
  :group 'xcode-archive)

(defcustom xcode-archive-destination "generic/platform=iOS"
  "Build destination for archive builds.
Use generic destinations for archives since they produce universal binaries."
  :type '(choice (const :tag "iOS (generic)" "generic/platform=iOS")
                 (const :tag "macOS" "generic/platform=macOS")
                 (const :tag "tvOS" "generic/platform=tvOS")
                 (const :tag "watchOS" "generic/platform=watchOS")
                 (string :tag "Custom destination"))
  :group 'xcode-archive)

(defcustom xcode-archive-archive-directory ".build/Archives"
  "Directory (relative to project root) for storing .xcarchive bundles."
  :type 'string
  :group 'xcode-archive)

(defcustom xcode-archive-export-directory ".build/Export"
  "Directory (relative to project root) for storing exported IPA files."
  :type 'string
  :group 'xcode-archive)

(defcustom xcode-archive-debug nil
  "Enable debug logging for archive operations."
  :type 'boolean
  :group 'xcode-archive)

;;; ============================================================================
;;; Internal State
;;; ============================================================================

(defvar xcode-archive--current-process nil
  "The currently running archive/export/upload process.")

(defvar xcode-archive--log-buffer-name "*xcode-archive-log*"
  "Name of the log buffer for archive operations.")

(defvar xcode-archive--last-archive-path nil
  "Path to the most recent .xcarchive bundle.")

(defvar xcode-archive--last-ipa-path nil
  "Path to the most recent exported .ipa file.")

(defvar xcode-archive--pipeline-state nil
  "Current state of the distribution pipeline.
Possible values: nil, archiving, exporting, uploading, complete, failed.")

;;; ============================================================================
;;; Notification Helpers
;;; ============================================================================

(defun xcode-archive--notify (message &optional seconds)
  "Send notification with MESSAGE, shown for SECONDS (default 2)."
  (if (fboundp 'swift-notification-send)
      (swift-notification-send :message message :seconds (or seconds 2))
    (message "%s" message)))

(defun xcode-archive--progress-start (title &optional message)
  "Start progress bar with TITLE and optional MESSAGE."
  (if (fboundp 'swift-notification-progress-start)
      (swift-notification-progress-start
       :id 'xcode-archive
       :title title
       :message message
       :icon "nf-dev-xcode"
       :percent 0)
    (message "%s: %s" title (or message ""))))

(defun xcode-archive--progress-update (percent &optional message)
  "Update progress bar to PERCENT with optional MESSAGE."
  (if (fboundp 'swift-notification-progress-update)
      (swift-notification-progress-update
       'xcode-archive
       :percent percent
       :message message)
    (message "[%d%%] %s" percent (or message ""))))

(defun xcode-archive--progress-finish (&optional message)
  "Finish progress bar with optional MESSAGE."
  (if (fboundp 'swift-notification-progress-finish)
      (swift-notification-progress-finish
       'xcode-archive
       (or message "Complete"))
    (message "%s" (or message "Complete"))))

(defun xcode-archive--progress-cancel ()
  "Cancel the progress bar."
  (if (fboundp 'swift-notification-progress-cancel)
      (swift-notification-progress-cancel 'xcode-archive)
    (message "Cancelled")))

(defun xcode-archive--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to the archive log buffer."
  (let ((msg (apply #'format format-string args))
        (buf (get-buffer-create xcode-archive--log-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%H:%M:%S")
                      msg)))
    (when xcode-archive-debug
      (message "[xcode-archive] %s" msg))))

;;; ============================================================================
;;; Configuration & Validation
;;; ============================================================================

(defun xcode-archive--validate-project ()
  "Validate that we are in a valid Xcode project.
Returns project root or signals an error."
  (unless (and (fboundp 'xcode-project-is-xcodeproject)
               (xcode-project-is-xcodeproject))
    (user-error "Not in an Xcode project. Archive requires a .xcodeproj or .xcworkspace"))
  (or (xcode-project-project-root)
      (user-error "Could not determine project root")))

(defun xcode-archive--validate-signing-config ()
  "Validate that signing configuration is sufficient.
Returns t if valid, signals error otherwise."
  (unless xcode-archive-development-team
    (if (y-or-n-p "Development Team ID is not configured. Configure now?")
        (call-interactively #'xcode-archive-set-team-id)
      (user-error "Development Team ID is required for archive builds")))
  t)

(defun xcode-archive--validate-upload-config ()
  "Validate that upload configuration is sufficient.
Returns t if valid, signals error otherwise."
  (unless xcode-archive-api-key-id
    (if (y-or-n-p "API Key ID is not configured. Configure now?")
        (call-interactively #'xcode-archive-set-api-key)
      (user-error "API Key ID is required for TestFlight upload")))
  (unless xcode-archive-api-issuer-id
    (if (y-or-n-p "API Issuer ID is not configured. Configure now?")
        (call-interactively #'xcode-archive-set-api-issuer)
      (user-error "API Issuer ID is required for TestFlight upload")))
  (unless xcode-archive-api-key-path
    (if (y-or-n-p "API key path (.p8 file) is not configured. Configure now?")
        (call-interactively #'xcode-archive-set-api-key-path)
      (user-error "API key path is required for TestFlight upload")))
  (unless (file-exists-p (expand-file-name xcode-archive-api-key-path))
    (user-error "API key file not found: %s" xcode-archive-api-key-path))
  t)

(defun xcode-archive--ensure-directory (dir project-root)
  "Ensure DIR (relative to PROJECT-ROOT) exists. Return absolute path."
  (let ((abs-path (expand-file-name dir project-root)))
    (unless (file-directory-p abs-path)
      (make-directory abs-path t)
      (xcode-archive--log "Created directory: %s" abs-path))
    abs-path))

;;; ============================================================================
;;; Interactive Configuration Commands
;;; ============================================================================

(defun xcode-archive-set-team-id (team-id)
  "Set the Development TEAM-ID for archive builds."
  (interactive "sApple Development Team ID (10 characters): ")
  (when (and (> (length team-id) 0)
             (not (string-match-p "^[A-Z0-9]\\{10\\}$" team-id)))
    (unless (y-or-n-p (format "Team ID '%s' doesn't look like a standard 10-char ID. Use anyway?" team-id))
      (user-error "Cancelled")))
  (setq xcode-archive-development-team team-id)
  (xcode-archive--save-settings)
  (xcode-archive--notify (format "Team ID set to: %s" team-id)))

(defun xcode-archive-set-api-key (key-id)
  "Set the App Store Connect API KEY-ID."
  (interactive "sApp Store Connect API Key ID: ")
  (setq xcode-archive-api-key-id key-id)
  (xcode-archive--save-settings)
  (xcode-archive--notify (format "API Key ID set to: %s" key-id)))

(defun xcode-archive-set-api-issuer (issuer-id)
  "Set the App Store Connect API ISSUER-ID."
  (interactive "sApp Store Connect API Issuer ID: ")
  (setq xcode-archive-api-issuer-id issuer-id)
  (xcode-archive--save-settings)
  (xcode-archive--notify (format "API Issuer ID set to: %s" issuer-id)))

(defun xcode-archive-set-api-key-path (path)
  "Set the PATH to the App Store Connect API private key (.p8 file)."
  (interactive "fPath to API key (.p8 file): ")
  (unless (string-suffix-p ".p8" path)
    (unless (y-or-n-p (format "File '%s' doesn't end in .p8. Use anyway?" (file-name-nondirectory path)))
      (user-error "Cancelled")))
  (setq xcode-archive-api-key-path (expand-file-name path))
  (xcode-archive--save-settings)
  (xcode-archive--notify (format "API key path set to: %s" (file-name-nondirectory path))))

(defun xcode-archive-set-export-method ()
  "Interactively set the export method."
  (interactive)
  (let* ((methods '(("App Store / TestFlight" . app-store)
                    ("Ad Hoc" . ad-hoc)
                    ("Development" . development)
                    ("Enterprise" . enterprise)))
         (choice (completing-read "Export method: " (mapcar #'car methods) nil t)))
    (setq xcode-archive-export-method (cdr (assoc choice methods)))
    (xcode-archive--save-settings)
    (xcode-archive--notify (format "Export method set to: %s" choice))))

(defun xcode-archive-configure ()
  "Interactive configuration wizard for archive and distribution settings."
  (interactive)
  ;; Team ID
  (let ((team (read-string
               (format "Development Team ID%s: "
                       (if xcode-archive-development-team
                           (format " [%s]" xcode-archive-development-team)
                         ""))
               nil nil xcode-archive-development-team)))
    (when (> (length team) 0)
      (setq xcode-archive-development-team team)))

  ;; Export method
  (let* ((methods '(("app-store" . app-store)
                    ("ad-hoc" . ad-hoc)
                    ("development" . development)
                    ("enterprise" . enterprise)))
         (current (symbol-name xcode-archive-export-method))
         (choice (completing-read
                  (format "Export method [%s]: " current)
                  (mapcar #'car methods) nil t nil nil current)))
    (setq xcode-archive-export-method (cdr (assoc choice methods))))

  ;; API Key ID
  (let ((key (read-string
              (format "API Key ID%s: "
                      (if xcode-archive-api-key-id
                          (format " [%s]" xcode-archive-api-key-id)
                        ""))
              nil nil xcode-archive-api-key-id)))
    (when (> (length key) 0)
      (setq xcode-archive-api-key-id key)))

  ;; API Issuer ID
  (let ((issuer (read-string
                 (format "API Issuer ID%s: "
                         (if xcode-archive-api-issuer-id
                             (format " [%s]" xcode-archive-api-issuer-id)
                           ""))
                 nil nil xcode-archive-api-issuer-id)))
    (when (> (length issuer) 0)
      (setq xcode-archive-api-issuer-id issuer)))

  ;; API Key Path
  (let ((path (read-file-name
               (format "API key (.p8 file)%s: "
                       (if xcode-archive-api-key-path
                           (format " [%s]" (file-name-nondirectory xcode-archive-api-key-path))
                         ""))
               nil xcode-archive-api-key-path t)))
    (when (and path (> (length path) 0) (file-exists-p path))
      (setq xcode-archive-api-key-path (expand-file-name path))))

  (xcode-archive--save-settings)
  (xcode-archive--notify "Distribution settings configured")
  (xcode-archive-show-config))

;;; ============================================================================
;;; Settings Persistence
;;; ============================================================================

(defun xcode-archive--save-settings ()
  "Save archive settings to project .swift-development/ directory."
  (when (fboundp 'swift-project-settings-update)
    (when-let* ((root (ignore-errors (xcode-project-project-root))))
      (let ((archive-settings
             (list :archive-team-id xcode-archive-development-team
                   :archive-export-method (symbol-name xcode-archive-export-method)
                   :archive-api-key-id xcode-archive-api-key-id
                   :archive-api-issuer-id xcode-archive-api-issuer-id
                   :archive-api-key-path xcode-archive-api-key-path)))
        (swift-project-settings-update root :archive-settings archive-settings)))))

(defun xcode-archive--load-settings ()
  "Load archive settings from project .swift-development/ directory."
  (when (fboundp 'swift-project-settings-get)
    (when-let* ((root (ignore-errors (xcode-project-project-root)))
                (settings (swift-project-settings-get root :archive-settings)))
      (when-let* ((team (plist-get settings :archive-team-id)))
        (setq xcode-archive-development-team team))
      (when-let* ((method (plist-get settings :archive-export-method)))
        (setq xcode-archive-export-method (intern method)))
      (when-let* ((key (plist-get settings :archive-api-key-id)))
        (setq xcode-archive-api-key-id key))
      (when-let* ((issuer (plist-get settings :archive-api-issuer-id)))
        (setq xcode-archive-api-issuer-id issuer))
      (when-let* ((path (plist-get settings :archive-api-key-path)))
        (setq xcode-archive-api-key-path path))
      (xcode-archive--log "Loaded archive settings from project"))))

(defun xcode-archive-show-config ()
  "Display current archive and distribution configuration."
  (interactive)
  (xcode-archive--load-settings)
  (let ((config-lines
         (list
          (format "%-20s %s" "Team ID:"
                  (if xcode-archive-development-team
                      (propertize xcode-archive-development-team 'face 'success)
                    (propertize "Not configured" 'face 'error)))
          (format "%-20s %s" "Export Method:"
                  (propertize (symbol-name xcode-archive-export-method) 'face 'font-lock-type-face))
          (format "%-20s %s" "Configuration:"
                  (propertize xcode-archive-configuration 'face 'font-lock-type-face))
          (format "%-20s %s" "Destination:"
                  (propertize xcode-archive-destination 'face 'font-lock-type-face))
          (format "%-20s %s" "Upload Symbols:"
                  (if xcode-archive-upload-symbols
                      (propertize "Yes" 'face 'success)
                    (propertize "No" 'face 'warning)))
          ""
          (format "%-20s %s" "API Key ID:"
                  (if xcode-archive-api-key-id
                      (propertize xcode-archive-api-key-id 'face 'success)
                    (propertize "Not configured" 'face 'error)))
          (format "%-20s %s" "API Issuer ID:"
                  (if xcode-archive-api-issuer-id
                      (propertize (substring xcode-archive-api-issuer-id 0
                                             (min 8 (length xcode-archive-api-issuer-id)))
                                  'face 'success)
                    (propertize "Not configured" 'face 'error)))
          (format "%-20s %s" "API Key Path:"
                  (if xcode-archive-api-key-path
                      (propertize (file-name-nondirectory xcode-archive-api-key-path) 'face 'success)
                    (propertize "Not configured" 'face 'error))))))
    (message "Distribution Config:\n%s" (string-join config-lines "\n"))))

;;; ============================================================================
;;; ExportOptions.plist Generation
;;; ============================================================================

(defun xcode-archive--export-method-string ()
  "Get the export method as a string for the plist."
  (pcase xcode-archive-export-method
    ('app-store "app-store")
    ('ad-hoc "ad-hoc")
    ('development "development")
    ('enterprise "enterprise")
    (_ "app-store")))

(defun xcode-archive--generate-export-options-plist (output-path)
  "Generate ExportOptions.plist at OUTPUT-PATH for the current configuration."
  (let ((plist-content
         (concat
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
          "<plist version=\"1.0\">\n"
          "<dict>\n"
          "\t<key>method</key>\n"
          (format "\t<string>%s</string>\n" (xcode-archive--export-method-string))
          (when xcode-archive-development-team
            (concat
             "\t<key>teamID</key>\n"
             (format "\t<string>%s</string>\n" xcode-archive-development-team)))
          "\t<key>signingStyle</key>\n"
          "\t<string>automatic</string>\n"
          "\t<key>uploadSymbols</key>\n"
          (if xcode-archive-upload-symbols
              "\t<true/>\n"
            "\t<false/>\n")
          ;; For app-store exports, set destination to upload
          (when (eq xcode-archive-export-method 'app-store)
            (concat
             "\t<key>destination</key>\n"
             "\t<string>upload</string>\n"))
          ;; Compile bitcode is deprecated since Xcode 14 but keep for compatibility
          "\t<key>compileBitcode</key>\n"
          "\t<false/>\n"
          ;; Strip Swift symbols to reduce binary size
          "\t<key>stripSwiftSymbols</key>\n"
          "\t<true/>\n"
          "</dict>\n"
          "</plist>\n")))
    (with-temp-file output-path
      (insert plist-content))
    (xcode-archive--log "Generated ExportOptions.plist at %s" output-path)
    output-path))

;;; ============================================================================
;;; Archive Command
;;; ============================================================================

(defun xcode-archive--archive-path (project-root)
  "Generate archive path under PROJECT-ROOT with timestamp."
  (let* ((archive-dir (xcode-archive--ensure-directory
                       xcode-archive-archive-directory project-root))
         (scheme-name (xcode-project-scheme-display-name))
         (timestamp (format-time-string "%Y-%m-%d_%H%M%S"))
         (archive-name (format "%s-%s.xcarchive" scheme-name timestamp)))
    (expand-file-name archive-name archive-dir)))

(defun xcode-archive--archive-command (_project-root archive-path)
  "Build the xcodebuild archive command.
_PROJECT-ROOT is the project directory (used for context).
ARCHIVE-PATH is where the .xcarchive will be created."
  (let ((workspace-or-project (xcode-project-get-workspace-or-project))
        (scheme (xcode-project-scheme-display-name)))
    (mapconcat
     #'identity
     (delq nil
           (list
            "xcrun xcodebuild archive"
            (when workspace-or-project
              (format "%s" workspace-or-project))
            (format "-scheme '%s'" scheme)
            (format "-configuration %s" xcode-archive-configuration)
            (format "-destination '%s'" xcode-archive-destination)
            (format "-archivePath '%s'" archive-path)
            ;; Automatic code signing
            "CODE_SIGN_STYLE=Automatic"
            (when xcode-archive-development-team
              (format "DEVELOPMENT_TEAM=%s" xcode-archive-development-team))
            ;; Performance flags
            "-parallelizeTargets"
            (format "-jobs %d" (num-processors))
            ;; Skip unnecessary steps for archive
            "-skipMacroValidation"
            ;; Ensure proper archive settings
            "ONLY_ACTIVE_ARCH=NO"
            "ENABLE_BITCODE=NO"))
     " ")))

(defun xcode-archive-archive-app ()
  "Archive the current Xcode project.
Creates a .xcarchive bundle suitable for export and distribution."
  (interactive)
  (xcode-archive--load-settings)
  (let* ((project-root (xcode-archive--validate-project))
         (_ (xcode-archive--validate-signing-config))
         (archive-path (xcode-archive--archive-path project-root))
         (command (xcode-archive--archive-command project-root archive-path))
         (default-directory project-root))
    (xcode-archive--log "Starting archive: %s" command)
    (setq xcode-archive--pipeline-state 'archiving)
    (xcode-archive--progress-start "Archiving" (xcode-project-scheme-display-name))
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (progn
             (setq xcode-archive--last-archive-path archive-path)
             (setq xcode-archive--pipeline-state nil)
             (xcode-archive--progress-finish
              (format "Archive complete: %s" (file-name-nondirectory archive-path)))
             (xcode-archive--log "Archive succeeded: %s" archive-path))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Archive FAILED")
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter
     (lambda (string)
       (cond
        ((string-match-p "Compiling\\|CompileC\\|SwiftCompile" string)
         (xcode-archive--progress-update 20 "Compiling..."))
        ((string-match-p "Linking\\|Ld " string)
         (xcode-archive--progress-update 50 "Linking..."))
        ((string-match-p "CodeSign\\|Signing" string)
         (xcode-archive--progress-update 70 "Signing..."))
        ((string-match-p "Touch\\|ProcessInfoPlist" string)
         (xcode-archive--progress-update 85 "Finalizing..."))
        ((string-match-p "ARCHIVE SUCCEEDED\\|archive Succeeded" string)
         (xcode-archive--progress-update 95 "Archive complete")))))))

;;; ============================================================================
;;; Export Command
;;; ============================================================================

(defun xcode-archive--export-command (archive-path export-path plist-path)
  "Build the xcodebuild -exportArchive command.
ARCHIVE-PATH is the .xcarchive bundle.
EXPORT-PATH is the output directory for the IPA.
PLIST-PATH is the ExportOptions.plist file."
  (mapconcat
   #'identity
   (list
    "xcrun xcodebuild -exportArchive"
    (format "-archivePath '%s'" archive-path)
    (format "-exportPath '%s'" export-path)
    (format "-exportOptionsPlist '%s'" plist-path))
   " "))

(defun xcode-archive-export-ipa (&optional archive-path)
  "Export an IPA from an .xcarchive bundle.
If ARCHIVE-PATH is nil, uses the last archive created."
  (interactive)
  (xcode-archive--load-settings)
  (let* ((project-root (xcode-archive--validate-project))
         (archive (or archive-path
                      xcode-archive--last-archive-path
                      (read-file-name "Select .xcarchive: " nil nil t nil
                                      (lambda (f) (string-suffix-p ".xcarchive" f)))))
         (export-dir (xcode-archive--ensure-directory
                      xcode-archive-export-directory project-root))
         (plist-path (expand-file-name "ExportOptions.plist" export-dir))
         (_ (xcode-archive--generate-export-options-plist plist-path))
         (command (xcode-archive--export-command archive export-dir plist-path))
         (default-directory project-root))
    (unless (file-exists-p archive)
      (user-error "Archive not found: %s" archive))
    (xcode-archive--log "Starting export: %s" command)
    (setq xcode-archive--pipeline-state 'exporting)
    (xcode-archive--progress-start "Exporting" "Generating IPA...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (let ((ipa-file (xcode-archive--find-ipa export-dir)))
             (setq xcode-archive--last-ipa-path ipa-file)
             (setq xcode-archive--pipeline-state nil)
             (xcode-archive--progress-finish
              (format "Export complete: %s"
                      (if ipa-file (file-name-nondirectory ipa-file) "IPA created")))
             (xcode-archive--log "Export succeeded: %s" (or ipa-file export-dir)))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Export FAILED")
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter
     (lambda (string)
       (cond
        ((string-match-p "Signing" string)
         (xcode-archive--progress-update 40 "Signing..."))
        ((string-match-p "EXPORT SUCCEEDED\\|export Succeeded" string)
         (xcode-archive--progress-update 95 "Export complete")))))))

(defun xcode-archive--find-ipa (directory)
  "Find the first .ipa file in DIRECTORY."
  (car (directory-files directory t "\\.ipa$" t)))

;;; ============================================================================
;;; Upload Command
;;; ============================================================================

(defun xcode-archive--upload-command (ipa-path)
  "Build the xcrun altool upload command for IPA-PATH."
  (mapconcat
   #'identity
   (list
    "xcrun altool --upload-app"
    (format "--file '%s'" ipa-path)
    "--type ios"
    (format "--apiKey '%s'" xcode-archive-api-key-id)
    (format "--apiIssuer '%s'" xcode-archive-api-issuer-id))
   " "))

(defun xcode-archive--validate-command (ipa-path)
  "Build the xcrun altool validate command for IPA-PATH."
  (mapconcat
   #'identity
   (list
    "xcrun altool --validate-app"
    (format "--file '%s'" ipa-path)
    "--type ios"
    (format "--apiKey '%s'" xcode-archive-api-key-id)
    (format "--apiIssuer '%s'" xcode-archive-api-issuer-id))
   " "))

(defun xcode-archive-upload-to-testflight (&optional ipa-path)
  "Upload an IPA to TestFlight via App Store Connect.
If IPA-PATH is nil, uses the last exported IPA."
  (interactive)
  (xcode-archive--load-settings)
  (let* ((project-root (xcode-archive--validate-project))
         (_ (xcode-archive--validate-upload-config))
         (ipa (or ipa-path
                  xcode-archive--last-ipa-path
                  (read-file-name "Select .ipa file: " nil nil t nil
                                  (lambda (f) (string-suffix-p ".ipa" f)))))
         (command (xcode-archive--upload-command ipa))
         (default-directory project-root))
    (unless (file-exists-p ipa)
      (user-error "IPA file not found: %s" ipa))
    ;; Copy API key to expected location if needed
    (xcode-archive--ensure-api-key-accessible)
    (xcode-archive--log "Starting upload: %s" command)
    (setq xcode-archive--pipeline-state 'uploading)
    (xcode-archive--progress-start "Uploading" "Uploading to TestFlight...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (progn
             (setq xcode-archive--pipeline-state 'complete)
             (xcode-archive--progress-finish "Upload to TestFlight complete!")
             (xcode-archive--log "Upload succeeded!")
             (xcode-archive--notify "App uploaded to TestFlight!" 5))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Upload FAILED")
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter
     (lambda (string)
       (cond
        ((string-match-p "Uploading\\|uploading" string)
         (xcode-archive--progress-update 50 "Uploading..."))
        ((string-match-p "verif" string)
         (xcode-archive--progress-update 80 "Verifying...")))))))

(defun xcode-archive-validate-app (&optional ipa-path)
  "Validate an IPA before uploading to App Store Connect.
If IPA-PATH is nil, uses the last exported IPA."
  (interactive)
  (xcode-archive--load-settings)
  (let* ((project-root (xcode-archive--validate-project))
         (_ (xcode-archive--validate-upload-config))
         (ipa (or ipa-path
                  xcode-archive--last-ipa-path
                  (read-file-name "Select .ipa file: " nil nil t nil
                                  (lambda (f) (string-suffix-p ".ipa" f)))))
         (command (xcode-archive--validate-command ipa))
         (default-directory project-root))
    (unless (file-exists-p ipa)
      (user-error "IPA file not found: %s" ipa))
    (xcode-archive--ensure-api-key-accessible)
    (xcode-archive--log "Starting validation: %s" command)
    (xcode-archive--progress-start "Validating" "Validating app...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (progn
             (xcode-archive--progress-finish "Validation passed!")
             (xcode-archive--log "Validation succeeded!")
             (xcode-archive--notify "App validation passed!" 3))
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Validation FAILED")
         (xcode-archive--show-error-buffer output)))
     nil)))

(defun xcode-archive--ensure-api-key-accessible ()
  "Ensure the API key .p8 file is in a location altool can find.
altool looks for API keys in ~/.appstoreconnect/private_keys/
and ~/.private_keys/ and the current directory."
  (when xcode-archive-api-key-path
    (let* ((key-dir (expand-file-name "private_keys"
                                       (expand-file-name ".appstoreconnect" "~")))
           (key-filename (format "AuthKey_%s.p8" xcode-archive-api-key-id))
           (target-path (expand-file-name key-filename key-dir)))
      (unless (file-exists-p target-path)
        (make-directory key-dir t)
        (copy-file (expand-file-name xcode-archive-api-key-path)
                   target-path t)
        (xcode-archive--log "Copied API key to %s" target-path)))))

;;; ============================================================================
;;; Full Distribution Pipeline
;;; ============================================================================

(defun xcode-archive-distribute ()
  "Run the full distribution pipeline: archive, export, upload to TestFlight.
This is the main entry point for distributing an app."
  (interactive)
  (xcode-archive--load-settings)
  (let* ((project-root (xcode-archive--validate-project))
         (_ (xcode-archive--validate-signing-config))
         (_ (xcode-archive--validate-upload-config))
         (archive-path (xcode-archive--archive-path project-root))
         (export-dir (xcode-archive--ensure-directory
                      xcode-archive-export-directory project-root))
         (plist-path (expand-file-name "ExportOptions.plist" export-dir)))

    ;; Generate ExportOptions.plist upfront
    (xcode-archive--generate-export-options-plist plist-path)

    ;; Clear log buffer
    (when-let* ((buf (get-buffer xcode-archive--log-buffer-name)))
      (with-current-buffer buf (erase-buffer)))

    (xcode-archive--log "=== Distribution Pipeline Started ===")
    (xcode-archive--log "Project: %s" project-root)
    (xcode-archive--log "Scheme: %s" (xcode-project-scheme-display-name))
    (xcode-archive--log "Team: %s" xcode-archive-development-team)
    (xcode-archive--log "Export: %s" (xcode-archive--export-method-string))

    ;; Start Step 1: Archive
    (xcode-archive--pipeline-step-archive
     project-root archive-path export-dir plist-path)))

(defun xcode-archive--pipeline-step-archive (project-root archive-path export-dir plist-path)
  "Pipeline step 1: Archive the app.
PROJECT-ROOT, ARCHIVE-PATH, EXPORT-DIR, PLIST-PATH are pipeline context."
  (let ((command (xcode-archive--archive-command project-root archive-path))
        (default-directory project-root))
    (setq xcode-archive--pipeline-state 'archiving)
    (xcode-archive--progress-start "Distributing" "Step 1/3: Archiving...")
    (xcode-archive--log "Step 1/3: Archiving...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (progn
             (setq xcode-archive--last-archive-path archive-path)
             (xcode-archive--log "Step 1/3: Archive complete")
             ;; Continue to Step 2: Export
             (xcode-archive--pipeline-step-export
              project-root archive-path export-dir plist-path))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Pipeline FAILED at archive step")
         (xcode-archive--notify "Distribution failed: Archive error" 5)
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter for archive phase (0-33%)
     (lambda (string)
       (cond
        ((string-match-p "Compiling\\|CompileC\\|SwiftCompile" string)
         (xcode-archive--progress-update 10 "Step 1/3: Compiling..."))
        ((string-match-p "Linking\\|Ld " string)
         (xcode-archive--progress-update 20 "Step 1/3: Linking..."))
        ((string-match-p "CodeSign\\|Signing" string)
         (xcode-archive--progress-update 27 "Step 1/3: Signing..."))
        ((string-match-p "Touch\\|ProcessInfoPlist" string)
         (xcode-archive--progress-update 30 "Step 1/3: Finalizing..."))
        ((string-match-p "ARCHIVE SUCCEEDED\\|archive Succeeded" string)
         (xcode-archive--progress-update 33 "Step 1/3: Complete")))))))

(defun xcode-archive--pipeline-step-export (project-root archive-path export-dir plist-path)
  "Pipeline step 2: Export IPA.
PROJECT-ROOT, ARCHIVE-PATH, EXPORT-DIR, PLIST-PATH are pipeline context."
  (let ((command (xcode-archive--export-command archive-path export-dir plist-path))
        (default-directory project-root))
    (setq xcode-archive--pipeline-state 'exporting)
    (xcode-archive--progress-update 35 "Step 2/3: Exporting IPA...")
    (xcode-archive--log "Step 2/3: Exporting IPA...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (let ((ipa-file (xcode-archive--find-ipa export-dir)))
             (setq xcode-archive--last-ipa-path ipa-file)
             (xcode-archive--log "Step 2/3: Export complete: %s"
                                 (or ipa-file "IPA created"))
             ;; Continue to Step 3: Upload
             (xcode-archive--pipeline-step-upload project-root ipa-file))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Pipeline FAILED at export step")
         (xcode-archive--notify "Distribution failed: Export error" 5)
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter for export phase (33-66%)
     (lambda (string)
       (cond
        ((string-match-p "Signing" string)
         (xcode-archive--progress-update 45 "Step 2/3: Signing..."))
        ((string-match-p "EXPORT SUCCEEDED\\|export Succeeded" string)
         (xcode-archive--progress-update 66 "Step 2/3: Complete")))))))

(defun xcode-archive--pipeline-step-upload (project-root ipa-path)
  "Pipeline step 3: Upload to TestFlight.
PROJECT-ROOT is the project directory.
IPA-PATH is the path to the .ipa file."
  (unless ipa-path
    (setq xcode-archive--pipeline-state 'failed)
    (xcode-archive--progress-cancel)
    (xcode-archive--log "Pipeline FAILED: No IPA file found after export")
    (xcode-archive--notify "Distribution failed: No IPA file found" 5)
    (user-error "No IPA file found in export directory"))
  (xcode-archive--ensure-api-key-accessible)
  (let ((command (xcode-archive--upload-command ipa-path))
        (default-directory project-root))
    (setq xcode-archive--pipeline-state 'uploading)
    (xcode-archive--progress-update 68 "Step 3/3: Uploading to TestFlight...")
    (xcode-archive--log "Step 3/3: Uploading to TestFlight...")
    (xcode-archive--run-async-step
     command
     project-root
     (lambda (success output)
       (if success
           (progn
             (setq xcode-archive--pipeline-state 'complete)
             (xcode-archive--progress-finish "Distributed to TestFlight!")
             (xcode-archive--log "=== Distribution Pipeline Complete ===")
             (xcode-archive--log "Archive: %s" xcode-archive--last-archive-path)
             (xcode-archive--log "IPA: %s" ipa-path)
             (xcode-archive--notify "App distributed to TestFlight!" 5))
         (setq xcode-archive--pipeline-state 'failed)
         (xcode-archive--progress-cancel)
         (xcode-archive--log "Pipeline FAILED at upload step")
         (xcode-archive--notify "Distribution failed: Upload error" 5)
         (xcode-archive--show-error-buffer output)))
     ;; Progress filter for upload phase (66-100%)
     (lambda (string)
       (cond
        ((string-match-p "Uploading\\|uploading" string)
         (xcode-archive--progress-update 80 "Step 3/3: Uploading..."))
        ((string-match-p "verif" string)
         (xcode-archive--progress-update 90 "Step 3/3: Verifying...")))))))

;;; ============================================================================
;;; Async Process Runner
;;; ============================================================================

(defun xcode-archive--run-async-step (command project-root callback &optional filter-fn)
  "Run COMMAND asynchronously from PROJECT-ROOT.
CALLBACK is called with (SUCCESS OUTPUT) when done.
FILTER-FN is an optional function called with each output chunk for progress."
  ;; Kill any existing process
  (when (and xcode-archive--current-process
             (process-live-p xcode-archive--current-process))
    (kill-process xcode-archive--current-process))
  (let* ((log-buffer (get-buffer-create xcode-archive--log-buffer-name))
         (output-buffer (generate-new-buffer " *xcode-archive-output*"))
         (default-directory project-root)
         (process
          (make-process
           :name "xcode-archive"
           :buffer output-buffer
           :command (list shell-file-name shell-command-switch
                         (concat "exec " command " 2>&1"))
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((exit-code (process-exit-status proc))
                      (output (with-current-buffer (process-buffer proc)
                                (buffer-string)))
                      (success (and (zerop exit-code)
                                    (not (string-match-p "\\(?:BUILD FAILED\\|ARCHIVE FAILED\\|EXPORT FAILED\\|error:\\)" output)))))
                 ;; Log full output
                 (with-current-buffer log-buffer
                   (goto-char (point-max))
                   (insert (format "\n--- Process exited with code %d ---\n" exit-code)))
                 ;; Clean up output buffer
                 (kill-buffer output-buffer)
                 (setq xcode-archive--current-process nil)
                 ;; Call the callback
                 (funcall callback success output))))
           :filter
           (lambda (proc string)
             ;; Append to process buffer
             (when (buffer-live-p (process-buffer proc))
               (with-current-buffer (process-buffer proc)
                 (goto-char (point-max))
                 (insert string)))
             ;; Also log to the log buffer
             (when (buffer-live-p log-buffer)
               (with-current-buffer log-buffer
                 (goto-char (point-max))
                 (insert string)))
             ;; Call filter function for progress updates
             (when filter-fn
               (funcall filter-fn string))))))
    (setq xcode-archive--current-process process)
    (xcode-archive--log "Process started: %s" (process-name process))))

(defun xcode-archive--show-error-buffer (output)
  "Show error OUTPUT in a dedicated buffer."
  (let ((buf (get-buffer-create "*xcode-archive-errors*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Archive/Export/Upload Error ===\n\n")
      ;; Extract error lines
      (let ((lines (split-string output "\n")))
        (dolist (line lines)
          (when (or (string-match-p "error:" line)
                    (string-match-p "warning:" line)
                    (string-match-p "FAILED" line)
                    (string-match-p "Error Domain" line)
                    (string-match-p "\\*\\*" line))
            (insert line "\n"))))
      (insert "\n\n=== Full Output ===\n\n")
      (insert output)
      (goto-char (point-min)))
    (display-buffer buf)))

(defun xcode-archive-cancel ()
  "Cancel the current archive/export/upload operation."
  (interactive)
  (when (and xcode-archive--current-process
             (process-live-p xcode-archive--current-process))
    (kill-process xcode-archive--current-process)
    (setq xcode-archive--current-process nil)
    (setq xcode-archive--pipeline-state nil)
    (xcode-archive--progress-cancel)
    (xcode-archive--log "Operation cancelled by user")
    (xcode-archive--notify "Distribution cancelled" 2)))

(defun xcode-archive-show-log ()
  "Show the archive log buffer."
  (interactive)
  (let ((buf (get-buffer-create xcode-archive--log-buffer-name)))
    (display-buffer buf)))

;;; ============================================================================
;;; Transient Menu
;;; ============================================================================

(defun xcode-archive--transient-status ()
  "Status line for the archive transient menu."
  (let ((team (if xcode-archive-development-team
                  (propertize xcode-archive-development-team 'face 'success)
                (propertize "Not set" 'face 'error)))
        (method (propertize (symbol-name xcode-archive-export-method) 'face 'font-lock-type-face))
        (api-key (if xcode-archive-api-key-id
                     (propertize xcode-archive-api-key-id 'face 'success)
                   (propertize "Not set" 'face 'error)))
        (state (pcase xcode-archive--pipeline-state
                 ('archiving (propertize "Archiving..." 'face 'warning))
                 ('exporting (propertize "Exporting..." 'face 'warning))
                 ('uploading (propertize "Uploading..." 'face 'warning))
                 ('complete (propertize "Complete" 'face 'success))
                 ('failed (propertize "Failed" 'face 'error))
                 (_ ""))))
    (concat
     (propertize "Distribution" 'face 'font-lock-keyword-face)
     (format "  Team: %s  Method: %s  API: %s" team method api-key)
     (when (> (length state) 0)
       (format "  [%s]" state)))))

;;;###autoload
(transient-define-prefix xcode-archive-transient ()
  "Distribution - Archive, Export, and Upload to TestFlight."
  [:description xcode-archive--transient-status]
  ["Actions"
   [("d" "Distribute (full pipeline)" xcode-archive-distribute)
    ("a" "Archive only" xcode-archive-archive-app)
    ("e" "Export IPA only" xcode-archive-export-ipa)
    ("u" "Upload to TestFlight" xcode-archive-upload-to-testflight)
    ("v" "Validate app" xcode-archive-validate-app)]]
  ["Configuration"
   [("t" "Set Team ID" xcode-archive-set-team-id)
    ("k" "Set API Key" xcode-archive-set-api-key)
    ("K" "Set API Issuer" xcode-archive-set-api-issuer)
    ("p" "Set API Key path (.p8)" xcode-archive-set-api-key-path)
    ("m" "Set export method" xcode-archive-set-export-method)
    ("c" "Configure all..." xcode-archive-configure)]]
  ["Info & Control"
   [("i" "Show config" xcode-archive-show-config)
    ("l" "Show log" xcode-archive-show-log)
    ("x" "Cancel operation" xcode-archive-cancel)]]
  [("q" "Quit" transient-quit-one)])

(provide 'xcode-archive)
;;; xcode-archive.el ends here
