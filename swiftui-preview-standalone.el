;;; swiftui-preview-standalone.el --- Standalone Swift file preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: Swift, swiftui, preview, ios

;;; Commentary:

;; This module provides SwiftUI preview functionality for standalone Swift files.
;; It works without requiring a full Xcode project - just a single .swift file
;; with only system framework imports (SwiftUI, UIKit, Foundation, etc).
;;
;; Usage:
;; 1. Open a standalone Swift file with a #Preview block
;; 2. M-x swiftui-preview-standalone-generate
;;
;; Requirements:
;; - Xcode command line tools
;; - A booted iOS Simulator
;; - Swift file with only system imports

;;; Code:

(require 'cl-lib)
(require 'json)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--get-booted-simulator "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")
(declare-function swiftui-preview-capture-simulator "swiftui-preview")
(declare-function ios-simulator-simulator-identifier "ios-simulator")

(defgroup swiftui-preview-standalone nil
  "Standalone Swift file preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-standalone-")

(defcustom swiftui-preview-standalone-simulator "iPhone 16 Pro"
  "Default simulator name for standalone previews."
  :type 'string
  :group 'swiftui-preview-standalone)

(defcustom swiftui-preview-standalone-ios-version "18.0"
  "iOS deployment target for standalone builds."
  :type 'string
  :group 'swiftui-preview-standalone)

(defcustom swiftui-preview-standalone-capture-delay 3.0
  "Delay in seconds after launching app before capturing screenshot."
  :type 'number
  :group 'swiftui-preview-standalone)

(defcustom swiftui-preview-standalone-keep-temp nil
  "If non-nil, keep temporary build files for debugging."
  :type 'boolean
  :group 'swiftui-preview-standalone)

(defvar swiftui-preview-standalone--system-frameworks
  '("SwiftUI" "UIKit" "Foundation" "Combine" "CoreGraphics" "CoreAnimation"
    "QuartzCore" "AVFoundation" "MapKit" "CoreLocation" "CoreData" "CloudKit"
    "StoreKit" "GameKit" "SpriteKit" "SceneKit" "ARKit" "RealityKit" "Metal"
    "MetalKit" "Vision" "CoreML" "NaturalLanguage" "Speech" "Intents"
    "UserNotifications" "WidgetKit" "AppIntents" "Charts" "Swift" "os" "Darwin"
    "Dispatch" "ObjectiveC" "Accelerate" "simd" "Security" "CryptoKit"
    "LocalAuthentication" "AuthenticationServices" "SafariServices" "WebKit"
    "PhotosUI" "Photos" "Contacts" "ContactsUI" "EventKit" "EventKitUI"
    "Messages" "MessageUI" "HealthKit" "CoreMotion" "CoreBluetooth"
    "MultipeerConnectivity" "Network" "NetworkExtension" "CallKit" "PushKit"
    "CoreTelephony" "SystemConfiguration" "MobileCoreServices"
    "UniformTypeIdentifiers" "LinkPresentation" "QuickLook" "QuickLookThumbnailing"
    "Observation" "SwiftData")
  "List of system frameworks that can be used in standalone previews.")

(defun swiftui-preview-standalone--extract-imports ()
  "Extract all import statements from current buffer.
Returns a list of imported module names."
  (save-excursion
    (goto-char (point-min))
    (let ((imports '()))
      (while (re-search-forward "^import[[:space:]]+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
        (push (match-string 1) imports))
      (delete-dups (nreverse imports)))))

(defun swiftui-preview-standalone--has-only-system-imports-p ()
  "Check if current buffer only uses system framework imports.
Returns t if all imports are system frameworks, nil otherwise."
  (let ((imports (swiftui-preview-standalone--extract-imports)))
    (cl-every (lambda (imp)
                (member imp swiftui-preview-standalone--system-frameworks))
              imports)))

(defun swiftui-preview-standalone--non-system-imports ()
  "Get list of non-system imports in current buffer."
  (let ((imports (swiftui-preview-standalone--extract-imports)))
    (cl-remove-if (lambda (imp)
                    (member imp swiftui-preview-standalone--system-frameworks))
                  imports)))

(defun swiftui-preview-standalone--find-simulator-udid (name)
  "Find simulator UDID by NAME."
  (let ((output (shell-command-to-string
                 "xcrun simctl list devices available -j 2>/dev/null")))
    (condition-case nil
        (let* ((json (json-read-from-string output))
               (devices (alist-get 'devices json)))
          (catch 'found
            (dolist (runtime-devices devices)
              (when (string-match-p "iOS" (symbol-name (car runtime-devices)))
                (dolist (device (cdr runtime-devices))
                  (when (and (equal (alist-get 'name device) name)
                             (eq (alist-get 'isAvailable device) t))
                    (throw 'found (alist-get 'udid device))))))
            nil))
      (error nil))))

(defun swiftui-preview-standalone--ensure-simulator-booted (udid)
  "Ensure simulator with UDID is booted."
  (let ((state (shell-command-to-string
                (format "xcrun simctl list devices -j | grep -A5 '%s' | grep state" udid))))
    (unless (string-match-p "Booted" state)
      (message "Booting simulator...")
      (call-process "xcrun" nil nil nil "simctl" "boot" udid)
      ;; Wait a bit for boot
      (sleep-for 2))))

(defun swiftui-preview-standalone--generate-host-app (swift-file preview-body imports temp-dir)
  "Generate a PreviewHost app for SWIFT-FILE with PREVIEW-BODY.
IMPORTS is the list of imports to include.
TEMP-DIR is where to create the files.
Returns the path to the generated app source file."
  (let* ((filename (file-name-nondirectory swift-file))
         (import-statements (mapconcat (lambda (imp) (format "import %s" imp))
                                       imports "\n"))
         (indented-body (replace-regexp-in-string
                         "^" "            "
                         preview-body))
         (host-content (format "// Auto-generated PreviewHost for %s
// Generated by swiftui-preview-standalone

%s

@main
struct PreviewHostApp: App {
    var body: some Scene {
        WindowGroup {
            PreviewContent()
        }
    }
}

struct PreviewContent: View {
    var body: some View {
%s
    }
}
" filename import-statements indented-body))
         (host-file (expand-file-name "PreviewHostApp.swift" temp-dir)))

    ;; Create temp directory
    (make-directory temp-dir t)

    ;; Write host file
    (with-temp-file host-file
      (insert host-content))

    ;; Copy original Swift file if it contains type definitions we need
    (let ((orig-copy (expand-file-name filename temp-dir)))
      ;; Remove #Preview and @main from copy to avoid conflicts
      (with-temp-file orig-copy
        (insert-file-contents swift-file)
        (goto-char (point-min))
        ;; Remove @main if present
        (while (re-search-forward "@main[[:space:]]*\n?" nil t)
          (replace-match ""))
        ;; Remove #Preview blocks (they're in the host now)
        (goto-char (point-min))
        (while (re-search-forward "#Preview[^{]*{" nil t)
          (let ((start (match-beginning 0)))
            (goto-char (1- (point)))
            (forward-sexp)
            (delete-region start (point))))))

    host-file))

(defun swiftui-preview-standalone--build-app (temp-dir simulator-udid)
  "Build the preview app in TEMP-DIR for SIMULATOR-UDID.
Returns the path to the built .app or nil on failure."
  (let* ((derived-data (expand-file-name "DerivedData" temp-dir))
         (sdk (string-trim (shell-command-to-string
                            "xcrun --sdk iphonesimulator --show-sdk-path")))
         (swift-files (directory-files temp-dir t "\\.swift$"))
         (output-app (expand-file-name "PreviewHost.app" temp-dir))
         (output-binary (expand-file-name "PreviewHost" output-app))
         (args (append
                (list "swiftc"
                      "-sdk" sdk
                      "-target" (format "arm64-apple-ios%s-simulator"
                                       swiftui-preview-standalone-ios-version)
                      "-o" output-binary
                      "-parse-as-library")
                swift-files)))

    ;; Create .app bundle
    (make-directory output-app t)

    ;; Create Info.plist
    (with-temp-file (expand-file-name "Info.plist" output-app)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>CFBundleExecutable</key>
    <string>PreviewHost</string>
    <key>CFBundleIdentifier</key>
    <string>com.swift-development.preview-standalone</string>
    <key>CFBundleName</key>
    <string>PreviewHost</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>MinimumOSVersion</key>
    <string>" swiftui-preview-standalone-ios-version "</string>
    <key>UIDeviceFamily</key>
    <array>
        <integer>1</integer>
        <integer>2</integer>
    </array>
    <key>UILaunchScreen</key>
    <dict/>
    <key>UIApplicationSceneManifest</key>
    <dict>
        <key>UIApplicationSupportsMultipleScenes</key>
        <false/>
        <key>UISceneConfigurations</key>
        <dict/>
    </dict>
</dict>
</plist>"))

    ;; Compile Swift files
    (message "Building preview app...")
    (let* ((cmd (mapconcat #'shell-quote-argument args " "))
           (output (shell-command-to-string (concat cmd " 2>&1")))
           (success (file-exists-p output-binary)))

      (if success
          (progn
            (message "Build successful")
            output-app)
        (message "Build failed: %s" output)
        nil))))

(defun swiftui-preview-standalone--run-and-capture (app-path simulator-udid output-path)
  "Install and run APP-PATH on SIMULATOR-UDID, then capture to OUTPUT-PATH."
  (let ((bundle-id "com.swift-development.preview-standalone"))

    ;; Terminate any existing instance
    (call-process "xcrun" nil nil nil "simctl" "terminate" simulator-udid bundle-id)

    ;; Install the app
    (message "Installing preview app...")
    (let ((result (call-process "xcrun" nil nil nil
                                "simctl" "install" simulator-udid app-path)))
      (unless (= result 0)
        (error "Failed to install app")))

    ;; Open Simulator window
    (call-process "open" nil nil nil "-a" "Simulator"
                  "--args" "-CurrentDeviceUDID" simulator-udid)
    (sleep-for 1)

    ;; Launch the app
    (message "Launching preview app...")
    (call-process "xcrun" nil nil nil "simctl" "launch" simulator-udid bundle-id)

    ;; Wait for app to render
    (message "Waiting %.1f seconds for render..."
             swiftui-preview-standalone-capture-delay)
    (sleep-for swiftui-preview-standalone-capture-delay)

    ;; Capture screenshot
    (message "Capturing screenshot...")
    (make-directory (file-name-directory output-path) t)
    (let ((result (call-process "xcrun" nil nil nil
                                "simctl" "io" simulator-udid "screenshot" output-path)))
      (when (= result 0)
        ;; Terminate app
        (call-process "xcrun" nil nil nil "simctl" "terminate" simulator-udid bundle-id)
        output-path))))

;;;###autoload
(defun swiftui-preview-standalone-generate ()
  "Generate SwiftUI preview for standalone Swift file.
Works with Swift files that only use system framework imports.
Creates a minimal app, builds it, runs in simulator, and captures screenshot."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (unless (string-match-p "\\.swift$" (buffer-file-name))
    (user-error "Not a Swift file"))

  ;; Check for non-system imports
  (let ((non-system (swiftui-preview-standalone--non-system-imports)))
    (when non-system
      (user-error "File has non-system imports: %s. Use project-based preview instead"
                  (string-join non-system ", "))))

  ;; Get preview body
  (let ((preview-body (swiftui-preview--get-first-preview-body)))
    (unless preview-body
      (user-error "No #Preview block found in file"))

    ;; Save file
    (save-buffer)

    (let* ((swift-file (buffer-file-name))
           (filename (file-name-sans-extension (file-name-nondirectory swift-file)))
           (imports (swiftui-preview-standalone--extract-imports))
           ;; Ensure SwiftUI is in imports
           (imports (if (member "SwiftUI" imports) imports (cons "SwiftUI" imports)))
           (temp-dir (expand-file-name
                      (format "swift-preview-standalone-%s" filename)
                      temporary-file-directory))
           (output-path (expand-file-name
                         (format "%s.png" filename)
                         temporary-file-directory))
           (simulator-udid (or (swiftui-preview-standalone--find-simulator-udid
                               swiftui-preview-standalone-simulator)
                              (and (fboundp 'ios-simulator-simulator-identifier)
                                   (ios-simulator-simulator-identifier))
                              (swiftui-preview--get-booted-simulator))))

      (unless simulator-udid
        (user-error "No simulator found. Boot a simulator first"))

      ;; Ensure simulator is booted
      (swiftui-preview-standalone--ensure-simulator-booted simulator-udid)

      ;; Clean up old temp dir
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))

      ;; Generate host app
      (message "Generating preview host for %s..." filename)
      (swiftui-preview-standalone--generate-host-app
       swift-file preview-body imports temp-dir)

      ;; Build
      (let ((app-path (swiftui-preview-standalone--build-app temp-dir simulator-udid)))
        (unless app-path
          (unless swiftui-preview-standalone-keep-temp
            (delete-directory temp-dir t))
          (user-error "Build failed"))

        ;; Run and capture
        (let ((captured (swiftui-preview-standalone--run-and-capture
                        app-path simulator-udid output-path)))
          ;; Clean up
          (unless swiftui-preview-standalone-keep-temp
            (delete-directory temp-dir t))

          (if captured
              (progn
                (swiftui-preview--display-image captured)
                (message "Standalone preview generated: %s" captured))
            (user-error "Failed to capture preview")))))))

;;;###autoload
(defun swiftui-preview-standalone-check ()
  "Check if current file is suitable for standalone preview.
Returns t if file can use standalone preview, nil otherwise."
  (interactive)
  (if (not (buffer-file-name))
      (progn (message "Not visiting a file") nil)
    (if (not (string-match-p "\\.swift$" (buffer-file-name)))
        (progn (message "Not a Swift file") nil)
      (let ((non-system (swiftui-preview-standalone--non-system-imports)))
        (if non-system
            (progn
              (message "Cannot use standalone: non-system imports: %s"
                       (string-join non-system ", "))
              nil)
          (if (swiftui-preview--get-first-preview-body)
              (progn
                (message "File is suitable for standalone preview!")
                t)
            (message "No #Preview block found")
            nil))))))

(provide 'swiftui-preview-standalone)
;;; swiftui-preview-standalone.el ends here
