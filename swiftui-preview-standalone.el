;;; swiftui-preview-standalone.el --- Standalone Swift file preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: Swift, swiftui, preview, ios

;;; Commentary:

;; SwiftUI preview for standalone Swift files (system imports only).
;; Delegates shared utilities to `swiftui-preview-core':
;; - Import extraction, simulator, capture, host-app generation,
;;   Info.plist, preview block removal.
;;
;; Usage:
;; 1. Open a standalone Swift file with a #Preview block
;; 2. M-x swiftui-preview-standalone-generate

;;; Code:

(require 'cl-lib)
(require 'swiftui-preview-core)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")

(defgroup swiftui-preview-standalone nil
  "Standalone Swift file preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-standalone-")

(defcustom swiftui-preview-standalone-ios-version "18.0"
  "iOS deployment target for standalone builds."
  :type 'string
  :group 'swiftui-preview-standalone)

(defcustom swiftui-preview-standalone-keep-temp nil
  "If non-nil, keep temporary build files for debugging."
  :type 'boolean
  :group 'swiftui-preview-standalone)

(defvar swiftui-preview-standalone--system-frameworks
  '("SwiftUI" "UIKit" "Foundation" "Combine" "CoreGraphics"
    "CoreAnimation" "QuartzCore" "AVFoundation" "MapKit"
    "CoreLocation" "CoreData" "CloudKit" "StoreKit" "GameKit"
    "SpriteKit" "SceneKit" "ARKit" "RealityKit" "Metal"
    "MetalKit" "Vision" "CoreML" "NaturalLanguage" "Speech"
    "Intents" "UserNotifications" "WidgetKit" "AppIntents"
    "Charts" "Swift" "os" "Darwin" "Dispatch" "ObjectiveC"
    "Accelerate" "simd" "Security" "CryptoKit"
    "LocalAuthentication" "AuthenticationServices"
    "SafariServices" "WebKit" "PhotosUI" "Photos" "Contacts"
    "ContactsUI" "EventKit" "EventKitUI" "Messages" "MessageUI"
    "HealthKit" "CoreMotion" "CoreBluetooth"
    "MultipeerConnectivity" "Network" "NetworkExtension"
    "CallKit" "PushKit" "CoreTelephony" "SystemConfiguration"
    "MobileCoreServices" "UniformTypeIdentifiers"
    "LinkPresentation" "QuickLook" "QuickLookThumbnailing"
    "Observation" "SwiftData")
  "List of system frameworks that can be used in standalone previews.")

(defun swiftui-preview-standalone--non-system-imports ()
  "Get list of non-system imports in current buffer."
  (let ((imports (swiftui-preview-core-extract-imports)))
    (cl-remove-if
     (lambda (imp)
       (member imp swiftui-preview-standalone--system-frameworks))
     imports)))

;;; Build

(defun swiftui-preview-standalone--prepare-sources
    (swift-file preview-body imports temp-dir &optional output-path)
  "Prepare SWIFT-FILE for building with PREVIEW-BODY and IMPORTS.
Writes the host app via core and copies the source file
with #Preview blocks and @main removed.
TEMP-DIR is the build directory.
OUTPUT-PATH is the absolute path for the snapshot PNG (embedded in Swift).
Returns the host file path."
  (let* ((filename (file-name-nondirectory swift-file))
         (color-scheme
          (swiftui-preview-core-detect-color-scheme preview-body)))
    ;; Generate host app with snapshot support via core
    ;; output-path is embedded in the Swift code so the app knows where
    ;; to write the PNG without relying on environment variables
    (swiftui-preview-core-write-host-app
     preview-body imports temp-dir filename color-scheme
     nil nil nil output-path)

    ;; Copy original source, removing #Preview and @main
    (let ((orig-copy (expand-file-name filename temp-dir))
          (content (with-temp-buffer
                     (insert-file-contents swift-file)
                     (buffer-string))))
      (setq content (replace-regexp-in-string
                     "@main[[:space:]]*\n?" "" content))
      (setq content
            (swiftui-preview-core-remove-preview-blocks content))
      (with-temp-file orig-copy
        (insert content)))))

(defun swiftui-preview-standalone--build-app (temp-dir)
  "Build the preview app in TEMP-DIR.
Returns the path to the built .app or nil on failure."
  (let* ((sdk (swiftui-preview-core-sdk-path))
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

    ;; Create Info.plist using core
    (with-temp-file (expand-file-name "Info.plist" output-app)
      (insert (swiftui-preview-core-generate-info-plist
               "com.swift-development.preview-standalone"
               swiftui-preview-standalone-ios-version)))

    ;; Compile
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

;;;###autoload
(defun swiftui-preview-standalone-generate ()
  "Generate SwiftUI preview for standalone Swift file.
Works with Swift files that only use system framework imports."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (unless (string-match-p "\\.swift$" (buffer-file-name))
    (user-error "Not a Swift file"))

  ;; Check for non-system imports
  (let ((non-system (swiftui-preview-standalone--non-system-imports)))
    (when non-system
      (user-error
       "File has non-system imports: %s.  Use project-based preview"
       (string-join non-system ", "))))

  ;; Get preview body
  (let ((preview-body (swiftui-preview--get-first-preview-body)))
    (unless preview-body
      (user-error "No #Preview block found in file"))

    ;; Save file
    (save-buffer)

    (let* ((swift-file (buffer-file-name))
           (filename (file-name-sans-extension
                      (file-name-nondirectory swift-file)))
           (imports (swiftui-preview-core-extract-imports))
           (imports (if (member "SwiftUI" imports)
                        imports
                      (cons "SwiftUI" imports)))
           (temp-dir (expand-file-name
                      (format "swift-preview-standalone-%s" filename)
                      temporary-file-directory))
           (output-path (expand-file-name
                         (format "%s.png" filename)
                         temporary-file-directory))
           (simulator-udid (swiftui-preview-core-find-simulator)))

      (unless simulator-udid
        (user-error "No simulator found.  Boot a simulator first"))

      ;; Ensure simulator is booted (delegates to ios-simulator)
      (swiftui-preview-core-ensure-simulator-booted simulator-udid)

      ;; Clean up old temp dir
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t))

      ;; Prepare sources (host app + cleaned source)
      (message "Generating preview host for %s..." filename)
      (swiftui-preview-standalone--prepare-sources
       swift-file preview-body imports temp-dir output-path)

      ;; Build
      (let ((app-path
             (swiftui-preview-standalone--build-app temp-dir)))
        (unless app-path
          (unless swiftui-preview-standalone-keep-temp
            (delete-directory temp-dir t))
          (user-error "Build failed"))

        ;; Capture using core pipeline (install, launch, poll)
        (condition-case err
            (let ((captured
                   (swiftui-preview-core-capture
                    app-path simulator-udid output-path
                    "com.swift-development.preview-standalone")))
              ;; Clean up
              (unless swiftui-preview-standalone-keep-temp
                (delete-directory temp-dir t))
              (if captured
                  (progn
                    (swiftui-preview--display-image captured)
                    (message "Standalone preview generated: %s"
                             captured))
                (user-error "Failed to capture preview")))
          (error
           (unless swiftui-preview-standalone-keep-temp
             (delete-directory temp-dir t))
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun swiftui-preview-standalone-check ()
  "Check if current file is suitable for standalone preview.
Returns t if file can use standalone preview, nil otherwise."
  (interactive)
  (cond
   ((not (buffer-file-name))
    (message "Not visiting a file") nil)
   ((not (string-match-p "\\.swift$" (buffer-file-name)))
    (message "Not a Swift file") nil)
   ((swiftui-preview-standalone--non-system-imports)
    (message "Cannot use standalone: non-system imports: %s"
             (string-join
              (swiftui-preview-standalone--non-system-imports) ", "))
    nil)
   ((swiftui-preview--get-first-preview-body)
    (message "File is suitable for standalone preview!")
    t)
   (t
    (message "No #Preview block found")
    nil)))

(provide 'swiftui-preview-standalone)
;;; swiftui-preview-standalone.el ends here
