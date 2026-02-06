;;; swiftui-preview-core.el --- Core SwiftUI preview utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: swift, swiftui, preview, ios

;;; Commentary:

;; Core utilities shared between all swiftui-preview modules.
;; This module provides preview-specific functionality:
;; - Import extraction from Swift buffers
;; - Preview host Swift code generation (with snapshot & color scheme)
;; - Info.plist generation for preview apps
;; - Preview block removal from Swift source
;; - Ruby script execution (sync and async)
;; - Non-blocking file polling
;; - Preview capture pipeline (install, launch, poll)
;;
;; Simulator interaction is delegated to `ios-simulator.el'.
;; Async process execution is delegated to `swift-async.el'.
;; SDK/toolchain paths use `swift-cache.el' when available.
;;
;; All other swiftui-preview-* modules should use these functions
;; instead of reimplementing them.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'swift-async nil t)

;; Forward declarations -- simulator & project utilities
(declare-function ios-simulator-simulator-identifier "ios-simulator")
(declare-function ios-simulator-setup-simulator-dwim "ios-simulator")
(declare-function ios-simulator-booted-simulator "ios-simulator")
(declare-function swift-async-run "swift-async")
(declare-function swift-async-run-sync "swift-async")
(declare-function swift-cache-with "swift-cache")

;;; Customization

(defgroup swiftui-preview-core nil
  "Core SwiftUI preview settings shared across all preview modules."
  :group 'swiftui-preview
  :prefix "swiftui-preview-core-")

(defcustom swiftui-preview-core-capture-timeout 10.0
  "Timeout in seconds for preview capture polling."
  :type 'number
  :group 'swiftui-preview-core)

(defcustom swiftui-preview-core-verbose nil
  "If non-nil, show verbose output during preview generation.
Applies to all preview modules."
  :type 'boolean
  :group 'swiftui-preview-core)

;;; Internal variables

(defvar swiftui-preview-core--scripts-dir
  (expand-file-name "scripts"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing Ruby helper scripts.")

;;; Path Helpers (cached via swift-cache when available)

(defvar swiftui-preview-core--cached-sdk-path nil
  "Session-level fallback cache for SDK path.")

(defvar swiftui-preview-core--cached-toolchain-path nil
  "Session-level fallback cache for toolchain path.")

(defun swiftui-preview-core-sdk-path ()
  "Return the iphonesimulator SDK path, caching the result."
  (or swiftui-preview-core--cached-sdk-path
      (setq swiftui-preview-core--cached-sdk-path
            (if (fboundp 'swift-async-run-sync)
                (swift-async-run-sync
                 '("xcrun" "--sdk" "iphonesimulator"
                   "--show-sdk-path")
                 :timeout 5)
              (string-trim
               (shell-command-to-string
                "xcrun --sdk iphonesimulator --show-sdk-path"))))))

(defun swiftui-preview-core-toolchain-path ()
  "Return the Xcode toolchain path, caching the result."
  (or swiftui-preview-core--cached-toolchain-path
      (setq swiftui-preview-core--cached-toolchain-path
            (if (fboundp 'swift-async-run-sync)
                (swift-async-run-sync
                 '("xcode-select" "-p") :timeout 5)
              (string-trim
               (shell-command-to-string "xcode-select -p"))))))

;;; Import Extraction

(defun swiftui-preview-core-extract-imports ()
  "Extract all import statements from current buffer.
Returns a deduplicated list of imported module names."
  (save-excursion
    (goto-char (point-min))
    (let ((imports '()))
      (while (re-search-forward
              "^import[[:space:]]+\\([A-Za-z_][A-Za-z0-9_]*\\)"
              nil t)
        (push (match-string 1) imports))
      (delete-dups (nreverse imports)))))

;;; Simulator Helpers (thin wrappers around ios-simulator.el)

(defun swiftui-preview-core-find-simulator ()
  "Find simulator UDID for preview.
Delegates to `ios-simulator-simulator-identifier' which handles
selection, caching and fallback logic."
  (when (fboundp 'ios-simulator-simulator-identifier)
    (ios-simulator-simulator-identifier)))

(defun swiftui-preview-core-find-simulator-async (callback)
  "Find simulator UDID asynchronously, call CALLBACK with result.
Delegates to `ios-simulator-simulator-identifier' with :callback."
  (if (fboundp 'ios-simulator-simulator-identifier)
      (ios-simulator-simulator-identifier :callback callback)
    (funcall callback nil)))

(defun swiftui-preview-core-ensure-simulator-booted (udid)
  "Ensure simulator with UDID is booted.
Delegates to `ios-simulator-setup-simulator-dwim' which handles
boot, already-booted, and Simulator.app launch."
  (when (fboundp 'ios-simulator-setup-simulator-dwim)
    (ios-simulator-setup-simulator-dwim udid)))

(defun swiftui-preview-core-ensure-simulator-booted-async
    (udid callback)
  "Ensure simulator with UDID is booted, then call CALLBACK.
Uses `ios-simulator-setup-simulator-dwim' (async) and waits
briefly for the simulator to settle."
  (if (fboundp 'ios-simulator-setup-simulator-dwim)
      (progn
        (ios-simulator-setup-simulator-dwim udid)
        ;; Give the simulator a moment to settle before proceeding
        (run-at-time 1 nil callback))
    (funcall callback)))

;;; simctl Wrappers

(defun swiftui-preview-core-simctl (args)
  "Run simctl with ARGS synchronously.
Returns output string on success, nil on failure."
  (if (fboundp 'swift-async-run-sync)
      (swift-async-run-sync
       (append '("xcrun" "simctl") args) :timeout 10)
    (with-temp-buffer
      (when (zerop (apply #'call-process
                          "xcrun" nil t nil "simctl" args))
        (buffer-string)))))

(defun swiftui-preview-core-simctl-async
    (args callback &optional error-callback)
  "Run simctl with ARGS asynchronously, call CALLBACK with output.
ERROR-CALLBACK is called on failure.  If nil, CALLBACK receives
nil so async pipelines continue (useful for terminate/uninstall)."
  (if (fboundp 'swift-async-run)
      (let ((cb (or callback #'ignore)))
        (swift-async-run
         (append '("xcrun" "simctl") args)
         cb
         :timeout 15
         :error-callback (or error-callback
                          (lambda (_err) (funcall cb nil)))
         :process-key (format "preview-simctl-%s" (car args))))
    ;; Sync fallback
    (let ((result (swiftui-preview-core-simctl args)))
      (when callback (funcall callback result)))))

;;; Preview Capture

(defun swiftui-preview-core-capture
    (app-path simulator-udid output-path bundle-id)
  "Install APP-PATH, launch, and wait for snapshot (synchronous).
SIMULATOR-UDID is the target simulator.
OUTPUT-PATH is where the preview image will be saved.
BUNDLE-ID is the app\\='s bundle identifier.
Returns OUTPUT-PATH on success."
  (let ((process-environment
         (cons (format "SIMCTL_CHILD_PREVIEW_OUTPUT_PATH=%s"
                       output-path)
               process-environment)))
    ;; Terminate any existing instance
    (swiftui-preview-core-simctl
     (list "terminate" simulator-udid bundle-id))

    ;; Ensure output directory exists and remove old file
    (make-directory (file-name-directory output-path) t)
    (when (file-exists-p output-path)
      (delete-file output-path))

    ;; Install
    (message "Installing preview app...")
    (unless (swiftui-preview-core-simctl
             (list "install" simulator-udid app-path))
      (error "Failed to install preview app"))

    ;; Launch
    (message "Launching preview app...")
    (swiftui-preview-core-simctl
     (list "launch" simulator-udid bundle-id))

    ;; Wait for the app to generate the preview and exit
    (message "Waiting for preview generation...")
    (let ((timeout swiftui-preview-core-capture-timeout)
          (poll-interval 0.2)
          (elapsed 0.0))
      (while (and (< elapsed timeout)
                  (not (file-exists-p output-path)))
        (sleep-for poll-interval)
        (setq elapsed (+ elapsed poll-interval)))

      (unless (file-exists-p output-path)
        ;; Fallback: try simctl screenshot
        (message "Internal capture timed out, falling back to screenshot...")
        (swiftui-preview-core-simctl
         (list "io" simulator-udid "screenshot" output-path))))

    ;; Terminate app
    (swiftui-preview-core-simctl
     (list "terminate" simulator-udid bundle-id))

    (if (file-exists-p output-path)
        output-path
      (error "Failed to generate preview"))))

(defun swiftui-preview-core-poll-for-file
    (path timeout interval callback)
  "Poll for PATH to exist using a timer, then call CALLBACK.
TIMEOUT is max wait in seconds.  INTERVAL is poll frequency.
CALLBACK receives PATH if found, or nil if timed out.
Does not block Emacs."
  (let ((timer nil)
        (start-time (float-time)))
    (setq timer
          (run-at-time
           0 interval
           (lambda ()
             (cond
              ((file-exists-p path)
               (when (timerp timer) (cancel-timer timer))
               (funcall callback path))
              ((> (- (float-time) start-time) timeout)
               (when (timerp timer) (cancel-timer timer))
               (funcall callback nil))))))))

;;; Ruby Script Helpers

(defun swiftui-preview-core-run-ruby-script (script-name &rest args)
  "Run Ruby SCRIPT-NAME with ARGS synchronously.
Returns parsed JSON result or signals error."
  (let* ((script (expand-file-name
                  script-name swiftui-preview-core--scripts-dir))
         (cmd (mapconcat #'shell-quote-argument
                         (cons "ruby" (cons script args)) " "))
         (stderr-file (when swiftui-preview-core-verbose
                        (make-temp-file "preview-stderr-")))
         (full-cmd (if stderr-file
                       (concat cmd " 2>"
                               (shell-quote-argument stderr-file))
                     (concat cmd " 2>/dev/null")))
         (output (shell-command-to-string full-cmd))
         (json-array-type 'list)
         (json-output nil))

    (when swiftui-preview-core-verbose
      (message "[Preview Core] Running: %s" cmd))

    ;; Show stderr output in verbose mode
    (when (and stderr-file (file-exists-p stderr-file))
      (let ((stderr-content
             (with-temp-buffer
               (insert-file-contents stderr-file)
               (buffer-string))))
        (unless (string-empty-p stderr-content)
          (message "[Preview Core] Script log:\n%s"
                   stderr-content)))
      (delete-file stderr-file))

    ;; Parse JSON from stdout
    (condition-case nil
        (setq json-output (json-read-from-string output))
      (error
       (when swiftui-preview-core-verbose
         (message "[Preview Core] Script output (not JSON): %s"
                  output))
       nil))

    (if (and json-output (eq (alist-get 'success json-output) t))
        json-output
      (let ((error-msg (or (alist-get 'error json-output) output)))
        (error "Ruby script failed: %s" error-msg)))))

(defun swiftui-preview-core-run-ruby-script-async
    (script-name callback &rest args)
  "Run Ruby SCRIPT-NAME with ARGS asynchronously.
Calls CALLBACK with parsed JSON result on success."
  (let* ((script (expand-file-name
                  script-name swiftui-preview-core--scripts-dir))
         (cmd (append (list "ruby" script) args)))
    (when swiftui-preview-core-verbose
      (message "[Preview Core] Running async: %s"
               (string-join cmd " ")))
    (if (fboundp 'swift-async-run)
        (swift-async-run
         cmd
         (lambda (output)
           (let* ((json-array-type 'list)
                  (json-output
                   (when output
                     (condition-case nil
                         (json-read-from-string output)
                       (error nil)))))
             (if (and json-output
                      (eq (alist-get 'success json-output) t))
                 (funcall callback json-output)
               (let ((error-msg
                      (or (when json-output
                            (alist-get 'error json-output))
                          (or output "Unknown error"))))
                 (error "Ruby script failed: %s" error-msg)))))
         :timeout 30
         :error-callback
         (lambda (err)
           (error "Ruby script %s failed: %s" script-name err))
         :process-key (format "preview-ruby-%s" script-name))
      ;; No async - sync fallback
      (funcall callback
               (apply #'swiftui-preview-core-run-ruby-script
                      script-name args)))))

;;; Snapshot Swift Code

(defconst swiftui-preview-core--snapshot-code
  "
// MARK: - Snapshot Logic

func snapshotPreview() {
    let previewView = PreviewContent()

    let screenWidth: CGFloat = UIScreen.main.bounds.width
    let screenHeight: CGFloat = UIScreen.main.bounds.height

    // Measure with the correct interface style so colors resolve
    let measureController = UIHostingController(rootView: previewView)
    measureController.overrideUserInterfaceStyle = kInterfaceStyle
    measureController.view.backgroundColor = .clear
    measureController.view.frame = CGRect(
        origin: .zero,
        size: CGSize(width: screenWidth, height: screenHeight))
    measureController.view.layoutIfNeeded()

    let fittedSize = measureController.sizeThatFits(
        in: CGSize(width: screenWidth,
                   height: UIView.layoutFittingExpandedSize.height))

    let contentWidth = min(fittedSize.width, screenWidth)
    let contentHeight = min(fittedSize.height, screenHeight)

    let padding: CGFloat = 20
    let finalSize = CGSize(
        width: contentWidth + padding * 2,
        height: contentHeight + padding * 2
    )

    // Use systemBackground for explicit dark/light, clear otherwise
    let bgColor: Color = kHasExplicitColorScheme
        ? Color(uiColor: .systemBackground)
        : Color.clear

    let paddedView = previewView
        .frame(width: contentWidth, height: contentHeight)
        .padding(padding)
        .background(bgColor)

    let finalController = UIHostingController(rootView: paddedView)
    finalController.overrideUserInterfaceStyle = kInterfaceStyle
    finalController.view.frame = CGRect(origin: .zero, size: finalSize)
    finalController.view.backgroundColor = kHasExplicitColorScheme
        ? .systemBackground
        : .clear
    if !kHasExplicitColorScheme {
        finalController.view.isOpaque = false
        for subview in finalController.view.subviews {
            subview.backgroundColor = .clear
            subview.isOpaque = false
        }
    }
    finalController.view.layoutIfNeeded()

    let format = UIGraphicsImageRendererFormat()
    format.scale = UIScreen.main.scale
    format.opaque = false

    let renderer = UIGraphicsImageRenderer(size: finalSize, format: format)
    let image = renderer.image { context in
        finalController.view.drawHierarchy(
            in: CGRect(origin: .zero, size: finalSize),
            afterScreenUpdates: true)
    }

    guard let data = image.pngData() else {
        print(\"ERROR: Failed to create PNG data\")
        exit(1)
    }

    let outputPath = ProcessInfo.processInfo.environment[\"PREVIEW_OUTPUT_PATH\"]
        ?? \"/tmp/swift-development-preview.png\"

    let url = URL(fileURLWithPath: outputPath)
    try? FileManager.default.createDirectory(
        at: url.deletingLastPathComponent(),
        withIntermediateDirectories: true)

    do {
        try data.write(to: url)
        print(\"Preview saved to: \\(outputPath)\")
        print(\"Size: \\(Int(finalSize.width))x\\(Int(finalSize.height))\")
        let styleName = kInterfaceStyle == .dark ? \"dark\"
            : kInterfaceStyle == .light ? \"light\" : \"none\"
        print(\"Style: \\(styleName)\")
    } catch {
        print(\"ERROR: Failed to write preview: \\(error)\")
        exit(1)
    }

    exit(0)
}
"
  "Swift code for snapshot functionality with color scheme support.")

(defun swiftui-preview-core-generate-host-swift
    (preview-body imports
     &optional filename color-scheme testable-module use-testable live)
  "Generate PreviewHostApp.swift content with color scheme support.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
FILENAME is optional source file name for comment.
COLOR-SCHEME is \"dark\", \"light\", or nil.
TESTABLE-MODULE is the module name for @testable import.
USE-TESTABLE non-nil to use @testable on TESTABLE-MODULE.
LIVE non-nil generates a live app (no snapshot, no exit) for
interactive use in the simulator."
  (let* ((fname (or filename "SwiftUI Preview"))
         (interface-style (cond
                           ((equal color-scheme "dark") ".dark")
                           ((equal color-scheme "light") ".light")
                           (t ".unspecified")))
         (import-statements
          (mapconcat
           (lambda (imp)
             (if (and use-testable testable-module
                      (string= imp testable-module))
                 (format "@testable import %s" imp)
               (format "import %s" imp)))
           imports "\n"))
         (indented-body (replace-regexp-in-string
                         "^" "            " preview-body)))
    (if live
        ;; Live mode: plain SwiftUI app, no snapshot, no exit
        (format "// Auto-generated PreviewHost for %s
// Generated by swift-development (live mode)

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
" fname import-statements indented-body)
      ;; Snapshot mode: render one frame, write PNG, exit
      (format "// Auto-generated PreviewHost for %s
// Generated by swift-development

%s
import UIKit

let kInterfaceStyle: UIUserInterfaceStyle = %s
let kHasExplicitColorScheme: Bool = (kInterfaceStyle != .unspecified)

@main
struct PreviewHostApp: App {
    var body: some Scene {
        WindowGroup {
            PreviewContent()
                .onAppear {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                        snapshotPreview()
                    }
                }
        }
    }
}

struct PreviewContent: View {
    var body: some View {
%s
    }
}
%s" fname import-statements interface-style indented-body
              swiftui-preview-core--snapshot-code))))

(defun swiftui-preview-core-write-host-app
    (preview-body imports temp-dir
     &optional filename color-scheme testable-module use-testable live)
  "Write PreviewHostApp.swift to TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
FILENAME is optional source file name for comment.
COLOR-SCHEME, TESTABLE-MODULE, USE-TESTABLE passed to generate.
LIVE non-nil generates a live (interactive) app.
Returns path to written file."
  (let ((host-content (swiftui-preview-core-generate-host-swift
                       preview-body imports filename
                       color-scheme testable-module use-testable
                       live))
        (host-file (expand-file-name
                    "PreviewHostApp.swift" temp-dir)))
    (make-directory temp-dir t)
    (with-temp-file host-file
      (insert host-content))
    (when swiftui-preview-core-verbose
      (message "[Preview Core] Generated PreviewHost (%s) in %s"
               (if live "live" "snapshot") temp-dir))
    host-file))

;;; Preview Block Removal (robust brace-matching)

(defun swiftui-preview-core-remove-preview-blocks (content)
  "Remove #Preview blocks from CONTENT string.
Uses Emacs `forward-sexp' for robust brace matching."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "#Preview[[:space:]]*" nil t)
      (let ((start (match-beginning 0)))
        ;; Skip optional parameters in parentheses
        (when (looking-at "(")
          (forward-sexp))
        ;; Skip whitespace to find opening brace
        (skip-chars-forward " \t\n")
        (when (looking-at "{")
          (let ((brace-start (point)))
            (condition-case nil
                (progn
                  (forward-sexp)
                  (delete-region start (point))
                  (insert "// #Preview removed\n")
                  (goto-char start))
              (scan-error
               (goto-char (1+ brace-start))))))))
    (buffer-string)))

;;; Info.plist Generation

(defun swiftui-preview-core-generate-info-plist
    (bundle-id &optional ios-version)
  "Generate Info.plist content for preview app.
BUNDLE-ID is the CFBundleIdentifier.
IOS-VERSION is the MinimumOSVersion (defaults to \"17.0\")."
  (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \
\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>CFBundleExecutable</key>
    <string>PreviewHost</string>
    <key>CFBundleIdentifier</key>
    <string>%s</string>
    <key>CFBundleName</key>
    <string>PreviewHost</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>%s</string>
    <key>LSRequiresIPhoneOS</key>
    <true/>
    <key>UIApplicationSceneManifest</key>
    <dict>
        <key>UIApplicationSupportsMultipleScenes</key>
        <false/>
    </dict>
    <key>UILaunchScreen</key>
    <dict/>
    <key>UIDeviceFamily</key>
    <array>
        <integer>1</integer>
        <integer>2</integer>
    </array>
    <key>MinimumOSVersion</key>
    <string>%s</string>
</dict>
</plist>" bundle-id (format-time-string "%s") (or ios-version "17.0")))

;;; Color Scheme Detection

(defun swiftui-preview-core-detect-color-scheme (preview-body)
  "Detect preferred color scheme from PREVIEW-BODY.
Returns \"dark\", \"light\", or nil if none specified."
  (cond
   ((string-match-p "\\.preferredColorScheme(\\.dark" preview-body)
    "dark")
   ((string-match-p "\\.preferredColorScheme(\\.light" preview-body)
    "light")
   (t nil)))

(defun swiftui-preview-core-detect-color-scheme-from-buffer ()
  "Detect preferred color scheme by scanning the current buffer.
Returns \"dark\", \"light\", or nil if none specified."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((search-forward ".preferredColorScheme(.dark" nil t) "dark")
     ((progn (goto-char (point-min))
             (search-forward ".preferredColorScheme(.light" nil t))
      "light")
     (t nil))))

;;; Toggle verbose

;;;###autoload
(defun swiftui-preview-core-toggle-verbose ()
  "Toggle verbose mode for all SwiftUI preview modules."
  (interactive)
  (setq swiftui-preview-core-verbose
        (not swiftui-preview-core-verbose))
  (message "SwiftUI Preview verbose mode: %s"
           (if swiftui-preview-core-verbose "ON" "OFF")))

(provide 'swiftui-preview-core)
;;; swiftui-preview-core.el ends here
