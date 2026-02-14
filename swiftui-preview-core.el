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
(require 'filenotify)
(require 'json)
(require 'swift-async nil t)

;; Forward declarations -- simulator & project utilities
(declare-function ios-simulator-simulator-identifier "ios-simulator")
(declare-function ios-simulator-setup-simulator-dwim "ios-simulator")
(declare-function ios-simulator-booted-simulator "ios-simulator")
(declare-function ios-simulator-get-all-booted-simulators "ios-simulator")
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
If the simulator is already booted, calls CALLBACK immediately.
Otherwise uses `ios-simulator-setup-simulator-dwim' and waits 1s
for it to settle."
  (if (fboundp 'ios-simulator-setup-simulator-dwim)
      (let ((already-booted
             (and (fboundp 'ios-simulator-get-all-booted-simulators)
                  (cl-find udid
                           (ios-simulator-get-all-booted-simulators)
                           :key #'cdr :test #'string=))))
        (if already-booted
            ;; Already booted -- proceed immediately
            (progn
              (when swiftui-preview-core-verbose
                (message "[Preview Core] Simulator already booted, skipping delay"))
              (funcall callback))
          ;; Need to boot -- kick off and wait 1s
          (ios-simulator-setup-simulator-dwim udid)
          (run-at-time 1 nil callback)))
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
The output path is embedded in the generated Swift code at build time,
so no environment variable propagation is needed.
BUNDLE-ID is the app\\='s bundle identifier.
Returns OUTPUT-PATH on success."
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

  ;; Launch -- output path is embedded in the Swift code
  (message "Launching preview app...")
  (swiftui-preview-core-simctl
   (list "launch" simulator-udid bundle-id))

  ;; Wait for the app to generate the preview and exit
  (message "Waiting for preview generation...")
  (let ((timeout swiftui-preview-core-capture-timeout)
        (poll-interval 0.3)
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
    (error "Failed to generate preview")))

(defun swiftui-preview-core--poll-for-file (path timeout callback)
  "Poll for PATH using a timer, then call CALLBACK.
TIMEOUT is max wait in seconds.  Polls every 0.3 seconds.
CALLBACK receives PATH if found, or nil if timed out.
Used as fallback when `file-notify' is unavailable."
  (let ((timer nil)
        (start-time (float-time)))
    (setq timer
          (run-at-time
           0 0.3
           (lambda ()
             (cond
              ((file-exists-p path)
               (when (timerp timer) (cancel-timer timer))
               (funcall callback path))
              ((> (- (float-time) start-time) timeout)
               (when (timerp timer) (cancel-timer timer))
               (funcall callback nil))))))))

(defun swiftui-preview-core-watch-for-file (path timeout callback)
  "Watch for PATH to appear, then call CALLBACK.
TIMEOUT is max wait in seconds.
CALLBACK receives PATH if found, or nil if timed out.
Tries `file-notify-add-watch' on the parent directory (kqueue on
macOS) for near-instant detection.  Falls back to timer-based
polling if file descriptors are exhausted.  Does not block Emacs."
  (let* ((dir (file-name-directory path))
         (target-name (file-name-nondirectory path))
         (watch-desc nil)
         (timeout-timer nil)
         (fired nil)  ; guard against double-fire
         (cleanup (lambda ()
                    (when (and watch-desc (file-notify-valid-p watch-desc))
                      (file-notify-rm-watch watch-desc)
                      (setq watch-desc nil))
                    (when (timerp timeout-timer)
                      (cancel-timer timeout-timer)
                      (setq timeout-timer nil)))))
    ;; Ensure directory exists so we can watch it
    (make-directory dir t)
    ;; Check if file already exists (race condition guard)
    (if (file-exists-p path)
        (funcall callback path)
      ;; Try file-notify; fall back to polling on descriptor exhaustion
      (condition-case _err
          (progn
            (setq watch-desc
                  (file-notify-add-watch
                   dir '(change)
                   (lambda (event)
                     (when (and (not fired)
                                (memq (nth 1 event) '(created changed))
                                (let ((event-file (nth 2 event)))
                                  (string= (file-name-nondirectory
                                            (if (stringp event-file)
                                                event-file ""))
                                           target-name)))
                       (setq fired t)
                       (funcall cleanup)
                       (funcall callback path)))))
            ;; Timeout safety net
            (setq timeout-timer
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (unless fired
                       (setq fired t)
                       (funcall cleanup)
                       ;; Last-chance check
                       (if (file-exists-p path)
                           (funcall callback path)
                         (funcall callback nil)))))))
        (file-error
         ;; file-notify failed (no descriptors left, etc.) -- use polling
         (when swiftui-preview-core-verbose
           (message "[Preview Core] file-notify unavailable, using polling for %s"
                    (file-name-nondirectory path)))
         (swiftui-preview-core--poll-for-file path timeout callback))))))

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

(defun swiftui-preview-core--snapshot-code (output-path)
  "Generate Swift snapshot code that writes the preview PNG to OUTPUT-PATH.
OUTPUT-PATH is embedded as a string literal so the app does not depend
on environment variables (which `simctl launch' does not propagate)."
  (format "
// MARK: - Snapshot Logic

/// Find the content bounding box (top and bottom row) in a CGImage.
/// Scans from top-down and bottom-up to find the first/last rows with
/// non-background pixels.  Background color is sampled from the
/// bottom-right corner.
/// Returns (topRow, bottomRow) in pixel coordinates.
func findContentBounds(in cgImage: CGImage) -> (top: Int, bottom: Int) {
    let w = cgImage.width
    let h = cgImage.height
    guard w > 0, h > 0,
          let data = cgImage.dataProvider?.data,
          let ptr = CFDataGetBytePtr(data) else { return (0, h) }

    let bpp = cgImage.bitsPerPixel / 8
    let bpr = cgImage.bytesPerRow
    guard bpp >= 3 else { return (0, h) }

    // Sample background color from bottom-right corner
    let refOffset = (h - 1) * bpr + (w - 1) * bpp
    let bgR = ptr[refOffset]
    let bgG = ptr[refOffset + 1]
    let bgB = ptr[refOffset + 2]

    let tolerance: UInt8 = 4

    func isBackground(_ offset: Int) -> Bool {
        let dr = abs(Int(ptr[offset]) - Int(bgR))
        let dg = abs(Int(ptr[offset + 1]) - Int(bgG))
        let db = abs(Int(ptr[offset + 2]) - Int(bgB))
        return dr <= Int(tolerance) && dg <= Int(tolerance) && db <= Int(tolerance)
    }

    // Sample every 4th pixel per row for speed
    let step = max(1, bpp * 4)

    // Scan from top to find first content row
    var topRow = 0
    for row in 0..<h {
        let rowBase = row * bpr
        var found = false
        for col in stride(from: 0, to: w * bpp, by: step) {
            if !isBackground(rowBase + col) {
                found = true
                break
            }
        }
        if found { topRow = row; break }
    }

    // Scan from bottom to find last content row
    var bottomRow = h
    for row in stride(from: h - 1, through: 0, by: -1) {
        let rowBase = row * bpr
        var found = false
        for col in stride(from: 0, to: w * bpp, by: step) {
            if !isBackground(rowBase + col) {
                found = true
                break
            }
        }
        if found { bottomRow = row + 1; break }
    }

    return (topRow, bottomRow)
}

func snapshotPreview() {
    // Capture the app's own key window -- this is the window that is
    // already displaying PreviewContent() via the SwiftUI App lifecycle.
    // By screenshotting the live window we get NavigationView, toolbar,
    // ScrollView, async-loaded state, and everything else for free.
    guard let scene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
          let window = scene.windows.first else {
        print(\"ERROR: No active window found\")
        exit(1)
    }

    let screenSize = window.bounds.size
    let scale = UIScreen.main.scale

    let format = UIGraphicsImageRendererFormat()
    format.scale = scale
    format.opaque = false

    let renderer = UIGraphicsImageRenderer(size: screenSize, format: format)
    let fullImage = renderer.image { _ in
        window.drawHierarchy(
            in: CGRect(origin: .zero, size: screenSize),
            afterScreenUpdates: true)
    }

    guard let cgFull = fullImage.cgImage else {
        print(\"ERROR: Failed to get CGImage\")
        exit(1)
    }

    // Find where the actual content starts and ends (in pixel coordinates)
    let bounds = findContentBounds(in: cgFull)
    let paddingPx = Int(16 * scale)
    let cropTop = max(bounds.top - paddingPx, 0)
    let cropBottom = min(bounds.bottom + paddingPx, cgFull.height)

    print(\"DEBUG: screen=\\(Int(screenSize.width))x\\(Int(screenSize.height)) contentTop=\\(bounds.top)px contentBottom=\\(bounds.bottom)px cropTop=\\(cropTop)px cropBottom=\\(cropBottom)px\")

    // Crop to content bounds + padding on both sides
    let cropRect = CGRect(x: 0, y: cropTop, width: cgFull.width, height: cropBottom - cropTop)
    guard let croppedCG = cgFull.cropping(to: cropRect) else {
        print(\"ERROR: Failed to crop image\")
        exit(1)
    }

    let croppedImage = UIImage(cgImage: croppedCG, scale: scale, orientation: .up)
    guard let data = croppedImage.pngData() else {
        print(\"ERROR: Failed to create PNG data\")
        exit(1)
    }

    let outputPath = \"%s\"
    print(\"DEBUG: Writing preview to: \\(outputPath)\")

    let url = URL(fileURLWithPath: outputPath)
    try? FileManager.default.createDirectory(
        at: url.deletingLastPathComponent(),
        withIntermediateDirectories: true)

    do {
        try data.write(to: url)
        let finalW = croppedCG.width
        let finalH = croppedCG.height
        print(\"Preview saved to: \\(outputPath)\")
        print(\"Size: \\(finalW)x\\(finalH)\")
        let styleName = kInterfaceStyle == .dark ? \"dark\"
            : kInterfaceStyle == .light ? \"light\" : \"none\"
        print(\"Style: \\(styleName)\")
    } catch {
        print(\"ERROR: Failed to write preview: \\(error)\")
        exit(1)
    }

    exit(0)
}
" output-path))

(defun swiftui-preview-core-generate-host-swift
    (preview-body imports
     &optional filename color-scheme testable-module use-testable live
     output-path)
  "Generate PreviewHostApp.swift content with color scheme support.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
FILENAME is optional source file name for comment.
COLOR-SCHEME is \"dark\", \"light\", or nil.
TESTABLE-MODULE is the module name for @testable import.
USE-TESTABLE non-nil to use @testable on TESTABLE-MODULE.
LIVE non-nil generates a live app (no snapshot, no exit) for
interactive use in the simulator.
OUTPUT-PATH is the absolute path where the snapshot PNG will be written.
Embedded as a string literal in the generated Swift code."
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
import UIKit

@main
struct PreviewHostApp: App {
    init() {
        // Ensure UINavigationBar renders visibly in the minimal preview host.
        // Without this, navigation bars may be fully transparent because the
        // preview app has no global appearance configuration.
        let appearance = UINavigationBarAppearance()
        appearance.configureWithDefaultBackground()
        UINavigationBar.appearance().standardAppearance = appearance
        UINavigationBar.appearance().compactAppearance = appearance
        UINavigationBar.appearance().scrollEdgeAppearance = appearance
    }

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
    init() {
        let appearance = UINavigationBarAppearance()
        appearance.configureWithDefaultBackground()
        UINavigationBar.appearance().standardAppearance = appearance
        UINavigationBar.appearance().compactAppearance = appearance
        UINavigationBar.appearance().scrollEdgeAppearance = appearance
    }

    var body: some Scene {
        WindowGroup {
            PreviewContent()
                .onAppear {
                    // Allow enough time for @StateObject init, .task {},
                    // .onAppear data loading, and at least one layout pass
                    // with populated content before taking the snapshot.
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) {
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
              (swiftui-preview-core--snapshot-code
               (or output-path "/tmp/swift-development-preview.png"))))))

(defun swiftui-preview-core-write-host-app
    (preview-body imports temp-dir
     &optional filename color-scheme testable-module use-testable live
     output-path)
  "Write PreviewHostApp.swift to TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
FILENAME is optional source file name for comment.
COLOR-SCHEME, TESTABLE-MODULE, USE-TESTABLE passed to generate.
LIVE non-nil generates a live (interactive) app.
OUTPUT-PATH is the absolute path for the snapshot PNG.
Returns path to written file."
  (let ((host-content (swiftui-preview-core-generate-host-swift
                       preview-body imports filename
                       color-scheme testable-module use-testable
                       live output-path))
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
