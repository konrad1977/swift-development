;;; swiftui-preview-dynamic.el --- Dynamic target injection preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: swift, swiftui, preview, ios

;;; Commentary:

;; This module provides zero-config SwiftUI preview functionality using
;; dynamic target injection.  It extracts #Preview blocks from Swift files
;; and creates a temporary PreviewHost target in the Xcode project to build
;; and capture previews.
;;
;; Features:
;; - Zero configuration required
;; - Works with existing #Preview macros
;; - Supports Xcode projects, workspaces, and SPM packages
;; - Fast builds (only builds required modules)
;; - External screenshot capture via simctl
;;
;; Usage:
;; M-x swiftui-preview-dynamic-generate

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'swift-async nil t)
(require 'swift-project-settings nil t)
(require 'swiftui-preview-spm nil t)
(require 'swiftui-preview-standalone nil t)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--detect-preview-definitions "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")
(declare-function swiftui-preview-setup-check "swiftui-preview-setup")
(declare-function swiftui-preview-setup-wizard "swiftui-preview-setup")
(declare-function swiftui-preview-spm-generate "swiftui-preview-spm")
(declare-function swiftui-preview-standalone-generate "swiftui-preview-standalone")
(declare-function swift-async-run "swift-async")
(declare-function swift-async-run-sync "swift-async")
(declare-function ios-simulator-simulator-identifier "ios-simulator")
(declare-function swift-development-compile-app "swift-development")
(declare-function swift-project-settings-get-product-name "swift-project-settings")
(declare-function swift-project-settings-get-deployment-target "swift-project-settings")
(declare-function swift-project-settings-get-swift-module-name "swift-project-settings")
(declare-function swift-project-settings-get-build-dir "swift-project-settings")
(declare-function xcode-project-project-root "xcode-project")

;; External variables
(defvar xcode-build-config-enable-testability)

(defgroup swiftui-preview-dynamic nil
  "Dynamic target injection preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-dynamic-")

(defcustom swiftui-preview-dynamic-simulator "iPhone 16 Pro"
  "Default simulator name for previews."
  :type 'string
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-capture-delay 1.0
  "Delay in seconds after app launch before capturing screenshot.
Lower values are faster but may capture before the view fully renders.
Default 1.0 works well for most views; increase for complex views."
  :type 'number
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-keep-target nil
  "If non-nil, keep the injected PreviewHost target after capture.
Useful for debugging build issues."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-verbose nil
  "If non-nil, show verbose output during preview generation."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

;;;###autoload
(defun swiftui-preview-dynamic-toggle-verbose ()
  "Toggle verbose mode for SwiftUI preview generation."
  (interactive)
  (setq swiftui-preview-dynamic-verbose (not swiftui-preview-dynamic-verbose))
  (message "SwiftUI Preview verbose mode: %s"
           (if swiftui-preview-dynamic-verbose "ON" "OFF")))

(defcustom swiftui-preview-save-location 'project
  "Where to save preview images.
'project = .swift-development/previews/ in project root
'temp = system temp directory (lost on restart)
'custom = use `swiftui-preview-custom-save-directory'"
  :type '(choice (const :tag "Project directory" project)
                 (const :tag "Temporary" temp)
                 (const :tag "Custom directory" custom))
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-custom-save-directory nil
  "Custom directory for saving previews when `swiftui-preview-save-location' is 'custom."
  :type '(choice (const nil) directory)
  :group 'swiftui-preview-dynamic)

(defvar swiftui-preview-dynamic--scripts-dir
  (expand-file-name "scripts" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing Ruby helper scripts.")

(defvar swiftui-preview-dynamic--temp-dir nil
  "Temporary directory for current preview generation.")

(defvar swiftui-preview-dynamic--current-project nil
  "Current project path being previewed.")

(defvar swiftui-preview-dynamic--current-source-file nil
  "Source file path currently being previewed.")



(defvar swiftui-preview-dynamic--pending-preview-buffer nil
  "Buffer to generate preview for after testability rebuild.")

;;; Project Type Detection



(defun swiftui-preview-dynamic--detect-project-type ()
  "Detect the project type for current file.
Returns a plist with :type and :path keys.
Type is one of: 'xcode-project, 'xcode-workspace, 'spm-package, 'standalone."
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               default-directory))
        (result nil))
    ;; Search upward
    (while (and dir (not result) (not (equal dir "/")))
      ;; Check for workspace first (but not inside .xcodeproj)
      (let ((workspaces (directory-files dir t "\\.xcworkspace$")))
        ;; Filter out workspaces inside .xcodeproj
        (setq workspaces (cl-remove-if
                          (lambda (ws)
                            (string-match-p "\\.xcodeproj/" ws))
                          workspaces))
        (when workspaces
          (setq result (list :type 'xcode-workspace
                            :path (car workspaces)
                            :root dir))))
      
      ;; Check for project
      (unless result
        (let ((projects (directory-files dir t "\\.xcodeproj$")))
          (when projects
            (setq result (list :type 'xcode-project
                              :path (car projects)
                              :root dir)))))
      
      ;; Check for Package.swift - but continue searching for workspace/project above
      (unless result
        (let ((package (expand-file-name "Package.swift" dir)))
          (when (file-exists-p package)
            ;; Found SPM package, but check if there's a workspace above first
            (let ((parent dir)
                  (found-workspace nil))
              (while (and parent (not found-workspace)
                          (not (string= parent "/")))
                (setq parent (file-name-directory (directory-file-name parent)))
                (let ((workspaces (directory-files parent t "\\.xcworkspace$")))
                  (setq workspaces (cl-remove-if
                                    (lambda (ws)
                                      (string-match-p "\\.xcodeproj/" ws))
                                    workspaces))
                  (when workspaces
                    (setq found-workspace (car workspaces)))))
              (if found-workspace
                  ;; Use workspace instead of SPM package
                  (setq result (list :type 'xcode-workspace
                                    :path found-workspace
                                    :root (file-name-directory found-workspace)))
                ;; No workspace above, use SPM package
                (setq result (list :type 'spm-package
                                  :path package
                                  :root dir)))))))
      
      (setq dir (file-name-directory (directory-file-name dir))))
    
    ;; If nothing found, it's standalone
    (or result (list :type 'standalone :path nil :root nil))))

(defun swiftui-preview-dynamic--detect-module ()
  "Detect module name from current file path.
Returns module name string or nil.
First tries Ruby script (detects sub-module from file path), then
falls back to swift-project-settings (returns main app target)."
  ;; Try Ruby script first - it detects the correct sub-module (e.g., KYC not Bruce)
  (or (when-let* ((file (buffer-file-name))
                  (script (expand-file-name "preview-helper.rb" swiftui-preview-dynamic--scripts-dir)))
        (let ((output (shell-command-to-string
                       (format "ruby %s detect-module %s 2>/dev/null"
                               (shell-quote-argument script)
                               (shell-quote-argument file)))))
          (let ((module (string-trim output)))
            (unless (string-empty-p module)
              module))))
      ;; Fall back to swift-project-settings (main app target name)
      (when (fboundp 'swift-project-settings-get-product-name)
        (let* ((project-root (or (when (fboundp 'xcode-project-project-root)
                                   (xcode-project-project-root))
                                 (locate-dominating-file default-directory ".xcodeproj")
                                 (locate-dominating-file default-directory ".xcworkspace")))
               (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                         xcode-project--current-xcode-scheme)))
          (when (and project-root scheme)
            (swift-project-settings-get-product-name project-root scheme))))))

(defun swiftui-preview-dynamic--extract-imports ()
  "Extract all import statements from current buffer.
Returns a list of imported module names."
  (save-excursion
    (goto-char (point-min))
    (let ((imports '()))
      (while (re-search-forward "^import[[:space:]]+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
        (push (match-string 1) imports))
      (delete-dups (nreverse imports)))))

;;; Preview Host Generation

(defun swiftui-preview-dynamic--generate-preview-host (preview-body imports temp-dir &optional testable-module)
  "Generate PreviewHostApp.swift in TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of modules to import.
TESTABLE-MODULE is the module to import with @testable (for internal types).
@testable is used when testability is enabled (xcode-build-config-enable-testability)."
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         ;; Generate import statements
         ;; Use @testable when testability is enabled (either via xcodebuild or swiftc with testability build)
         (use-testable (and testable-module
                            (bound-and-true-p xcode-build-config-enable-testability)))
         (import-statements (mapconcat
                             (lambda (imp)
                               (if (and use-testable (string= imp testable-module))
                                   (format "@testable import %s" imp)
                                 (format "import %s" imp)))
                             imports "\n"))
         ;; Indent preview body for proper Swift formatting
         (indented-body (replace-regexp-in-string
                         "^" "            "
                         preview-body))
         (host-content (format "// Auto-generated PreviewHost for %s
// Generated by swift-development swiftui-preview-dynamic

%s
import UIKit

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

// MARK: - Snapshot Logic

func snapshotPreview() {
    let previewView = PreviewContent()
    
    // Create hosting controller to measure and render the view
    let hostingController = UIHostingController(rootView: previewView.fixedSize())
    
    // Measure the intrinsic size
    let targetSize = hostingController.sizeThatFits(in: CGSize(width: CGFloat.infinity, height: CGFloat.infinity))
    
    // Add padding around the content
    let padding: CGFloat = 40
    let finalSize = CGSize(
        width: targetSize.width + padding * 2,
        height: targetSize.height + padding * 2
    )
    
    // Create the final view with padding and clear background
    let paddedView = previewView
        .fixedSize()
        .padding(padding)
        .frame(width: finalSize.width, height: finalSize.height, alignment: .center)
        .background(Color.clear)
    
    let finalController = UIHostingController(rootView: paddedView)
    finalController.view.frame = CGRect(origin: .zero, size: finalSize)
    finalController.view.backgroundColor = .clear
    finalController.view.layoutIfNeeded()
    
    // Render to image with transparency
    let format = UIGraphicsImageRendererFormat()
    format.scale = UIScreen.main.scale
    format.opaque = false  // Enable transparency
    
    let renderer = UIGraphicsImageRenderer(size: finalSize, format: format)
    let image = renderer.image { context in
        finalController.view.drawHierarchy(in: CGRect(origin: .zero, size: finalSize), afterScreenUpdates: true)
    }
    
    // Save to file
    guard let data = image.pngData() else {
        print(\"ERROR: Failed to create PNG data\")
        exit(1)
    }
    
    // Get output path from environment or use default
    let outputPath = ProcessInfo.processInfo.environment[\"PREVIEW_OUTPUT_PATH\"] 
        ?? \"/tmp/swift-development-preview.png\"
    
    let url = URL(fileURLWithPath: outputPath)
    try? FileManager.default.createDirectory(at: url.deletingLastPathComponent(), withIntermediateDirectories: true)
    
    do {
        try data.write(to: url)
        print(\"Preview saved to: \\(outputPath)\")
        print(\"Size: \\(Int(finalSize.width))x\\(Int(finalSize.height))\")
    } catch {
        print(\"ERROR: Failed to write preview: \\(error)\")
        exit(1)
    }
    
    exit(0)
}
" filename import-statements indented-body))
         (host-file (expand-file-name "PreviewHostApp.swift" temp-dir)))
    
    ;; Ensure temp dir exists
    (make-directory temp-dir t)
    
    ;; Write host file
    (with-temp-file host-file
      (insert host-content))
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Generated PreviewHost in %s" temp-dir)
      (message "[Preview] Preview body (first 200 chars): %s"
               (substring preview-body 0 (min 200 (length preview-body)))))
    
    host-file))

;;; Ruby Script Interaction

(defun swiftui-preview-dynamic--run-ruby-script (script-name &rest args)
  "Run Ruby script SCRIPT-NAME with ARGS.
Returns parsed JSON result or signals error."
  (let* ((script (expand-file-name script-name swiftui-preview-dynamic--scripts-dir))
         (cmd (mapconcat #'shell-quote-argument (cons "ruby" (cons script args)) " "))
         ;; Capture stderr separately when verbose, otherwise discard it
         (stderr-file (when swiftui-preview-dynamic-verbose
                        (make-temp-file "preview-stderr-")))
         (full-cmd (if stderr-file
                       (concat cmd " 2>" (shell-quote-argument stderr-file))
                     (concat cmd " 2>/dev/null")))
         (output (shell-command-to-string full-cmd))
         (json-array-type 'list)
         (json-output nil))
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Running: %s" cmd))
    
    ;; Show stderr output in verbose mode
    (when (and stderr-file (file-exists-p stderr-file))
      (let ((stderr-content (with-temp-buffer
                              (insert-file-contents stderr-file)
                              (buffer-string))))
        (unless (string-empty-p stderr-content)
          (message "[Preview] Script log:\n%s" stderr-content)))
      (delete-file stderr-file))
    
    ;; Try to parse JSON from stdout (which should only contain JSON now)
    (condition-case nil
        (setq json-output (json-read-from-string output))
      (error
       (when swiftui-preview-dynamic-verbose
         (message "[Preview] Script output (not JSON): %s" output))
       nil))
    
    (if (and json-output (eq (alist-get 'success json-output) t))
        json-output
      (let ((error-msg (or (alist-get 'error json-output) output)))
        (error "Ruby script failed: %s" error-msg)))))

(defun swiftui-preview-dynamic--inject-target (project-path preview-dir module-name imports &optional source-file)
  "Inject PreviewHost target into PROJECT-PATH.
PREVIEW-DIR contains the generated preview host files.
MODULE-NAME is the module to depend on.
IMPORTS is list of modules to import.
SOURCE-FILE is the original Swift file being previewed.
Returns the injection result including source_target if found."
  (let ((args (list "--project" project-path
                    "--preview-dir" preview-dir)))
    (when module-name
      (setq args (append args (list "--module" module-name))))
    (when imports
      (setq args (append args (list "--imports" (mapconcat #'identity imports ",")))))
    (when source-file
      (setq args (append args (list "--source-file" source-file))))
    (when swiftui-preview-dynamic-verbose
      (setq args (append args (list "--verbose"))))
    
    (apply #'swiftui-preview-dynamic--run-ruby-script "preview-inject-target.rb" args)))

(defun swiftui-preview-dynamic--cleanup-target (project-path)
  "Remove PreviewHost target from PROJECT-PATH."
  (condition-case err
      (swiftui-preview-dynamic--run-ruby-script
       "preview-cleanup-target.rb"
       "--project" project-path)
    (error
     (when swiftui-preview-dynamic-verbose
       (message "[Preview] Cleanup warning: %s" (error-message-string err))))))

;;; Simulator Interaction

(defun swiftui-preview-dynamic--find-simulator ()
  "Find simulator UDID to use for preview.
Tries: selected simulator, default simulator, first booted."
  (or
   ;; Try ios-simulator if available
   (and (fboundp 'ios-simulator-simulator-identifier)
        (ios-simulator-simulator-identifier))
   ;; Try to find by name
   (let* ((script (expand-file-name "preview-helper.rb" swiftui-preview-dynamic--scripts-dir))
          (output (shell-command-to-string
                   (format "ruby %s find-simulator %s 2>/dev/null"
                           (shell-quote-argument script)
                           (shell-quote-argument swiftui-preview-dynamic-simulator)))))
     (let ((udid (string-trim output)))
       (unless (string-empty-p udid)
         udid)))
   ;; Try first booted
   (let* ((script (expand-file-name "preview-helper.rb" swiftui-preview-dynamic--scripts-dir))
          (output (shell-command-to-string
                   (format "ruby %s first-booted 2>/dev/null"
                           (shell-quote-argument script)))))
     (let ((udid (string-trim output)))
       (unless (string-empty-p udid)
         udid)))))

(defun swiftui-preview-dynamic--ensure-simulator-booted (udid)
  "Ensure simulator with UDID is booted."
  (let* ((json-array-type 'list)  ; Parse JSON arrays as lists, not vectors
         (json-output (if (fboundp 'swift-async-run-sync)
                          (swift-async-run-sync
                           '("xcrun" "simctl" "list" "devices" "-j")
                           :timeout 5 :parse-json t)
                        (json-read-from-string
                         (shell-command-to-string
                          "xcrun simctl list devices -j 2>/dev/null")))))
    (when json-output
      (let ((booted nil))
        ;; Check if device is booted
        (catch 'found
          (dolist (runtime (alist-get 'devices json-output))
            ;; Handle both list and vector device arrays
            (let ((devices (cdr runtime)))
              (when (vectorp devices)
                (setq devices (append devices nil)))
              (dolist (device devices)
                (when (and (equal (alist-get 'udid device) udid)
                           (equal (alist-get 'state device) "Booted"))
                  (setq booted t)
                  (throw 'found t))))))
        (unless booted
          (message "Booting simulator...")
          (swiftui-preview-dynamic--simctl (list "boot" udid))
          (sleep-for 2))))))

;;; Build and Capture

(defun swiftui-preview-dynamic--get-output-path (project-root filename)
  "Get output path for preview image.
PROJECT-ROOT is the project directory.
FILENAME is the base name for the preview."
  (let ((preview-name (file-name-sans-extension filename)))
    (pcase swiftui-preview-save-location
      ('project
       (let ((preview-dir (expand-file-name ".swift-development/previews" project-root)))
         (make-directory preview-dir t)
         (expand-file-name (format "%s.png" preview-name) preview-dir)))
      ('temp
       (expand-file-name (format "preview-%s.png" preview-name) temporary-file-directory))
      ('custom
       (if swiftui-preview-custom-save-directory
           (progn
             (make-directory swiftui-preview-custom-save-directory t)
             (expand-file-name (format "%s.png" preview-name) swiftui-preview-custom-save-directory))
         (expand-file-name (format "preview-%s.png" preview-name) temporary-file-directory))))))

(defvar swiftui-preview-dynamic--build-callback nil
  "Callback function for async build completion.")

(defvar swiftui-preview-dynamic--build-context nil
  "Context for async build (simulator-udid, output-path, etc).")


(defun swiftui-preview-dynamic--project-root-and-scheme ()
  "Get current project root and scheme as a cons cell (ROOT . SCHEME).
Returns nil if not available."
  (let ((root (when (fboundp 'xcode-project-project-root)
                (xcode-project-project-root)))
        (scheme (when (boundp 'xcode-project--current-xcode-scheme)
                  xcode-project--current-xcode-scheme)))
    (when (and root scheme)
      (cons root scheme))))

(defun swiftui-preview-dynamic--find-derived-data (project-path)
  "Find existing DerivedData directory for PROJECT-PATH.
Returns path to DerivedData or nil if not found.
Tries cached build-dir from settings first (deriving DerivedData root),
then checks project-local .build, then ~/Library/Developer/Xcode/DerivedData."
  ;; Try settings first - derive DerivedData root from cached target-build-dir
  (or
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (build-dir (when (fboundp 'swift-project-settings-get-build-dir)
                            (swift-project-settings-get-build-dir
                             (car info) (cdr info) "iphonesimulator"))))
     ;; build-dir is like .../Build/Products/Debug-iphonesimulator/
     ;; DerivedData root is 3 levels up
     (let ((derived-root (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (directory-file-name build-dir))))))))
       (when (and derived-root (file-directory-p derived-root))
         (when swiftui-preview-dynamic-verbose
           (message "[Preview] DerivedData from settings: %s" derived-root))
         derived-root)))
   ;; Fall back to filesystem scanning
   (let* ((project-dir (file-name-directory project-path))
          ;; Check for project-local .build (used by swift-development/Emacs builds)
          (local-build-dir (expand-file-name ".build" project-dir)))
     (if (file-directory-p local-build-dir)
         (progn
           (when swiftui-preview-dynamic-verbose
             (message "[Preview] Found local .build directory"))
           local-build-dir)
       ;; Fall back to Xcode DerivedData
       (let* ((project-name (file-name-sans-extension
                             (file-name-nondirectory project-path)))
              (xcode-derived-data
               (expand-file-name
                (if (boundp 'xcode-build-config-xcode-cache-dir)
                    xcode-build-config-xcode-cache-dir
                  "~/Library/Developer/Xcode/DerivedData")))
              (matching-dirs nil))
         (when (file-directory-p xcode-derived-data)
           (setq matching-dirs
                 (directory-files xcode-derived-data t
                                 (concat "^" (regexp-quote project-name) "-"))))
         (when matching-dirs
           (car (sort matching-dirs
                      (lambda (a b)
                        (time-less-p (nth 5 (file-attributes b))
                                    (nth 5 (file-attributes a))))))))))))

(defun swiftui-preview-dynamic--find-products-dir (derived-data)
  "Find the Build/Products directory with built modules in DERIVED-DATA.
Returns the path to the iphonesimulator products directory.
Tries cached build-dir from settings first, falls back to scanning."
  (or
   ;; Try settings first (returns the exact products dir)
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (build-dir (when (fboundp 'swift-project-settings-get-build-dir)
                            (swift-project-settings-get-build-dir
                             (car info) (cdr info) "iphonesimulator"))))
     (when (file-directory-p build-dir)
       (when swiftui-preview-dynamic-verbose
         (message "[Preview] Products dir from settings: %s" build-dir))
       build-dir))
   ;; Fall back to scanning
   (let ((products-base (expand-file-name "Build/Products" derived-data)))
     (when (file-directory-p products-base)
       ;; Look for any *-iphonesimulator directory, preferring Debug
       (let ((dirs (directory-files products-base t "-iphonesimulator$")))
         (or
          ;; Prefer Debug-iphonesimulator
          (cl-find-if (lambda (d) (string-match-p "Debug-iphonesimulator$" d)) dirs)
          ;; Or Debug (something)-iphonesimulator
          (cl-find-if (lambda (d) (string-match-p "Debug.*-iphonesimulator$" d)) dirs)
          ;; Or just the first one
          (car dirs)))))))

(defun swiftui-preview-dynamic--detect-ios-version (products-dir)
  "Detect iOS deployment target from saved settings or built modules.
Tries swift-project-settings first, falls back to scanning swiftmodule binaries.
Returns version string like \"26.0\" or \"17.0\" as fallback."
  (or
   ;; Try settings first (fast, reliable)
   (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
               (version (when (fboundp 'swift-project-settings-get-deployment-target)
                          (swift-project-settings-get-deployment-target (car info) (cdr info)))))
     (when swiftui-preview-dynamic-verbose
       (message "[Preview] iOS version from settings: %s" version))
     version)
   ;; Fall back to scanning swiftmodule binary
   (when products-dir
     (let* ((info (swiftui-preview-dynamic--project-root-and-scheme))
            (product-name (when (and info (fboundp 'swift-project-settings-get-product-name))
                            (swift-project-settings-get-product-name (car info) (cdr info))))
            (swiftmodule-dir (or (when product-name
                                   (let ((path (expand-file-name (concat product-name ".swiftmodule") products-dir)))
                                     (when (file-directory-p path) path)))
                                 (car (directory-files products-dir t "\\.swiftmodule$")))))
       (when swiftmodule-dir
         (let ((module-file (expand-file-name "arm64-apple-ios-simulator.swiftmodule" swiftmodule-dir)))
           (when (file-exists-p module-file)
             (let ((output (shell-command-to-string
                            (format "strings %s 2>/dev/null | grep -oE 'ios[0-9]+\\.[0-9]+' | head -1"
                                    (shell-quote-argument module-file)))))
               (when (string-match "ios\\([0-9]+\\.[0-9]+\\)" output)
                 (match-string 1 output))))))))
   ;; Final fallback
   "17.0"))


(defun swiftui-preview-dynamic--recompile-stale-sources (object-files source-file products-dir derived-data ios-version)
  "Recompile stale .o files in OBJECT-FILES using swiftc -c.
SOURCE-FILE is the Swift file being previewed.
Returns a new list of object files with stale ones replaced by fresh copies.
Only recompiles files whose .swift source is newer than the .o file."
  (let* ((source-base (file-name-sans-extension
                       (file-name-nondirectory source-file)))
         (target-triple (format "arm64-apple-ios%s-simulator" ios-version))
         (sdk-path (string-trim (shell-command-to-string
                                  "xcrun --sdk iphonesimulator --show-sdk-path")))
         (source-mtime (file-attribute-modification-time
                        (file-attributes source-file)))
         ;; Detect module name for the .o file being recompiled
         ;; Try settings first, then derive from .o path
         (module-name (or (when-let* ((info (swiftui-preview-dynamic--project-root-and-scheme)))
                            (when (fboundp 'swift-project-settings-get-product-name)
                              (swift-project-settings-get-product-name (car info) (cdr info))))
                          "testpreview"))
         (result '()))
    (dolist (o-file object-files)
      (let ((o-base (file-name-sans-extension (file-name-nondirectory o-file))))
        (if (and (string= o-base source-base)
                 source-mtime
                 (time-less-p (file-attribute-modification-time
                               (file-attributes o-file))
                              source-mtime))
            ;; This .o is stale - recompile the source file
            (let* ((fresh-o (expand-file-name
                             (concat source-base ".o")
                             swiftui-preview-dynamic--temp-dir))
                   (compile-args
                    (append
                     (list "xcrun" "--sdk" "iphonesimulator"
                           "swiftc" "-c"
                           "-parse-as-library"
                           source-file
                           "-o" fresh-o
                           "-module-name" module-name
                           "-sdk" sdk-path
                           "-target" target-triple
                           "-I" products-dir
                           "-F" products-dir
                           "-F" (concat sdk-path "/System/Library/Frameworks"))
                     ;; Enable testability if configured
                     (when (bound-and-true-p xcode-build-config-enable-testability)
                       (list "-enable-testing"))
                     ;; Add GeneratedModuleMaps if available
                     (let ((gmm (expand-file-name
                                 "Build/Intermediates.noindex/GeneratedModuleMaps-iphonesimulator"
                                 derived-data)))
                       (when (file-directory-p gmm)
                         (list "-I" gmm))))))
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Recompiling stale: %s (module: %s)" source-base module-name))
              (let ((exit-code (apply #'call-process
                                      (car compile-args) nil
                                      (get-buffer-create "*SwiftUI Preview Build*")
                                      nil (cdr compile-args))))
                (if (= exit-code 0)
                    (progn
                      (when swiftui-preview-dynamic-verbose
                        (message "[Preview] Recompiled %s.o successfully" source-base))
                      (push fresh-o result))
                  (when swiftui-preview-dynamic-verbose
                    (message "[Preview] Recompile failed for %s, using stale .o" source-base))
                  (push o-file result))))
          ;; Not stale or not the current file - keep as is
          (push o-file result))))
    (nreverse result)))

(defun swiftui-preview-dynamic--ensure-testability-build (callback)
  "Ensure project is built with testability, then call CALLBACK.
If testability is already enabled and build exists, calls CALLBACK immediately.
Otherwise, enables testability, rebuilds, and then calls CALLBACK."
  (require 'xcode-build-config nil t)
  
  (if (bound-and-true-p xcode-build-config-enable-testability)
      ;; Already enabled, just call callback
      (funcall callback)
    ;; Need to enable and rebuild
    (message "Enabling testability for SwiftUI preview...")
    (setq xcode-build-config-enable-testability t)
    
    ;; Store callback for after build completes
    (setq swiftui-preview-dynamic--pending-preview-buffer (current-buffer))
    
    ;; Add a hook to run preview after build completes
    (add-hook 'compilation-finish-functions
              #'swiftui-preview-dynamic--after-testability-build)
    
    ;; Trigger rebuild
    (if (fboundp 'swift-development-compile-app)
        (progn
          (message "Rebuilding with testability enabled... (preview will start after build)")
          (swift-development-compile-app))
      (remove-hook 'compilation-finish-functions
                   #'swiftui-preview-dynamic--after-testability-build)
      (user-error "swift-development-compile-app not available. Please rebuild manually with testability"))))

(defun swiftui-preview-dynamic--after-testability-build (buffer status)
  "Hook called after testability build completes.
BUFFER is the compilation buffer, STATUS is the exit status string."
  ;; Remove ourselves from the hook
  (remove-hook 'compilation-finish-functions
               #'swiftui-preview-dynamic--after-testability-build)
  
  (if (string-match-p "finished" status)
      ;; Build succeeded - generate preview
      (progn
        (message "Testability build complete. Generating preview...")
        (when (buffer-live-p swiftui-preview-dynamic--pending-preview-buffer)
          (with-current-buffer swiftui-preview-dynamic--pending-preview-buffer
            ;; Small delay to ensure build output is fully processed
            (run-at-time 0.5 nil #'swiftui-preview-dynamic-generate))))
    ;; Build failed
    (message "Build failed. Fix errors and try preview again.")))

(defcustom swiftui-preview-dynamic-use-xcodebuild nil
  "If non-nil, use xcodebuild with injected target.
If nil, use direct swiftc compilation with existing DerivedData modules.
Set to nil if xcodeproj gem has issues with your Xcode version."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--build-async (project-path simulator-udid is-workspace callback)
  "Build PreviewHost scheme asynchronously.
PROJECT-PATH is the project or workspace path.
SIMULATOR-UDID is the target simulator.
IS-WORKSPACE is non-nil if PROJECT-PATH is a workspace.
CALLBACK is called with (app-path) on success or (nil error-msg) on failure."
  (let* ((existing-derived-data (swiftui-preview-dynamic--find-derived-data project-path)))
    
    ;; Choose build method based on configuration and available DerivedData
    (if (and (not swiftui-preview-dynamic-use-xcodebuild) existing-derived-data)
        ;; Use direct swiftc compilation
        (swiftui-preview-dynamic--build-with-swiftc existing-derived-data simulator-udid callback)
      ;; Use xcodebuild (original method)
      (swiftui-preview-dynamic--build-with-xcodebuild 
       project-path simulator-udid is-workspace existing-derived-data callback))))

(defun swiftui-preview-dynamic--build-with-xcodebuild (project-path simulator-udid is-workspace existing-derived-data callback)
  "Build using xcodebuild with injected PreviewHost target."
  (let* ((derived-data (or existing-derived-data
                          (expand-file-name "DerivedData" swiftui-preview-dynamic--temp-dir)))
         (build-args (list "xcodebuild" "build"
                           (if is-workspace "-workspace" "-project") project-path
                           "-scheme" "PreviewHost"
                           "-configuration" "Debug"
                           "-destination" (format "platform=iOS Simulator,id=%s" simulator-udid)
                           "-derivedDataPath" derived-data
                           "ONLY_ACTIVE_ARCH=YES"
                           "ENABLE_TESTABILITY=YES"
                           "GCC_TREAT_WARNINGS_AS_ERRORS=NO"))
         (output-buffer (get-buffer-create "*SwiftUI Preview Build*")))
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Using DerivedData: %s%s"
               derived-data
               (if existing-derived-data " (existing)" " (new)"))
      (message "[Preview] Build command: %s" (string-join build-args " ")))
    
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Building PreviewHost with xcodebuild...\n\n")))
    
    (setq swiftui-preview-dynamic--build-callback callback)
    (setq swiftui-preview-dynamic--build-context
          (list :derived-data derived-data))
    
    (let ((process (make-process
                    :name "swiftui-preview-build"
                    :buffer output-buffer
                    :command build-args
                    :sentinel #'swiftui-preview-dynamic--build-sentinel)))
      (message "Building preview with xcodebuild... (async)")
      process)))

(defcustom swiftui-preview-dynamic-excluded-modules '()
  "Modules to exclude from preview linking.
These are typically third-party SDKs that require external frameworks
not available in the simulator environment (e.g., payment SDKs, analytics).
Set this in your .dir-locals.el for project-specific exclusions.

Example:
  ((swift-mode . ((swiftui-preview-dynamic-excluded-modules
                   . (\"Adyen\" \"GoogleSignIn\" \"Sentry\")))))"
  :type '(repeat string)
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--find-intermediates-dir (derived-data)
  "Find the Intermediates.noindex directory with object files in DERIVED-DATA.
Returns list of paths to Objects-normal/arm64 directories.
Structure: Intermediates.noindex/PROJECT.build/Debug-iphonesimulator/TARGET.build/Objects-normal/arm64/"
  (let ((intermediates-base (expand-file-name "Build/Intermediates.noindex" derived-data))
        (result '()))
    (when (file-directory-p intermediates-base)
      ;; Find PROJECT.build directories
      (dolist (project-build-dir (directory-files intermediates-base t "\\.build$"))
        (when (file-directory-p project-build-dir)
          ;; Find Debug-iphonesimulator (or similar) directories
          (dolist (sim-dir (directory-files project-build-dir t "-iphonesimulator$"))
            (when (file-directory-p sim-dir)
              ;; Find TARGET.build directories inside
              (dolist (target-build-dir (directory-files sim-dir t "\\.build$"))
                (when (file-directory-p target-build-dir)
                  (let ((objects-dir (expand-file-name "Objects-normal/arm64" target-build-dir)))
                    (when (file-directory-p objects-dir)
                      (push objects-dir result))))))))))
    (nreverse result)))


(defun swiftui-preview-dynamic--is-app-entry-p (object-file)
  "Check if OBJECT-FILE is an app entry point that should be excluded.
Detects @main by checking for _main symbol in the object file."
  (let ((name (file-name-sans-extension (file-name-nondirectory object-file))))
    ;; Quick name-based check first (most App entry points end with App)
    (when (string-suffix-p "App" name)
      ;; Verify by checking for _main symbol
      (with-temp-buffer
        (call-process "nm" nil t nil "-g" "-j" "-U" object-file)
        (goto-char (point-min))
        (re-search-forward "^_main$" nil t)))))

(defun swiftui-preview-dynamic--detect-external-frameworks (products-dir)
  "Detect binary frameworks in PRODUCTS-DIR that aren't available in the simulator.
Returns list of framework names (e.g., (\"Adyen3DS2\" \"Sentry\"))."
  (let ((frameworks '()))
    (when (file-directory-p products-dir)
      (dolist (fw (directory-files products-dir t "\\.framework$"))
        (push (file-name-sans-extension (file-name-nondirectory fw)) frameworks)))
    (nreverse frameworks)))

(defun swiftui-preview-dynamic--links-external-framework-p (object-file external-frameworks)
  "Check if OBJECT-FILE auto-links any of EXTERNAL-FRAMEWORKS.
Uses objdump to inspect LC_LINKER_OPTION entries."
  (when external-frameworks
    (with-temp-buffer
      (call-process "objdump" nil t nil "--macho" "--all-headers" object-file)
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward "string #2 \\(.+\\)" nil t))
          (when (member (match-string 1) external-frameworks)
            (setq found t)))
        found))))

(defun swiftui-preview-dynamic--collect-object-files (products-dir derived-data)
  "Collect .o files from PRODUCTS-DIR and DERIVED-DATA Intermediates directory.
For SPM packages, links all available .o files to resolve transitive dependencies,
but auto-excludes modules that depend on external binary frameworks.
For Xcode projects, .o files are in Intermediates (excluding app entry points).
Also excludes modules in `swiftui-preview-dynamic-excluded-modules'."
  (let ((object-files '())
        (external-frameworks (swiftui-preview-dynamic--detect-external-frameworks products-dir)))
    (when (and external-frameworks swiftui-preview-dynamic-verbose)
      (message "[Preview] External frameworks detected: %s" external-frameworks))
    
    ;; First try Products dir (SPM-style builds) - link all .o to resolve transitive deps
    (when (and products-dir (file-directory-p products-dir))
      (dolist (file (directory-files products-dir t "\\.o$"))
        (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
          (unless (or (member module-name swiftui-preview-dynamic-excluded-modules)
                      (swiftui-preview-dynamic--links-external-framework-p file external-frameworks))
            (push file object-files)))))
    
    ;; If no .o files found in Products, try Intermediates.noindex (Xcode project builds)
    (when (and (null object-files) derived-data)
      (let ((intermediates-dirs (swiftui-preview-dynamic--find-intermediates-dir derived-data)))
        (dolist (int-dir intermediates-dirs)
          (when (file-directory-p int-dir)
            (dolist (file (directory-files int-dir t "\\.o$"))
              (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
                (unless (or (member module-name swiftui-preview-dynamic-excluded-modules)
                            (swiftui-preview-dynamic--is-app-entry-p file))
                  (push file object-files))))))))
    
    (nreverse object-files)))

(defun swiftui-preview-dynamic--copy-app-resources (products-dir app-dir object-files)
  "Copy resource bundles and assets from PRODUCTS-DIR into APP-DIR.
Copies SPM resource bundles for linked modules (based on OBJECT-FILES)
and Assets.car from the built app if available.
Returns list of copied resource names."
  (let ((copied-resources '())
        (linked-modules (mapcar (lambda (f)
                                  (file-name-sans-extension (file-name-nondirectory f)))
                                object-files)))
    (when (file-directory-p products-dir)
      ;; Copy SPM resource bundles (.bundle directories)
      (dolist (item (directory-files products-dir t "\\.bundle$"))
        (let* ((bundle-name (file-name-nondirectory item))
               ;; Bundle names are like "ModuleName_ModuleName.bundle"
               ;; Extract the module name (first part before underscore)
               (module-name (car (split-string (file-name-sans-extension bundle-name) "_"))))
          ;; Only copy bundles for modules we're actually linking
          (when (or (member module-name linked-modules)
                    ;; Also check if any linked module starts with bundle's module name
                    ;; (handles cases like MyModule_MyModule.bundle matching MyModule.o)
                    (seq-find (lambda (m) (string-prefix-p module-name m)) linked-modules))
            (let ((dest (expand-file-name bundle-name app-dir)))
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Copying bundle: %s" bundle-name))
              (copy-directory item dest t t t)
              (push bundle-name copied-resources)))))

      ;; Copy Assets.car from the built app (compiled asset catalog)
      (dolist (built-app (directory-files products-dir t "\\.app$"))
        (let ((assets-car (expand-file-name "Assets.car" built-app)))
          (when (file-exists-p assets-car)
            (let ((dest (expand-file-name "Assets.car" app-dir)))
              (when swiftui-preview-dynamic-verbose
                (message "[Preview] Copying Assets.car from %s" (file-name-nondirectory built-app)))
              (copy-file assets-car dest t)
              (push "Assets.car" copied-resources))))))
    (nreverse copied-resources)))

(defun swiftui-preview-dynamic--build-with-swiftc (derived-data simulator-udid callback)
  "Build PreviewHost using swiftc directly with modules from DERIVED-DATA."
  (let* ((products-dir (swiftui-preview-dynamic--find-products-dir derived-data))
         (preview-swift (expand-file-name "PreviewHostApp.swift" swiftui-preview-dynamic--temp-dir))
         (app-dir (expand-file-name "PreviewHost.app" swiftui-preview-dynamic--temp-dir))
         (executable (expand-file-name "PreviewHost" app-dir))
         (output-buffer (get-buffer-create "*SwiftUI Preview Build*")))
    
    (if (not products-dir)
        (funcall callback nil "No built products found. Build your project first with M-x swift-development-compile-app")
      
      (when swiftui-preview-dynamic-verbose
        (message "[Preview] Using products from: %s" products-dir))
      
      ;; Create app bundle structure
      (make-directory app-dir t)
      
      ;; Create Info.plist
      (with-temp-file (expand-file-name "Info.plist" app-dir)
        (insert (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>CFBundleExecutable</key>
    <string>PreviewHost</string>
    <key>CFBundleIdentifier</key>
    <string>com.swift-development.preview-host</string>
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
</dict>
</plist>" (format-time-string "%s"))))
      
      ;; Collect object files from Products or Intermediates
      (let* ((raw-object-files (swiftui-preview-dynamic--collect-object-files products-dir derived-data))
             ;; Detect iOS version from built modules (needed for recompilation)
             (ios-version (swiftui-preview-dynamic--detect-ios-version products-dir))
             ;; Recompile stale source files to fresh .o files
             (object-files (if swiftui-preview-dynamic--current-source-file
                               (swiftui-preview-dynamic--recompile-stale-sources
                                raw-object-files
                                swiftui-preview-dynamic--current-source-file
                                products-dir derived-data ios-version)
                             raw-object-files))
             ;; Copy resource bundles for linked modules into app bundle
             (copied-resources (swiftui-preview-dynamic--copy-app-resources 
                                products-dir app-dir object-files))
             (target-triple (format "arm64-apple-ios%s-simulator" ios-version))
             (sdk-path (string-trim (shell-command-to-string 
                                     "xcrun --sdk iphonesimulator --show-sdk-path")))
             (toolchain-path (string-trim (shell-command-to-string
                                           "xcode-select -p")))
             ;; Detect if object files need profiling runtime
             (needs-profiling (cl-some
                               (lambda (f)
                                 (zerop (call-process "nm" nil nil nil "-g" f "-s" "__DATA" "__llvm_prf_cnts")))
                               object-files))
             ;; Detect if object files need thread sanitizer
             (needs-tsan (cl-some
                          (lambda (f)
                            (zerop (call-process "nm" nil nil nil "-g" f "-s" "__DATA" "__tsan_default_options")))
                          object-files))
             ;; Profile runtime library path
             (clang-rt-dir (car (directory-files
                                 (concat toolchain-path "/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang")
                                 t "^[0-9]" t)))
             ;; Find GeneratedModuleMaps for ObjC/mixed modules
             (generated-modulemaps-dir
              (let ((dir (expand-file-name 
                          "Build/Intermediates.noindex/GeneratedModuleMaps-iphonesimulator" 
                          derived-data)))
                (when (file-directory-p dir) dir)))
             ;; Find ObjC include dirs from SourcePackages (for transitive module deps)
             (objc-include-dirs
              (let ((checkouts (expand-file-name "SourcePackages/checkouts" derived-data))
                    (dirs '()))
                (when (file-directory-p checkouts)
                  (dolist (inc-dir (directory-files-recursively checkouts "^include$" t))
                    (when (file-directory-p inc-dir)
                      (push inc-dir dirs))))
                (nreverse dirs)))
             ;; Build with swiftc - compile and link in one step
             (build-args (append 
                          (list "xcrun" "--sdk" "iphonesimulator"
                                "swiftc"
                                "-parse-as-library"  ;; Required for @main attribute
                                preview-swift
                                "-o" executable
                                "-sdk" sdk-path
                                "-target" target-triple
                                ;; Module search paths
                                "-I" products-dir
                                ;; Framework search paths  
                                "-F" products-dir
                                "-F" (concat sdk-path "/System/Library/Frameworks")
                                ;; Library search paths for linking
                                "-L" products-dir
                                "-L" (concat toolchain-path "/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/iphonesimulator")
                                ;; System frameworks
                                "-framework" "SwiftUI"
                                "-framework" "UIKit"
                                "-framework" "Foundation"
                                "-framework" "Combine"
                                "-framework" "CoreGraphics"
                                "-framework" "CoreFoundation")
                          ;; Generated module maps for ObjC/mixed modules
                          (when generated-modulemaps-dir
                            (list "-I" generated-modulemaps-dir))
                          ;; ObjC include dirs for transitive module dependencies
                          (cl-mapcan (lambda (dir) (list "-Xcc" "-I" "-Xcc" dir))
                                     objc-include-dirs)
                          ;; Conditionally add thread sanitizer
                          (when needs-tsan '("-sanitize=thread"))
                          ;; Conditionally link profiling runtime
                          (when (and needs-profiling clang-rt-dir)
                            (list (expand-file-name "lib/darwin/libclang_rt.profile_iossim.a" clang-rt-dir)))
                          ;; Add all object files
                          object-files)))
        
        (when swiftui-preview-dynamic-verbose
          (message "[Preview] iOS version: %s" ios-version)
          (message "[Preview] Target: %s" target-triple)
          (message "[Preview] Object files: %s" object-files)
          (message "[Preview] Copied resources: %s" copied-resources)
          (message "[Preview] swiftc command: %s" (string-join build-args " ")))
        
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Building PreviewHost with swiftc...\n")
            (insert (format "Products dir: %s\n" products-dir))
            (insert (format "Object files: %s\n" (string-join (mapcar #'file-name-nondirectory object-files) ", ")))
            (insert (format "Resource bundles: %s\n\n" 
                            (if copied-resources 
                                (string-join copied-resources ", ")
                              "none")))))
        
        (setq swiftui-preview-dynamic--build-callback callback)
        (setq swiftui-preview-dynamic--build-context
              (list :derived-data derived-data
                    :app-path app-dir))
        
        (let ((process (make-process
                        :name "swiftui-preview-build"
                        :buffer output-buffer
                        :command build-args
                        :sentinel #'swiftui-preview-dynamic--swiftc-sentinel)))
          (message "Building preview with swiftc... (async)")
          process)))))

(defun swiftui-preview-dynamic--swiftc-sentinel (process event)
  "Handle swiftc build PROCESS completion EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((exit-code (process-exit-status process))
           (callback swiftui-preview-dynamic--build-callback)
           (context swiftui-preview-dynamic--build-context)
           (app-path (plist-get context :app-path)))
      
      (if (= exit-code 0)
          (progn
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] swiftc build succeeded: %s" app-path))
            (when callback
              (funcall callback app-path nil)))
        ;; Build failed
        (display-buffer (process-buffer process))
        (when callback
          (funcall callback nil "swiftc build failed. See *SwiftUI Preview Build* buffer"))))))

(defun swiftui-preview-dynamic--build-sentinel (process event)
  "Handle build PROCESS completion EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((exit-code (process-exit-status process))
           (callback swiftui-preview-dynamic--build-callback)
           (context swiftui-preview-dynamic--build-context)
           (derived-data (plist-get context :derived-data)))
      
      (if (= exit-code 0)
          ;; Find built app - search in common locations
          (let* ((products-dir (expand-file-name "Build/Products" derived-data))
                 (app-path
                  (or
                   ;; Try Debug first
                   (let ((p (expand-file-name "Debug-iphonesimulator/PreviewHost.app" products-dir)))
                     (when (file-directory-p p) p))
                   ;; Try any *-iphonesimulator directory
                   (car (seq-filter
                         (lambda (p)
                           (and (file-directory-p p)
                                (not (string-match-p "\\.dSYM" p))))
                         (directory-files-recursively
                          products-dir
                          "PreviewHost\\.app$"
                          t))))))
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] Built app found: %s" app-path))
            (when callback
              (funcall callback app-path nil)))
        ;; Build failed
        (display-buffer (process-buffer process))
        (when callback
          (funcall callback nil "Build failed. See *SwiftUI Preview Build* buffer"))))))



(defcustom swiftui-preview-dynamic-crop-screenshot t
  "If non-nil, crop screenshot to a centered square region.
This provides a cleaner preview focused on just the view content,
removing status bar, home indicator, and excess background."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-width 900
  "Width of the crop region in pixels.
Default 900 works well for most views on iPhone Pro (1206px width).
Increase for wider views, decrease for narrower components."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-height 1000
  "Height of the crop region in pixels.
Default 1000 works well for most views.
Increase for taller views, decrease for shorter components."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defcustom swiftui-preview-dynamic-crop-offset-y 750
  "Vertical offset from top for the crop region in pixels.
Default 750 centers the crop on an iPhone Pro screen (2622px height).
Adjust if your content is positioned differently."
  :type 'integer
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--crop-image (image-path)
  "Crop IMAGE-PATH to a centered square region around the content.
Uses sips (macOS built-in) to crop the image in place.
Returns IMAGE-PATH on success."
  (when swiftui-preview-dynamic-crop-screenshot
    (let* ((dimensions (shell-command-to-string
                        (format "sips -g pixelWidth -g pixelHeight %s 2>/dev/null"
                                (shell-quote-argument image-path))))
           (width (when (string-match "pixelWidth: \\([0-9]+\\)" dimensions)
                    (string-to-number (match-string 1 dimensions))))
           (height (when (string-match "pixelHeight: \\([0-9]+\\)" dimensions)
                     (string-to-number (match-string 1 dimensions)))))
      (when (and width height (> height 0) (> width 0))
        ;; Calculate crop parameters for a centered region
        (let* ((crop-width swiftui-preview-dynamic-crop-width)
               (crop-height swiftui-preview-dynamic-crop-height)
               ;; Center horizontally
               (crop-x (max 0 (/ (- width crop-width) 2)))
               ;; Use configured Y offset
               (crop-y swiftui-preview-dynamic-crop-offset-y)
               ;; Ensure we don't crop outside the image
               (actual-width (min crop-width (- width crop-x)))
               (actual-height (min crop-height (- height crop-y))))
          (when (and (> actual-width 100) (> actual-height 100))
            (when swiftui-preview-dynamic-verbose
              (message "[Preview] Cropping: %dx%d -> %dx%d (offset: %d,%d)"
                       width height actual-width actual-height crop-x crop-y))
            ;; Use sips with --cropOffset Y X followed by -c HEIGHT WIDTH
            (call-process "sips" nil nil nil
                          "--cropOffset" (number-to-string crop-y) (number-to-string crop-x)
                          "-c" (number-to-string actual-height) (number-to-string actual-width)
                          image-path))))))
  image-path)

(defcustom swiftui-preview-dynamic-show-simulator nil
  "If non-nil, show the Simulator window during preview capture.
When nil (default), capture runs headless for faster performance."
  :type 'boolean
  :group 'swiftui-preview-dynamic)

(defun swiftui-preview-dynamic--simctl (args)
  "Run simctl with ARGS synchronously.
Uses swift-async-run-sync if available, otherwise call-process."
  (if (fboundp 'swift-async-run-sync)
      (swift-async-run-sync (append '("xcrun" "simctl") args) :timeout 10)
    (zerop (apply #'call-process "xcrun" nil nil nil "simctl" args))))

(defun swiftui-preview-dynamic--capture (app-path simulator-udid output-path)
  "Install APP-PATH, launch, and wait for internal snapshot to OUTPUT-PATH.
The app renders the view internally with transparent background and saves to file.
Returns OUTPUT-PATH on success."
  (let ((bundle-id "com.swift-development.preview-host")
        (process-environment (cons (format "SIMCTL_CHILD_PREVIEW_OUTPUT_PATH=%s" output-path)
                                   process-environment)))
    
    ;; Terminate any existing instance and uninstall to avoid stale binary
    (swiftui-preview-dynamic--simctl (list "terminate" simulator-udid bundle-id))
    (swiftui-preview-dynamic--simctl (list "uninstall" simulator-udid bundle-id))
    
    ;; Ensure output directory exists
    (make-directory (file-name-directory output-path) t)
    
    ;; Delete old output file if exists
    (when (file-exists-p output-path)
      (delete-file output-path))
    
    ;; Install fresh
    (message "Installing preview app...")
    (unless (swiftui-preview-dynamic--simctl (list "install" simulator-udid app-path))
      (error "Failed to install preview app"))
    
    ;; Only open Simulator window if explicitly requested
    (when swiftui-preview-dynamic-show-simulator
      (if (fboundp 'swift-async-run-sync)
          (swift-async-run-sync '("open" "-a" "Simulator") :timeout 5)
        (call-process "open" nil nil nil "-a" "Simulator"))
      (sleep-for 1))
    
    ;; Launch app - environment variable SIMCTL_CHILD_PREVIEW_OUTPUT_PATH
    ;; is passed to the app as PREVIEW_OUTPUT_PATH
    ;; The app will render the view internally and save to this path
    (message "Launching preview app...")
    (swiftui-preview-dynamic--simctl (list "launch" simulator-udid bundle-id))
    
    ;; Wait for the app to generate the preview and exit
    ;; The app calls exit(0) after saving, so we poll for the file
    (message "Waiting for preview generation...")
    (let ((timeout 10.0)
          (poll-interval 0.2)
          (elapsed 0.0))
      (while (and (< elapsed timeout)
                  (not (file-exists-p output-path)))
        (sleep-for poll-interval)
        (setq elapsed (+ elapsed poll-interval)))
      
      (unless (file-exists-p output-path)
        ;; Fallback: try simctl screenshot if internal capture failed
        (message "Internal capture timed out, falling back to screenshot...")
        (swiftui-preview-dynamic--simctl
         (list "io" simulator-udid "screenshot" output-path))
        (when (file-exists-p output-path)
          (swiftui-preview-dynamic--crop-image output-path))))
    
    ;; Terminate app (may already be terminated)
    (swiftui-preview-dynamic--simctl (list "terminate" simulator-udid bundle-id))
    
    (if (file-exists-p output-path)
        output-path
      (error "Failed to generate preview"))))

;;; Main Entry Points

;;;###autoload
(defun swiftui-preview-dynamic-generate ()
  "Generate SwiftUI preview using dynamic target injection.
Works with Xcode projects and workspaces.
For SPM packages, use `swiftui-preview-spm-generate'.

If testability is not enabled, this will automatically enable it
and rebuild the project before generating the preview."
  (interactive)
  
  ;; Check setup
  (unless (swiftui-preview-setup-check)
    (swiftui-preview-setup-wizard)
    (unless (swiftui-preview-setup-check)
      (user-error "Setup incomplete. Run M-x swiftui-preview-setup-wizard")))
  
  ;; Validate
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (unless (string-match-p "\\.swift$" (buffer-file-name))
    (user-error "Not a Swift file"))
  
  ;; Save file first so buffer and disk are in sync
  (save-buffer)
  
  ;; Get preview body from current buffer
  (let ((preview-body (swiftui-preview--get-first-preview-body)))
    (unless preview-body
      (user-error "No #Preview block found in file"))
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Extracted body: %s"
               (substring preview-body 0 (min 100 (length preview-body)))))
    
    ;; Check if we need testability and it's not enabled
    ;; Only check for swiftc mode (not xcodebuild which handles it differently)
    (if (and (not swiftui-preview-dynamic-use-xcodebuild)
             (not (bound-and-true-p xcode-build-config-enable-testability)))
        ;; Need to enable testability and rebuild first
        (swiftui-preview-dynamic--ensure-testability-build
         (lambda ()
           ;; This callback is called immediately if testability is already enabled
           ;; The hook will re-run this function after build completes
           nil))
      
      ;; Testability is enabled (or using xcodebuild) - proceed with preview
      ;; Detect project
      (let* ((project-info (swiftui-preview-dynamic--detect-project-type))
             (project-type (plist-get project-info :type))
             (project-path (plist-get project-info :path))
             (project-root (plist-get project-info :root)))
        
        ;; Handle based on project type
        (pcase project-type
          ((or 'xcode-project 'xcode-workspace)
           (swiftui-preview-dynamic--generate-for-xcode
            preview-body project-path project-root))
          ('spm-package
           (if (fboundp 'swiftui-preview-spm-generate)
               (swiftui-preview-spm-generate)
             (user-error "SPM package detected. SPM preview not yet loaded")))
          ('standalone
           (if (fboundp 'swiftui-preview-standalone-generate)
               (swiftui-preview-standalone-generate)
             (user-error "Standalone file. Standalone preview not yet loaded")))
          (_
           (user-error "Unknown project type")))))))

(defun swiftui-preview-dynamic--find-xcodeproj (path)
  "Find .xcodeproj from PATH.
If PATH is a workspace, find the main .xcodeproj in the same directory.
If PATH is already a .xcodeproj, return it."
  (cond
   ((string-suffix-p ".xcodeproj" path)
    path)
   ((string-suffix-p ".xcworkspace" path)
    ;; Find .xcodeproj in the same directory as the workspace
    (let* ((dir (file-name-directory path))
           (workspace-name (file-name-sans-extension (file-name-nondirectory path)))
           (projects (directory-files dir t "\\.xcodeproj$")))
      ;; Prefer project with same name as workspace
      (or (cl-find-if (lambda (p)
                        (string= (file-name-sans-extension (file-name-nondirectory p))
                                 workspace-name))
                      projects)
          (car projects)
          (error "No .xcodeproj found for workspace %s" path))))
   (t
    (error "Unknown project type: %s" path))))

(defun swiftui-preview-dynamic--generate-for-xcode (preview-body project-path project-root)
  "Generate preview for Xcode project.
PREVIEW-BODY is the extracted #Preview content.
PROJECT-PATH is path to .xcodeproj or .xcworkspace.
PROJECT-ROOT is the project root directory."
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (source-file (buffer-file-name))
         (xcodeproj-path (swiftui-preview-dynamic--find-xcodeproj project-path))
         (is-workspace (string-suffix-p ".xcworkspace" project-path))
         (module-name (swiftui-preview-dynamic--detect-module))
         (imports (swiftui-preview-dynamic--extract-imports))
         (simulator-udid (swiftui-preview-dynamic--find-simulator))
         (output-path (swiftui-preview-dynamic--get-output-path project-root filename))
         (temp-dir (make-temp-file "swiftui-preview-" t)))
    
    (unless simulator-udid
      (user-error "No simulator found. Boot a simulator first"))
    
    ;; Store for cleanup and source recompilation
    (setq swiftui-preview-dynamic--temp-dir temp-dir
          swiftui-preview-dynamic--current-project xcodeproj-path
          swiftui-preview-dynamic--current-source-file source-file)
    
    ;; Ensure SwiftUI is in imports
    (unless (member "SwiftUI" imports)
      (push "SwiftUI" imports))
    
    ;; Add module to imports if detected
    (when (and module-name (not (member module-name imports)))
      (push module-name imports))
    
    
    (when swiftui-preview-dynamic-verbose
      (message "[Preview] Module: %s" module-name)
      (message "[Preview] Imports: %s" imports)
      (message "[Preview] Source file: %s" source-file)
      (message "[Preview] Xcodeproj: %s" xcodeproj-path)
      (message "[Preview] Is workspace: %s" is-workspace))
    
    (condition-case err
        (progn
          ;; Ensure simulator is booted (quick check)
          (swiftui-preview-dynamic--ensure-simulator-booted simulator-udid)
          
          ;; Generate preview host
          (message "Generating preview host...")
          (swiftui-preview-dynamic--generate-preview-host preview-body imports temp-dir module-name)
          
          ;; Only inject target into xcodeproj when using xcodebuild
          ;; swiftc path compiles directly from temp dir
          (when swiftui-preview-dynamic-use-xcodebuild
            (message "Injecting preview target...")
            (swiftui-preview-dynamic--inject-target xcodeproj-path temp-dir module-name imports source-file))
          
          ;; Build asynchronously
          (message "Building preview (async)...")
          (swiftui-preview-dynamic--build-async
           (if is-workspace project-path xcodeproj-path)
           simulator-udid
           is-workspace
           ;; Callback when build completes
           (lambda (app-path error-msg)
             (if error-msg
                 (progn
                   (swiftui-preview-dynamic--show-error error-msg)
                   (when swiftui-preview-dynamic-use-xcodebuild
                     (unless swiftui-preview-dynamic-keep-target
                       (swiftui-preview-dynamic--cleanup xcodeproj-path))))
               (if (not app-path)
                   (progn
                     (swiftui-preview-dynamic--show-error "Build produced no app")
                     (when swiftui-preview-dynamic-use-xcodebuild
                       (unless swiftui-preview-dynamic-keep-target
                         (swiftui-preview-dynamic--cleanup xcodeproj-path))))
                 ;; Build succeeded - capture and display
                 (condition-case err
                     (let ((captured (swiftui-preview-dynamic--capture app-path simulator-udid output-path)))
                       (swiftui-preview--display-image captured)
                       (message "Preview generated: %s" captured)
                       ;; Cleanup after success
                       (when swiftui-preview-dynamic-use-xcodebuild
                         (unless swiftui-preview-dynamic-keep-target
                           (swiftui-preview-dynamic--cleanup xcodeproj-path)))
                       (when (and temp-dir (file-exists-p temp-dir))
                         (delete-directory temp-dir t)))
                   (error
                    (swiftui-preview-dynamic--show-error (error-message-string err))
                    (when swiftui-preview-dynamic-use-xcodebuild
                      (unless swiftui-preview-dynamic-keep-target
                        (swiftui-preview-dynamic--cleanup xcodeproj-path))))))))))
      
      (error
       ;; Show error for pre-build failures
       (swiftui-preview-dynamic--show-error (error-message-string err))
       (when swiftui-preview-dynamic-use-xcodebuild
         (unless swiftui-preview-dynamic-keep-target
           (swiftui-preview-dynamic--cleanup xcodeproj-path)))
       (when (and temp-dir (file-exists-p temp-dir))
         (delete-directory temp-dir t))))))

(defun swiftui-preview-dynamic--cleanup (project-path)
  "Clean up injected target from PROJECT-PATH."
  (when project-path
    (message "Cleaning up...")
    (swiftui-preview-dynamic--cleanup-target project-path)
    ;; Also clean temp dir
    (when (and swiftui-preview-dynamic--temp-dir
               (file-exists-p swiftui-preview-dynamic--temp-dir))
      (delete-directory swiftui-preview-dynamic--temp-dir t)
      (setq swiftui-preview-dynamic--temp-dir nil))))

(defun swiftui-preview-dynamic--show-error (error-msg)
  "Show error with details and retry option.
ERROR-MSG is the error message to display."
  (let ((buffer (get-buffer-create "*SwiftUI Preview Error*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "SwiftUI Preview Error\n")
        (insert "=====================\n\n")
        (insert error-msg "\n\n")
        (insert "Possible causes:\n")
        (insert "  - Module not found (check file location)\n")
        (insert "  - Build errors in dependencies\n")
        (insert "  - Missing imports\n\n")
        (insert "Actions:\n")
        (insert "  M-x swiftui-preview-dynamic-generate  - Retry\n")
        (insert "  M-x swiftui-preview-dynamic-cleanup   - Manual cleanup\n")
        (insert "  See *SwiftUI Preview Build* buffer for build output\n")
        (special-mode)))
    (display-buffer buffer)))

;;;###autoload
(defun swiftui-preview-dynamic-cleanup ()
  "Manually clean up injected PreviewHost target."
  (interactive)
  (if swiftui-preview-dynamic--current-project
      (progn
        (swiftui-preview-dynamic--cleanup swiftui-preview-dynamic--current-project)
        (setq swiftui-preview-dynamic--current-project nil)
        (message "Cleanup complete"))
    (message "No project to clean up")))

;;;###autoload
(defun swiftui-preview-dynamic-select ()
  "Select which #Preview to generate if multiple exist."
  (interactive)
  (let ((previews (swiftui-preview--detect-preview-definitions)))
    (if (null previews)
        (user-error "No #Preview blocks found")
      (if (= (length previews) 1)
          (swiftui-preview-dynamic-generate)
        ;; Multiple previews - let user choose
        (let* ((choices (mapcar (lambda (p)
                                  (cons (or (plist-get p :name) "Preview")
                                        (plist-get p :body)))
                                (cl-remove-if-not
                                 (lambda (p) (eq (plist-get p :type) 'preview-macro))
                                 previews)))
               (selected (completing-read "Select preview: "
                                         (mapcar #'car choices)
                                         nil t)))
          ;; Generate with selected preview body
          ;; For now, just use the first one
          ;; TODO: Pass selected preview to generation
          (swiftui-preview-dynamic-generate))))))

(provide 'swiftui-preview-dynamic)
;;; swiftui-preview-dynamic.el ends here
