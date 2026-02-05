;;; swiftui-preview-spm.el --- SPM package SwiftUI preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: swift, swiftui, preview, ios, spm

;;; Commentary:

;; This module provides SwiftUI preview functionality for Swift Package Manager
;; (SPM) packages.  It creates a temporary Xcode project that depends on the
;; local SPM package, builds it, and captures a preview screenshot.
;;
;; Usage:
;; 1. Open a Swift file with a #Preview block in an SPM package
;; 2. M-x swiftui-preview-spm-generate
;;
;; Requirements:
;; - Ruby with xcodeproj gem installed
;; - Xcode command line tools
;; - A booted iOS Simulator

;;; Code:

(require 'cl-lib)
(require 'json)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")
(declare-function swiftui-preview-setup-check "swiftui-preview-setup")
(declare-function swiftui-preview-setup-wizard "swiftui-preview-setup")
(declare-function ios-simulator-simulator-identifier "ios-simulator")

(defgroup swiftui-preview-spm nil
  "SPM package SwiftUI preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-spm-")

(defcustom swiftui-preview-spm-simulator "iPhone 16 Pro"
  "Default simulator name for SPM previews."
  :type 'string
  :group 'swiftui-preview-spm)

(defcustom swiftui-preview-spm-capture-delay 3.0
  "Delay in seconds after app launch before capturing screenshot."
  :type 'number
  :group 'swiftui-preview-spm)

(defcustom swiftui-preview-spm-keep-temp nil
  "If non-nil, keep temporary project for debugging."
  :type 'boolean
  :group 'swiftui-preview-spm)

(defcustom swiftui-preview-spm-verbose nil
  "If non-nil, show verbose output during preview generation."
  :type 'boolean
  :group 'swiftui-preview-spm)

(defvar swiftui-preview-spm--scripts-dir
  (expand-file-name "scripts" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing Ruby helper scripts.")

(defvar swiftui-preview-spm--temp-dir nil
  "Temporary directory for current preview generation.")

;;; Helper Functions

(defun swiftui-preview-spm--find-package-swift ()
  "Find Package.swift by searching upward from current file.
Returns the full path or nil if not found."
  (when-let* ((file (buffer-file-name))
              (dir (file-name-directory file)))
    (locate-dominating-file dir "Package.swift")))

(defun swiftui-preview-spm--detect-module ()
  "Detect module name from current file path.
Returns module name string or nil."
  (when-let* ((file (buffer-file-name))
              (package-dir (swiftui-preview-spm--find-package-swift)))
    ;; Common patterns: Sources/ModuleName/...
    (let ((relative (file-relative-name file package-dir)))
      (when (string-match "Sources/\\([^/]+\\)/" relative)
        (match-string 1 relative)))))

(defun swiftui-preview-spm--extract-imports ()
  "Extract all import statements from current buffer.
Returns a list of imported module names."
  (save-excursion
    (goto-char (point-min))
    (let ((imports '()))
      (while (re-search-forward "^import[[:space:]]+\\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
        (push (match-string 1) imports))
      (delete-dups (nreverse imports)))))

(defun swiftui-preview-spm--find-simulator ()
  "Find simulator UDID to use for preview."
  (or
   (and (fboundp 'ios-simulator-simulator-identifier)
        (ios-simulator-simulator-identifier))
   (let* ((script (expand-file-name "preview-helper.rb" swiftui-preview-spm--scripts-dir))
          (output (shell-command-to-string
                   (format "ruby %s find-simulator %s 2>/dev/null"
                           (shell-quote-argument script)
                           (shell-quote-argument swiftui-preview-spm-simulator)))))
     (let ((udid (string-trim output)))
       (unless (string-empty-p udid)
         udid)))
   (let* ((script (expand-file-name "preview-helper.rb" swiftui-preview-spm--scripts-dir))
          (output (shell-command-to-string
                   (format "ruby %s first-booted 2>/dev/null"
                           (shell-quote-argument script)))))
     (let ((udid (string-trim output)))
       (unless (string-empty-p udid)
         udid)))))

(defun swiftui-preview-spm--ensure-simulator-booted (udid)
  "Ensure simulator with UDID is booted."
  (let ((state (shell-command-to-string
                (format "xcrun simctl list devices -j 2>/dev/null | grep -A5 '%s' | grep state" udid))))
    (unless (string-match-p "Booted" state)
      (message "Booting simulator...")
      (call-process "xcrun" nil nil nil "simctl" "boot" udid)
      (sleep-for 2))))

;;; Source File Processing

(defun swiftui-preview-spm--copy-source-file (source-file dest-dir)
  "Copy SOURCE-FILE to DEST-DIR, removing #Preview blocks.
Returns path to copied file."
  (let* ((filename (file-name-nondirectory source-file))
         (dest-file (expand-file-name (concat "Source_" filename) dest-dir))
         (content (with-temp-buffer
                    (insert-file-contents source-file)
                    (buffer-string))))
    
    ;; Remove #Preview blocks with proper brace matching
    (setq content (swiftui-preview-spm--remove-preview-blocks content))
    
    ;; Remove @main if present
    (setq content (replace-regexp-in-string "@main\\s*\n" "// @main removed\n" content))
    
    (make-directory dest-dir t)
    (with-temp-file dest-file
      (insert content))
    
    (when swiftui-preview-spm-verbose
      (message "[SPM Preview] Copied source file: %s" dest-file))
    
    dest-file))

(defun swiftui-preview-spm--remove-preview-blocks (content)
  "Remove #Preview blocks from CONTENT string."
  (let ((result "")
        (i 0)
        (len (length content)))
    (while (< i len)
      (if (and (<= (+ i 8) len)
               (string= (substring content i (+ i 8)) "#Preview"))
          ;; Found #Preview, skip to matching closing brace
          (let ((j (+ i 8))
                (depth 0)
                (found-brace nil))
            ;; Find opening brace
            (while (and (< j len) (not found-brace))
              (when (= (aref content j) ?{)
                (setq found-brace t)
                (setq depth 1))
              (setq j (1+ j)))
            ;; Find matching closing brace
            (while (and (< j len) (> depth 0))
              (let ((char (aref content j)))
                (cond
                 ((= char ?{) (setq depth (1+ depth)))
                 ((= char ?}) (setq depth (1- depth)))
                 ((= char ?\") 
                  ;; Skip string literal
                  (setq j (1+ j))
                  (while (and (< j len) (/= (aref content j) ?\"))
                    (when (= (aref content j) ?\\)
                      (setq j (1+ j)))
                    (setq j (1+ j))))))
              (setq j (1+ j)))
            (setq result (concat result "// #Preview removed\n"))
            (setq i j))
        ;; Not #Preview, copy character
        (setq result (concat result (substring content i (1+ i))))
        (setq i (1+ i))))
    result))

;;; Preview Host Generation

(defun swiftui-preview-spm--generate-preview-host (preview-body imports temp-dir source-file)
  "Generate PreviewHostApp.swift in TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of imports from the source file.
SOURCE-FILE is the original Swift file being previewed."
  (let* ((filename (file-name-nondirectory (or source-file (buffer-file-name))))
         ;; Build import statements - don't include the module itself since we copy the source
         ;; Remove the module name from imports if present
         (filtered-imports (seq-remove (lambda (imp)
                                         (member imp '("KYC"))) ;; Could be dynamic
                                       imports))
         (all-imports (delete-dups (cons "SwiftUI" filtered-imports)))
         (import-statements (mapconcat (lambda (imp) (format "import %s" imp))
                                       all-imports "\n"))
         ;; Copy source file to preview dir
         (_copied (when source-file
                    (swiftui-preview-spm--copy-source-file source-file temp-dir)))
         ;; Indent preview body
         (indented-body (replace-regexp-in-string
                         "^" "            "
                         preview-body))
         (host-content (format "// Auto-generated PreviewHost for %s
// Generated by swift-development swiftui-preview-spm

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
    
    let hostingController = UIHostingController(rootView: previewView.fixedSize())
    let targetSize = hostingController.sizeThatFits(in: CGSize(width: CGFloat.infinity, height: CGFloat.infinity))
    
    let padding: CGFloat = 40
    let finalSize = CGSize(
        width: targetSize.width + padding * 2,
        height: targetSize.height + padding * 2
    )
    
    let paddedView = previewView
        .fixedSize()
        .padding(padding)
        .frame(width: finalSize.width, height: finalSize.height, alignment: .center)
        .background(Color.clear)
    
    let finalController = UIHostingController(rootView: paddedView)
    finalController.view.frame = CGRect(origin: .zero, size: finalSize)
    finalController.view.backgroundColor = .clear
    finalController.view.layoutIfNeeded()
    
    let format = UIGraphicsImageRendererFormat()
    format.scale = UIScreen.main.scale
    format.opaque = false
    
    let renderer = UIGraphicsImageRenderer(size: finalSize, format: format)
    let image = renderer.image { context in
        finalController.view.drawHierarchy(in: CGRect(origin: .zero, size: finalSize), afterScreenUpdates: true)
    }
    
    guard let data = image.pngData() else {
        print(\"ERROR: Failed to create PNG data\")
        exit(1)
    }
    
    let outputPath = ProcessInfo.processInfo.environment[\"PREVIEW_OUTPUT_PATH\"] 
        ?? \"/tmp/swift-development-preview.png\"
    
    let url = URL(fileURLWithPath: outputPath)
    try? FileManager.default.createDirectory(at: url.deletingLastPathComponent(), withIntermediateDirectories: true)
    
    do {
        try data.write(to: url)
        print(\"Preview saved to: \\(outputPath)\")
    } catch {
        print(\"ERROR: \\(error)\")
        exit(1)
    }
    
    exit(0)
}
" filename import-statements indented-body))
         (host-file (expand-file-name "PreviewHostApp.swift" temp-dir)))
    
    (make-directory temp-dir t)
    (with-temp-file host-file
      (insert host-content))
    
    (when swiftui-preview-spm-verbose
      (message "[SPM Preview] Generated PreviewHost in %s" temp-dir))
    
    host-file))

;;; Ruby Script Interaction

(defun swiftui-preview-spm--run-ruby-script (script-name &rest args)
  "Run Ruby script SCRIPT-NAME with ARGS.
Returns parsed JSON result or signals error."
  (let* ((script (expand-file-name script-name swiftui-preview-spm--scripts-dir))
         (cmd (mapconcat #'shell-quote-argument (cons "ruby" (cons script args)) " "))
         (output (shell-command-to-string (concat cmd " 2>&1")))
         (json-output nil))
    
    (when swiftui-preview-spm-verbose
      (message "[SPM Preview] Running: %s" cmd))
    
    (condition-case nil
        (setq json-output (json-read-from-string output))
      (error nil))
    
    (if (and json-output (eq (alist-get 'success json-output) t))
        json-output
      (let ((error-msg (or (alist-get 'error json-output) output)))
        (error "Ruby script failed: %s" error-msg)))))

(defun swiftui-preview-spm--create-project (package-path module-name preview-dir output-dir)
  "Create temporary Xcode project for SPM package.
PACKAGE-PATH is path to Package.swift.
MODULE-NAME is the module to depend on.
PREVIEW-DIR contains the preview host files.
OUTPUT-DIR is where to create the project."
  (let ((args (list "preview-spm-create.rb"
                    "--package" package-path
                    "--module" module-name
                    "--preview-dir" preview-dir
                    "--output" output-dir)))
    (when swiftui-preview-spm-verbose
      (setq args (append args (list "--verbose"))))
    (apply #'swiftui-preview-spm--run-ruby-script args)))

;;; Build and Capture

(defun swiftui-preview-spm--build (project-path simulator-udid)
  "Build PreviewHost in PROJECT-PATH for SIMULATOR-UDID.
Returns path to built app or nil on failure."
  (let* ((derived-data (expand-file-name "DerivedData" swiftui-preview-spm--temp-dir))
         (build-cmd (format "xcodebuild build -project %s -scheme PreviewHost -destination 'platform=iOS Simulator,id=%s' -derivedDataPath %s 2>&1"
                            (shell-quote-argument project-path)
                            simulator-udid
                            (shell-quote-argument derived-data)))
         (output-buffer (get-buffer-create "*SwiftUI Preview Build*"))
         (exit-code nil))
    
    (when swiftui-preview-spm-verbose
      (message "[SPM Preview] Build command: %s" build-cmd))
    
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Building SPM Preview...\n\n")))
    
    (setq exit-code (call-process-shell-command build-cmd nil output-buffer))
    
    (if (= exit-code 0)
        (let ((app-path (car (directory-files-recursively
                              derived-data
                              "PreviewHost\\.app$"
                              nil
                              (lambda (d) (not (string-match-p "\\.dSYM" d)))))))
          (when swiftui-preview-spm-verbose
            (message "[SPM Preview] Built app: %s" app-path))
          app-path)
      (display-buffer output-buffer)
      (error "Build failed. See *SwiftUI Preview Build* for details"))))

(defun swiftui-preview-spm--capture (app-path simulator-udid output-path)
  "Install APP-PATH, launch, and wait for internal snapshot.
The app renders the view with transparent background and saves to OUTPUT-PATH.
Returns OUTPUT-PATH on success."
  (let ((bundle-id "com.swift-development.preview-spm")
        (process-environment (cons (format "SIMCTL_CHILD_PREVIEW_OUTPUT_PATH=%s" output-path)
                                   process-environment)))
    
    (call-process "xcrun" nil nil nil "simctl" "terminate" simulator-udid bundle-id)
    
    ;; Ensure output directory exists and remove old file
    (make-directory (file-name-directory output-path) t)
    (when (file-exists-p output-path)
      (delete-file output-path))
    
    (message "Installing preview app...")
    (unless (= 0 (call-process "xcrun" nil nil nil "simctl" "install" simulator-udid app-path))
      (error "Failed to install preview app"))
    
    (message "Launching preview app...")
    (call-process "xcrun" nil nil nil "simctl" "launch" simulator-udid bundle-id)
    
    ;; Wait for the app to generate the preview and exit
    (message "Waiting for preview generation...")
    (let ((timeout 10.0)
          (poll-interval 0.2)
          (elapsed 0.0))
      (while (and (< elapsed timeout)
                  (not (file-exists-p output-path)))
        (sleep-for poll-interval)
        (setq elapsed (+ elapsed poll-interval)))
      
      (unless (file-exists-p output-path)
        ;; Fallback: try simctl screenshot
        (message "Internal capture timed out, falling back to screenshot...")
        (call-process "xcrun" nil nil nil
                      "simctl" "io" simulator-udid "screenshot" output-path)))
    
    (call-process "xcrun" nil nil nil "simctl" "terminate" simulator-udid bundle-id)
    
    (if (file-exists-p output-path)
        output-path
      (error "Failed to generate preview"))))

;;; Main Entry Point

;;;###autoload
(defun swiftui-preview-spm-generate ()
  "Generate SwiftUI preview for file in SPM package.
Creates a temporary Xcode project, builds the preview, and captures screenshot."
  (interactive)
  
  ;; Check setup
  (when (fboundp 'swiftui-preview-setup-check)
    (unless (swiftui-preview-setup-check)
      (if (fboundp 'swiftui-preview-setup-wizard)
          (swiftui-preview-setup-wizard)
        (user-error "SwiftUI Preview setup incomplete"))))
  
  ;; Validate
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (unless (string-match-p "\\.swift$" (buffer-file-name))
    (user-error "Not a Swift file"))
  
  ;; Find package
  (let ((package-dir (swiftui-preview-spm--find-package-swift)))
    (unless package-dir
      (user-error "No Package.swift found. Not an SPM package"))
    
    (let ((package-path (expand-file-name "Package.swift" package-dir)))
      
      ;; Get preview body
      (let ((preview-body (swiftui-preview--get-first-preview-body)))
        (unless preview-body
          (user-error "No #Preview block found in file"))
        
        ;; Save file
        (save-buffer)
        
        (let* ((filename (file-name-nondirectory (buffer-file-name)))
               (module-name (swiftui-preview-spm--detect-module))
               (imports (swiftui-preview-spm--extract-imports))
               (simulator-udid (swiftui-preview-spm--find-simulator))
               (temp-dir (make-temp-file "swiftui-preview-spm-" t))
               (preview-dir (expand-file-name "PreviewHost" temp-dir))
               (project-dir (expand-file-name "Project" temp-dir))
               (output-path (expand-file-name
                             (format "%s.png" (file-name-sans-extension filename))
                             (expand-file-name ".swift-development/previews" package-dir))))
          
          (unless simulator-udid
            (user-error "No simulator found. Boot a simulator first"))
          
          (unless module-name
            (user-error "Could not detect module name from file path"))
          
          (setq swiftui-preview-spm--temp-dir temp-dir)
          
          (when swiftui-preview-spm-verbose
            (message "[SPM Preview] Package: %s" package-path)
            (message "[SPM Preview] Module: %s" module-name)
            (message "[SPM Preview] Imports: %s" imports))
          
          (condition-case err
              (progn
                ;; Ensure simulator is booted
                (swiftui-preview-spm--ensure-simulator-booted simulator-udid)
                
                ;; Generate preview host (also copies source file)
                (message "Generating preview host...")
                (swiftui-preview-spm--generate-preview-host
                 preview-body imports preview-dir (buffer-file-name))
                
                ;; Create temp Xcode project (without module dependency since we copy source)
                (message "Creating temporary Xcode project...")
                (let ((result (swiftui-preview-spm--create-project
                               package-path module-name preview-dir project-dir)))
                  (let ((project-path (alist-get 'project_path result)))
                    
                    ;; Build
                    (message "Building preview...")
                    (let ((app-path (swiftui-preview-spm--build project-path simulator-udid)))
                      (unless app-path
                        (error "Build produced no app"))
                      
                      ;; Capture
                      (let ((captured (swiftui-preview-spm--capture
                                       app-path simulator-udid output-path)))
                        ;; Display
                        (swiftui-preview--display-image captured)
                        (message "SPM Preview generated: %s" captured))))))
            
            (error
             (message "SPM Preview error: %s" (error-message-string err))
             (signal (car err) (cdr err))))
          
          ;; Cleanup
          (unless swiftui-preview-spm-keep-temp
            (when (and temp-dir (file-exists-p temp-dir))
              (delete-directory temp-dir t))))))))

;;;###autoload
(defun swiftui-preview-spm-check ()
  "Check if current file is in an SPM package.
Returns t if in SPM package, nil otherwise."
  (interactive)
  (if (swiftui-preview-spm--find-package-swift)
      (progn
        (when (called-interactively-p 'any)
          (message "File is in an SPM package"))
        t)
    (when (called-interactively-p 'any)
      (message "Not in an SPM package"))
    nil))

(provide 'swiftui-preview-spm)
;;; swiftui-preview-spm.el ends here
