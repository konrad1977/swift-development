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
;; Shared utilities (import extraction, simulator, Ruby scripts, capture) are
;; provided by `swiftui-preview-core'.
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
(require 'swiftui-preview-core)

;; Forward declarations
(declare-function swiftui-preview--get-first-preview-body "swiftui-preview")
(declare-function swiftui-preview--display-image "swiftui-preview")
(declare-function swiftui-preview-setup-check "swiftui-preview-setup")
(declare-function swiftui-preview-setup-wizard "swiftui-preview-setup")

(defgroup swiftui-preview-spm nil
  "SPM package SwiftUI preview settings."
  :group 'swiftui-preview
  :prefix "swiftui-preview-spm-")

(defcustom swiftui-preview-spm-keep-temp nil
  "If non-nil, keep temporary project for debugging."
  :type 'boolean
  :group 'swiftui-preview-spm)

(defvar swiftui-preview-spm--temp-dir nil
  "Temporary directory for current preview generation.")

;;; Helper Functions

(defun swiftui-preview-spm--find-package-swift ()
  "Find Package.swift by searching upward from current file.
Returns the directory containing Package.swift, or nil if not found."
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

;;; Source File Processing

(defun swiftui-preview-spm--copy-source-file (source-file dest-dir)
  "Copy SOURCE-FILE to DEST-DIR, removing #Preview blocks.
Returns path to copied file."
  (let* ((filename (file-name-nondirectory source-file))
         (dest-file (expand-file-name (concat "Source_" filename) dest-dir))
         (content (with-temp-buffer
                    (insert-file-contents source-file)
                    (buffer-string))))
    ;; Remove #Preview blocks with robust brace matching
    (setq content (swiftui-preview-core-remove-preview-blocks content))
    ;; Remove @main if present
    (setq content (replace-regexp-in-string "@main\\s*\n" "// @main removed\n" content))

    (make-directory dest-dir t)
    (with-temp-file dest-file
      (insert content))

    (when swiftui-preview-core-verbose
      (message "[SPM Preview] Copied source file: %s" dest-file))

    dest-file))

;;; Preview Host Generation

(defun swiftui-preview-spm--generate-preview-host
    (preview-body imports temp-dir source-file &optional output-path)
  "Generate PreviewHostApp.swift in TEMP-DIR.
PREVIEW-BODY is the extracted #Preview content.
IMPORTS is list of imports from the source file.
SOURCE-FILE is the original Swift file being previewed.
OUTPUT-PATH is the absolute path for the snapshot PNG (embedded in Swift)."
  ;; Copy source file to preview dir (removes #Preview blocks)
  (when source-file
    (swiftui-preview-spm--copy-source-file source-file temp-dir))

  ;; Build import list - ensure SwiftUI is present, filter module's own name
  (let* ((module-name (swiftui-preview-spm--detect-module))
         (filtered-imports (if module-name
                              (seq-remove (lambda (imp) (string= imp module-name)) imports)
                            imports))
         (all-imports (delete-dups (cons "SwiftUI" filtered-imports)))
         (filename (file-name-nondirectory (or source-file (buffer-file-name))))
         (color-scheme (swiftui-preview-core-detect-color-scheme preview-body)))

    ;; Use core to write the host app with embedded output-path
    (swiftui-preview-core-write-host-app
     preview-body all-imports temp-dir filename color-scheme
     nil nil nil output-path)))

;;; Ruby Script Interaction

(defun swiftui-preview-spm--create-project (package-path module-name preview-dir output-dir)
  "Create temporary Xcode project for SPM package.
PACKAGE-PATH is path to Package.swift.
MODULE-NAME is the module to depend on.
PREVIEW-DIR contains the preview host files.
OUTPUT-DIR is where to create the project."
  (let ((args (list "--package" package-path
                    "--module" module-name
                    "--preview-dir" preview-dir
                    "--output" output-dir)))
    (when swiftui-preview-core-verbose
      (setq args (append args (list "--verbose"))))
    (apply #'swiftui-preview-core-run-ruby-script "preview-spm-create.rb" args)))

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

    (when swiftui-preview-core-verbose
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
          (when swiftui-preview-core-verbose
            (message "[SPM Preview] Built app: %s" app-path))
          app-path)
      (display-buffer output-buffer)
      (error "Build failed. See *SwiftUI Preview Build* for details"))))

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
               (imports (swiftui-preview-core-extract-imports))
               (simulator-udid (swiftui-preview-core-find-simulator))
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

          (when swiftui-preview-core-verbose
            (message "[SPM Preview] Package: %s" package-path)
            (message "[SPM Preview] Module: %s" module-name)
            (message "[SPM Preview] Imports: %s" imports))

          (condition-case err
              (progn
                ;; Ensure simulator is booted
                (swiftui-preview-core-ensure-simulator-booted simulator-udid)

                ;; Generate preview host (also copies source file)
                (message "Generating preview host...")
                (swiftui-preview-spm--generate-preview-host
                 preview-body imports preview-dir (buffer-file-name)
                 output-path)

                ;; Create temp Xcode project
                (message "Creating temporary Xcode project...")
                (let ((result (swiftui-preview-spm--create-project
                               package-path module-name preview-dir project-dir)))
                  (let ((project-path (alist-get 'project_path result)))

                    ;; Build
                    (message "Building preview...")
                    (let ((app-path (swiftui-preview-spm--build project-path simulator-udid)))
                      (unless app-path
                        (error "Build produced no app"))

                      ;; Capture using core
                      (let ((captured (swiftui-preview-core-capture
                                       app-path simulator-udid output-path
                                       "com.swift-development.preview-spm")))
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
