;;; xcode-clean.el --- Cleaning utilities for Xcode projects -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1") (async "1.9"))
;; Keywords: swift, xcode, clean

;;; Commentary:

;; Provides cleaning utilities for Xcode projects including build folder cleanup,
;; Swift package cache management, and derived data cleanup.

;;; Code:

(require 'cl-lib)
(require 'async nil t)  ; Optional - only needed for async cleaning
(require 'swift-notification nil t)  ; For knockknock/mode-line-hud notifications

;; Forward declarations for incremental build cache invalidation
(declare-function swift-incremental-build-clear-cache "swift-incremental-build")

(defun xcode-clean--invalidate-incremental-cache ()
  "Invalidate the incremental build cache after a clean operation.
Cached swiftc commands reference files inside the build directory
that no longer exist after cleaning."
  (when (fboundp 'swift-incremental-build-clear-cache)
    (swift-incremental-build-clear-cache)))

(defun xcode-clean--notify (message &optional seconds)
  "Send notification with MESSAGE, shown for SECONDS (default 2)."
  (if (fboundp 'swift-notification-send)
      (swift-notification-send :message message :seconds (or seconds 2))
    (message "%s" message)))

(defgroup xcode-clean nil
  "Cleaning utilities for Xcode projects."
  :group 'programming
  :prefix "xcode-clean-")

(defcustom xcode-clean-ignore-list '(".git" ".DS_Store" "Package.resolved")
  "List of files/folders to ignore when cleaning build folders."
  :type '(repeat string)
  :group 'xcode-clean)

(defun xcode-clean-directory-contents (directory ignore-list)
  "Delete contents of DIRECTORY, ignoring items in IGNORE-LIST.
Returns a list of errors encountered during deletion.
This function handles errors gracefully and continues cleaning even if some deletions fail."
  (let ((errors nil))
    (when (file-directory-p directory)
      (dolist (file (directory-files directory t "^[^.]"))
        (let ((file-name (file-name-nondirectory file)))
          (unless (member file-name ignore-list)
            (condition-case err
                (if (file-directory-p file)
                    (delete-directory file t)
                  (delete-file file))
              (file-error
               (push (format "%s: %s" file (error-message-string err)) errors)))))))
    errors))

(defun xcode-clean-swift-package-caches ()
  "Clean Swift package manager caches safely."
  (interactive)
  (let ((package-cache-dir (expand-file-name "~/Library/Caches/org.swift.packages"))
        (cloned-sources-dir (expand-file-name "~/Library/Caches/org.swift.cloned-sources"))
        (cleaned 0))

    (xcode-clean--notify "Cleaning Swift package caches..." 3)

    (when (file-directory-p package-cache-dir)
      (condition-case err
          (progn
            (delete-directory package-cache-dir t)
            (setq cleaned (1+ cleaned)))
        (file-error (message "Could not clean package cache: %s" (error-message-string err)))))

    (when (file-directory-p cloned-sources-dir)
      (condition-case err
          (progn
            (delete-directory cloned-sources-dir t)
            (setq cleaned (1+ cleaned)))
        (file-error (message "Could not clean cloned sources: %s" (error-message-string err)))))

    (xcode-clean--notify (format "Cleaned %d Swift package cache locations" cleaned) 2)))

(defun xcode-clean-project-derived-data (project-name)
  "Clean Xcode derived data for PROJECT-NAME."
  (interactive
   (list (read-string "Project name: " (when (fboundp 'xcode-project-product-name)
                                         (xcode-project-product-name)))))
  (let* ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (project-pattern (concat "^" (regexp-quote project-name)))
         (deleted 0))

    (when (and project-name (file-directory-p derived-data-dir))
      (xcode-clean--notify (format "Cleaning derived data for %s..." project-name) 3)
      (dolist (dir (directory-files derived-data-dir t project-pattern))
        (when (file-directory-p dir)
          (condition-case err
              (progn
                (delete-directory dir t)
                (setq deleted (1+ deleted)))
            (file-error (message "Could not delete %s: %s" dir (error-message-string err)))))))

    (xcode-clean--notify (format "Deleted %d derived data folder(s) for %s" deleted project-name) 2)))

(defun xcode-clean-all-derived-data (&optional preserve-module-cache)
  "Clean all Xcode derived data.
If PRESERVE-MODULE-CACHE is non-nil, keep the ModuleCache folder."
  (interactive "P")
  (let ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
        (deleted 0))

    (when (and (file-directory-p derived-data-dir)
               (yes-or-no-p "Delete ALL Xcode derived data? "))
      (xcode-clean--notify "Cleaning all derived data..." 3)
      (dolist (file (directory-files derived-data-dir t "^[^.]"))
        (when (file-directory-p file)
          (let ((file-name (file-name-nondirectory file)))
            (unless (and preserve-module-cache (string= file-name "ModuleCache"))
              (condition-case err
                  (progn
                    (delete-directory file t)
                    (setq deleted (1+ deleted)))
                (file-error (message "Could not delete %s: %s" file (error-message-string err)))))))))

    (xcode-clean--notify (format "Deleted %d derived data folder(s)" deleted) 2)))

(cl-defun xcode-clean-build-folder-async (&key directory ignore-list callback display-name)
  "Clean DIRECTORY asynchronously, ignoring items in IGNORE-LIST.
Calls CALLBACK with result string when done.
DISPLAY-NAME is used for user messages."
  (let ((name (or display-name "project")))
    ;; Invalidate incremental build cache before cleaning
    (xcode-clean--invalidate-incremental-cache)
    (if (file-directory-p directory)
        (progn
          (xcode-clean--notify (format "Cleaning build folder for %s..." name) 3)
          ;; Start progress bar
          (when (fboundp 'swift-notification-progress-start)
            (swift-notification-progress-start
             :id 'xcode-clean
             :title "Cleaning"
             :message name
             :percent 0))
          (async-start
           `(lambda ()
              (let ((errors nil))
                (dolist (file (directory-files ,directory t "^[^.]"))
                  (let ((file-name (file-name-nondirectory file)))
                    (unless (member file-name ',ignore-list)
                      (condition-case err
                          (if (file-directory-p file)
                              (delete-directory file t)
                            (delete-file file))
                        (file-error
                         (push (format "%s: %s" file (error-message-string err)) errors))))))
                (if errors
                    (format "completed with %d errors" (length errors))
                  "successfully")))
           (lambda (result)
             ;; Finish progress bar
             (if (string-match-p "successfully" result)
                 (when (fboundp 'swift-notification-progress-finish)
                   (swift-notification-progress-finish 'xcode-clean (format "Cleaned %s" name)))
               (when (fboundp 'swift-notification-progress-cancel)
                 (swift-notification-progress-cancel 'xcode-clean)))
             (xcode-clean--notify (format "Cleaning %s %s" name result) 2)
             (when callback
               (funcall callback result)))))
      (xcode-clean--notify "Build folder is empty or does not exist." 2)
      (when callback
        (funcall callback "skipped - folder not found")))))

;;; ============================================================================
;;; xcodebuild clean integration
;;; ============================================================================

(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-get-workspace-or-project "xcode-project")
(declare-function xcode-project-scheme-display-name "xcode-project")
(declare-function swift-async-run "swift-async")

(defun xcode-clean-xcodebuild (&optional callback)
  "Run `xcodebuild clean' for the current scheme and workspace/project.
This is the proper Xcode-native clean that respects the build system state.
Calls CALLBACK when done."
  (interactive)
  (let* ((default-directory (if (fboundp 'xcode-project-project-root)
                                (or (xcode-project-project-root) default-directory)
                              default-directory))
         (workspace-or-project (when (fboundp 'xcode-project-get-workspace-or-project)
                                 (xcode-project-get-workspace-or-project)))
         (scheme (when (fboundp 'xcode-project-scheme-display-name)
                   (xcode-project-scheme-display-name)))
         (cmd (mapconcat 'identity
                         (delq nil
                               (list "xcrun" "xcodebuild" "clean"
                                     (when workspace-or-project
                                       (format "%s" workspace-or-project))
                                     (when scheme
                                       (format "-scheme '%s'" scheme))
                                     "-derivedDataPath .build"))
                         " ")))
    (xcode-clean--notify "Running xcodebuild clean..." 3)
    ;; Invalidate incremental build cache before cleaning
    (xcode-clean--invalidate-incremental-cache)
    (if (fboundp 'swift-async-run)
        (swift-async-run
         cmd
         (lambda (_output)
            (xcode-clean--notify "xcodebuild clean completed" 2)
            (when callback (funcall callback)))
         :timeout 30
         :error-callback (lambda (_key err)
                           (xcode-clean--notify
                            (format "xcodebuild clean failed: %s" err) 3)))
      ;; Fallback: synchronous
      (shell-command cmd)
      (xcode-clean--notify "xcodebuild clean completed" 2)
      (when callback (funcall callback)))))

(provide 'xcode-clean)
;;; xcode-clean.el ends here
