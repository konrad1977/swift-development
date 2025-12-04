;;; xcode-clean.el --- Cleaning utilities for Xcode projects -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (async "1.9"))
;; Keywords: swift, xcode, clean

;;; Commentary:

;; Provides cleaning utilities for Xcode projects including build folder cleanup,
;; Swift package cache management, and derived data cleanup.

;;; Code:

(require 'cl-lib)
(require 'async nil t)  ; Optional - only needed for async cleaning

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

    (when (file-directory-p package-cache-dir)
      (message "Cleaning Swift package cache...")
      (condition-case err
          (progn
            (delete-directory package-cache-dir t)
            (setq cleaned (1+ cleaned)))
        (file-error (message "Could not clean package cache: %s" (error-message-string err)))))

    (when (file-directory-p cloned-sources-dir)
      (message "Cleaning Swift cloned sources...")
      (condition-case err
          (progn
            (delete-directory cloned-sources-dir t)
            (setq cleaned (1+ cleaned)))
        (file-error (message "Could not clean cloned sources: %s" (error-message-string err)))))

    (message "Cleaned %d Swift package cache locations" cleaned)))

(defun xcode-clean-project-derived-data (project-name)
  "Clean Xcode derived data for PROJECT-NAME."
  (interactive
   (list (read-string "Project name: " (when (fboundp 'xcode-project-product-name)
                                         (xcode-project-product-name)))))
  (let* ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
         (project-pattern (concat "^" (regexp-quote project-name)))
         (deleted 0))

    (when (and project-name (file-directory-p derived-data-dir))
      (message "Cleaning derived data for %s..." project-name)
      (dolist (dir (directory-files derived-data-dir t project-pattern))
        (when (file-directory-p dir)
          (condition-case err
              (progn
                (delete-directory dir t)
                (setq deleted (1+ deleted)))
            (file-error (message "Could not delete %s: %s" dir (error-message-string err)))))))

    (message "Deleted %d derived data folder(s) for %s" deleted project-name)))

(defun xcode-clean-all-derived-data (&optional preserve-module-cache)
  "Clean all Xcode derived data.
If PRESERVE-MODULE-CACHE is non-nil, keep the ModuleCache folder."
  (interactive "P")
  (let ((derived-data-dir (expand-file-name "~/Library/Developer/Xcode/DerivedData"))
        (deleted 0))

    (when (and (file-directory-p derived-data-dir)
               (yes-or-no-p "Delete ALL Xcode derived data? "))
      (message "Cleaning all derived data...")
      (dolist (file (directory-files derived-data-dir t "^[^.]"))
        (when (file-directory-p file)
          (let ((file-name (file-name-nondirectory file)))
            (unless (and preserve-module-cache (string= file-name "ModuleCache"))
              (condition-case err
                  (progn
                    (delete-directory file t)
                    (setq deleted (1+ deleted)))
                (file-error (message "Could not delete %s: %s" file (error-message-string err)))))))))

    (message "Deleted %d derived data folder(s)" deleted)))

(cl-defun xcode-clean-build-folder-async (&key directory ignore-list callback display-name)
  "Clean DIRECTORY asynchronously, ignoring items in IGNORE-LIST.
Calls CALLBACK with result string when done.
DISPLAY-NAME is used for user messages."
  (let ((name (or display-name "project")))
    (if (file-directory-p directory)
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
           (message "Cleaning %s %s" name result)
           (when callback
             (funcall callback result))))
      (message "Build folder is empty or does not exist.")
      (when callback
        (funcall callback "skipped - folder not found")))))

(provide 'xcode-clean)
;;; xcode-clean.el ends here
