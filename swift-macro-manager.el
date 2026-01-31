;;; swift-macro-manager.el --- Swift Macro approval management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, macros, spm

;;; Commentary:

;; Manages Swift macro approvals for SPM packages.
;;
;; Swift 5.9+ macros from packages require explicit approval before they
;; can be used in builds.  This module handles:
;;
;; - Reading/writing the macros.json approval file
;; - Detecting macro approval errors from build output
;; - Resolving package fingerprints from Package.resolved
;; - Providing interactive commands for macro approval
;;
;; The approval file is located at:
;;   ~/Library/org.swift.swiftpm/security/macros.json
;;
;; Format:
;;   [
;;     {
;;       "fingerprint": "abc123...",
;;       "packageIdentity": "swift-dependencies",
;;       "targetName": "DependenciesMacrosPlugin"
;;     }
;;   ]

;;; Code:

(require 'json)
(require 'cl-lib)

;; Optional dependencies
(require 'swift-notification nil t)
(require 'xcode-project nil t)

;; Forward declarations
(declare-function swift-notification-send "swift-notification")
(declare-function xcode-project-project-root "xcode-project")
(declare-function spm--project-root "swift-package-manager")
(declare-function spm--package-resolved-path "swift-package-manager")

;;; Customization

(defgroup swift-macro-manager nil
  "Swift macro approval management."
  :group 'swift-development
  :prefix "spm-macro-")

(defcustom spm-macro-auto-approve nil
  "When non-nil, automatically approve macros without prompting.
Use with caution - only enable if you trust all package sources."
  :type 'boolean
  :group 'swift-macro-manager)

(defcustom spm-macro-notify-on-approval t
  "When non-nil, show notification when macros are approved."
  :type 'boolean
  :group 'swift-macro-manager)

;;; Constants

(defconst spm-macro--security-dir
  (expand-file-name "~/Library/org.swift.swiftpm/security/")
  "Directory containing SPM security files.")

(defconst spm-macro--macros-json-path
  (expand-file-name "macros.json" spm-macro--security-dir)
  "Path to the macros.json approval file.")

;; Note: Xcode uses CURLY QUOTES (U+201C, U+201D) not straight quotes
;; The pattern needs to match both curly and straight quotes for robustness
;; U+201C = LEFT DOUBLE QUOTATION MARK, U+201D = RIGHT DOUBLE QUOTATION MARK
(defconst spm-macro--error-pattern-curly
  (concat "Macro [" "\u201C" "\u201D" "]"
          "\\([^" "\u201C" "\u201D" "]+\\)"
          "[" "\u201C" "\u201D" "] from package ["
          "\u201C" "\u201D" "]"
          "\\([^" "\u201C" "\u201D" "]+\\)"
          "[" "\u201C" "\u201D" "]")
  "Pattern to match macro approval errors with curly quotes.")

(defconst spm-macro--error-pattern-straight
  "Macro \"\\([^\"]+\\)\" from package \"\\([^\"]+\\)\""
  "Pattern to match macro approval errors with straight quotes.")

;;; Variables

(defvar spm-macro--last-detected-errors nil
  "List of macro errors detected from the last build.
Each element is a plist with :target-name, :package-identity, :message.")

(defvar spm-macro--approval-buffer "*Swift Macro Approvals*"
  "Buffer name for displaying macro approvals.")

;;; Core Functions

(defun spm-macro--notify (message &optional seconds)
  "Send notification with MESSAGE, shown for SECONDS (default 2)."
  (when spm-macro-notify-on-approval
    (if (fboundp 'swift-notification-send)
        (swift-notification-send :message message :seconds (or seconds 2))
      (message "%s" message))))

(defun spm-macro--project-root ()
  "Get the project root directory."
  (cond
   ((fboundp 'spm--project-root) (spm--project-root))
   ((fboundp 'xcode-project-project-root) (xcode-project-project-root))
   (t (or (locate-dominating-file default-directory "Package.swift")
          (locate-dominating-file default-directory "*.xcodeproj")
          default-directory))))

(defun spm-macro--ensure-security-dir ()
  "Ensure the security directory exists."
  (unless (file-directory-p spm-macro--security-dir)
    (make-directory spm-macro--security-dir t)))

;;; Reading/Writing macros.json

(defun spm-macro-read-approved ()
  "Read and return the list of approved macros from macros.json.
Returns a list of alists with keys: fingerprint, packageIdentity, targetName."
  (if (file-exists-p spm-macro--macros-json-path)
      (condition-case err
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read-file spm-macro--macros-json-path))
        (error
         (message "Failed to parse macros.json: %s" (error-message-string err))
         nil))
    nil))

(defun spm-macro-write-approved (macros)
  "Write MACROS list to macros.json.
MACROS should be a list of alists with keys: fingerprint, packageIdentity, targetName.
Returns t on success, nil on failure."
  (spm-macro--ensure-security-dir)
  (condition-case err
      (let ((json-encoding-pretty-print t)
            (json-encoding-default-indentation "  "))
        (with-temp-file spm-macro--macros-json-path
          (insert (json-encode macros))
          (insert "\n"))
        t)
    (error
     (message "Failed to write macros.json: %s" (error-message-string err))
     nil)))

;;; Package.resolved Parsing

(defun spm-macro--find-package-resolved ()
  "Find the Package.resolved file for the current project.
Checks both SPM and Xcode project locations."
  (let ((root (spm-macro--project-root)))
    (or
     ;; SPM project: Package.resolved at root
     (let ((direct (expand-file-name "Package.resolved" root)))
       (when (file-exists-p direct) direct))
     ;; Xcode project: inside .xcodeproj
     (let ((xcodeproj (car (directory-files root t "\\.xcodeproj$"))))
       (when xcodeproj
         (let ((workspace-resolved
                (expand-file-name
                 "project.xcworkspace/xcshareddata/swiftpm/Package.resolved"
                 xcodeproj)))
           (when (file-exists-p workspace-resolved) workspace-resolved))))
     ;; Xcode workspace: inside .xcworkspace
     (let ((xcworkspace (car (directory-files root t "\\.xcworkspace$"))))
       (when xcworkspace
         (let ((workspace-resolved
                (expand-file-name
                 "xcshareddata/swiftpm/Package.resolved"
                 xcworkspace)))
           (when (file-exists-p workspace-resolved) workspace-resolved)))))))

(defun spm-macro--parse-package-resolved ()
  "Parse Package.resolved and return a hash table of package-identity -> info.
Info is a plist with :version, :revision, :checksum."
  (let ((resolved-path (spm-macro--find-package-resolved)))
    (when (and resolved-path (file-exists-p resolved-path))
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (data (json-read-file resolved-path))
                 (version (cdr (assoc 'version data)))
                 (pins (cond
                        ;; Version 2 and 3 format
                        ((or (= version 2) (= version 3))
                         (cdr (assoc 'pins data)))
                        ;; Version 1 format
                        ((= version 1)
                         (let ((obj (cdr (assoc 'object data))))
                           (cdr (assoc 'pins obj))))
                        (t nil)))
                 (result (make-hash-table :test 'equal)))
            (dolist (pin pins)
              (let* ((identity (or (cdr (assoc 'identity pin))
                                   (cdr (assoc 'package pin))))
                     (state (cdr (assoc 'state pin)))
                     (info (list :version (cdr (assoc 'version state))
                                 :revision (cdr (assoc 'revision state))
                                 :checksum (cdr (assoc 'checksum state)))))
                (when identity
                  ;; Store with lowercase identity for case-insensitive lookup
                  (puthash (downcase identity) info result))))
            result)
        (error
         (message "Failed to parse Package.resolved: %s" (error-message-string err))
         nil)))))

(defun spm-macro-get-fingerprint (package-identity)
  "Get the fingerprint (revision) for PACKAGE-IDENTITY from Package.resolved.
The fingerprint is the git revision SHA or checksum."
  (let* ((packages (spm-macro--parse-package-resolved))
         (normalized-id (downcase package-identity)))
    (when packages
      (let ((info (gethash normalized-id packages)))
        (when info
          (or (plist-get info :revision)
              (plist-get info :checksum)))))))

;;; Macro Error Detection

(defun spm-macro-parse-build-output (output)
  "Parse build OUTPUT for macro approval errors.
Returns a list of plists with :target-name, :package-identity, :message.
Handles both curly quotes (Xcode) and straight quotes."
  (let ((errors nil)
        (seen (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      ;; Try curly quotes first (common in Xcode output)
      (while (re-search-forward spm-macro--error-pattern-curly nil t)
        (let* ((target-name (match-string 1))
               (package-identity (match-string 2))
               (key (concat (downcase package-identity) "/" target-name)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push (list :target-name target-name
                        :package-identity package-identity
                        :message (match-string 0))
                  errors))))
      ;; Also try straight quotes
      (goto-char (point-min))
      (while (re-search-forward spm-macro--error-pattern-straight nil t)
        (let* ((target-name (match-string 1))
               (package-identity (match-string 2))
               (key (concat (downcase package-identity) "/" target-name)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push (list :target-name target-name
                        :package-identity package-identity
                        :message (match-string 0))
                  errors)))))
    (setq spm-macro--last-detected-errors (nreverse errors))
    spm-macro--last-detected-errors))

(defun spm-macro-has-unapproved-p (output)
  "Return non-nil if OUTPUT contains unapproved macro errors."
  (let ((errors (spm-macro-parse-build-output output)))
    (and errors (> (length errors) 0))))

;;; Macro Approval

(defun spm-macro--is-already-approved (package-identity target-name approved-list)
  "Check if macro with PACKAGE-IDENTITY and TARGET-NAME is in APPROVED-LIST.
Returns the existing entry if found, nil otherwise."
  (let ((normalized-id (downcase package-identity)))
    (cl-find-if (lambda (entry)
                  (and (string= (downcase (cdr (assoc 'packageIdentity entry)))
                                normalized-id)
                       (string= (cdr (assoc 'targetName entry)) target-name)))
                approved-list)))

(defun spm-macro-approve-macro (package-identity target-name)
  "Approve the macro TARGET-NAME from PACKAGE-IDENTITY.
Returns t on success, nil on failure."
  (let ((fingerprint (spm-macro-get-fingerprint package-identity)))
    (if (not fingerprint)
        (progn
          (message "Could not find fingerprint for package '%s'. Build project first."
                   package-identity)
          nil)
      (let* ((approved (spm-macro-read-approved))
             (normalized-id (downcase package-identity))
             (existing (spm-macro--is-already-approved package-identity target-name approved)))
        (if existing
            ;; Update fingerprint if changed
            (let ((existing-fp (cdr (assoc 'fingerprint existing))))
              (if (string= existing-fp fingerprint)
                  (progn
                    (message "Macro %s/%s already approved with current fingerprint"
                             package-identity target-name)
                    t)
                ;; Update fingerprint
                (setf (cdr (assoc 'fingerprint existing)) fingerprint)
                (when (spm-macro-write-approved approved)
                  (spm-macro--notify (format "Updated fingerprint for %s" target-name))
                  t)))
          ;; Add new entry
          (let ((new-entry `((fingerprint . ,fingerprint)
                             (packageIdentity . ,normalized-id)
                             (targetName . ,target-name))))
            (push new-entry approved)
            (when (spm-macro-write-approved approved)
              (spm-macro--notify (format "Approved macro: %s" target-name))
              t)))))))

(defun spm-macro-approve-all (macro-errors)
  "Approve all macros in MACRO-ERRORS list.
MACRO-ERRORS should be a list of plists with :target-name and :package-identity.
Returns the number of macros approved."
  (let ((count 0))
    (dolist (error macro-errors)
      (let ((target-name (plist-get error :target-name))
            (package-identity (plist-get error :package-identity)))
        (when (spm-macro-approve-macro package-identity target-name)
          (cl-incf count))))
    (when (> count 0)
      (spm-macro--notify (format "Approved %d macro(s)" count)))
    count))

;;; Macro Source Inspection

(defun spm-macro--find-derived-data-checkouts ()
  "Find the SourcePackages/checkouts directory in DerivedData."
  (let* ((root (spm-macro--project-root))
         (project-name (file-name-nondirectory (directory-file-name root)))
         ;; Normalize project name for DerivedData matching
         (derived-data-base "~/Library/Developer/Xcode/DerivedData/")
         (pattern (concat (expand-file-name derived-data-base) "*" project-name "*/SourcePackages/checkouts")))
    (car (file-expand-wildcards pattern))))

(defun spm-macro--find-package-checkout (package-identity)
  "Find the checkout directory for PACKAGE-IDENTITY."
  (let ((checkouts-dir (spm-macro--find-derived-data-checkouts)))
    (when checkouts-dir
      (let* ((normalized-id (downcase (replace-regexp-in-string "[^a-z0-9]" "" package-identity)))
             (dirs (directory-files checkouts-dir t "^[^.]")))
        (cl-find-if
         (lambda (dir)
           (let ((dir-name (downcase (replace-regexp-in-string
                                      "[^a-z0-9]" ""
                                      (file-name-nondirectory dir)))))
             (or (string-match-p normalized-id dir-name)
                 (string-match-p dir-name normalized-id))))
         dirs)))))

(defun spm-macro-find-source-files (package-identity target-name)
  "Find Swift source files for macro TARGET-NAME in PACKAGE-IDENTITY.
Returns a list of file paths."
  (let ((checkout-dir (spm-macro--find-package-checkout package-identity)))
    (when checkout-dir
      (let ((sources-dir (expand-file-name (concat "Sources/" target-name) checkout-dir)))
        (when (file-directory-p sources-dir)
          (let ((files (directory-files-recursively sources-dir "\\.swift$")))
            ;; Sort with macro-related files first
            (sort files
                  (lambda (a b)
                    (let ((a-is-macro (string-match-p "macro" (downcase a)))
                          (b-is-macro (string-match-p "macro" (downcase b))))
                      (cond
                       ((and a-is-macro (not b-is-macro)) t)
                       ((and (not a-is-macro) b-is-macro) nil)
                       (t (string< a b))))))))))))

;;; Interactive Commands

;;;###autoload
(defun spm-macro-approve-unapproved ()
  "Approve unapproved macros from the last build.
If no macro errors were detected, prompts to enter macro details manually."
  (interactive)
  (if spm-macro--last-detected-errors
      (let* ((errors spm-macro--last-detected-errors)
             (count (length errors)))
        (if spm-macro-auto-approve
            (progn
              (spm-macro-approve-all errors)
              (message "Approved %d macro(s). Rebuild to apply changes." count))
          (when (yes-or-no-p
                 (format "Approve %d unapproved macro(s)? " count))
            (spm-macro-approve-all errors)
            (message "Approved %d macro(s). Rebuild to apply changes." count))))
    (message "No unapproved macros detected from last build. Run a build first.")))

;;;###autoload
(defun spm-macro-approve-interactive ()
  "Interactively approve a macro by entering package and target name."
  (interactive)
  (let ((package-identity (read-string "Package identity: "))
        (target-name (read-string "Macro target name: ")))
    (if (spm-macro-approve-macro package-identity target-name)
        (message "Macro approved. Rebuild to apply changes.")
      (message "Failed to approve macro."))))

;;;###autoload
(defun spm-macro-list-approved ()
  "Display currently approved macros in a buffer."
  (interactive)
  (let ((macros (spm-macro-read-approved)))
    (with-current-buffer (get-buffer-create spm-macro--approval-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize " Approved Swift Macros\n\n" 'face 'bold))
        (insert (format "  %-30s %-25s %s\n"
                        (propertize "Package" 'face 'font-lock-keyword-face)
                        (propertize "Target" 'face 'font-lock-function-name-face)
                        (propertize "Fingerprint" 'face 'font-lock-comment-face)))
        (insert "  " (make-string 75 ?─) "\n")
        (if macros
            (dolist (macro macros)
              (let ((pkg (cdr (assoc 'packageIdentity macro)))
                    (target (cdr (assoc 'targetName macro)))
                    (fp (cdr (assoc 'fingerprint macro))))
                (insert (format "  %-30s %-25s %s\n"
                                (propertize (or pkg "?") 'face 'font-lock-keyword-face)
                                (propertize (or target "?") 'face 'font-lock-function-name-face)
                                (propertize (truncate-string-to-width (or fp "?") 20 nil nil "...")
                                            'face 'font-lock-comment-face)))))
          (insert "\n  No approved macros.\n"))
        (insert "\n  " (make-string 75 ?─) "\n")
        (insert (format "\n  Total: %d macro(s)\n" (length macros)))
        (insert (propertize "\n  File: " 'face 'font-lock-comment-face))
        (insert (propertize spm-macro--macros-json-path 'face 'link))
        (insert "\n"))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer spm-macro--approval-buffer)))

;;;###autoload
(defun spm-macro-inspect-source ()
  "Open the source files for an unapproved macro to review before approving."
  (interactive)
  (if spm-macro--last-detected-errors
      (let* ((errors spm-macro--last-detected-errors)
             (choices (mapcar (lambda (e)
                                (format "%s / %s"
                                        (plist-get e :package-identity)
                                        (plist-get e :target-name)))
                              errors))
             (selection (completing-read "Select macro to inspect: " choices nil t))
             (idx (cl-position selection choices :test 'string=))
             (error (nth idx errors))
             (package-identity (plist-get error :package-identity))
             (target-name (plist-get error :target-name))
             (files (spm-macro-find-source-files package-identity target-name)))
        (if files
            (progn
              (find-file (car files))
              (message "Opened %s (1 of %d files)" (file-name-nondirectory (car files)) (length files)))
          (message "Could not find source files for %s/%s. Build the project first."
                   package-identity target-name)))
    (message "No unapproved macros detected. Run a build first.")))

;;;###autoload
(defun spm-macro-remove-approval ()
  "Remove approval for a macro (useful for testing)."
  (interactive)
  (let* ((macros (spm-macro-read-approved))
         (choices (mapcar (lambda (m)
                            (format "%s / %s"
                                    (cdr (assoc 'packageIdentity m))
                                    (cdr (assoc 'targetName m))))
                          macros)))
    (if (null choices)
        (message "No approved macros to remove.")
      (let* ((selection (completing-read "Remove approval for: " choices nil t))
             (idx (cl-position selection choices :test 'string=))
             (to-remove (nth idx macros))
             (new-list (cl-remove to-remove macros :test 'equal)))
        (when (yes-or-no-p (format "Remove approval for %s? " selection))
          (if (spm-macro-write-approved new-list)
              (message "Removed approval for %s" selection)
            (message "Failed to update macros.json")))))))

;;;###autoload
(defun spm-macro-check-and-offer-approval (build-output)
  "Check BUILD-OUTPUT for macro errors and offer to approve them.
Returns t if macros were approved, nil otherwise.
This is meant to be called from the build process."
  (let ((errors (spm-macro-parse-build-output build-output)))
    (when errors
      (let ((count (length errors)))
        (cond
         ;; Auto-approve if enabled
         (spm-macro-auto-approve
          (spm-macro-approve-all errors)
          t)
         ;; Otherwise prompt
         ((yes-or-no-p
           (format "Build failed: %d unapproved macro(s) detected. Approve and rebuild? " count))
          (spm-macro-approve-all errors)
          t)
         (t nil))))))

(provide 'swift-macro-manager)
;;; swift-macro-manager.el ends here
