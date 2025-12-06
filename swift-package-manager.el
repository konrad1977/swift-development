;;; swift-package-manager.el --- Swift Package Manager UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))
;; Keywords: swift, spm, packages

;;; Commentary:

;; Interactive UI for managing Swift Package Manager dependencies.
;; Provides functions to list, add, remove, and update Swift packages.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'transient)
(require 'swift-project nil t)

(defgroup swift-package-manager nil
  "Swift Package Manager integration."
  :group 'swift-development
  :tag "SPM")

(defcustom spm-cache-paths
  '("~/.swiftpm"
    "~/Library/Caches/org.swift.swiftpm"
    "~/Library/Caches/org.swift.packages"
    "~/Library/Caches/org.swift.cloned-sources")
  "Paths to SPM cache directories."
  :type '(repeat directory)
  :group 'swift-package-manager)

(defcustom spm-buffer-name "*SPM Dependencies*"
  "Name of the SPM dependencies buffer."
  :type 'string
  :group 'swift-package-manager)

(defvar spm--current-project-root nil
  "Current project root for SPM operations.")

(defvar spm--dependencies-cache nil
  "Cache of parsed dependencies.")

;; Faces for the dependency list
(defface spm-package-name-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for package names."
  :group 'swift-package-manager)

(defface spm-version-face
  '((t (:inherit font-lock-constant-face)))
  "Face for version numbers."
  :group 'swift-package-manager)

(defface spm-url-face
  '((t (:inherit link)))
  "Face for package URLs."
  :group 'swift-package-manager)

(defface spm-header-face
  '((t (:inherit font-lock-keyword-face :weight bold :height 1.1)))
  "Face for headers."
  :group 'swift-package-manager)

;;; Core functions

(defun spm--project-root ()
  "Get the project root directory."
  (or spm--current-project-root
      (when (fboundp 'swift-project-root)
        (swift-project-root))
      (locate-dominating-file default-directory "Package.swift")
      (locate-dominating-file default-directory "*.xcodeproj")
      default-directory))

(defun spm--run-command (cmd)
  "Run CMD in project root and return output."
  (let* ((root (or spm--current-project-root (spm--project-root)))
         (default-directory root)
         (full-cmd (format "cd %s && %s" (shell-quote-argument root) cmd)))
    (with-temp-buffer
      (shell-command full-cmd (current-buffer) (current-buffer))
      (buffer-string))))

(defun spm--package-resolved-path ()
  "Get path to Package.resolved file."
  (let ((root (spm--project-root)))
    (or (let ((direct (expand-file-name "Package.resolved" root)))
          (when (file-exists-p direct) direct))
        (let ((xcode-path (car (directory-files root t "\\.xcodeproj$"))))
          (when xcode-path
            (let ((workspace-resolved (expand-file-name
                                       "project.xcworkspace/xcshareddata/swiftpm/Package.resolved"
                                       xcode-path)))
              (when (file-exists-p workspace-resolved) workspace-resolved)))))))

(defun spm--parse-package-resolved ()
  "Parse Package.resolved file and return list of dependencies."
  (let ((resolved-path (spm--package-resolved-path)))
    (when (and resolved-path (file-exists-p resolved-path))
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (data (json-read-file resolved-path))
                 (version (cdr (assoc 'version data)))
                 (pins (cond
                        ((or (= version 2) (= version 3))
                         (cdr (assoc 'pins data)))
                        ((= version 1)
                         (let ((obj (cdr (assoc 'object data))))
                           (cdr (assoc 'pins obj))))
                        (t nil))))
            (mapcar #'spm--parse-pin pins))
        (error
         (message "Failed to parse Package.resolved: %s" (error-message-string err))
         nil)))))

(defun spm--parse-pin (pin)
  "Parse a single PIN entry from Package.resolved."
  (let* ((identity (or (cdr (assoc 'identity pin))
                       (cdr (assoc 'package pin))))
         (location (or (cdr (assoc 'location pin))
                       (cdr (assoc 'repositoryURL pin))))
         (state (cdr (assoc 'state pin)))
         (version (cdr (assoc 'version state)))
         (branch (cdr (assoc 'branch state)))
         (revision (cdr (assoc 'revision state))))
    (list :name identity
          :url location
          :version (or version "")
          :branch (or branch "")
          :revision (or revision ""))))

(defun spm--get-dependency-names ()
  "Get list of dependency names."
  (mapcar (lambda (d) (plist-get d :name))
          (or spm--dependencies-cache (spm--parse-package-resolved))))

;;; Display functions

(defun spm--format-dependency-line (dep)
  "Format a single dependency DEP for display."
  (let* ((name (or (plist-get dep :name) "Unknown"))
         (version (plist-get dep :version))
         (branch (plist-get dep :branch))
         (url (plist-get dep :url))
         (short-url (when url
                      (replace-regexp-in-string "^https?://\\(www\\.\\)?github\\.com/" "" url)))
         (version-str (cond
                       ((and version (not (string-empty-p version))) version)
                       ((and branch (not (string-empty-p branch))) (format "branch:%s" branch))
                       (t "unknown"))))
    (format "  %-28s %-12s %s"
            (propertize name 'face 'spm-package-name-face)
            (propertize version-str 'face 'spm-version-face)
            (if short-url
                (propertize short-url 'face 'spm-url-face)
              ""))))

(defun spm--render-buffer (dependencies)
  "Render DEPENDENCIES in the SPM buffer."
  (let ((inhibit-read-only t)
        (project-name (file-name-nondirectory
                       (directory-file-name (spm--project-root)))))
    (erase-buffer)
    (insert (propertize (format " Swift Package Dependencies - %s\n\n" project-name)
                        'face 'spm-header-face))
    (insert (format "  %-28s %-12s %s\n"
                    (propertize "Package" 'face 'bold)
                    (propertize "Version" 'face 'bold)
                    (propertize "Source" 'face 'bold)))
    (insert "  " (make-string 70 ?─) "\n")
    (if dependencies
        (dolist (dep dependencies)
          (insert (spm--format-dependency-line dep) "\n"))
      (insert "\n  No dependencies found.\n"))
    (insert "\n  " (make-string 70 ?─) "\n")
    (insert (propertize "  Press '?' for actions menu\n" 'face 'font-lock-comment-face))
    (goto-char (point-min))))

;;; Mode definition

(defvar spm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'spm-dispatch)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'spm-refresh)
    map)
  "Keymap for SPM mode.")

(define-derived-mode spm-mode special-mode "SPM"
  "Major mode for viewing Swift Package Manager dependencies.

Press '?' to open the actions menu.

\\{spm-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (hl-line-mode 1))

;;; Transient menu

(transient-define-prefix spm-dispatch ()
  "Swift Package Manager actions."
  ["Package Actions"
   [("u" "Update package..." spm-update-package)
    ("U" "Update all packages" spm-update-all)
    ("a" "Add package..." spm-add-package)
    ("d" "Remove package..." spm-remove-package)]
   [("r" "Resolve dependencies" spm-resolve)
    ("g" "Refresh list" spm-refresh)
    ("G" "Show dependency graph" spm-dependency-graph)]]
  ["Maintenance"
   [("c" "Clean SPM cache" spm-clean-cache)
    ("i" "Package info" spm-describe-package)]]
  ["Navigation"
   [("q" "Quit" quit-window)]])

;;; Interactive commands

;;;###autoload
(defun spm-list-dependencies ()
  "Show all Swift Package dependencies in a dedicated buffer."
  (interactive)
  (let* ((root (spm--project-root))
         (dependencies (spm--parse-package-resolved)))
    (setq spm--current-project-root root)
    (setq spm--dependencies-cache dependencies)
    (with-current-buffer (get-buffer-create spm-buffer-name)
      (spm-mode)
      (setq-local spm--current-project-root root)
      (spm--render-buffer dependencies))
    (pop-to-buffer spm-buffer-name)))

;;;###autoload
(defun spm-refresh ()
  "Refresh the dependencies list."
  (interactive)
  (let ((dependencies (spm--parse-package-resolved)))
    (setq spm--dependencies-cache dependencies)
    (when (get-buffer spm-buffer-name)
      (with-current-buffer spm-buffer-name
        (spm--render-buffer dependencies)))
    (message "Dependencies refreshed")))

;;;###autoload
(defun spm-add-package (url version)
  "Add a new Swift package from URL with VERSION."
  (interactive
   (list (read-string "Package URL (GitHub URL or user/repo): ")
         (read-string "Version (empty for latest): ")))
  (let* ((full-url (if (string-match-p "^https?://" url)
                       url
                     (format "https://github.com/%s" url)))
         (cmd (if (and version (not (string-empty-p version)))
                  (format "swift package add %s --from %s"
                          (shell-quote-argument full-url)
                          (shell-quote-argument version))
                (format "swift package add %s"
                        (shell-quote-argument full-url)))))
    (message "Adding package...")
    (let ((output (spm--run-command cmd)))
      (if (string-match-p "error:" output)
          (message "Failed to add package: %s" (string-trim output))
        (message "Package added successfully")
        (spm-refresh)))))

;;;###autoload
(defun spm-remove-package (name)
  "Remove Swift package with NAME."
  (interactive
   (list (completing-read "Remove package: " (spm--get-dependency-names) nil t)))
  (when (yes-or-no-p (format "Remove package '%s'? " name))
    (message "Removing package %s..." name)
    (let ((output (spm--run-command
                   (format "swift package remove %s" (shell-quote-argument name)))))
      (if (string-match-p "error:" output)
          (message "Failed to remove package: %s" (string-trim output))
        (message "Package '%s' removed" name)
        (spm-refresh)))))

;;;###autoload
(defun spm-update-package (name)
  "Update Swift package with NAME to latest compatible version."
  (interactive
   (list (completing-read "Update package: " (spm--get-dependency-names) nil t)))
  (message "Updating package %s..." name)
  (let ((output (spm--run-command
                 (format "swift package update %s" (shell-quote-argument name)))))
    (if (string-match-p "error:" output)
        (message "Failed to update: %s" (string-trim output))
      (message "Package '%s' updated" name)
      (spm-refresh))))

;;;###autoload
(defun spm-update-all ()
  "Update all Swift packages to latest compatible versions."
  (interactive)
  (when (yes-or-no-p "Update all packages? ")
    (message "Updating all packages...")
    (let ((output (spm--run-command "swift package update")))
      (if (string-match-p "error:" output)
          (message "Update failed: %s" (string-trim output))
        (message "All packages updated")
        (spm-refresh)))))

;;;###autoload
(defun spm-resolve ()
  "Resolve package dependencies."
  (interactive)
  (message "Resolving packages...")
  (let ((output (spm--run-command "swift package resolve")))
    (if (string-match-p "error:" output)
        (message "Resolution failed: %s" (string-trim output))
      (message "Packages resolved")
      (spm-refresh))))

;;;###autoload
(defun spm-dependency-graph ()
  "Show dependency graph for the project."
  (interactive)
  (let ((output (spm--run-command "swift package show-dependencies --format dot")))
    (if (string-match-p "error:" output)
        (message "Failed to generate graph: %s" (string-trim output))
      (with-current-buffer (get-buffer-create "*SPM Dependency Graph*")
        (erase-buffer)
        (insert output)
        (when (fboundp 'graphviz-dot-mode)
          (graphviz-dot-mode))
        (goto-char (point-min)))
      (pop-to-buffer "*SPM Dependency Graph*")
      (message "Dependency graph generated (DOT format)"))))

;;;###autoload
(defun spm-clean-cache ()
  "Clean SPM cache directories."
  (interactive)
  (when (yes-or-no-p "Clean all SPM caches? This may require re-downloading packages. ")
    (let ((cleaned 0))
      (dolist (path spm-cache-paths)
        (let ((expanded (expand-file-name path)))
          (when (file-directory-p expanded)
            (delete-directory expanded t)
            (cl-incf cleaned))))
      (let ((build-dir (expand-file-name ".build" (spm--project-root))))
        (when (file-directory-p build-dir)
          (delete-directory build-dir t)
          (cl-incf cleaned)))
      (message "Cleaned %d cache directories" cleaned))))

;;;###autoload
(defun spm-describe-package ()
  "Describe the Swift package."
  (interactive)
  (let ((output (spm--run-command "swift package describe")))
    (with-current-buffer (get-buffer-create "*SPM Package Info*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (pop-to-buffer "*SPM Package Info*")))

;;;###autoload
(transient-define-prefix spm-transient ()
  "Swift Package Manager."
  ["Swift Package Manager"
   [("l" "List dependencies" spm-list-dependencies)
    ("a" "Add package..." spm-add-package)
    ("u" "Update package..." spm-update-package)
    ("U" "Update all" spm-update-all)]
   [("r" "Resolve" spm-resolve)
    ("c" "Clean cache" spm-clean-cache)
    ("G" "Dependency graph" spm-dependency-graph)
    ("i" "Package info" spm-describe-package)]]
  [("q" "Quit" transient-quit-one)])

(provide 'swift-package-manager)
;;; swift-package-manager.el ends here
