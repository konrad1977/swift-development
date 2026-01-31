;;; swift-package-manager.el --- Swift Package Manager UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
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
(require 'swift-notification nil t)
(require 'xcode-project nil t)
(require 'xcode-build-config nil t)

;; Forward declarations
(defvar swift-development-force-package-resolution)
(defvar swift-development-use-periphery)
(defvar swift-development-analysis-mode)
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-get-workspace-or-project "xcode-project")
(declare-function xcode-project-scheme "xcode-project")
(declare-function xcode-project-notify "xcode-project")
(declare-function xcode-build-config-check-swift-packages-in-build "xcode-build-config")
(declare-function xcode-build-config-setup-build-environment "xcode-build-config")

(defun spm--notify (message &optional seconds)
  "Send notification with MESSAGE, shown for SECONDS (default 2)."
  (if (fboundp 'swift-notification-send)
      (swift-notification-send :message message :seconds (or seconds 2))
    (message "%s" message)))

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
    (spm--notify "Dependencies refreshed" 2)))

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
    (spm--notify "Adding package..." 3)
    (let ((output (spm--run-command cmd)))
      (if (string-match-p "error:" output)
          (spm--notify (format "Failed to add package: %s" (string-trim output)) 3)
        (spm--notify "Package added successfully" 2)
        (spm-refresh)))))

;;;###autoload
(defun spm-remove-package (name)
  "Remove Swift package with NAME."
  (interactive
   (list (completing-read "Remove package: " (spm--get-dependency-names) nil t)))
  (when (yes-or-no-p (format "Remove package '%s'? " name))
    (spm--notify (format "Removing package %s..." name) 3)
    (let ((output (spm--run-command
                   (format "swift package remove %s" (shell-quote-argument name)))))
      (if (string-match-p "error:" output)
          (spm--notify (format "Failed to remove package: %s" (string-trim output)) 3)
        (spm--notify (format "Package '%s' removed" name) 2)
        (spm-refresh)))))

;;;###autoload
(defun spm-update-package (name)
  "Update Swift package with NAME to latest compatible version."
  (interactive
   (list (completing-read "Update package: " (spm--get-dependency-names) nil t)))
  (spm--notify (format "Updating package %s..." name) 3)
  (let ((output (spm--run-command
                 (format "swift package update %s" (shell-quote-argument name)))))
    (if (string-match-p "error:" output)
        (spm--notify (format "Failed to update: %s" (string-trim output)) 3)
      (spm--notify (format "Package '%s' updated" name) 2)
      (spm-refresh))))

;;;###autoload
(defun spm-update-all ()
  "Update all Swift packages to latest compatible versions."
  (interactive)
  (when (yes-or-no-p "Update all packages? ")
    (spm--notify "Updating all packages..." 3)
    (let ((output (spm--run-command "swift package update")))
      (if (string-match-p "error:" output)
          (spm--notify (format "Update failed: %s" (string-trim output)) 3)
        (spm--notify "All packages updated" 2)
        (spm-refresh)))))

;;;###autoload
(defun spm-resolve ()
  "Resolve package dependencies."
  (interactive)
  (spm--notify "Resolving packages..." 3)
  (let ((output (spm--run-command "swift package resolve")))
    (if (string-match-p "error:" output)
        (spm--notify (format "Resolution failed: %s" (string-trim output)) 3)
      (spm--notify "Packages resolved" 2)
      (spm-refresh))))

;;;###autoload
(defun spm-dependency-graph ()
  "Show dependency graph for the project."
  (interactive)
  (spm--notify "Generating dependency graph..." 2)
  (let ((output (spm--run-command "swift package show-dependencies --format dot")))
    (if (string-match-p "error:" output)
        (spm--notify (format "Failed to generate graph: %s" (string-trim output)) 3)
      (with-current-buffer (get-buffer-create "*SPM Dependency Graph*")
        (erase-buffer)
        (insert output)
        (when (fboundp 'graphviz-dot-mode)
          (graphviz-dot-mode))
        (goto-char (point-min)))
      (pop-to-buffer "*SPM Dependency Graph*")
      (spm--notify "Dependency graph generated" 2))))

;;;###autoload
(defun spm-clean-cache ()
  "Clean SPM cache directories."
  (interactive)
  (when (yes-or-no-p "Clean all SPM caches? This may require re-downloading packages. ")
    (spm--notify "Cleaning SPM caches..." 3)
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
      (spm--notify (format "Cleaned %d cache directories" cleaned) 2))))

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

;;; Build Integration Functions
;; These functions integrate SPM with the build process

(defun spm-extract-package-name (url-or-path)
  "Extract package name from URL-OR-PATH.
Examples:
  https://github.com/user/MyPackage.git -> MyPackage
  https://github.com/user/my-package -> my-package
  /path/to/LocalPackage -> LocalPackage"
  (let ((cleaned (replace-regexp-in-string "\\.git$" "" url-or-path)))
    (if (string-match "\\([^/]+\\)$" cleaned)
        (match-string 1 cleaned)
      url-or-path)))

;;;###autoload
(defun spm-watch-download ()
  "Watch the package download progress in real-time."
  (interactive)
  (let* ((project-root (spm--project-root))
         (source-packages (expand-file-name ".build/SourcePackages" project-root))
         (global-cache (expand-file-name "~/Library/Caches/org.swift.packages")))
    ;; Get package names from directories
    (let ((source-pkg-names (when (file-exists-p source-packages)
                              (mapcar (lambda (dir)
                                       (file-name-nondirectory dir))
                                     (directory-files source-packages t "^[^.]" t))))
          (cache-pkg-names (when (file-exists-p global-cache)
                            (mapcar (lambda (dir)
                                     (file-name-nondirectory dir))
                                   (directory-files global-cache t "^[^.]" t)))))
      
      (message "Package Status:")
      (message "  Global cache: %s" 
               (if cache-pkg-names
                   (format "%d packages [%s...]" 
                           (length cache-pkg-names)
                           (string-join (seq-take cache-pkg-names 3) ", "))
                 "Empty"))
      (message "  .build/SourcePackages: %s" 
               (if source-pkg-names
                   (format "%d packages [%s...]" 
                           (length source-pkg-names)
                           (string-join (seq-take source-pkg-names 3) ", "))
                 "Not yet created"))
      
      ;; Check current activity in build output and update mode-line HUD
      (when (get-buffer "*Swift Build Output*")
        (with-current-buffer "*Swift Build Output*"
          (goto-char (point-max))
          (when (fboundp 'xcode-project-notify)
            (cond
             ;; Check for package operations and update notification system
             ((re-search-backward "Fetching \\(.*\\)" nil t)
              (let ((pkg-name (spm-extract-package-name (match-string 1))))
                (xcode-project-notify :message (format "Fetching %s"
                                                       (propertize pkg-name 'face 'font-lock-function-name-face)))))
             ((re-search-backward "Cloning \\(.*\\)" nil t)
              (let ((pkg-name (spm-extract-package-name (match-string 1))))
                (xcode-project-notify :message (format "Cloning %s"
                                                       (propertize pkg-name 'face 'font-lock-function-name-face)))))
             ((re-search-backward "Computing version for \\(.*\\)" nil t)
              (let ((pkg-name (spm-extract-package-name (match-string 1))))
                (xcode-project-notify :message (format "Computing %s"
                                                       (propertize pkg-name 'face 'font-lock-variable-name-face)))))
             ((re-search-backward "Resolving \\(.*\\)" nil t)
              (let ((pkg-name (spm-extract-package-name (match-string 1))))
                (xcode-project-notify :message (format "Resolving %s"
                                                       (propertize pkg-name 'face 'font-lock-keyword-face)))))
             ((re-search-backward "Creating working copy for \\(.*\\)" nil t)
              (let ((pkg-name (spm-extract-package-name (match-string 1))))
                (xcode-project-notify :message (format "Creating %s"
                                                       (propertize pkg-name 'face 'font-lock-type-face)))))
             ;; Check for compilation of packages
             ((re-search-backward "Building \\(.*\\)\\.o" nil t)
              (let ((module-name (file-name-nondirectory (match-string 1))))
                (xcode-project-notify :message (format "Building %s"
                                                       (propertize module-name 'face 'font-lock-builtin-face)))))
             ((re-search-backward "Compiling \\(.*\\)\\.swift" nil t)
              (let ((file-name (file-name-nondirectory (match-string 1))))
                (xcode-project-notify :message (format "Compiling %s"
                                                       (propertize file-name 'face 'warning))))))))))))

;;;###autoload
(defun spm-monitor-build-progress ()
  "Monitor build progress with package status updates."
  (interactive)
  (run-with-timer 0 2 'spm-watch-download))

;;;###autoload
(defun spm-check-status ()
  "Check and display Swift package status for .build-based builds."
  (interactive)
  (let* ((project-root (spm--project-root))
         (local-source-packages (expand-file-name ".build/SourcePackages" project-root))
         (local-checkouts (expand-file-name ".build/checkouts" project-root))
         (build-intermediates (expand-file-name ".build/Build/Intermediates.noindex" project-root))
         (build-dir (expand-file-name ".build" project-root))
         ;; Find DerivedData for this project
         (derived-data-glob (format "~/Library/Developer/Xcode/DerivedData/*%s*/SourcePackages" 
                                   (file-name-nondirectory (directory-file-name project-root))))
         (xcode-source-packages (car (file-expand-wildcards derived-data-glob)))
         (packages-exist (when (fboundp 'xcode-build-config-check-swift-packages-in-build)
                           (xcode-build-config-check-swift-packages-in-build)))
         ;; Count packages in various locations
         (local-pkg-count (if (file-exists-p local-source-packages)
                             (length (directory-files local-source-packages nil "^[^.]" t)) 0))
         (xcode-pkg-count (if (and xcode-source-packages (file-exists-p xcode-source-packages))
                             (length (directory-files xcode-source-packages nil "^[^.]" t)) 0))
         (compiled-packages (when (and (file-exists-p build-intermediates)
                                      (file-directory-p build-intermediates))
                             (condition-case nil
                                 (length (directory-files build-intermediates nil "\\.build$" t))
                               (error 0)))))
    (message "Swift Package Status:
Packages available: %s
Compiled in build: %s packages
Local .build/SourcePackages: %d packages
Xcode DerivedData packages: %d packages  
.build/checkouts: %s
.build directory: %s
Resolution mode: %s
Force resolution: %s
Xcode SourcePackages location: %s"
             (if packages-exist "Yes" "No")
             (or compiled-packages 0)
             local-pkg-count
             xcode-pkg-count
             (if (file-exists-p local-checkouts) "Exists" "Missing")
             (if (file-exists-p build-dir) "Exists" "Missing")
             (if (boundp 'xcode-build-config-skip-package-resolution)
                 xcode-build-config-skip-package-resolution
               "N/A")
             (if (and (boundp 'swift-development-force-package-resolution)
                      swift-development-force-package-resolution)
                 "Yes" "No")
             (or xcode-source-packages "Not found"))))

;;;###autoload
(defun spm-force-resolve ()
  "Force Swift package dependency resolution on next build."
  (interactive)
  (setq swift-development-force-package-resolution t)
  (spm--notify "Will force package resolution on next build"))

;;;###autoload
(defun spm-toggle-resolution-mode ()
  "Toggle Swift package resolution mode between auto/always/never."
  (interactive)
  (when (boundp 'xcode-build-config-skip-package-resolution)
    (setq xcode-build-config-skip-package-resolution
          (cond
           ((eq xcode-build-config-skip-package-resolution 'auto) 'always)
           ((eq xcode-build-config-skip-package-resolution 'always) 'never)
           ((eq xcode-build-config-skip-package-resolution 'never) 'auto)
           (t 'auto)))
    (spm--notify (format "Package resolution mode: %s" xcode-build-config-skip-package-resolution))))

;;;###autoload
(defun spm-prebuild ()
  "Pre-build Swift packages to speed up subsequent builds.
This builds all package dependencies once so they're cached for future builds."
  (interactive)
  (let* ((project-root (spm--project-root))
         (workspace-or-project (when (fboundp 'xcode-project-get-workspace-or-project)
                                 (xcode-project-get-workspace-or-project)))
         (scheme (when (fboundp 'xcode-project-scheme)
                   (xcode-project-scheme))))
    (spm--notify "Pre-building Swift packages...")
    (async-shell-command
     (format "cd '%s' && xcrun xcodebuild build %s -scheme %s -destination 'generic/platform=iOS Simulator' -derivedDataPath .build -onlyUsePackageVersionsFromResolvedFile -skipPackageUpdates -configuration Debug SWIFT_COMPILATION_MODE=wholemodule BUILD_LIBRARIES_FOR_DISTRIBUTION=YES | head -100"
             project-root (or workspace-or-project "") (or scheme ""))
     "*Swift Package Prebuild*")
    (spm--notify "Pre-building packages in background")))

;;;###autoload
(defun spm-clean-build-dir ()
  "Clean .build directory to force fresh package download."
  (interactive)
  (let* ((project-root (spm--project-root))
         (build-dir (expand-file-name ".build" project-root)))
    (if (file-exists-p build-dir)
        (when (yes-or-no-p "Clean .build directory? This will force re-download of all packages.")
          (spm--notify "Cleaning .build directory...")
          (delete-directory build-dir t)
          (spm--notify ".build directory cleaned"))
      (spm--notify ".build directory doesn't exist"))))

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
  ["Build Integration"
   [("s" "Check status" spm-check-status)
    ("w" "Watch download" spm-watch-download)
    ("p" "Prebuild packages" spm-prebuild)]
   [("C" "Clean .build dir" spm-clean-build-dir)
    ("t" "Toggle resolution mode" spm-toggle-resolution-mode)
    ("f" "Force resolve next build" spm-force-resolve)]]
  [("q" "Quit" transient-quit-one)])

;; Backwards compatibility aliases
(defalias 'swift-development-watch-package-download 'spm-watch-download)
(defalias 'swift-development-monitor-build-progress 'spm-monitor-build-progress)
(defalias 'swift-development-check-package-status 'spm-check-status)
(defalias 'swift-development-force-resolve-packages 'spm-force-resolve)
(defalias 'swift-development-toggle-package-resolution 'spm-toggle-resolution-mode)
(defalias 'swift-development-prebuild-packages 'spm-prebuild)
(defalias 'swift-development-clean-build-packages 'spm-clean-build-dir)
(defalias 'swift-development-extract-package-name 'spm-extract-package-name)

(provide 'swift-package-manager)
;;; swift-package-manager.el ends here
