;;; swift-file-watcher.el --- File change detection for Swift projects -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, files, tools

;;; Commentary:

;; This module provides file change detection for Swift projects using
;; Emacs' file-notify API.  It enables instant rebuild detection by
;; tracking file changes in real-time, eliminating the need for expensive
;; find/stat operations on every build.
;;
;; Usage:
;;   (swift-file-watcher-start "/path/to/project")  ; Start watching
;;   (swift-file-watcher-needs-rebuild-p)           ; Check if rebuild needed (instant)
;;   (swift-file-watcher-mark-built)                ; Mark as built after successful build
;;   (swift-file-watcher-stop)                      ; Stop watching
;;   (swift-file-watcher-status)                    ; Show debug status

;;; Code:

(require 'filenotify)
(require 'cl-lib)

;; Optional dependencies
(require 'nerd-icons nil t)

;;; ============================================================================
;;; Customization Variables
;;; ============================================================================

(defgroup swift-file-watcher nil
  "File change detection for Swift projects."
  :group 'swift-development
  :prefix "swift-file-watcher-")

(defcustom swift-file-watcher-enabled t
  "Enable file watching for instant rebuild detection.
When enabled, file changes are tracked in real-time using file-notify.
When disabled, falls back to find/stat method."
  :type 'boolean
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-show-indicator nil
  "Show rebuild status indicator in mode line.
When non-nil, displays an indicator showing whether rebuild is needed."
  :type 'boolean
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-indicator-style 'text
  "Style of the mode line indicator.
- `text': Show [Swift] or [Swift*] 
- `icon': Use nerd-icons (if available)
- `minimal': Show only * or nothing"
  :type '(choice (const :tag "Text: [Swift*]" text)
                 (const :tag "Icons (nerd-icons)" icon)
                 (const :tag "Minimal: *" minimal))
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-debounce-delay 0.5
  "Delay in seconds before processing file change events.
Multiple rapid changes are coalesced into a single update.
Lower values are more responsive but may cause more CPU usage."
  :type 'number
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-ignored-directories
  '("Pods" "Carthage" ".build" "DerivedData" ".git" "node_modules"
    "vendor" "Build" "build" ".swiftpm" "SourcePackages")
  "Directories to ignore when setting up file watchers.
These directories are typically large and contain generated content."
  :type '(repeat string)
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-debug nil
  "Enable debug logging for file watcher operations."
  :type 'boolean
  :group 'swift-file-watcher)

(defcustom swift-file-watcher-max-depth 10
  "Maximum directory depth to watch for file changes.
Higher values catch deeper nested source files but use more watchers.
Default is 10, which handles most iOS project structures."
  :type 'integer
  :group 'swift-file-watcher)

;;; ============================================================================
;;; Internal Variables
;;; ============================================================================

(defvar swift-file-watcher--watchers nil
  "List of active file-notify watch descriptors.")

(defvar swift-file-watcher--project-root nil
  "Root directory of the currently watched project.")

(defvar swift-file-watcher--dirty-p nil
  "Non-nil if files have changed since last build.")

(defvar swift-file-watcher--last-change nil
  "Information about the last file change: (timestamp . filepath).")

(defvar swift-file-watcher--debounce-timer nil
  "Timer for debouncing file change events.")

(defvar swift-file-watcher--watched-dirs nil
  "List of directories currently being watched.")

(defvar swift-file-watcher--change-count 0
  "Number of file changes since last build.")

;;; ============================================================================
;;; Watched Extensions (from swift-development)
;;; ============================================================================

(defvar swift-file-watcher--default-extensions
  '("swift" "m" "mm" "h" "c" "cpp" "storyboard" "xib" "xcassets"
    "strings" "plist" "json" "xcconfig")
  "Default file extensions to watch for changes.")

(defun swift-file-watcher--get-watched-extensions ()
  "Get list of file extensions to watch.
Uses `swift-development-watched-extensions' if available."
  (if (boundp 'swift-development-watched-extensions)
      swift-development-watched-extensions
    swift-file-watcher--default-extensions))

;;; ============================================================================
;;; Ignore Patterns (from swift-development)
;;; ============================================================================

(defvar swift-file-watcher--default-ignore-patterns
  '("*Tests/*" "*/Tests.swift" "*UITests/*" "*/.build/*"
    "*/DerivedData/*" "*/.git/*" "*/Pods/*" "*/Carthage/*")
  "Default path patterns to ignore.")

(defun swift-file-watcher--get-ignore-patterns ()
  "Get list of path patterns to ignore.
Uses `swift-development-ignore-paths' if available."
  (if (boundp 'swift-development-ignore-paths)
      swift-development-ignore-paths
    swift-file-watcher--default-ignore-patterns))

;;; ============================================================================
;;; File Filtering
;;; ============================================================================

(defun swift-file-watcher--should-watch-file-p (filepath)
  "Return non-nil if FILEPATH should trigger a rebuild.
Checks file extension and ignore patterns."
  (when (and filepath (stringp filepath) (file-exists-p filepath))
    (let* ((ext (file-name-extension filepath))
           (watched-exts (swift-file-watcher--get-watched-extensions))
           (ignore-patterns (swift-file-watcher--get-ignore-patterns))
           (relative-path (if swift-file-watcher--project-root
                              (file-relative-name filepath swift-file-watcher--project-root)
                            filepath)))
      (and
       ;; Check extension matches
       ext
       (member ext watched-exts)
       ;; Check not in ignored paths
       (not (cl-some (lambda (pattern)
                       (string-match-p (wildcard-to-regexp pattern) relative-path))
                     ignore-patterns))
       ;; Check not in ignored directories
       (not (cl-some (lambda (dir)
                       (string-match-p (format "\\(^\\|/\\)%s\\(/\\|$\\)" (regexp-quote dir))
                                       relative-path))
                     swift-file-watcher-ignored-directories))))))

(defun swift-file-watcher--should-watch-dir-p (dirpath)
  "Return non-nil if DIRPATH should be watched.
Excludes directories in `swift-file-watcher-ignored-directories'."
  (let ((dirname (file-name-nondirectory (directory-file-name dirpath))))
    (not (member dirname swift-file-watcher-ignored-directories))))

;;; ============================================================================
;;; Directory Discovery
;;; ============================================================================

(defun swift-file-watcher--find-watch-dirs (project-root)
  "Find directories to watch in PROJECT-ROOT.
Returns a list of directories that likely contain source files.
Recursively searches up to `swift-file-watcher-max-depth' levels deep."
  (let ((dirs '())
        (max-depth (or swift-file-watcher-max-depth 6)))
    
    (cl-labels ((collect-dirs (dir depth)
                  (when (and (< depth max-depth)
                             (swift-file-watcher--should-watch-dir-p dir))
                    (push dir dirs)
                    (dolist (entry (ignore-errors (directory-files dir t "^[^.]" t)))
                      (when (file-directory-p entry)
                        (collect-dirs entry (1+ depth)))))))
      (collect-dirs project-root 0))
    
    (when swift-file-watcher-debug
      (message "[FileWatcher] Found %d directories to watch" (length dirs)))
    
    (nreverse dirs)))

;;; ============================================================================
;;; File Notify Callback
;;; ============================================================================

(defun swift-file-watcher--handle-event (event)
  "Handle a file-notify EVENT.
EVENT is a list (DESCRIPTOR ACTION FILE [FILE1])."
  (when swift-file-watcher-debug
    (message "[FileWatcher] Event: %S" event))
  
  (let* ((action (nth 1 event))
         (file (nth 2 event)))
    ;; Only process relevant actions
    (when (and (memq action '(created changed renamed))
               file
               (swift-file-watcher--should-watch-file-p file))
      ;; Debounce rapid changes
      (when swift-file-watcher--debounce-timer
        (cancel-timer swift-file-watcher--debounce-timer))
      
      (setq swift-file-watcher--debounce-timer
            (run-with-timer
             swift-file-watcher-debounce-delay nil
             (lambda ()
               (setq swift-file-watcher--dirty-p t
                     swift-file-watcher--last-change (cons (current-time) file)
                     swift-file-watcher--change-count (1+ swift-file-watcher--change-count))
               
               (when swift-file-watcher-debug
                 (message "[FileWatcher] Marked dirty: %s" 
                          (file-name-nondirectory file)))
               
               ;; Force mode-line update if indicator is enabled
               (when swift-file-watcher-show-indicator
                 (force-mode-line-update t))))))))

;;; ============================================================================
;;; Public API
;;; ============================================================================

;;;###autoload
(cl-defun swift-file-watcher-start (project-root)
  "Start watching PROJECT-ROOT for file changes.
Sets up file-notify watchers for relevant directories."
  (interactive (list (read-directory-name "Project root: ")))
  
  (unless swift-file-watcher-enabled
    (when swift-file-watcher-debug
      (message "[FileWatcher] Disabled, not starting"))
    (cl-return-from swift-file-watcher-start nil))
  
  ;; Stop any existing watchers first
  (swift-file-watcher-stop)
  
  (setq swift-file-watcher--project-root (expand-file-name project-root)
        swift-file-watcher--dirty-p nil
        swift-file-watcher--last-change nil
        swift-file-watcher--change-count 0)
  
  ;; Find directories to watch
  (let ((dirs (swift-file-watcher--find-watch-dirs swift-file-watcher--project-root)))
    (setq swift-file-watcher--watched-dirs dirs)
    
    ;; Create watchers for each directory
    (dolist (dir dirs)
      (condition-case err
          (let ((descriptor (file-notify-add-watch
                             dir
                             '(change)
                             #'swift-file-watcher--handle-event)))
            (push descriptor swift-file-watcher--watchers)
            (when swift-file-watcher-debug
              (message "[FileWatcher] Watching: %s" dir)))
        (error
         (when swift-file-watcher-debug
           (message "[FileWatcher] Failed to watch %s: %s" dir (error-message-string err))))))
    
    (when swift-file-watcher-debug
      (message "[FileWatcher] Started with %d watchers for %s"
               (length swift-file-watcher--watchers)
               (file-name-nondirectory (directory-file-name project-root))))
    
    ;; Return number of active watchers
    (length swift-file-watcher--watchers)))

;;;###autoload
(defun swift-file-watcher-stop ()
  "Stop all file watchers."
  (interactive)
  (when swift-file-watcher--debounce-timer
    (cancel-timer swift-file-watcher--debounce-timer)
    (setq swift-file-watcher--debounce-timer nil))
  
  (dolist (descriptor swift-file-watcher--watchers)
    (condition-case nil
        (file-notify-rm-watch descriptor)
      (error nil)))  ; Ignore errors from already-removed watchers
  
  (let ((count (length swift-file-watcher--watchers)))
    (setq swift-file-watcher--watchers nil
          swift-file-watcher--watched-dirs nil
          swift-file-watcher--project-root nil)
    
    (when swift-file-watcher-debug
      (message "[FileWatcher] Stopped %d watchers" count))
    
    count))

;;;###autoload
(defun swift-file-watcher-restart ()
  "Restart file watchers for current project."
  (interactive)
  (let ((project-root swift-file-watcher--project-root))
    (if project-root
        (progn
          (swift-file-watcher-stop)
          (swift-file-watcher-start project-root)
          (message "File watcher restarted for %s" 
                   (file-name-nondirectory (directory-file-name project-root))))
      (message "No project currently being watched"))))

;;;###autoload
(defun swift-file-watcher-needs-rebuild-p ()
  "Return non-nil if files have changed since last build.
This is an instant check (no I/O) when watcher is active."
  swift-file-watcher--dirty-p)

;;;###autoload
(defun swift-file-watcher-mark-built ()
  "Mark the project as successfully built.
Resets the dirty flag and change counter."
  (setq swift-file-watcher--dirty-p nil
        swift-file-watcher--change-count 0)
  
  (when swift-file-watcher-debug
    (message "[FileWatcher] Marked as built"))
  
  ;; Force mode-line update if indicator is enabled
  (when swift-file-watcher-show-indicator
    (force-mode-line-update t)))

;;;###autoload
(defun swift-file-watcher-mark-dirty ()
  "Force the project to be marked as needing rebuild.
Use this after scheme changes or other configuration changes."
  (setq swift-file-watcher--dirty-p t)
  (when swift-file-watcher-debug
    (message "[FileWatcher] Marked as dirty (forced)"))
  (when swift-file-watcher-show-indicator
    (force-mode-line-update t)))

;;;###autoload
(defun swift-file-watcher-active-p ()
  "Return non-nil if file watcher is currently active."
  (and swift-file-watcher-enabled
       swift-file-watcher--watchers
       (> (length swift-file-watcher--watchers) 0)))

;;;###autoload
(defun swift-file-watcher-status ()
  "Display status information about the file watcher."
  (interactive)
  (let ((buf (get-buffer-create "*Swift File Watcher Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Swift File Watcher Status ===\n\n")
        
        ;; Basic status
        (insert (format "Enabled: %s\n" (if swift-file-watcher-enabled "YES" "NO")))
        (insert (format "Active: %s\n" (if (swift-file-watcher-active-p) "YES" "NO")))
        (insert (format "Project: %s\n\n" (or swift-file-watcher--project-root "None")))
        
        ;; Watched directories
        (insert "=== Watched Directories ===\n")
        (if swift-file-watcher--watched-dirs
            (dolist (dir swift-file-watcher--watched-dirs)
              (insert (format "  %s %s\n"
                              (if (file-exists-p dir) "+" "-")
                              (if swift-file-watcher--project-root
                                  (file-relative-name dir swift-file-watcher--project-root)
                                dir))))
          (insert "  (none)\n"))
        (insert "\n")
        
        ;; Watcher count
        (insert (format "Active watchers: %d\n\n" (length swift-file-watcher--watchers)))
        
        ;; Rebuild status
        (insert "=== Rebuild Status ===\n")
        (insert (format "Rebuild needed: %s\n" (if swift-file-watcher--dirty-p "YES" "NO")))
        (insert (format "Changes since build: %d\n" swift-file-watcher--change-count))
        
        (when swift-file-watcher--last-change
          (let* ((timestamp (car swift-file-watcher--last-change))
                 (filepath (cdr swift-file-watcher--last-change))
                 (age (float-time (time-subtract (current-time) timestamp))))
            (insert (format "Last change: %s\n" (file-name-nondirectory filepath)))
            (insert (format "  Path: %s\n" filepath))
            (insert (format "  Time: %s ago\n"
                            (cond
                             ((< age 60) (format "%.0f seconds" age))
                             ((< age 3600) (format "%.0f minutes" (/ age 60)))
                             (t (format "%.1f hours" (/ age 3600))))))))
        (insert "\n")
        
        ;; Configuration
        (insert "=== Configuration ===\n")
        (insert (format "Debounce delay: %.1fs\n" swift-file-watcher-debounce-delay))
        (insert (format "Show indicator: %s\n" (if swift-file-watcher-show-indicator "YES" "NO")))
        (insert (format "Indicator style: %s\n" swift-file-watcher-indicator-style))
        (insert (format "Debug mode: %s\n" (if swift-file-watcher-debug "YES" "NO")))
        (insert "\n")
        
        ;; Watched extensions
        (insert "=== Watched Extensions ===\n")
        (insert (format "  %s\n" (string-join (swift-file-watcher--get-watched-extensions) ", ")))
        (insert "\n")
        
        ;; Ignored directories
        (insert "=== Ignored Directories ===\n")
        (insert (format "  %s\n" (string-join swift-file-watcher-ignored-directories ", ")))
        
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;;; ============================================================================
;;; Mode Line Indicator
;;; ============================================================================

(defun swift-file-watcher--modeline-indicator ()
  "Generate mode line indicator string based on current state."
  (when swift-file-watcher-show-indicator
    (let ((active (swift-file-watcher-active-p))
          (dirty swift-file-watcher--dirty-p))
      (pcase swift-file-watcher-indicator-style
        ('text
         (cond
          ((not active) " [Swift?]")
          (dirty " [Swift*]")
          (t " [Swift]")))
        ('icon
         (if (fboundp 'nerd-icons-mdicon)
             (cond
              ((not active) 
               (concat " " (nerd-icons-mdicon "nf-md-help_circle" :face 'font-lock-comment-face)))
              (dirty 
               (concat " " (nerd-icons-mdicon "nf-md-circle" :face 'warning)))
              (t 
               (concat " " (nerd-icons-mdicon "nf-md-check_circle" :face 'success))))
           ;; Fallback if nerd-icons not available
           (cond
            ((not active) " ?")
            (dirty " *")
            (t " +"))))
        ('minimal
         (cond
          ((not active) " ?")
          (dirty " *")
          (t "")))
        (_ "")))))

;;;###autoload
(defun swift-file-watcher-add-to-modeline ()
  "Add file watcher indicator to the mode line.
Call this in your init file to enable the indicator."
  (unless (member '(:eval (swift-file-watcher--modeline-indicator)) mode-line-misc-info)
    (push '(:eval (swift-file-watcher--modeline-indicator)) mode-line-misc-info)))

;;;###autoload
(defun swift-file-watcher-remove-from-modeline ()
  "Remove file watcher indicator from the mode line."
  (setq mode-line-misc-info
        (delete '(:eval (swift-file-watcher--modeline-indicator)) mode-line-misc-info)))

;;; ============================================================================
;;; Package.swift/Package.resolved Watching
;;; ============================================================================

(defun swift-file-watcher--is-package-file-p (filepath)
  "Return non-nil if FILEPATH is a Swift package manifest or lockfile."
  (when (stringp filepath)
    (let ((filename (file-name-nondirectory filepath)))
      (member filename '("Package.swift" "Package.resolved")))))

(defvar swift-file-watcher-package-changed-hook nil
  "Hook run when Package.swift or Package.resolved changes.
Functions receive the changed file path as argument.")

(defun swift-file-watcher--check-package-change (filepath)
  "Check if FILEPATH is a package file and run hook if so."
  (when (swift-file-watcher--is-package-file-p filepath)
    (when swift-file-watcher-debug
      (message "[FileWatcher] Package file changed: %s" filepath))
    (run-hook-with-args 'swift-file-watcher-package-changed-hook filepath)))

;; Integrate package checking into the main event handler
(advice-add 'swift-file-watcher--handle-event :after
            (lambda (event)
              (let ((file (nth 2 event)))
                (when file
                  (swift-file-watcher--check-package-change file)))))

;;; ============================================================================
;;; After-Save Hook (backup for file-notify)
;;; ============================================================================

(defun swift-file-watcher--after-save-hook ()
  "Mark project as dirty when a watched file is saved in Emacs.
This is a backup mechanism since file-notify may not always
trigger for files saved from within Emacs on macOS."
  (when (and swift-file-watcher--project-root
             (buffer-file-name)
             (swift-file-watcher--should-watch-file-p (buffer-file-name)))
    (let ((file (buffer-file-name)))
      (setq swift-file-watcher--dirty-p t
            swift-file-watcher--last-change (cons (current-time) file)
            swift-file-watcher--change-count (1+ swift-file-watcher--change-count))
      (when swift-file-watcher-debug
        (message "[FileWatcher] After-save marked dirty: %s"
                 (file-name-nondirectory file)))
      (when swift-file-watcher-show-indicator
        (force-mode-line-update t)))))

(add-hook 'after-save-hook #'swift-file-watcher--after-save-hook)

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun swift-file-watcher-cleanup ()
  "Clean up file watcher resources.
Called automatically on Emacs exit."
  (swift-file-watcher-stop))

(add-hook 'kill-emacs-hook #'swift-file-watcher-cleanup)

(provide 'swift-file-watcher)
;;; swift-file-watcher.el ends here
