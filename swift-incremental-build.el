;;; swift-incremental-build.el --- Incremental build pipeline for Swift -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, xcode, development
;; URL: https://github.com/konrad1977/swift-development

;;; Commentary:
;; Incremental build pipeline that bypasses xcodebuild for fast
;; edit-compile-run cycles.  Extracts compile/link commands from
;; xcodebuild logs and the `.compile' database, then replays only
;; the steps needed for changed modules (arm64, simulator target).
;;
;; Architecture:
;; Xcode uses a "debug dylib" pattern where App.app/App is a thin
;; stub that loads App.debug.dylib at runtime.  SPM packages produce
;; relocatable .o files (clang -r) that are linked into the dylib.
;; This module exploits that by:
;;
;;   1. Recompiling only the changed SPM module (swiftc -incremental)
;;   2. Re-linking the module's relocatable .o (clang -r)
;;   3. Re-linking the debug dylib (clang -dynamiclib)
;;   4. Patching the .app bundle and re-signing
;;   5. Installing and launching on simulator
;;
;; API change detection:
;; After each module compiles, its Intermediates .swiftmodule is
;; hashed and compared against a persistently stored hash from
;; the previous compilation.  If the hash changed, the public API
;; may have changed and downstream modules (determined by a real
;; dependency graph built from `import' statements in source
;; files) are added to the build.  Persistent hashes eliminate
;; flip-flop instability across build configurations.
;;
;; Typical incremental cycle: ~7-14s vs ~189s full xcodebuild.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Optional dependencies - graceful fallback
(require 'xcode-project nil t)
(require 'ios-simulator nil t)
(require 'swift-notification nil t)
(require 'swift-project nil t)
(require 'swift-project-settings nil t)

;; Forward declarations for optional dependencies
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-derived-data-path "xcode-project")
(declare-function xcode-project-scheme-display-name "xcode-project")
(declare-function xcode-project-fetch-or-load-app-identifier "xcode-project")
(declare-function xcode-project-notify "xcode-project")
(declare-function xcode-project--clean-display-name "xcode-project")
(declare-function ios-simulator-simulator-identifier "ios-simulator")
(declare-function ios-simulator-install-and-run-app "ios-simulator")
(declare-function ios-simulator-kill-buffer "ios-simulator")
(declare-function swift-notification-progress-start "swift-notification")
(declare-function swift-notification-progress-update "swift-notification")
(declare-function swift-notification-progress-finish "swift-notification")
(declare-function swift-notification-progress-cancel "swift-notification")
(declare-function swift-notification-send "swift-notification")
(declare-function swift-project-settings--directory "swift-project-settings")
(declare-function swift-project-settings--ensure-directory "swift-project-settings")
(declare-function swift-project-settings--read-file "swift-project-settings")
(declare-function swift-project-settings--write-file "swift-project-settings")
(declare-function swift-project-settings--sanitize-scheme-name "swift-project-settings")
(declare-function swift-file-watcher-changed-files "swift-file-watcher")
(declare-function swift-file-watcher-active-p "swift-file-watcher")
(declare-function swift-file-watcher-mark-built "swift-file-watcher")
(declare-function swift-error-proxy-parse-output "swift-error-proxy")
(declare-function swift-development-compile "swift-development")

;; Silence byte-compiler for variables from other modules
(defvar xcode-project--current-xcode-scheme)

;;; Customization

(defgroup swift-incremental-build nil
  "Incremental build pipeline for Swift projects."
  :group 'swift-development
  :prefix "swift-incremental-build-")

(defcustom swift-incremental-build-enabled t
  "Enable incremental builds.
When non-nil, small changes in SPM modules are compiled directly
with swiftc instead of running a full xcodebuild.  Set to nil to
disable and always use xcodebuild."
  :type 'boolean
  :group 'swift-incremental-build)

(defcustom swift-incremental-build-debug nil
  "Enable debug logging for incremental builds."
  :type 'boolean
  :group 'swift-incremental-build)

(defcustom swift-incremental-build-codesign-identity "-"
  "Code signing identity for incremental builds.
Defaults to ad-hoc signing which is sufficient for simulator."
  :type 'string
  :group 'swift-incremental-build)

(defcustom swift-incremental-build-max-modules 4
  "Maximum number of changed modules for incremental build.
If more modules have changed, fall back to full xcodebuild
since the incremental advantage diminishes."
  :type 'integer
  :group 'swift-incremental-build)

(defcustom swift-incremental-build-max-cascade-modules 25
  "Maximum modules to recompile during cascade rebuild.
When an API change causes linker failure, downstream modules are
recompiled automatically.  If the total (changed + downstream)
exceeds this limit, fall back to full xcodebuild instead."
  :type 'integer
  :group 'swift-incremental-build)

;;; Internal state

(defvar swift-incremental-build--module-commands (make-hash-table :test 'equal)
  "Hash table of extracted build commands per module.
Keys are module names (strings), values are plists with:
  :compile-command - swiftc command string
  :link-command    - clang -r command string
  :compile-order   - topological position (integer)
  :file-lists      - list of SwiftFileList paths (from .compile)")

(defvar swift-incremental-build--dylib-link-command nil
  "Extracted clang -dynamiclib command for the app's debug dylib (string).
Populated by `--extract-dylib-link-command' from build log output.")

(defvar swift-incremental-build--active-process nil
  "Currently running incremental build process.")

(defvar swift-incremental-build--build-start-time nil
  "Timestamp when the current incremental build started.")

(defvar swift-incremental-build--app-bundle-path nil
  "Cached path to the .app bundle in Build/Products.")

(defvar swift-incremental-build--intermediates-path nil
  "Cached path to Build/Intermediates.noindex.")

(defvar swift-incremental-build--current-run nil
  "Non-nil when the current incremental build should install+launch on success.")

(defvar swift-incremental-build--originally-changed-modules nil
  "List of module names that were originally changed by the user.
Set at the start of each build in `--run-for-modules'.
Used by `--handle-step-failure' to compute downstream modules
for cascade rebuild.")

(defvar swift-incremental-build--cascade-attempted nil
  "Non-nil if a cascade rebuild has already been attempted.
Prevents infinite loop: incremental -> cascade -> cascade -> ...
Reset at the start of each fresh build in `--run-for-modules'.")

(defvar swift-incremental-build--swiftmodule-hashes (make-hash-table :test 'equal)
  "Persistent hash table mapping module names to .swiftmodule SHA256.
Stores the hash from the LAST successful compilation of each module.
API change detection compares post-build hash against this stored value.
Persisted to disk so hashes survive Emacs restarts and stabilize after
the first incremental build (no flip-flop between build configurations).")

(defvar swift-incremental-build--reverse-deps (make-hash-table :test 'equal)
  "Reverse dependency graph: module -> list of modules that import it.
Built by `--build-dependency-graph' from source file import statements.
Used by `--downstream-modules' for precise cascade detection.")

;;; Debug logging

(defun swift-incremental-build--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS when debug mode is enabled."
  (when swift-incremental-build-debug
    (apply #'message (concat "[incremental-build] " format-string) args)))

;;; Module classification

(defun swift-incremental-build--module-third-party-p (module-name)
  "Return non-nil if MODULE-NAME is a third-party SPM dependency.
Checks the `:third-party' flag on the cached module data.
This flag is set during `.compile' database parsing based on
the presence of `-suppress-warnings' in the compile command,
which Xcode adds automatically to all external dependencies."
  (when-let* ((data (gethash module-name swift-incremental-build--module-commands)))
    (plist-get data :third-party)))

;;; Source file synchronization
;;
;; When new .swift files are added to (or removed from) a module,
;; the cached compile command becomes stale because it references a
;; fixed file list.  The functions below detect source changes on
;; disk and patch SwiftFileList / compile command before compilation.

(defun swift-incremental-build--common-source-directory (files)
  "Find the common parent directory of source FILES.
For SPM modules this is typically .../Sources/<Module>/."
  (when files
    (let ((dirs (mapcar #'file-name-directory files)))
      (let ((prefix (car dirs)))
        (while (and prefix
                    (not (cl-every (lambda (d) (string-prefix-p prefix d)) dirs)))
          (setq prefix (file-name-directory
                        (directory-file-name prefix))))
        prefix))))

(defun swift-incremental-build--module-source-directory (module-name)
  "Derive the source directory for MODULE-NAME.
Tries the SwiftFileList first (from :file-lists on the cached data).
Falls back to parsing .swift paths from the compile-command string.
Returns the common parent directory, or nil.
Returns nil for modules whose sources span the project root (e.g.
the main app target) since a recursive scan would be too broad."
  (when-let* ((data (gethash module-name swift-incremental-build--module-commands)))
    (let ((dir
           (or
            ;; Primary: read paths from SwiftFileList
            (when-let* ((file-lists (plist-get data :file-lists))
                        (fl (car file-lists))
                        ((stringp fl))
                        ((file-exists-p fl)))
              (with-temp-buffer
                (insert-file-contents fl)
                (let ((files (split-string (buffer-string) "\n" t)))
                  (swift-incremental-build--common-source-directory files))))
            ;; Fallback: parse .swift paths from the compile-command string
            (when-let* ((cmd (plist-get data :compile-command)))
              (swift-incremental-build--common-source-directory
               (swift-incremental-build--extract-swift-files-from-command cmd))))))
      ;; Safety: only return the directory if it looks like an SPM Sources dir
      ;; or is otherwise narrow enough.  If the common dir is the project root
      ;; or contains DerivedSources from multiple modules, the recursive scan
      ;; would pick up thousands of unrelated files.
      (when (and dir (string-match-p "/Sources/" dir))
        dir))))

(defun swift-incremental-build--discover-current-sources (source-dir)
  "Find all .swift files recursively under SOURCE-DIR.
Returns a sorted list of absolute paths."
  (when (and source-dir (file-directory-p source-dir))
    (sort (directory-files-recursively source-dir "\\.swift\\'")
          #'string<)))

(defun swift-incremental-build--extract-swift-files-from-command (command)
  "Extract .swift file paths from COMMAND string.
Returns a sorted list of absolute paths found in the command."
  (let ((files '())
        (start 0))
    (while (string-match " \\(/[^ ]*\\.swift\\)\\b" command start)
      (push (match-string 1 command) files)
      (setq start (match-end 0)))
    (sort files #'string<)))

(defun swift-incremental-build--read-filelist-paths (command)
  "Extract file paths from @SwiftFileList references in COMMAND.
Returns a list of absolute .swift file paths, or nil."
  (let ((fl-files '())
        (start 0))
    (while (string-match "@\\(/[^ ]*\\)" command start)
      (let ((fl-path (match-string 1 command)))
        (when (and (stringp fl-path) (file-exists-p fl-path))
          (with-temp-buffer
            (insert-file-contents fl-path)
            (setq fl-files
                  (append fl-files
                          (split-string (buffer-string) "\n" t))))))
      (setq start (match-end 0)))
    fl-files))

(defun swift-incremental-build--deduplicate-filenames-in-command (command module-name)
  "Remove .swift files with duplicate basenames from COMMAND.
Swift's per-file compilation mode forbids multiple files sharing the
same filename (even in different directories) because filenames are
used to distinguish private declarations.  This commonly happens in
app targets that include DerivedSources from multiple dependencies
\(e.g. GeneratedAssetSymbols.swift from several asset catalogs).

Checks both inline .swift paths and files listed in @SwiftFileList.
When duplicates are found in a filelist, rewrites the filelist file.
When duplicates are found inline, removes them from the command string.
Keeps the file that belongs to MODULE-NAME's own build directory.
Returns the cleaned command string (unchanged if no duplicates)."
  (let* ((inline-files (swift-incremental-build--extract-swift-files-from-command command))
         (filelist-files (swift-incremental-build--read-filelist-paths command))
         (all-files (append inline-files filelist-files))
         (seen (make-hash-table :test 'equal))   ;; basename -> list of paths
         (has-dupes nil))
    ;; Group all files by basename
    (dolist (f all-files)
      (let ((basename (file-name-nondirectory f)))
        (puthash basename (cons f (gethash basename seen)) seen)))
    ;; Check for duplicates
    (maphash (lambda (_basename paths)
               (when (> (length paths) 1)
                 (setq has-dupes t)))
             seen)
    (if (not has-dupes)
        command
      ;; Build set of files to remove: for each duplicate basename,
      ;; keep the one belonging to this module, remove the rest.
      (let ((to-remove '())
            (module-pattern (format "/%s\\.build/" (regexp-quote module-name))))
        (maphash
         (lambda (_basename paths)
           (when (> (length paths) 1)
             ;; Prefer the path matching this module's build dir
             (let ((own (cl-find-if
                         (lambda (p) (string-match-p module-pattern p))
                         paths)))
               (dolist (p paths)
                 (unless (equal p (or own (car paths)))
                   (push p to-remove))))))
         seen)
        (when to-remove
          (swift-incremental-build--log
           "Removing %d duplicate-filename source files from %s compile command: %s"
           (length to-remove) module-name
           (mapconcat #'file-name-nondirectory to-remove ", "))
          ;; Remove from inline paths in command string
          (let ((result command))
            (dolist (f to-remove)
              (setq result (replace-regexp-in-string
                            (concat " " (regexp-quote f) "\\b") "" result)))
            ;; Also rewrite any @SwiftFileList files that contained duplicates
            (let ((start 0))
              (while (string-match "@\\(/[^ ]*\\)" command start)
                (let ((fl-path (match-string 1 command)))
                  (when (and (stringp fl-path) (file-exists-p fl-path))
                    (let* ((fl-contents (with-temp-buffer
                                          (insert-file-contents fl-path)
                                          (split-string (buffer-string) "\n" t)))
                           (cleaned (cl-remove-if
                                     (lambda (f) (member f to-remove))
                                     fl-contents)))
                      (when (< (length cleaned) (length fl-contents))
                        (with-temp-file fl-path
                          (insert (mapconcat #'identity cleaned "\n") "\n"))
                        (swift-incremental-build--log
                         "Rewrote SwiftFileList %s (removed %d duplicates)"
                         fl-path (- (length fl-contents) (length cleaned)))))))
                (setq start (match-end 0))))
            result))))))

(defun swift-incremental-build--sync-module-sources (module-name)
  "Synchronize source file list for MODULE-NAME with files on disk.
If new files have been added or old files removed, updates:
 - The SwiftFileList file (for commands using @filelist)
 - The compile-command string (for commands with inline file paths)
Returns non-nil if the file list was updated."
  (when-let* ((data (gethash module-name swift-incremental-build--module-commands))
              (source-dir (swift-incremental-build--module-source-directory module-name)))
    (let* ((disk-files (swift-incremental-build--discover-current-sources source-dir))
           (file-lists (plist-get data :file-lists))
           (fl-path (and file-lists (car file-lists)))
           (old-files
            (or
             ;; Primary: read from SwiftFileList
             (when (and fl-path (stringp fl-path) (file-exists-p fl-path))
               (with-temp-buffer
                 (insert-file-contents fl-path)
                 (sort (split-string (buffer-string) "\n" t) #'string<)))
             ;; Fallback: parse from compile-command
             (when-let* ((cmd (plist-get data :compile-command)))
               (swift-incremental-build--extract-swift-files-from-command cmd))))
           (changed (not (equal disk-files old-files))))
      (when (and changed disk-files)
        (swift-incremental-build--log
         "Source files changed for %s: %d on disk vs %d cached"
         module-name (length disk-files) (length (or old-files '())))
        ;; 1) Update the SwiftFileList file (used by @filelist commands)
        (when (and fl-path (stringp fl-path))
          (with-temp-file fl-path
            (insert (mapconcat #'identity disk-files "\n") "\n"))
          (swift-incremental-build--log
           "Updated SwiftFileList: %s" fl-path))
        ;; 2) Patch inline file paths in the compile-command string.
        ;;    Build-log commands list .swift files directly instead of @filelist.
        (when-let* ((compile-cmd (plist-get data :compile-command))
                    ((not (string-match-p "@/" compile-cmd))))
          (let ((patched (swift-incremental-build--patch-inline-sources
                          compile-cmd old-files disk-files)))
            (when patched
              (puthash module-name
                       (plist-put data :compile-command patched)
                       swift-incremental-build--module-commands)
              (swift-incremental-build--log
               "Patched inline sources in compile-command for %s"
               module-name))))
        ;; 3) Rebuild dependency graph since new files may add imports
        (swift-incremental-build--log
         "Scheduling dependency graph rebuild for new source files")
        (swift-incremental-build--build-dependency-graph)
        t))))

(defun swift-incremental-build--patch-inline-sources (command old-files new-files)
  "Patch COMMAND string, replacing OLD-FILES with NEW-FILES.
The command is a shell string where .swift paths appear as arguments.
Returns the patched command, or nil if no old files were found."
  (let ((added (cl-set-difference new-files old-files :test #'equal))
        (removed (cl-set-difference old-files new-files :test #'equal))
        (result command)
        (patched nil))
    ;; Remove deleted files from command
    (dolist (f removed)
      (let ((escaped (regexp-quote f)))
        ;; Match the file path possibly preceded/followed by space
        (when (string-match-p escaped result)
          (setq result (replace-regexp-in-string
                        (concat " " escaped "\\b") "" result))
          (setq patched t))))
    ;; Add new files to command — insert them right before the first
    ;; existing .swift file so they appear in the file list portion.
    (when added
      (let ((insertion (mapconcat (lambda (f) (concat " " f)) added "")))
        ;; Find first .swift reference in the command to insert before
        (if (string-match " /[^ ]*\\.swift\\b" result)
            (setq result (concat (substring result 0 (match-beginning 0))
                                 insertion
                                 (substring result (match-beginning 0)))
                  patched t)
          ;; Fallback: append before -module-name
          (when (string-match " -module-name " result)
            (setq result (concat (substring result 0 (match-beginning 0))
                                 insertion
                                 (substring result (match-beginning 0)))
                  patched t)))))
    (when patched result)))

;;; Module detection

(defun swift-incremental-build--detect-module (&optional file)
  "Detect which SPM module FILE belongs to.
FILE defaults to the current buffer's file.
Returns the module name as a string, or nil if not detected.

Detection strategy: look for Sources/<ModuleName>/ in the path,
which is the standard SPM layout."
  (let ((path (or file (buffer-file-name))))
    (when path
      (cond
       ;; SPM layout: .../Sources/<Module>/...
       ((string-match "/Sources/\\([^/]+\\)/" path)
        (match-string 1 path))
       ;; Also check for module name in .build path pattern
       ((string-match "/\\([^/]+\\)\\.build/" path)
        (match-string 1 path))))))

(defun swift-incremental-build--detect-module-from-build-target (&optional file)
  "Detect if FILE belongs to the main app target (not an SPM module).
Checks all cached module names to see if any appears as a directory
component in the file path.  Returns the matching module name."
  (let* ((path (or file (buffer-file-name)))
         (result nil))
    (when (and path (not (swift-incremental-build--detect-module path)))
      (maphash (lambda (module-name _data)
                 (when (and (not result)
                            (string-match-p
                             (format "/%s/" (regexp-quote module-name))
                             path))
                   (setq result module-name)))
               swift-incremental-build--module-commands))
    result))

;;; Changed module detection

(defun swift-incremental-build--changed-modules ()
  "Detect all SPM modules with changed files since last build.
Uses file-watcher changed-files when available, otherwise falls
back to just the current buffer's module.
Returns a deduplicated list of module name strings, or nil."
  (let ((modules (make-hash-table :test 'equal))
        (changed-files (when (and (fboundp 'swift-file-watcher-active-p)
                                  (fboundp 'swift-file-watcher-changed-files)
                                  (swift-file-watcher-active-p))
                         (swift-file-watcher-changed-files))))
    (if changed-files
        (progn
          (dolist (file changed-files)
            (when-let* ((module (or (swift-incremental-build--detect-module file)
                                    (swift-incremental-build--detect-module-from-build-target file))))
              (puthash module t modules)))
          ;; Also include current buffer's module in case it was just edited
          ;; but not yet saved when file-watcher last captured
          (when-let* ((current-module (or (swift-incremental-build--detect-module)
                                          (swift-incremental-build--detect-module-from-build-target))))
            (puthash current-module t modules))
          (let ((result '())
                (skipped '()))
            (maphash (lambda (m _)
                       (if (swift-incremental-build--module-third-party-p m)
                           (push m skipped)
                         (push m result)))
                     modules)
            (when skipped
              (swift-incremental-build--log
               "Ignoring changes in third-party modules: %s" skipped))
            (swift-incremental-build--log "Changed modules: %s" result)
            result))
      ;; Fallback: no file-watcher data, return current buffer's module only
      (when-let* ((m (or (swift-incremental-build--detect-module)
                         (swift-incremental-build--detect-module-from-build-target))))
        (swift-incremental-build--log "No file-watcher data, using current module: %s" m)
        (list m)))))

(defun swift-incremental-build--all-modules-have-commands-p (modules)
  "Return non-nil if all MODULES have cached compile commands.
The link-module step is optional (main app targets lack it)."
  (cl-every (lambda (module)
              (let ((data (gethash module swift-incremental-build--module-commands)))
                (and data
                     (plist-get data :compile-command))))
            modules))

(defun swift-incremental-build--module-compile-order (module-name)
  "Return the compile-order for MODULE-NAME, or `most-positive-fixnum'."
  (let ((data (gethash module-name swift-incremental-build--module-commands)))
    (or (and data (plist-get data :compile-order))
        most-positive-fixnum)))

(defun swift-incremental-build--sort-modules-by-order (modules)
  "Sort MODULES by their `:compile-order' (topological build order).
Returns a new sorted list.  Modules without order sort last."
  (sort (copy-sequence modules)
        (lambda (a b)
          (< (swift-incremental-build--module-compile-order a)
             (swift-incremental-build--module-compile-order b)))))

(defun swift-incremental-build--build-dependency-graph ()
  "Build the reverse dependency graph from source file imports.
Uses `:file-lists' stored on each module in `--module-commands'
to find SwiftFileList files, reads source paths from them, and
scans for `import <Module>' statements.  If `:file-lists' is
missing (commands loaded from disk cache), reads the `.compile'
database to get file lists.  Populates
`swift-incremental-build--reverse-deps' (module -> list of importers)."
  (clrhash swift-incremental-build--reverse-deps)
  (let ((all-modules (make-hash-table :test 'equal))
        (forward-deps (make-hash-table :test 'equal))
        (file-lists-map (make-hash-table :test 'equal)))
    ;; Collect all known module names
    (maphash (lambda (name _) (puthash name t all-modules))
             swift-incremental-build--module-commands)
    ;; Collect file-lists from module-commands first
    (maphash (lambda (name data)
               (when-let* ((fl (plist-get data :file-lists)))
                 (puthash name fl file-lists-map)))
             swift-incremental-build--module-commands)
    ;; If no file-lists found, read them from .compile database
    (when (= (hash-table-count file-lists-map) 0)
      (swift-incremental-build--log
       "No :file-lists in memory, trying .compile fallback")
      (let ((db-path (swift-incremental-build--compile-database-path)))
        (if (not db-path)
            (swift-incremental-build--log
             "No .compile found (project-root=%s)"
             (ignore-errors (xcode-project-project-root)))
          (condition-case err
              (let* ((json-array-type 'list)
                     (json-object-type 'alist)
                     (json-key-type 'symbol)
                     (entries (json-read-file db-path))
                     (seen (make-hash-table :test 'equal)))
                ;; Deduplicate: keep last occurrence per module
                (dolist (entry entries)
                  (when-let* ((name (alist-get 'module_name entry)))
                    (puthash name entry seen)))
                (maphash (lambda (name entry)
                           (when-let* ((fl (alist-get 'fileLists entry)))
                             (puthash name fl file-lists-map)))
                         seen)
                (swift-incremental-build--log
                 "Read file-lists for %d modules from .compile"
                 (hash-table-count file-lists-map)))
            (error
             (swift-incremental-build--log
              "Failed to parse .compile: %s" (error-message-string err)))))))
    ;; For each internal module, read its SwiftFileList and scan imports.
    ;; Third-party modules are skipped (they never change, and scanning
    ;; their sources wastes time).  They remain in all-modules so that
    ;; imports matching them are still recognized.
    (maphash
     (lambda (name _)
       (when (not (swift-incremental-build--module-third-party-p name))
         (let ((file-lists (gethash name file-lists-map))
               (sources '()))
           ;; Read SwiftFileList to get source file paths
           (dolist (fl file-lists)
             (when (and fl (stringp fl) (file-exists-p fl))
               (with-temp-buffer
                 (insert-file-contents fl)
                 (setq sources
                       (append sources
                               (split-string
                                (buffer-string) "\n" t))))))
         ;; Scan each source file for import statements
         (let ((imports '()))
           (dolist (src sources)
             (when (file-exists-p src)
                (with-temp-buffer
                  (insert-file-contents src)
                  (goto-char (point-min))
                  (while (re-search-forward
                          "^\\(?:@testable\\s-+\\)?import\\s-+\\(?:\\(?:struct\\|class\\|enum\\|protocol\\|func\\|typealias\\)\\s-+\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)"
                          nil t)
                    (let ((imp (match-string 1)))
                      (when (and (gethash imp all-modules)
                                 (not (string= imp name)))
                        (cl-pushnew imp imports :test #'equal)))))))
            (puthash name imports forward-deps)))))
     all-modules)
    ;; Build reverse graph: for each module, record who imports it
    (maphash
     (lambda (name imports)
       (dolist (imp imports)
         (let ((existing (gethash imp swift-incremental-build--reverse-deps)))
           (cl-pushnew name existing :test #'equal)
           (puthash imp existing swift-incremental-build--reverse-deps))))
     forward-deps)
    ;; Log summary and save to disk
    (let ((count 0))
      (maphash (lambda (_k v) (when v (cl-incf count)))
               swift-incremental-build--reverse-deps)
      (swift-incremental-build--log
       "Dependency graph built: %d modules have dependents" count)
      (swift-incremental-build--save-dependency-graph))))

(defun swift-incremental-build--downstream-modules (changed-modules)
  "Return internal modules that transitively depend on CHANGED-MODULES.
Uses the reverse dependency graph built from import analysis.
Falls back to compile-order if the graph is empty.
Third-party modules are excluded from the result since they never
change and don't need recompilation.
Returns a sorted list of module names excluding CHANGED-MODULES,
or nil if none found."
  (if (= (hash-table-count swift-incremental-build--reverse-deps) 0)
      ;; Fallback: compile-order based (conservative)
      (swift-incremental-build--downstream-modules-by-order changed-modules)
    ;; Graph-based: transitive closure via BFS
    (let ((visited (make-hash-table :test 'equal))
          (queue (copy-sequence changed-modules))
          (result '())
          (skipped-third-party 0))
      ;; Mark changed modules as visited (don't include in result)
      (dolist (m changed-modules)
        (puthash m t visited))
      ;; BFS through reverse-deps
      (while queue
        (let* ((current (pop queue))
               (dependents (gethash current swift-incremental-build--reverse-deps)))
          (dolist (dep dependents)
            (unless (gethash dep visited)
              (puthash dep t visited)
              (if (swift-incremental-build--module-third-party-p dep)
                  (cl-incf skipped-third-party)
                (push dep result))
              ;; Continue traversal through third-party too, in case
              ;; an internal module depends on a third-party that
              ;; depends on another internal module
              (push dep queue)))))
      (when (> skipped-third-party 0)
        (swift-incremental-build--log
         "Skipped %d third-party modules from cascade" skipped-third-party))
      (when result
        (swift-incremental-build--sort-modules-by-order result)))))

(defun swift-incremental-build--downstream-modules-by-order (changed-modules)
  "Fallback: return internal modules with higher compile-order than CHANGED-MODULES.
Conservative proxy when no dependency graph is available.
Third-party modules are excluded."
  (let* ((changed-set (make-hash-table :test 'equal))
         (min-order most-positive-fixnum)
         (downstream '()))
    (dolist (m changed-modules)
      (puthash m t changed-set)
      (let ((order (swift-incremental-build--module-compile-order m)))
        (when (< order min-order)
          (setq min-order order))))
    (maphash (lambda (name data)
               (let ((order (or (plist-get data :compile-order)
                                 most-positive-fixnum)))
                 (when (and (> order min-order)
                            (not (gethash name changed-set))
                            (not (plist-get data :third-party)))
                   (push name downstream))))
             swift-incremental-build--module-commands)
    (when downstream
      (swift-incremental-build--sort-modules-by-order downstream))))

;;; Path resolution

(defun swift-incremental-build--derived-data-root ()
  "Get the build root for the current project.
Returns the path containing Build/ (e.g. .../DerivedData/Bruce-<hash>/
or .../project/.build/).
Tries xcode-project first, falls back to extracting from cached dylib path."
  (or
   ;; Try xcode-project API - go up from BUILD_DIR to the root containing Build/
   (when (fboundp 'xcode-project-derived-data-path)
     (when-let* ((build-dir (xcode-project-derived-data-path)))
       (cond
        ;; Standard DerivedData layout
        ((string-match "\\(.*/DerivedData/[^/]+\\)/" build-dir)
         (match-string 1 build-dir))
        ;; Local .build layout (from -derivedDataPath .build)
        ((string-match "\\(.*/.build\\)/Build/" build-dir)
         (match-string 1 build-dir))
        ;; BUILD_DIR is the products dir itself, go up two levels
        ((string-match "\\(.*\\)/Build/Products/" build-dir)
         (match-string 1 build-dir)))))
   ;; Fallback: project-root/.build
   (when-let* ((project-root (ignore-errors (xcode-project-project-root))))
     (let ((build-root (expand-file-name ".build" project-root)))
       (when (file-directory-p build-root) build-root)))
   ;; Last resort: extract from cached dylib link command
   (when swift-incremental-build--dylib-link-command
     (let ((dylib-path (swift-incremental-build--extract-output-path
                        swift-incremental-build--dylib-link-command)))
       (when dylib-path
         (cond
          ((string-match "\\(.*/DerivedData/[^/]+\\)/" dylib-path)
           (match-string 1 dylib-path))
          ((string-match "\\(.*/.build\\)/Build/" dylib-path)
           (match-string 1 dylib-path))))))))

(defun swift-incremental-build--intermediates-dir ()
  "Get the Build/Intermediates.noindex/ directory path."
  (or swift-incremental-build--intermediates-path
      (when-let* ((root (swift-incremental-build--derived-data-root)))
        (setq swift-incremental-build--intermediates-path
              (expand-file-name "Build/Intermediates.noindex" root)))))

(defun swift-incremental-build--products-dir ()
  "Get the Build/Products/<config>-iphonesimulator/ directory path.
Prefers the local .build directory used by -derivedDataPath .build."
  (or
   ;; First priority: local .build/Build/Products/<config>-iphonesimulator/
   ;; This is what -derivedDataPath .build produces
   (when-let* ((project-root (ignore-errors (xcode-project-project-root))))
     (let ((products-base (expand-file-name ".build/Build/Products" project-root)))
       (when (file-directory-p products-base)
         (let ((config-dirs (directory-files products-base t "-iphonesimulator$")))
           (when config-dirs
             (swift-incremental-build--log "products-dir from .build: %s" (car config-dirs))
             (car config-dirs))))))
   ;; Derive from dylib link command output path (most reliable)
   (when-let* ((dylib-path (swift-incremental-build--extract-output-path
                            swift-incremental-build--dylib-link-command)))
     (when (string-match "\\(.*/Build/Products/[^/]+\\)/" dylib-path)
       (let ((dir (match-string 1 dylib-path)))
         (when (file-directory-p dir)
           (swift-incremental-build--log "products-dir from dylib path: %s" dir)
           dir))))
   ;; Last resort: xcode-project API (may point to global DerivedData)
   (when (fboundp 'xcode-project-derived-data-path)
     (let ((dir (xcode-project-derived-data-path)))
       (when (and dir (file-directory-p dir))
         (swift-incremental-build--log "products-dir from API: %s" dir)
         dir)))))

(defun swift-incremental-build--app-bundle ()
  "Get the path to the .app bundle."
  (or swift-incremental-build--app-bundle-path
      (when-let* ((products (swift-incremental-build--products-dir)))
        (let ((app-dir (car (directory-files products t "\\.app$"))))
          (when app-dir
            (setq swift-incremental-build--app-bundle-path app-dir))))))

(defun swift-incremental-build--module-build-dir (module-name)
  "Get the build directory for MODULE-NAME under Intermediates."
  (when-let* ((intermediates (swift-incremental-build--intermediates-dir)))
    (expand-file-name
     (format "%s.build" module-name) intermediates)))

(defun swift-incremental-build--swiftmodule-intermediates-path (module-name)
  "Return path to MODULE-NAME's .swiftmodule in Intermediates.
Searches ALL config directories (e.g. Debug-iphonesimulator,
Debug (Staging)-iphonesimulator) for Objects-normal-tsan/arm64/
or Objects-normal/arm64/ variants."
  (when-let* ((module-build (swift-incremental-build--module-build-dir module-name)))
    (let ((config-dirs (when (file-directory-p module-build)
                         (directory-files module-build t "-iphonesimulator$")))
          (result nil))
      (dolist (config-dir config-dirs)
        (unless result
          (let* ((inner (expand-file-name
                         (format "%s.build" module-name) config-dir))
                 (candidates
                  (list (expand-file-name
                         (format "Objects-normal-tsan/arm64/%s.swiftmodule"
                                 module-name)
                         inner)
                        (expand-file-name
                         (format "Objects-normal/arm64/%s.swiftmodule"
                                 module-name)
                         inner))))
            (setq result (cl-find-if #'file-exists-p candidates)))))
      result)))

(defun swift-incremental-build--swiftmodule-products-path (module-name)
  "Return path to MODULE-NAME's .swiftmodule in Products directory.
This is the location downstream modules read when importing."
  (when-let* ((products (swift-incremental-build--products-dir)))
    (let ((module-dir (expand-file-name
                       (format "%s.swiftmodule" module-name) products)))
      (when (file-directory-p module-dir)
        (expand-file-name "arm64-apple-ios-simulator.swiftmodule" module-dir)))))

(defun swift-incremental-build--hash-file (file-path)
  "Return SHA256 hash of FILE-PATH as a hex string, or nil if file missing."
  (when (and file-path (file-exists-p file-path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file-path)
      (secure-hash 'sha256 (current-buffer)))))

(defun swift-incremental-build--snapshot-swiftmodule-hashes (modules)
  "Ensure persistent hashes exist for MODULES.
For modules that already have a stored hash, this is a no-op.
For modules without a stored hash (first build, or after cache
clear), reads the current Intermediates .swiftmodule and records
its hash as the baseline.  This means the very first incremental
build after a full xcodebuild may detect a false API change
\(different build config), but subsequent builds will be stable."
  (dolist (module modules)
    (unless (gethash module swift-incremental-build--swiftmodule-hashes)
      (let* ((path (swift-incremental-build--swiftmodule-intermediates-path module))
             (hash (swift-incremental-build--hash-file path)))
        (when hash
          (puthash module hash swift-incremental-build--swiftmodule-hashes)
          (swift-incremental-build--log
           "Baseline hash for %s: %s" module (substring hash 0 12)))))))

(defun swift-incremental-build--check-api-changed (module-name)
  "Check if MODULE-NAME's public API changed after compilation.
Compares post-build .swiftmodule hash against the persistently
stored hash from the last compilation.  Updates the persistent
hash so subsequent builds with identical code produce stable
results (no flip-flop).
Returns non-nil if the API may have changed.
For modules without a .swiftmodule (3rd-party), returns nil."
  (let* ((old-hash (gethash module-name swift-incremental-build--swiftmodule-hashes))
         (new-path (swift-incremental-build--swiftmodule-intermediates-path module-name))
         (new-hash (swift-incremental-build--hash-file new-path)))
    (cond
     ;; No old hash and no new hash — 3rd-party module, skip
     ((and (not old-hash) (not new-hash))
      (swift-incremental-build--log
       "%s: no .swiftmodule found, skipping API check" module-name)
      nil)
     ;; No old hash but new exists — first build or module was missing
     ((not old-hash)
      (swift-incremental-build--log
       "%s: no stored hash (first build?), recording and assuming unchanged"
       module-name)
      ;; Record for future comparison
      (puthash module-name new-hash swift-incremental-build--swiftmodule-hashes)
      nil)
     ;; Old hash but file gone — shouldn't happen, be conservative
     ((not new-hash)
      (swift-incremental-build--log
       "%s: .swiftmodule missing after compile!" module-name)
      nil)
     ;; Both exist — compare stored vs post-build
     ((not (string= old-hash new-hash))
      (swift-incremental-build--log
       "API change detected in %s (%s -> %s)"
       module-name
       (substring old-hash 0 12)
       (substring new-hash 0 12))
      ;; Update persistent hash so next build is stable
      (puthash module-name new-hash swift-incremental-build--swiftmodule-hashes)
      t)
     (t
      (swift-incremental-build--log "%s: .swiftmodule unchanged" module-name)
      nil))))

(defun swift-incremental-build--copy-swiftmodule-to-products (module-name)
  "Copy MODULE-NAME's .swiftmodule from Intermediates to Products.
Downstream modules read from Products, so this must happen before
they are compiled against the new interface."
  (let ((src (swift-incremental-build--swiftmodule-intermediates-path module-name))
        (dst (swift-incremental-build--swiftmodule-products-path module-name)))
    (when (and src dst (file-exists-p src))
      (condition-case err
          (progn
            (copy-file src dst t)
            (swift-incremental-build--log "Copied %s.swiftmodule to Products" module-name))
        (error
         (swift-incremental-build--log "Failed to copy %s.swiftmodule: %s"
                                        module-name (error-message-string err)))))))

;;; Command extraction from build logs

(defun swift-incremental-build--parse-build-log (log-file)
  "Parse LOG-FILE to extract compile and link commands for all modules.
Populates `swift-incremental-build--module-commands' and
`swift-incremental-build--dylib-link-command'."
  (swift-incremental-build--log "Parsing build log: %s" log-file)
  (clrhash swift-incremental-build--module-commands)
  (setq swift-incremental-build--dylib-link-command nil)
  (with-temp-buffer
    (insert-file-contents log-file)
    (goto-char (point-min))
    ;; Extract swiftc compile commands for each module (arm64 only)
    (swift-incremental-build--extract-compile-commands)
    (goto-char (point-min))
    ;; Extract clang -r link commands for each module (arm64 only)
    (swift-incremental-build--extract-module-link-commands)
    (goto-char (point-min))
    ;; Extract the debug dylib link command (arm64 only)
    (swift-incremental-build--extract-dylib-link-command))
  (swift-incremental-build--log "Extracted commands for %d modules"
                                 (hash-table-count swift-incremental-build--module-commands))
  (when swift-incremental-build--dylib-link-command
    (swift-incremental-build--log "Extracted debug dylib link command")))

(defun swift-incremental-build--extract-compile-commands ()
  "Extract swiftc compile commands from current buffer (build log).
Only extracts arm64 commands.  Populates module-commands hash table.
Records `:compile-order' on each module reflecting the order they
appear in the build log (xcodebuild compiles in topological
dependency order)."
  (let ((order 0))
    (while (re-search-forward
            "builtin-SwiftDriver -- \\(/[^ ]*swiftc\\) -module-name \\([^ ]+\\) "
            nil t)
      (let* ((_swiftc-path (match-string 1))
             (module-name (match-string 2))
             (line-start (line-beginning-position))
             (line-end (line-end-position))
             (full-line (buffer-substring-no-properties line-start line-end)))
        ;; Only arm64, skip x86_64
        (when (and (string-match-p "-target arm64-" full-line)
                   (not (string-match-p "Swift-Compilation-Requirements" full-line)))
          (swift-incremental-build--log "Found compile command for module: %s (order %d)"
                                         module-name order)
          (let* ((cmd-string (swift-incremental-build--clean-command full-line))
                 (existing (gethash module-name swift-incremental-build--module-commands)))
            (puthash module-name
                     (plist-put (plist-put (or existing '())
                                           :compile-command cmd-string)
                                :compile-order order)
                     swift-incremental-build--module-commands)
            (cl-incf order)))))))

(defun swift-incremental-build--extract-module-link-commands ()
  "Extract clang -r (relocatable link) commands from current buffer.
Only extracts arm64 commands."
  (while (re-search-forward
          "\\(/[^ ]*clang\\) .* -target arm64-.* -r .*-nostdlib"
          nil t)
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (full-line (buffer-substring-no-properties line-start line-end)))
      ;; Extract module name from output path
      ;; Pattern 1: .../Binary/<Module>.o (Xcode 26 style)
      ;; Pattern 2: .../<Module>.o in Build/Products (standard xcodebuild)
      (let ((module-name
             (cond
              ;; Original pattern: .../Module.build/.../Binary/Module.o
              ((string-match "-o .*/\\([^/]+\\)\\.build/.*/Binary/\\([^.]+\\)\\.o" full-line)
               (match-string 2 full-line))
              ;; Fallback: -o .../Build/Products/.../<Module>.o
              ((string-match "-o .*/Build/Products/[^/]+/\\([^./\\\\]+\\)\\.o" full-line)
               (match-string 1 full-line))
              ;; Last resort: any -o .../<Module>.o at end of line
              ((string-match "-o .*/\\([^./\\\\]+\\)\\.o\\s-*$" full-line)
               (match-string 1 full-line)))))
        (when module-name
          (let* ((cmd-string (swift-incremental-build--clean-command full-line))
                 (existing (gethash module-name swift-incremental-build--module-commands)))
            (swift-incremental-build--log "Found link command for module: %s" module-name)
            (puthash module-name
                     (plist-put (or existing '())
                                :link-command cmd-string)
                     swift-incremental-build--module-commands)))))))

(defun swift-incremental-build--extract-dylib-link-command ()
  "Extract the debug dylib link command (arm64) from current buffer."
  ;; Try primary pattern: clang -dynamiclib with Binary/ output path
  (if (re-search-forward
       "\\(/[^ ]*clang\\) .* -target arm64-.* -dynamiclib .* -o .*/Binary/[^ ]*\\.debug\\.dylib"
       nil t)
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (full-line (buffer-substring-no-properties line-start line-end)))
        (setq swift-incremental-build--dylib-link-command
              (swift-incremental-build--clean-command full-line))
        (swift-incremental-build--log "Found debug dylib link command (primary pattern)"))
    ;; Try fallback pattern: any clang -dynamiclib producing a .debug.dylib
    (goto-char (point-min))
    (if (re-search-forward
         "\\(/[^ ]*clang\\) .* -dynamiclib .* -o .*\\.debug\\.dylib"
         nil t)
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (full-line (buffer-substring-no-properties line-start line-end)))
          (setq swift-incremental-build--dylib-link-command
                (swift-incremental-build--clean-command full-line))
          (swift-incremental-build--log "Found debug dylib link command (fallback pattern)"))
      ;; Debug: search for any -dynamiclib occurrence
      (goto-char (point-min))
      (swift-incremental-build--log "No debug dylib link command found!")
      (swift-incremental-build--log "Searching for any -dynamiclib in output...")
      (if (re-search-forward "-dynamiclib" nil t)
          (let* ((line-start (line-beginning-position))
                 (line-end (min (+ line-start 300) (line-end-position)))
                 (context (buffer-substring-no-properties line-start line-end)))
            (swift-incremental-build--log "Found -dynamiclib context: %s" context))
        (swift-incremental-build--log "No -dynamiclib found anywhere in build output (%d chars total)"
                                       (buffer-size))))))

(defun swift-incremental-build--clean-command (raw-command)
  "Clean RAW-COMMAND string from build log.
Removes leading whitespace/builtin prefixes.  Keeps shell escaping
intact (backslash-escaped parens, spaces, equals) since the command
will be run via bash."
  (let ((cmd raw-command))
    ;; Remove leading whitespace and builtin-* prefix
    (when (string-match "^\\s-*\\(?:builtin-[^ ]+ -- \\)?" cmd)
      (setq cmd (substring cmd (match-end 0))))
    ;; Only unescape \= since bash doesn't need that escaped,
    ;; but keep \( \) and \<space> as they are needed by bash
    (setq cmd (replace-regexp-in-string "\\\\=" "=" cmd))
    cmd))

;;; Command execution helpers

(defun swift-incremental-build--shell-command-to-list (cmd-string)
  "Convert CMD-STRING to a list suitable for make-process :command.
Handles quoted paths with spaces."
  ;; We run via shell to handle the complex quoting from xcodebuild logs
  (list "bash" "-c" cmd-string))

(defun swift-incremental-build--run-step (name command callback &optional working-dir filter-fn)
  "Run a build step NAME with COMMAND (string), calling CALLBACK on success.
CALLBACK receives the process exit code.
WORKING-DIR overrides default-directory if provided.
FILTER-FN, if non-nil, is called with each chunk of process output
for real-time progress updates (output is still saved to the buffer)."
  (swift-incremental-build--log "Running step: %s" name)
  (let ((default-directory (or working-dir
                               (xcode-project-project-root)
                               default-directory))
        (output-buffer (get-buffer-create "*swift-incremental-build*")))
    ;; Append step header to output buffer
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (insert (format "\n=== %s ===\n" name)))
    (let ((process
           (make-process
            :name (format "swift-incr-%s" name)
            :buffer nil  ;; We handle buffer output in the filter
            :command (swift-incremental-build--shell-command-to-list command)
            :filter
            (lambda (proc string)
              ;; Always append to output buffer
              (when (buffer-live-p output-buffer)
                (with-current-buffer output-buffer
                  (goto-char (point-max))
                  (insert string)))
              ;; Call custom filter for progress updates
              (when (and filter-fn (functionp filter-fn))
                (funcall filter-fn proc string)))
            :sentinel
            (lambda (proc _event)
              (when (memq (process-status proc) '(exit signal))
                (let ((exit-code (process-exit-status proc)))
                  (swift-incremental-build--log "Step %s finished with exit code %d" name exit-code)
                  (if (= exit-code 0)
                      (funcall callback exit-code)
                    (swift-incremental-build--handle-step-failure name exit-code output-buffer))))))))
      (setq swift-incremental-build--active-process process))))

(defun swift-incremental-build--handle-step-failure (step-name exit-code output-buffer)
  "Handle failure of STEP-NAME with EXIT-CODE.
Shows error from OUTPUT-BUFFER.  When the linker reports undefined
symbols (cross-module API mismatch), attempts cascade rebuild of
downstream modules first.  If cascade was already attempted (or no
downstream modules exist), falls back to full xcodebuild.
Compile step failures also fall back to xcodebuild automatically."
  (let* ((elapsed (swift-incremental-build--elapsed-time))
         (output (when (buffer-live-p output-buffer)
                   (with-current-buffer output-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))))
         (linker-mismatch (and output
                               (string-match-p "Undefined symbols" output)))
         (filename-collision (and output
                                  (string-match-p "filename .* used twice" output)))
         (is-compile-step (string-prefix-p "compile-" step-name)))
    (swift-incremental-build--log "FAILED: %s (exit %d) after %s" step-name exit-code elapsed)
    ;; Cancel progress
    (when (fboundp 'swift-notification-progress-cancel)
      (swift-notification-progress-cancel 'swift-incremental))
    (cond
     ;; Duplicate filename error — cannot be fixed incrementally,
     ;; fall back to full xcodebuild immediately.
     (filename-collision
      (swift-incremental-build--log
       "Filename collision detected in %s, falling back to xcodebuild" step-name)
      (swift-incremental-build--fallback-to-xcodebuild))

     ;; Linker mismatch + cascade not yet attempted -> try cascade
     ((and linker-mismatch
           (not swift-incremental-build--cascade-attempted)
           swift-incremental-build--originally-changed-modules)
      (let* ((downstream (swift-incremental-build--downstream-modules
                          swift-incremental-build--originally-changed-modules))
             (all-modules (and downstream
                               (swift-incremental-build--sort-modules-by-order
                                (append swift-incremental-build--originally-changed-modules
                                        downstream))))
             (too-many (and all-modules
                            (> (length all-modules)
                               swift-incremental-build-max-cascade-modules))))
        (cond
         ;; No downstream modules found — skip to xcodebuild
         ((not downstream)
          (swift-incremental-build--log
           "No downstream modules found, falling back to xcodebuild")
          (swift-incremental-build--fallback-to-xcodebuild))
         ;; Too many modules — not worth cascading
         (too-many
          (swift-incremental-build--log
           "Cascade would rebuild %d modules (max %d), falling back to xcodebuild"
           (length all-modules) swift-incremental-build-max-cascade-modules)
          (swift-incremental-build--fallback-to-xcodebuild))
         ;; Cascade rebuild
         (t
          (setq swift-incremental-build--cascade-attempted t)
          (swift-incremental-build--log
           "Cascade rebuild: %d downstream modules for %s"
           (length downstream)
           (mapconcat #'identity
                      swift-incremental-build--originally-changed-modules ", "))
          (when (fboundp 'xcode-project-notify)
            (xcode-project-notify
             :message (format "API change: rebuilding %d downstream modules..."
                              (length downstream))
             :seconds 3))
          (run-with-timer
           0.3 nil
           (lambda ()
             (swift-incremental-build--run-multi-pipeline
              all-modules
              swift-incremental-build--current-run)))))))

     ;; Linker mismatch + cascade already attempted -> full xcodebuild
     ((and linker-mismatch (fboundp 'swift-development-compile))
      (swift-incremental-build--log
       "Cascade failed, falling back to full xcodebuild")
      (swift-incremental-build--fallback-to-xcodebuild))

     ;; Compile step failure — fall back to xcodebuild since the
     ;; cached swiftc command may be stale or incompatible.
     (is-compile-step
      (swift-incremental-build--log
       "Compile step %s failed, falling back to xcodebuild" step-name)
      (swift-incremental-build--fallback-to-xcodebuild))

     ;; Other failure (codesign, patch, etc.) — show error
     (t
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (format "Incremental build failed at %s (exit %d)"
                          step-name exit-code)
         :seconds 5))
      ;; Parse errors through periphery for navigation
      (when (and output (fboundp 'swift-error-proxy-parse-output))
        (swift-error-proxy-parse-output output))
      ;; Display the output buffer so user can see the error
      (when (buffer-live-p output-buffer)
        (display-buffer output-buffer))))))

(defun swift-incremental-build--fallback-to-xcodebuild ()
  "Fall back to full xcodebuild as last resort.
Used when cascade rebuild is not possible or has failed."
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (propertize "Falling back to full xcodebuild"
                          'face 'warning)
     :seconds 3))
  (run-with-timer
   0.5 nil
   (lambda ()
     (when (fboundp 'swift-development-compile)
       (swift-development-compile
        :run swift-incremental-build--current-run
        :force t)))))

;;; Timing

(defun swift-incremental-build--elapsed-time ()
  "Return elapsed time since build started as a formatted string."
  (if swift-incremental-build--build-start-time
      (format "%.1fs" (float-time
                       (time-subtract (current-time)
                                      swift-incremental-build--build-start-time)))
    "?s"))

;;; Pipeline steps

(defun swift-incremental-build--module-working-dir (module-name)
  "Return the working directory for MODULE-NAME.
When the module was loaded from a .compile database the original
`:directory' field is used.  Otherwise falls back to the module
build directory under Intermediates.noindex so that build
artefacts (.o, .d, .dia, .swiftdeps, .swiftconstvalues) never
end up in the project root."
  (let* ((module-data (gethash module-name swift-incremental-build--module-commands))
         (dir (and module-data (plist-get module-data :directory))))
    (when (and dir (file-directory-p dir))
      dir)))

(defun swift-incremental-build--make-compile-filter (module-name module-index total-modules)
  "Create a process filter for compiling MODULE-NAME.
MODULE-INDEX is 1-indexed, TOTAL-MODULES is the total count.
Parses swiftc parseable-output JSON and progress-updates with
the currently compiling filename.  Also detects plain
\"Compiling <file>\" lines from swiftc -incremental output."
  (let ((file-count 0)
        (partial-line ""))
    (lambda (_proc string)
      (setq partial-line (concat partial-line string))
      (let ((lines (split-string partial-line "\n")))
        ;; Keep last (possibly incomplete) line for next chunk
        (setq partial-line (car (last lines)))
        (dolist (line (butlast lines))
          (let ((filename nil))
            ;; Try JSON parseable-output: {"kind":"began","name":"compile",...}
            (when (and (string-prefix-p "{" (string-trim-left line))
                       (string-match-p "\"began\"" line)
                       (string-match-p "\"compile\"" line))
              (condition-case nil
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'symbol)
                         (obj (json-read-from-string line))
                         (inputs (alist-get 'inputs obj)))
                    (when (and inputs (car inputs))
                      (setq filename (file-name-nondirectory
                                      (car inputs)))))
                (error nil)))
            ;; Fallback: plain "Compiling <file>.swift" lines
            (when (and (not filename)
                       (string-match "Compiling \\([^ ]+\\.swift\\)" line))
              (setq filename (match-string 1 line)))
            (when filename
              (cl-incf file-count)
              (let* ((module-prefix
                      (if (> total-modules 1)
                          (format "[%d/%d] %s"
                                  module-index total-modules module-name)
                        module-name))
                     ;; Progress range: 5-75% spread across modules
                     (module-start (+ 5 (* 70.0 (/ (1- (float module-index))
                                                    total-modules))))
                     (module-end (+ 5 (* 70.0 (/ (float module-index)
                                                  total-modules))))
                     ;; Within module, advance based on files compiled
                     ;; Cap at 90% of module range (leave room for emit-module)
                     (file-pct (min 0.9 (* 0.1 file-count)))
                     (pct (round (+ module-start
                                    (* file-pct (- module-end module-start))))))
                (run-with-timer
                 0 nil
                 (lambda ()
                   (when (fboundp 'swift-notification-progress-update)
                     (swift-notification-progress-update
                      'swift-incremental
                      :percent pct
                      :title module-prefix
                      :message (file-name-sans-extension filename)))))))))))))

(defun swift-incremental-build--step-compile-module (module-name callback
                                                     &optional module-index total-modules)
  "Compile MODULE-NAME using cached swiftc command.
MODULE-INDEX (1-based) and TOTAL-MODULES provide context for
progress display; both default to 1.
Syncs the source file list with disk (detects added/removed files),
strips `-experimental-emit-module-separately' so the module is
emitted inline, and removes duplicate-filename source files that
would cause Swift to error.  Calls CALLBACK on success."
  (let ((module-index (or module-index 1))
        (total-modules (or total-modules 1)))
    ;; Sync source files before compiling — detects added/removed .swift files
    (swift-incremental-build--sync-module-sources module-name)
    (let* ((module-data (gethash module-name swift-incremental-build--module-commands))
           (compile-cmd (plist-get module-data :compile-command))
           ;; Remove -experimental-emit-module-separately so .swiftmodule
           ;; is emitted inline rather than as a separate driver job.
           (compile-cmd (and compile-cmd
                             (replace-regexp-in-string
                              " -experimental-emit-module-separately\\b" ""
                              compile-cmd)))
           ;; Remove source files with duplicate basenames (e.g.
           ;; GeneratedAssetSymbols.swift from multiple asset catalogs)
           (compile-cmd (and compile-cmd
                             (swift-incremental-build--deduplicate-filenames-in-command
                              compile-cmd module-name))))
      (unless compile-cmd
        (error "No cached compile command for module %s. Run extract-commands first" module-name))
      (let ((module-prefix (if (> total-modules 1)
                               (format "[%d/%d] %s" module-index total-modules module-name)
                             module-name)))
        (when (fboundp 'swift-notification-progress-update)
          (swift-notification-progress-update 'swift-incremental
                                              :percent (round (+ 5 (* 70.0 (/ (1- (float module-index))
                                                                               total-modules))))
                                              :title "Compiling"
                                              :message module-prefix))
        (swift-incremental-build--run-step
         (format "compile-%s" module-name)
         compile-cmd
         callback
         (swift-incremental-build--module-working-dir module-name)
         (swift-incremental-build--make-compile-filter
          module-name module-index total-modules))))))

(defun swift-incremental-build--step-link-module (module-name callback)
  "Link MODULE-NAME relocatable .o using cached clang -r command.
Calls CALLBACK on success."
  (let* ((module-data (gethash module-name swift-incremental-build--module-commands))
         (link-cmd (plist-get module-data :link-command)))
    (unless link-cmd
      (error "No cached link command for module %s. Run extract-commands first" module-name))
    (when (fboundp 'swift-notification-progress-update)
      (swift-notification-progress-update 'swift-incremental
                                           :percent 76
                                           :title "Linking"
                                           :message (format "%s" module-name)))
    (swift-incremental-build--run-step
     (format "link-%s" module-name)
     link-cmd
     callback
     (swift-incremental-build--module-working-dir module-name))))

(defun swift-incremental-build--step-link-dylib (callback)
  "Link the debug dylib using cached clang -dynamiclib command.
Calls CALLBACK on success."
  (unless swift-incremental-build--dylib-link-command
    (error "No cached dylib link command. Run extract-commands first"))
  (when (fboundp 'swift-notification-progress-update)
    (swift-notification-progress-update 'swift-incremental
                                         :percent 78
                                         :title "Linking"
                                         :message "debug.dylib"))
  (swift-incremental-build--run-step
   "link-dylib"
   swift-incremental-build--dylib-link-command
   callback))

(defun swift-incremental-build--extract-output-path (link-command)
  "Extract the -o output path from LINK-COMMAND string.
Handles paths with backslash-escaped spaces and parentheses."
  (when (string-match "-o \\(\\(?:\\\\.\\|[^ \\\\]\\)+\\)" link-command)
    (let ((path (match-string 1 link-command)))
      ;; Unescape shell escaping for Emacs file operations
      (setq path (replace-regexp-in-string "\\\\\\(.\\)" "\\1" path))
      path)))

(defun swift-incremental-build--step-patch-app-bundle (callback)
  "Copy the debug dylib into the .app bundle and call CALLBACK.
Derives the dylib filename dynamically from the link command's
-o output path so it works for any project, not just a hardcoded name."
  (when (fboundp 'swift-notification-progress-update)
    (swift-notification-progress-update 'swift-incremental
                                         :percent 83
                                         :title "Patching"
                                         :message "app bundle"))
  (let* ((app-bundle (swift-incremental-build--app-bundle))
         (dylib-source (swift-incremental-build--extract-output-path
                        swift-incremental-build--dylib-link-command))
         (dylib-name (when dylib-source
                       (file-name-nondirectory dylib-source)))
         (dylib-dest (when (and app-bundle dylib-name)
                       (expand-file-name dylib-name app-bundle))))
    (unless (and dylib-source (file-exists-p dylib-source))
      (error "Cannot find built debug dylib at %s" dylib-source))
    (unless dylib-dest
      (error "Cannot determine app bundle path"))
    (if (string= (file-truename dylib-source) (file-truename dylib-dest))
        (progn
          (swift-incremental-build--log "Dylib already in app bundle, skipping copy")
          (funcall callback 0))
      (swift-incremental-build--log "Copying dylib: %s -> %s" dylib-source dylib-dest)
      (condition-case err
          (progn
            (copy-file dylib-source dylib-dest t)
            (funcall callback 0))
        (error
         (error "Failed to copy dylib to app bundle: %s"
                (error-message-string err)))))))

(defun swift-incremental-build--step-codesign (callback)
  "Re-sign the .app bundle and call CALLBACK on success."
  (when (fboundp 'swift-notification-progress-update)
    (swift-notification-progress-update 'swift-incremental
                                         :percent 88
                                         :title "Signing"
                                         :message "app bundle"))
  (let* ((app-bundle (swift-incremental-build--app-bundle))
         (sign-cmd (format "codesign --force --sign %s --timestamp=none --generate-entitlement-der %s"
                           swift-incremental-build-codesign-identity
                           (shell-quote-argument app-bundle))))
    (swift-incremental-build--run-step "codesign" sign-cmd callback)))

(defun swift-incremental-build--step-install-and-launch ()
  "Install the .app on simulator and launch it.
Finishes the incremental progress bar before handing off to the
simulator installer (which manages its own progress via `swift-build')."
  ;; Kill stale simulator buffer to avoid read-only errors
  (when (fboundp 'ios-simulator-kill-buffer)
    (ios-simulator-kill-buffer))
  (let* ((products-dir (swift-incremental-build--products-dir))
         (project-root (ignore-errors (xcode-project-project-root)))
         (sim-id (ignore-errors (ios-simulator-simulator-identifier)))
         (app-id (ignore-errors (xcode-project-fetch-or-load-app-identifier)))
         (elapsed (swift-incremental-build--elapsed-time)))
    (swift-incremental-build--log "Installing and launching (total so far: %s)" elapsed)
    ;; Finish incremental progress bar — build is done, install is a separate phase
    (when (fboundp 'swift-notification-progress-finish)
      (swift-notification-progress-finish
       'swift-incremental
       (format "Built (%s) - installing..." elapsed)))
    ;; Hand off to simulator installer (has its own 'swift-build progress)
    (ios-simulator-install-and-run-app
     :rootfolder project-root
     :build-folder (concat products-dir "/")
     :simulatorId sim-id
     :appIdentifier app-id
     :terminate-first t)))

;;; Pipeline orchestration

(defun swift-incremental-build--run-pipeline (module-name &optional run)
  "Run the incremental build pipeline for MODULE-NAME.
If RUN is non-nil, install and launch after building."
  (setq swift-incremental-build--build-start-time (current-time))
  ;; Clear output buffer
  (when-let* ((buf (get-buffer "*swift-incremental-build*")))
    (with-current-buffer buf (erase-buffer)))
  ;; Start progress
  (when (fboundp 'swift-notification-progress-start)
    (swift-notification-progress-start
     :id 'swift-incremental
     :title "Incremental Build"
     :message (format "Compiling %s..." module-name)
     :percent 5))
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (format "Incremental build: %s"
                      (propertize module-name 'face 'font-lock-builtin-face))
     :seconds 2))
  ;; Snapshot .swiftmodule hash before compilation
  (swift-incremental-build--snapshot-swiftmodule-hashes (list module-name))
  ;; Chain: compile -> check API -> (link-module if exists) -> link-dylib -> patch -> codesign -> install
  ;; Main app target (e.g. Bruce) has no separate module .o link step
  (let* ((module-data (gethash module-name swift-incremental-build--module-commands))
         (has-link-cmd (plist-get module-data :link-command))
          (finish-fn
           (lambda (_)
             ;; Mark file watcher as built so changed-files are cleared
             (when (fboundp 'swift-file-watcher-mark-built)
               (swift-file-watcher-mark-built))
             ;; Persist swiftmodule hashes for stable API detection
             (swift-incremental-build--save-swiftmodule-hashes)
             (let ((elapsed (swift-incremental-build--elapsed-time)))
               (swift-incremental-build--log "Build complete in %s" elapsed)
               (if run
                   (swift-incremental-build--step-install-and-launch)
                 (when (fboundp 'swift-notification-progress-finish)
                   (swift-notification-progress-finish
                    'swift-incremental
                    (format "Built %s (%s)" module-name elapsed)))
                 (when (fboundp 'xcode-project-notify)
                   (xcode-project-notify
                    :message (format "Incremental build succeeded (%s)"
                                     (propertize elapsed 'face 'success))
                    :seconds 3)))))))
    (swift-incremental-build--step-compile-module
     module-name
     (lambda (_)
       ;; After compile, check if .swiftmodule changed (API change)
       (let ((api-changed (swift-incremental-build--check-api-changed module-name))
             (downstream nil))
         (when api-changed
           (setq downstream (swift-incremental-build--downstream-modules (list module-name))))
         (if (and api-changed downstream)
             ;; API changed and there are downstream modules — expand to cascade
             (progn
               (swift-incremental-build--log
                "API change in %s: expanding to %d downstream modules"
                module-name (length downstream))
               (when (fboundp 'xcode-project-notify)
                 (xcode-project-notify
                  :message (format "API change in %s: rebuilding %d modules..."
                                   module-name (length downstream))
                  :seconds 3))
               ;; Copy updated .swiftmodule to Products for downstream
               (swift-incremental-build--copy-swiftmodule-to-products module-name)
               ;; Link the current module first if it has a link command
               (let* ((all-modules (swift-incremental-build--sort-modules-by-order
                                    (cons module-name downstream))))
                 ;; Snapshot hashes for downstream modules too
                 (swift-incremental-build--snapshot-swiftmodule-hashes downstream)
                 (setq swift-incremental-build--cascade-attempted t)
                 ;; Switch to multi-module pipeline (re-starts from scratch
                 ;; but module-name was already compiled — swiftc -incremental
                 ;; will skip unchanged files so it's fast)
                 (swift-incremental-build--run-multi-pipeline all-modules run)))
           ;; No API change (or no downstream) — continue normal pipeline
           (let ((continue-after-link
                  (lambda (_)
                    (swift-incremental-build--step-link-dylib
                     (lambda (_)
                       (swift-incremental-build--step-patch-app-bundle
                        (lambda (_)
                          (swift-incremental-build--step-codesign finish-fn))))))))
             (if has-link-cmd
                 (swift-incremental-build--step-link-module module-name continue-after-link)
               (swift-incremental-build--log "No module link for %s, skipping" module-name)
               (funcall continue-after-link nil)))))))))

(defun swift-incremental-build--run-multi-pipeline (modules &optional run)
  "Run incremental build pipeline for multiple MODULES.
Compiles and links each module sequentially in dependency order,
then links dylib, patches app bundle, codesigns, and optionally
installs if RUN is non-nil.
MODULES is a list of module name strings (sorted by compile-order)."
  (setq swift-incremental-build--build-start-time (current-time))
  ;; Ensure dependency order
  (setq modules (swift-incremental-build--sort-modules-by-order modules))
  ;; Clear output buffer
  (when-let* ((buf (get-buffer "*swift-incremental-build*")))
    (with-current-buffer buf (erase-buffer)))
  ;; Start progress
  (when (fboundp 'swift-notification-progress-start)
    (swift-notification-progress-start
     :id 'swift-incremental
     :title "Incremental Build"
     :message (format "Compiling %d modules..." (length modules))
     :percent 5))
  (when (fboundp 'xcode-project-notify)
    (xcode-project-notify
     :message (format "Incremental build: %s"
                      (propertize (mapconcat #'identity modules ", ")
                                  'face 'font-lock-builtin-face))
     :seconds 2))
  ;; Build the finish function (link-dylib -> patch -> codesign -> install)
  (let* ((total-modules (length modules))
          (finish-fn
           (lambda (_)
             ;; Mark file watcher as built so changed-files are cleared
             (when (fboundp 'swift-file-watcher-mark-built)
               (swift-file-watcher-mark-built))
             ;; Persist swiftmodule hashes for stable API detection
             (swift-incremental-build--save-swiftmodule-hashes)
             (let ((elapsed (swift-incremental-build--elapsed-time)))
               (swift-incremental-build--log "Build complete in %s" elapsed)
               (if run
                   (swift-incremental-build--step-install-and-launch)
                 (when (fboundp 'swift-notification-progress-finish)
                   (swift-notification-progress-finish
                    'swift-incremental
                    (format "Built %d modules (%s)" total-modules elapsed)))
                  (when (fboundp 'xcode-project-notify)
                    (xcode-project-notify
                     :message (format "Incremental build succeeded (%s)"
                                      (propertize elapsed 'face 'success))
                     :seconds 3))))))
          ;; After all modules compiled+linked, run shared steps
          (after-all-modules
           (lambda (_)
             (swift-incremental-build--step-link-dylib
              (lambda (_)
                (swift-incremental-build--step-patch-app-bundle
                 (lambda (_)
                   (swift-incremental-build--step-codesign finish-fn))))))))
     ;; Snapshot .swiftmodule hashes for all modules before compilation
     (swift-incremental-build--snapshot-swiftmodule-hashes modules)
     ;; Build a recursive chain: compile+link module 1, then module 2, ... then shared steps
     (swift-incremental-build--compile-modules-chain
      modules 1 total-modules after-all-modules)))

(defun swift-incremental-build--compile-modules-chain (modules current total callback)
  "Compile and link MODULES sequentially, then call CALLBACK.
CURRENT is 1-indexed step number, TOTAL is total module count.
After each module is compiled, checks if its .swiftmodule changed.
If so, copies the updated .swiftmodule to Products and adds
downstream modules to the remaining list (deduplicating)."
  (if (null modules)
      ;; All modules compiled and linked, proceed to shared steps
      (funcall callback nil)
    (let* ((module-name (car modules))
           (rest (cdr modules))
           (module-data (gethash module-name swift-incremental-build--module-commands))
           (has-link-cmd (and module-data (plist-get module-data :link-command)))
           (after-compile
            (lambda (_)
              ;; Check API change after compile
              (let ((api-changed (swift-incremental-build--check-api-changed module-name))
                    (downstream nil)
                    (expanded-rest rest)
                    (new-total total))
                (when api-changed
                  ;; Copy updated .swiftmodule so downstream sees new interface
                  (swift-incremental-build--copy-swiftmodule-to-products module-name)
                  ;; Find downstream modules and add them to the queue
                  (setq downstream (swift-incremental-build--downstream-modules (list module-name)))
                  (when downstream
                    (swift-incremental-build--log
                     "API change in %s: adding %d downstream modules"
                     module-name (length downstream))
                    ;; Add downstream modules not already in the queue
                    (dolist (dm downstream)
                      (unless (or (string= dm module-name)
                                  (member dm expanded-rest))
                        (setq expanded-rest (append expanded-rest (list dm)))
                        (cl-incf new-total)))
                    ;; Snapshot hashes for newly added modules
                    (swift-incremental-build--snapshot-swiftmodule-hashes downstream)))
                ;; Link the module (if it has a link command), then continue chain
                (if has-link-cmd
                    (swift-incremental-build--step-link-module
                     module-name
                     (lambda (_)
                       (swift-incremental-build--compile-modules-chain
                        expanded-rest (1+ current) new-total callback)))
                  (swift-incremental-build--log "No module link for %s, skipping" module-name)
                  (swift-incremental-build--compile-modules-chain
                   expanded-rest (1+ current) new-total callback))))))
      ;; Compile the module (progress handled by compile filter)
      (swift-incremental-build--step-compile-module
       module-name after-compile current total))))

;;; Smart build routing

(defun swift-incremental-build--build-artifacts-exist-p ()
  "Return non-nil if essential build artifacts still exist on disk.
Checks that the Intermediates.noindex directory and the app bundle
are present.  After a clean/reset these will be gone, meaning
cached swiftc commands would reference missing files (e.g.
OutputFileMap.json) and fail."
  (let* ((intermediates (swift-incremental-build--intermediates-dir))
         (app-bundle (swift-incremental-build--app-bundle))
         (ok (and intermediates (file-directory-p intermediates)
                  app-bundle (file-directory-p app-bundle))))
    (unless ok
      (swift-incremental-build--log
       "build-artifacts-exist-p: intermediates=%s app-bundle=%s => MISSING"
       (or intermediates "nil") (or app-bundle "nil")))
    ok))

(defun swift-incremental-build-ready-p ()
  "Return non-nil if incremental build is available for all changed modules.
Checks `swift-incremental-build-enabled' first.  Then verifies
that build artifacts exist on disk (Intermediates, app bundle),
that we have cached commands (in memory or on disk) for every
module with changed files.  Falls back to xcodebuild if any
module lacks commands, if too many modules changed, or if a
clean/reset removed the build directory."
  (if (not swift-incremental-build-enabled)
      (progn
        (swift-incremental-build--log "ready-p: incremental builds disabled")
        nil)
    (let* ((file (buffer-file-name))
           (loaded (when file (swift-incremental-build--ensure-commands-loaded)))
           (has-dylib swift-incremental-build--dylib-link-command)
           (artifacts-ok (when (and loaded has-dylib)
                           (swift-incremental-build--build-artifacts-exist-p)))
           (changed-modules (when (and loaded has-dylib artifacts-ok)
                              (swift-incremental-build--changed-modules)))
           (too-many (and changed-modules
                          (> (length changed-modules)
                             swift-incremental-build-max-modules)))
           (all-have-cmds (and changed-modules
                               (not too-many)
                               (swift-incremental-build--all-modules-have-commands-p
                                changed-modules)))
           (ready (and file loaded has-dylib artifacts-ok changed-modules
                       (not too-many) all-have-cmds t)))
      ;; If artifacts are gone, proactively clear stale cache so next
      ;; full xcodebuild can re-populate it cleanly.
      (when (and loaded has-dylib (not artifacts-ok))
        (swift-incremental-build--log "ready-p: build artifacts missing, clearing stale cache")
        (swift-incremental-build-clear-cache))
      (swift-incremental-build--log
       "ready-p: file=%s artifacts=%s modules=%s count=%d too-many=%s all-cmds=%s dylib=%s => %s"
       (if file "yes" "NO")
       (if artifacts-ok "yes" "NO")
       (or (and changed-modules (mapconcat #'identity changed-modules ",")) "NONE")
       (length (or changed-modules '()))
       (if too-many "YES" "no")
       (if all-have-cmds "yes" "NO")
       (if has-dylib "yes" "NO")
       (if ready "READY" "NOT-READY (falls back to xcodebuild)"))
      ready)))

;;; Persistence - save/load commands per scheme

(defun swift-incremental-build--commands-file (project-root scheme)
  "Get path for incremental commands file for PROJECT-ROOT and SCHEME."
  (when (and project-root scheme
             (fboundp 'swift-project-settings--directory)
             (fboundp 'swift-project-settings--sanitize-scheme-name))
    (let ((dir (swift-project-settings--directory project-root))
          (safe-scheme (swift-project-settings--sanitize-scheme-name scheme)))
      (expand-file-name (format "incremental-commands-%s" safe-scheme) dir))))

(defun swift-incremental-build--save-commands (project-root scheme)
  "Save cached commands to disk for PROJECT-ROOT and SCHEME."
  (when-let* ((file (swift-incremental-build--commands-file project-root scheme)))
    (when (fboundp 'swift-project-settings--ensure-directory)
      (swift-project-settings--ensure-directory project-root))
    (let ((data (list :dylib-link-command swift-incremental-build--dylib-link-command
                      :modules (let ((modules '()))
                                 (maphash (lambda (k v) (push (cons k v) modules))
                                          swift-incremental-build--module-commands)
                                 modules)
                      :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))))
      (swift-project-settings--write-file file data)
      (swift-incremental-build--log "Saved commands for %d modules to %s"
                                     (hash-table-count swift-incremental-build--module-commands)
                                     file))))

(defun swift-incremental-build--load-commands (project-root scheme)
  "Load cached commands from disk for PROJECT-ROOT and SCHEME.
Returns non-nil if commands were loaded successfully."
  (when-let* ((file (swift-incremental-build--commands-file project-root scheme))
              (data (when (file-exists-p file)
                      (swift-project-settings--read-file file))))
    (let ((dylib-cmd (plist-get data :dylib-link-command))
          (modules (plist-get data :modules))
          (timestamp (plist-get data :timestamp)))
      (when (and dylib-cmd modules)
        (setq swift-incremental-build--dylib-link-command dylib-cmd)
        (clrhash swift-incremental-build--module-commands)
        (dolist (entry modules)
          (puthash (car entry) (cdr entry)
                   swift-incremental-build--module-commands))
        (swift-incremental-build--log "Loaded %d modules from %s (saved %s)"
                                       (hash-table-count swift-incremental-build--module-commands)
                                       file (or timestamp "unknown"))
        t))))

;;; Persistence - swiftmodule hashes

(defun swift-incremental-build--hashes-file (project-root scheme)
  "Get path for swiftmodule hashes file for PROJECT-ROOT and SCHEME."
  (when (and project-root scheme
             (fboundp 'swift-project-settings--directory)
             (fboundp 'swift-project-settings--sanitize-scheme-name))
    (let ((dir (swift-project-settings--directory project-root))
          (safe-scheme (swift-project-settings--sanitize-scheme-name scheme)))
      (expand-file-name (format "swiftmodule-hashes-%s" safe-scheme) dir))))

(defun swift-incremental-build--save-swiftmodule-hashes ()
  "Save swiftmodule hashes to disk for persistence across sessions."
  (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
              (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
              (scheme (xcode-project--clean-display-name scheme-raw))
              (file (swift-incremental-build--hashes-file project-root scheme)))
    (when (fboundp 'swift-project-settings--ensure-directory)
      (swift-project-settings--ensure-directory project-root))
    (let ((data (let ((hashes '()))
                  (maphash (lambda (k v) (push (cons k v) hashes))
                           swift-incremental-build--swiftmodule-hashes)
                  hashes)))
      (when data
        (swift-project-settings--write-file file data)
        (swift-incremental-build--log
         "Saved %d swiftmodule hashes to disk"
         (hash-table-count swift-incremental-build--swiftmodule-hashes))))))

(defun swift-incremental-build--load-swiftmodule-hashes ()
  "Load swiftmodule hashes from disk.
Returns non-nil if hashes were loaded."
  (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
              (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
              (scheme (xcode-project--clean-display-name scheme-raw))
              (file (swift-incremental-build--hashes-file project-root scheme))
              (data (when (file-exists-p file)
                      (swift-project-settings--read-file file))))
    (when (listp data)
      (clrhash swift-incremental-build--swiftmodule-hashes)
      (dolist (entry data)
        (when (and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
          (puthash (car entry) (cdr entry)
                   swift-incremental-build--swiftmodule-hashes)))
      (swift-incremental-build--log
       "Loaded %d swiftmodule hashes from disk"
       (hash-table-count swift-incremental-build--swiftmodule-hashes))
      t)))

;;; Persistence - dependency graph

(defun swift-incremental-build--deps-file (project-root scheme)
  "Get path for dependency graph file for PROJECT-ROOT and SCHEME."
  (when (and project-root scheme
             (fboundp 'swift-project-settings--directory)
             (fboundp 'swift-project-settings--sanitize-scheme-name))
    (let ((dir (swift-project-settings--directory project-root))
          (safe-scheme (swift-project-settings--sanitize-scheme-name scheme)))
      (expand-file-name (format "dependency-graph-%s" safe-scheme) dir))))

(defun swift-incremental-build--save-dependency-graph ()
  "Save the reverse dependency graph to disk for persistence."
  (when (> (hash-table-count swift-incremental-build--reverse-deps) 0)
    (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
                (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
                (scheme (xcode-project--clean-display-name scheme-raw))
                (file (swift-incremental-build--deps-file project-root scheme)))
      (when (fboundp 'swift-project-settings--ensure-directory)
        (swift-project-settings--ensure-directory project-root))
      (let ((data (let ((deps '()))
                    (maphash (lambda (k v) (push (cons k v) deps))
                             swift-incremental-build--reverse-deps)
                    deps)))
        (swift-project-settings--write-file file data)
        (swift-incremental-build--log
         "Saved dependency graph (%d entries) to disk"
         (hash-table-count swift-incremental-build--reverse-deps))))))

(defun swift-incremental-build--load-dependency-graph ()
  "Load the reverse dependency graph from disk.
Returns non-nil if the graph was loaded."
  (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
              (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
              (scheme (xcode-project--clean-display-name scheme-raw))
              (file (swift-incremental-build--deps-file project-root scheme))
              (data (when (file-exists-p file)
                      (swift-project-settings--read-file file))))
    (when (listp data)
      (clrhash swift-incremental-build--reverse-deps)
      (dolist (entry data)
        (when (and (consp entry) (stringp (car entry)) (listp (cdr entry)))
          (puthash (car entry) (cdr entry)
                   swift-incremental-build--reverse-deps)))
      (swift-incremental-build--log
       "Loaded dependency graph (%d entries) from disk"
       (hash-table-count swift-incremental-build--reverse-deps))
      t)))

(defun swift-incremental-build--ensure-dependency-graph ()
  "Ensure the reverse dependency graph is available.
Tries sources in order:
  1. Already in memory (instant)
  2. Cached on disk from previous session (fast)
  3. Build from source file scanning (slow, but deferred)
For case 3, schedules the scan asynchronously so it does not
block the current build.  The build will use compile-order
fallback until the graph is ready."
  (cond
   ;; Already in memory
   ((> (hash-table-count swift-incremental-build--reverse-deps) 0)
    t)
   ;; Try disk cache
   ((swift-incremental-build--load-dependency-graph)
    t)
   ;; Schedule async rebuild — don't block the build
   (t
    (swift-incremental-build--log
     "No dependency graph available, scheduling async build")
    (run-with-idle-timer
     1 nil
     (lambda ()
       (swift-incremental-build--log "Building dependency graph (async)...")
       (swift-incremental-build--build-dependency-graph)
       (swift-incremental-build--save-dependency-graph)
       (swift-incremental-build--log "Dependency graph ready")))
    nil)))

(defun swift-incremental-build--ensure-commands-loaded ()
  "Ensure build commands are loaded.
Tries sources in order:
  1. Already in memory (fast path)
  2. .compile database + disk-cached link commands
  3. Disk-cached incremental-commands file (legacy fallback)
Also loads persistent swiftmodule hashes if not yet in memory.
Returns non-nil if commands are available."
  (let ((result
         (if (and swift-incremental-build--dylib-link-command
                  (> (hash-table-count swift-incremental-build--module-commands) 0))
             (progn
               ;; Commands in memory, ensure dependency graph (from disk or async)
               (swift-incremental-build--ensure-dependency-graph)
               t)
           ;; Try .compile database first (always has fresh compile commands)
           (let ((loaded (swift-incremental-build--load-from-compile-database)))
             (if loaded
                 (progn
                   (swift-incremental-build--log
                    "Loaded commands from .compile (modules=%d, dylib=%s)"
                    (hash-table-count swift-incremental-build--module-commands)
                    (if swift-incremental-build--dylib-link-command "yes" "NO"))
                   t)
               ;; Fallback: try legacy disk cache
               (let* ((project-root (ignore-errors (xcode-project-project-root)))
                      (scheme-raw (when project-root
                                    (bound-and-true-p xcode-project--current-xcode-scheme)))
                      (scheme (when scheme-raw
                                (xcode-project--clean-display-name scheme-raw))))
                 (when (and project-root scheme)
                   (let ((disk-loaded (swift-incremental-build--load-commands project-root scheme)))
                     ;; If loaded from disk but missing compile-order, import from .compile
                     (when (and disk-loaded
                                (swift-incremental-build--compile-database-path))
                       (swift-incremental-build--import-compile-order-from-database))
                     (swift-incremental-build--log
                      "Loaded commands from disk cache: %s" (if disk-loaded "yes" "no"))
                     disk-loaded))))))))
    ;; Load persistent swiftmodule hashes if not yet in memory
    (when (and result
               (= (hash-table-count swift-incremental-build--swiftmodule-hashes) 0))
      (swift-incremental-build--load-swiftmodule-hashes))
    result))

;;; Build output extraction (from string, not just file)

(defun swift-incremental-build--parse-build-output (output-string)
  "Parse OUTPUT-STRING to extract compile and link commands.
Same as `swift-incremental-build--parse-build-log' but works on a string
instead of a file.  Used for auto-extraction after xcodebuild completes."
  (swift-incremental-build--log "Parsing build output (%d chars)" (length output-string))
  (clrhash swift-incremental-build--module-commands)
  (setq swift-incremental-build--dylib-link-command nil)
  (with-temp-buffer
    (insert output-string)
    (goto-char (point-min))
    (swift-incremental-build--extract-compile-commands)
    (goto-char (point-min))
    (swift-incremental-build--extract-module-link-commands)
    (goto-char (point-min))
    (swift-incremental-build--extract-dylib-link-command))
  (let ((count (hash-table-count swift-incremental-build--module-commands)))
    (swift-incremental-build--log "Extracted commands for %d modules" count)
    (> count 0)))

;;; Auto-extraction hook for swift-development

(defun swift-incremental-build--merge-link-commands-from-output (output-string)
  "Extract link commands from OUTPUT-STRING and merge into existing cache.
Parses the build output into a temporary hash table using the same
proven extraction functions, then merges only `:link-command' entries
and the dylib link command back into the real cache.
This preserves compile-commands and compile-order from `.compile'.
Uses `unwind-protect' to guarantee global state is restored even
if extraction functions signal an error."
  (swift-incremental-build--log "Merging link commands from build output (%d chars)"
                                 (length output-string))
  (let ((saved-modules swift-incremental-build--module-commands)
        (saved-dylib swift-incremental-build--dylib-link-command)
        (parsed-modules nil)
        (parsed-dylib nil)
        (merged-links 0))
    ;; Parse into a fresh temporary hash table; restore globals on error
    (unwind-protect
        (progn
          (setq swift-incremental-build--module-commands
                (make-hash-table :test 'equal))
          (setq swift-incremental-build--dylib-link-command nil)
          (with-temp-buffer
            (insert output-string)
            (goto-char (point-min))
            (swift-incremental-build--extract-module-link-commands)
            (goto-char (point-min))
            (swift-incremental-build--extract-dylib-link-command))
          (setq parsed-modules swift-incremental-build--module-commands
                parsed-dylib swift-incremental-build--dylib-link-command))
      ;; Always restore the real cache
      (setq swift-incremental-build--module-commands saved-modules
            swift-incremental-build--dylib-link-command saved-dylib))
    ;; Merge link commands from parsed into real cache
    (maphash
     (lambda (name data)
       (let ((link-cmd (plist-get data :link-command))
             (existing (gethash name swift-incremental-build--module-commands)))
         (when (and link-cmd existing)
           (plist-put existing :link-command link-cmd)
           (cl-incf merged-links))))
     parsed-modules)
    ;; Merge dylib link command
    (when parsed-dylib
      (setq swift-incremental-build--dylib-link-command parsed-dylib))
    ;; Save updated cache to disk
    (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
                (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
                (scheme (xcode-project--clean-display-name scheme-raw)))
      (swift-incremental-build--save-commands project-root scheme))
    (swift-incremental-build--log
     "Merged %d link commands, dylib=%s"
     merged-links (if parsed-dylib "yes" "no"))))

;;;###autoload
(defun swift-incremental-build-extract-from-build-output (output-string)
  "Auto-extract build commands from xcodebuild OUTPUT-STRING.
Called automatically after a successful xcodebuild to populate the
incremental build cache for next time.  Strategy:
  1. Load compile commands from `.compile' database (best quality)
  2. Extract link commands from the build OUTPUT-STRING
  3. Merge link commands into the compile command cache
  4. Save everything to disk for persistence
This runs asynchronously via `run-with-timer' from the build sentinel."
  (swift-incremental-build--log
   "Auto-extracting from build output (%d chars)" (length output-string))
  (condition-case err
      (progn
        ;; Step 1: Ensure compile commands from .compile are loaded
        (let ((had-commands (> (hash-table-count
                                swift-incremental-build--module-commands) 0)))
          (unless had-commands
            (swift-incremental-build--load-from-compile-database))
          ;; Step 2: Merge link commands from build output
          (if (> (hash-table-count swift-incremental-build--module-commands) 0)
              (swift-incremental-build--merge-link-commands-from-output output-string)
            ;; No .compile file — fall back to full extraction from log
            (swift-incremental-build--parse-build-output output-string)
            ;; Save to disk
            (when-let* ((project-root (ignore-errors (xcode-project-project-root)))
                        (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
                        (scheme (xcode-project--clean-display-name scheme-raw)))
              (swift-incremental-build--save-commands project-root scheme))))
        (swift-incremental-build--log
         "Auto-extract complete: %d modules, dylib=%s"
         (hash-table-count swift-incremental-build--module-commands)
         (if swift-incremental-build--dylib-link-command "yes" "NO")))
    (error
     (swift-incremental-build--log
      "Auto-extract failed: %s" (error-message-string err)))))

;;; Public API

;;;###autoload
(defun swift-incremental-build-extract-commands (&optional log-file)
  "Bootstrap incremental build from LOG-FILE (or prompt for one).
Loads compile commands from `.compile' database, then merges
link commands from the build log.  Saves everything to disk.
Run this once after a full xcodebuild to enable incremental builds."
  (interactive
   (list (read-file-name "Build log file: " "/tmp/" nil t "swift-build-output.log")))
  (unless (and log-file (file-exists-p log-file))
    (error "Build log file not found: %s" log-file))
  (let ((output (with-temp-buffer
                  (insert-file-contents log-file)
                  (buffer-string))))
    (swift-incremental-build-extract-from-build-output output))
  (let ((module-count (hash-table-count swift-incremental-build--module-commands))
        (has-dylib swift-incremental-build--dylib-link-command)
        (link-count 0))
    (maphash (lambda (_k v)
               (when (plist-get v :link-command)
                 (cl-incf link-count)))
             swift-incremental-build--module-commands)
    (message "Incremental build ready: %d modules (%d with link), dylib=%s"
             module-count link-count
             (if has-dylib "yes" "NO"))))

;;;###autoload
(defun swift-incremental-build-compile-and-run ()
  "Incrementally compile all changed modules and run on simulator.
Detects changed modules from file-watcher data (or falls back to
current buffer's module), recompiles, relinks, codesigns, installs,
and launches.  Automatically loads cached commands from disk if not
already in memory."
  (interactive)
  (save-some-buffers t)
  (swift-incremental-build--ensure-commands-loaded)
  (swift-incremental-build--run-for-modules t))

;;;###autoload
(defun swift-incremental-build-compile ()
  "Incrementally compile all changed modules without running.
Detects changed modules from file-watcher data (or falls back to
current buffer's module), recompiles, relinks, and codesigns the
app bundle.  Automatically loads cached commands from disk if not
already in memory."
  (interactive)
  (save-some-buffers t)
  (swift-incremental-build--ensure-commands-loaded)
  (swift-incremental-build--run-for-modules nil))

(defun swift-incremental-build--cancel-if-running ()
  "Cancel any in-progress incremental build silently.
Called automatically before starting a new build to prevent
state corruption from concurrent builds."
  (when (and swift-incremental-build--active-process
             (process-live-p swift-incremental-build--active-process))
    (swift-incremental-build--log "Auto-cancelling previous build")
    (delete-process swift-incremental-build--active-process)
    (setq swift-incremental-build--active-process nil)
    (when (fboundp 'swift-notification-progress-cancel)
      (swift-notification-progress-cancel 'swift-incremental))))

(defun swift-incremental-build--run-for-modules (run)
  "Detect changed modules and run incremental build pipeline.
If RUN is non-nil, install and launch after building.
Auto-cancels any in-progress build before starting.
Resets cascade state so the three-stage fallback chain starts fresh:
incremental -> cascade downstream -> full xcodebuild."
  (swift-incremental-build--cancel-if-running)
  (setq swift-incremental-build--current-run run
        swift-incremental-build--cascade-attempted nil)
  ;; Don't clear swiftmodule hashes — they are persistent across builds
  ;; to avoid flip-flop instability from build config differences.
  (let* ((modules (swift-incremental-build--changed-modules))
         (sorted (and modules
                      (swift-incremental-build--sort-modules-by-order modules))))
    (unless sorted
      (error "Cannot detect module for current file: %s" (buffer-file-name)))
    (unless swift-incremental-build--dylib-link-command
      (error "No cached dylib link command. Run a full build first"))
    (unless (swift-incremental-build--all-modules-have-commands-p sorted)
      (error "Missing cached commands for some modules: %s. Run a full build first"
             (mapconcat #'identity sorted ", ")))
    ;; Store originally changed modules for cascade rebuild
    (setq swift-incremental-build--originally-changed-modules sorted)
    (if (= (length sorted) 1)
        ;; Single module - use existing optimized pipeline
        (swift-incremental-build--run-pipeline (car sorted) run)
      ;; Multiple modules - use multi-module pipeline (already sorted)
      (swift-incremental-build--log "Multi-module build: %s"
                                     (mapconcat #'identity sorted ", "))
      (swift-incremental-build--run-multi-pipeline sorted run))))

;;;###autoload
(defun swift-incremental-build-show-modules ()
  "Display the list of modules with cached build commands."
  (interactive)
  (if (= (hash-table-count swift-incremental-build--module-commands) 0)
      (message "No cached module commands. Run M-x swift-incremental-build-extract-commands first.")
    (let ((modules '()))
      (maphash (lambda (key value)
                 (push (format "  %3d  %s (compile: %s, link: %s)"
                               (or (plist-get value :compile-order) -1)
                               key
                               (if (plist-get value :compile-command) "yes" "NO")
                               (if (plist-get value :link-command) "yes" "NO"))
                       modules))
               swift-incremental-build--module-commands)
      (message "Cached modules (by build order):\n%s\nDylib link: %s"
               (string-join (sort modules #'string<) "\n")
               (if swift-incremental-build--dylib-link-command "yes" "NO")))))

;;;###autoload
(defun swift-incremental-build-cancel ()
  "Cancel the currently running incremental build."
  (interactive)
  (when (and swift-incremental-build--active-process
             (process-live-p swift-incremental-build--active-process))
    (delete-process swift-incremental-build--active-process)
    (setq swift-incremental-build--active-process nil)
    (when (fboundp 'swift-notification-progress-cancel)
      (swift-notification-progress-cancel 'swift-incremental))
    (message "Incremental build cancelled.")))

;;;###autoload
(defun swift-incremental-build-clear-cache ()
  "Clear all cached build commands."
  (interactive)
  (clrhash swift-incremental-build--module-commands)
  (clrhash swift-incremental-build--swiftmodule-hashes)
  (clrhash swift-incremental-build--reverse-deps)
  (setq swift-incremental-build--dylib-link-command nil
        swift-incremental-build--app-bundle-path nil
        swift-incremental-build--intermediates-path nil
        swift-incremental-build--originally-changed-modules nil
        swift-incremental-build--cascade-attempted nil)
  (message "Incremental build cache cleared."))

;;;###autoload
(defun swift-incremental-build-toggle ()
  "Toggle incremental builds on or off."
  (interactive)
  (setq swift-incremental-build-enabled (not swift-incremental-build-enabled))
  (message "Incremental builds: %s"
           (if swift-incremental-build-enabled "ENABLED" "DISABLED")))

;;;###autoload
(defun swift-incremental-build-toggle-debug ()
  "Toggle debug logging for incremental builds."
  (interactive)
  (setq swift-incremental-build-debug (not swift-incremental-build-debug))
  (message "Incremental build debug: %s" (if swift-incremental-build-debug "ON" "OFF")))

;;;###autoload
(defun swift-incremental-build-status ()
  "Show the current incremental build status and diagnostics."
  (interactive)
  (let* ((file (buffer-file-name))
         (module (when file
                   (or (swift-incremental-build--detect-module)
                       (swift-incremental-build--detect-module-from-build-target))))
         (module-count (hash-table-count swift-incremental-build--module-commands))
         (has-dylib (not (null swift-incremental-build--dylib-link-command)))
         (has-module-cmds (when module
                            (not (null (gethash module swift-incremental-build--module-commands)))))
         (module-data (when module
                        (gethash module swift-incremental-build--module-commands)))
         (has-compile (when module-data (plist-get module-data :compile-command)))
         (has-link (when module-data (plist-get module-data :link-command)))
         (products-dir (ignore-errors (swift-incremental-build--products-dir)))
         (app-bundle (ignore-errors (swift-incremental-build--app-bundle)))
         (scheme-raw (bound-and-true-p xcode-project--current-xcode-scheme))
         (ready (ignore-errors (swift-incremental-build-ready-p))))
    (message (concat
              "Incremental build status:\n"
              (format "  Enabled: %s\n" (if swift-incremental-build-enabled "YES" "NO"))
              (format "  Debug: %s\n" (if swift-incremental-build-debug "ON" "OFF"))
              (format "  Scheme: %s\n" (or scheme-raw "not set"))
              (format "  Current file: %s\n" (or (and file (abbreviate-file-name file)) "none"))
              (format "  Detected module: %s\n" (or module "none"))
              (format "  Cached modules: %d\n" module-count)
              (format "  Dylib link cmd: %s\n" (if has-dylib "yes" "NO"))
              (format "  Module commands: %s (compile=%s, link=%s)\n"
                      (if has-module-cmds "yes" "NO")
                      (if has-compile "yes" "NO")
                      (if has-link "yes" "NO"))
              (format "  Products dir: %s\n" (or (and products-dir (abbreviate-file-name products-dir)) "NOT FOUND"))
              (format "  App bundle: %s\n" (or (and app-bundle (abbreviate-file-name app-bundle)) "NOT FOUND"))
              (format "  Ready: %s%s" (if ready "YES" "NO")
                      (if (and module has-compile (not has-link))
                          " (main app target - uses xcodebuild)" ""))))))

;;; .compile database support

(defun swift-incremental-build--compile-database-path ()
  "Return path to .compile file in the project root, or nil."
  (when-let* ((root (ignore-errors (xcode-project-project-root))))
    (let ((path (expand-file-name ".compile" root)))
      (when (file-exists-p path)
        path))))

(defun swift-incremental-build--parse-compile-database (file)
  "Parse .compile JSON FILE and return an alist of (module-name . plist).
Each plist has :command, :directory, :file-lists, and :compile-order.
Deduplicates modules, keeping the last occurrence (which reflects
the final topological position in xcodebuild output).
Entries without a module_name (C/ObjC commands) are skipped."
  (let* ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol)
         (entries (json-read-file file))
         (seen (make-hash-table :test 'equal))
         (order 0))
    ;; First pass: record last occurrence index for each module
    (dolist (entry entries)
      (when-let* ((name (alist-get 'module_name entry)))
        (puthash name order seen)
        (cl-incf order)))
    ;; Second pass: collect unique modules at their last occurrence
    (let ((result '())
          (idx 0))
      (dolist (entry entries)
        (when-let* ((name (alist-get 'module_name entry)))
          (when (= idx (gethash name seen))
            (push (cons name
                        (list :command (alist-get 'command entry)
                              :directory (alist-get 'directory entry)
                              :file-lists (alist-get 'fileLists entry)
                              :compile-order (gethash name seen)))
                  result))
          (cl-incf idx)))
      ;; Return in compile-order (reverse of push order)
      (nreverse result))))

(defun swift-incremental-build--import-compile-order-from-database ()
  "Import compile-order from .compile into cached module commands.
Updates `:compile-order' on existing module plists without
touching the compile/link commands.  Returns number of modules
updated, or nil if .compile was not found."
  (when-let* ((db-path (swift-incremental-build--compile-database-path))
              (db-modules (swift-incremental-build--parse-compile-database db-path)))
    (let ((updated 0))
      (dolist (entry db-modules)
        (let* ((name (car entry))
               (db-order (plist-get (cdr entry) :compile-order))
               (existing (gethash name swift-incremental-build--module-commands)))
          (when (and existing db-order)
            (puthash name
                     (plist-put existing :compile-order db-order)
                     swift-incremental-build--module-commands)
            (cl-incf updated))))
      (swift-incremental-build--log
       "Imported compile-order for %d modules from .compile" updated)
      updated)))

(defun swift-incremental-build--load-from-compile-database ()
  "Load compile commands from .compile and merge with disk-cached link commands.
Populates `swift-incremental-build--module-commands' with
`:compile-command' and `:compile-order' from `.compile', then
merges `:link-command' and `swift-incremental-build--dylib-link-command'
from the saved incremental-commands file on disk.
Returns non-nil if at least compile-commands were loaded."
  (when-let* ((db-path (swift-incremental-build--compile-database-path))
              (db-modules (swift-incremental-build--parse-compile-database db-path)))
    (swift-incremental-build--log
     "Loading %d modules from .compile" (length db-modules))
    ;; Populate module-commands from .compile
    (clrhash swift-incremental-build--module-commands)
    (let ((third-party-count 0))
      (dolist (entry db-modules)
        (let* ((name (car entry))
               (data (cdr entry))
               (command (plist-get data :command))
               (is-third-party (and command
                                    (string-match-p "-suppress-warnings" command))))
          (when is-third-party (cl-incf third-party-count))
          (puthash name
                    (list :compile-command command
                          :compile-order (plist-get data :compile-order)
                          :file-lists (plist-get data :file-lists)
                          :directory (plist-get data :directory)
                          :third-party is-third-party)
                   swift-incremental-build--module-commands)))
      (swift-incremental-build--log
       "Classified modules: %d internal, %d third-party"
       (- (length db-modules) third-party-count) third-party-count))
    ;; Now try to merge link-commands from the disk cache
    (let* ((project-root (ignore-errors (xcode-project-project-root)))
           (scheme-raw (when project-root
                         (bound-and-true-p xcode-project--current-xcode-scheme)))
           (scheme (when scheme-raw
                     (xcode-project--clean-display-name scheme-raw)))
           (disk-file (when (and project-root scheme)
                        (swift-incremental-build--commands-file project-root scheme)))
           (disk-data (when (and disk-file (file-exists-p disk-file))
                        (swift-project-settings--read-file disk-file))))
      (when disk-data
        ;; Merge dylib link command
        (when-let* ((dylib-cmd (plist-get disk-data :dylib-link-command)))
          (setq swift-incremental-build--dylib-link-command dylib-cmd)
          (swift-incremental-build--log "Loaded dylib-link-command from disk cache"))
        ;; Merge link-commands per module
        (let ((disk-modules (plist-get disk-data :modules))
              (merged 0))
          (dolist (disk-entry disk-modules)
            (let* ((name (car disk-entry))
                   (disk-plist (cdr disk-entry))
                   (link-cmd (plist-get disk-plist :link-command))
                   (existing (gethash name swift-incremental-build--module-commands)))
              (when (and existing link-cmd)
                (puthash name
                         (plist-put existing :link-command link-cmd)
                         swift-incremental-build--module-commands)
                (cl-incf merged))))
          (swift-incremental-build--log
           "Merged link-commands for %d modules from disk cache" merged))))
    ;; Ensure dependency graph (from disk cache or async build)
    (swift-incremental-build--ensure-dependency-graph)
    ;; Return success
    (let ((count (hash-table-count swift-incremental-build--module-commands)))
      (swift-incremental-build--log
       "Loaded %d modules (dylib: %s)"
       count (if swift-incremental-build--dylib-link-command "yes" "NO"))
      (> count 0))))

;;;###autoload
(defun swift-incremental-build-show-compile-database ()
  "Display module order from the .compile database file.
Useful for verifying the topological dependency order."
  (interactive)
  (if-let* ((db-path (swift-incremental-build--compile-database-path))
            (modules (swift-incremental-build--parse-compile-database db-path)))
      (let ((lines (mapcar
                    (lambda (entry)
                      (let* ((name (car entry))
                             (order (plist-get (cdr entry) :compile-order))
                             (cached (gethash name swift-incremental-build--module-commands))
                             (is-3p (and cached (plist-get cached :third-party)))
                             (tag (cond (is-3p "3rd-party")
                                        (cached "internal")
                                        (t "NOT cached"))))
                        (format "  %3d  %-25s  [%s]" order name tag)))
                    modules))
            (third-party-count (cl-count-if
                                (lambda (entry)
                                  (let ((cached (gethash (car entry)
                                                         swift-incremental-build--module-commands)))
                                    (and cached (plist-get cached :third-party))))
                                modules)))
        (message ".compile modules (%d: %d internal, %d third-party):\n%s"
                 (length modules)
                 (- (length modules) third-party-count)
                 third-party-count
                 (string-join lines "\n")))
    (message "No .compile file found in project root.")))

;;;###autoload
(defun swift-incremental-build-show-dependency-graph ()
  "Display the reverse dependency graph (who depends on whom).
Third-party modules are marked with [3p]."
  (interactive)
  (if (= (hash-table-count swift-incremental-build--reverse-deps) 0)
      (message "No dependency graph. Trigger an incremental build to populate it.")
    (let ((lines '()))
      (maphash
       (lambda (module dependents)
         (when dependents
           (let* ((is-3p (swift-incremental-build--module-third-party-p module))
                  (label (if is-3p
                             (format "%-25s [3p]" module)
                           (format "%-25s" module)))
                  (dep-labels
                   (mapcar (lambda (d)
                             (if (swift-incremental-build--module-third-party-p d)
                                 (concat d " [3p]")
                               d))
                           (sort (copy-sequence dependents) #'string<))))
             (push (format "  %s <- %s"
                           label
                           (mapconcat #'identity dep-labels ", "))
                   lines))))
       swift-incremental-build--reverse-deps)
      (message "Reverse dependency graph:\n%s"
               (string-join (sort lines #'string<) "\n")))))

(provide 'swift-incremental-build)
;;; swift-incremental-build.el ends here
