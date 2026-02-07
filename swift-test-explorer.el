;;; swift-test-explorer.el --- Test Explorer for Swift/iOS development -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, testing, xcode

;;; Commentary:

;; Test Explorer for Swift/iOS development.
;;
;; Features:
;; - Tree view of tests organized by target/class/test
;; - Test discovery from Swift source files
;; - Run tests at point, by class, or all tests
;; - Support for both XCTest and Swift Testing (@Test macro)
;; - Integration with periphery for result parsing
;; - Navigation to test source
;; - Re-run failed tests
;;
;; Usage:
;;   M-x swift-test-explorer-show      - Open test explorer
;;   M-x swift-test-run-at-point       - Run test at cursor
;;   M-x swift-test-run-class          - Run all tests in class
;;   M-x swift-test-run-all            - Run all tests
;;   M-x swift-test-run-failed         - Re-run failed tests

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'swift-async)

;; Optional dependencies
(require 'xcode-project nil t)
(require 'xcode-build-config nil t)
(require 'swift-notification nil t)
(require 'nerd-icons nil t)

;; Forward declarations
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-scheme "xcode-project")
(declare-function xcode-project-get-workspace-or-project "xcode-project")
(declare-function xcode-build-config-build-app-command "xcode-build-config")
(declare-function swift-notification-send "swift-notification")
(declare-function periphery-run-test-parser "periphery")
(declare-function periphery-run-parser "periphery")
(declare-function swift-error-proxy-parse-output "swift-error-proxy")
(declare-function swift-error-proxy-has-errors-p "swift-error-proxy")

;;; Customization

(defgroup swift-test-explorer nil
  "Test Explorer for Swift development."
  :group 'swift-development
  :prefix "swift-test-")

(defcustom swift-test-explorer-window-width 40
  "Width of the test explorer window."
  :type 'integer
  :group 'swift-test-explorer)

(defcustom swift-test-explorer-window-position 'left
  "Position of the test explorer window."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'swift-test-explorer)

(defcustom swift-test-auto-discover t
  "Automatically discover tests when opening the explorer."
  :type 'boolean
  :group 'swift-test-explorer)

(defcustom swift-test-animate-running t
  "Animate running test indicators."
  :type 'boolean
  :group 'swift-test-explorer)

;;; Icon Functions (using nerd-icons with fallbacks)

(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-devicon "nerd-icons")

(defun swift-test-icon-passed ()
  "Get icon for passed tests."
  (if (fboundp 'nerd-icons-codicon)
      (nerd-icons-codicon "nf-cod-pass_filled" :face 'swift-test-explorer-passed-face)
    "✓"))

(defun swift-test-icon-failed ()
  "Get icon for failed tests."
  (if (fboundp 'nerd-icons-codicon)
      (nerd-icons-codicon "nf-cod-error" :face 'swift-test-explorer-failed-face)
    "✗"))

(defun swift-test-icon-running ()
  "Get icon for running tests."
  (if (fboundp 'nerd-icons-faicon)
      (nerd-icons-faicon "nf-fa-spinner" :face 'swift-test-explorer-running-face)
    "◌"))

(defun swift-test-icon-not-run ()
  "Get icon for tests not yet run."
  "◇")

(defun swift-test-icon-skipped ()
  "Get icon for skipped/disabled tests."
  (if (fboundp 'nerd-icons-faicon)
      (nerd-icons-faicon "nf-fa-minus_circle" :face 'swift-test-explorer-skipped-face)
    "⊘"))

(defun swift-test-icon-file ()
  "Get icon for test files (Swift logo in orange)."
  (if (fboundp 'nerd-icons-devicon)
      (nerd-icons-devicon "nf-dev-swift" :face 'swift-test-explorer-swift-icon-face)
    "📄"))

(defun swift-test-icon-class ()
  "Get icon for test classes/suites."
  (if (fboundp 'nerd-icons-codicon)
      (nerd-icons-codicon "nf-cod-symbol_class" :face 'swift-test-explorer-class-face)
    "◆"))

(defun swift-test-icon-collapsed ()
  "Get icon for collapsed nodes."
  "+")

(defun swift-test-icon-expanded ()
  "Get icon for expanded nodes."
  "-")

(defun swift-test-icon-spm ()
  "Get icon for SPM (Swift Package Manager) packages."
  "📦")

(defun swift-test-icon-xcode ()
  "Get icon for Xcode project targets."
  (if (fboundp 'nerd-icons-devicon)
      (nerd-icons-devicon "nf-dev-xcode" :face 'swift-test-explorer-xcode-face)
    "🍎"))

;;; Faces

(defface swift-test-explorer-target-face
  '((t :inherit default :weight normal))
  "Face for test targets."
  :group 'swift-test-explorer)

(defface swift-test-explorer-file-face
  '((t :inherit default))
  "Face for test files."
  :group 'swift-test-explorer)

(defface swift-test-explorer-swift-icon-face
  '((t :foreground "#F05138"))
  "Face for Swift file icon (orange)."
  :group 'swift-test-explorer)

(defface swift-test-explorer-class-face
  '((t :inherit font-lock-type-face))
  "Face for test classes/suites."
  :group 'swift-test-explorer)

(defface swift-test-explorer-test-face
  '((t :inherit font-lock-function-name-face))
  "Face for test functions."
  :group 'swift-test-explorer)

(defface swift-test-explorer-passed-face
  '((t :inherit success))
  "Face for passed tests."
  :group 'swift-test-explorer)

(defface swift-test-explorer-failed-face
  '((t :inherit error))
  "Face for failed tests."
  :group 'swift-test-explorer)

(defface swift-test-explorer-running-face
  '((t :inherit warning))
  "Face for running tests."
  :group 'swift-test-explorer)

(defface swift-test-explorer-not-run-face
  '((t :inherit shadow))
  "Face for tests not yet run."
  :group 'swift-test-explorer)

(defface swift-test-explorer-xcode-face
  '((t :inherit default))
  "Face for Xcode project icon."
  :group 'swift-test-explorer)

(defface swift-test-explorer-skipped-face
  '((t :inherit shadow :slant italic))
  "Face for skipped tests."
  :group 'swift-test-explorer)

;;; Data Structures

(cl-defstruct swift-test-node
  "A node in the test tree."
  id           ; Unique identifier (target/file/class/test)
  kind         ; 'target, 'file, 'class, or 'test
  name         ; Display name
  status       ; 'not-run, 'running, 'passed, 'failed, 'skipped
  filepath     ; Path to source file
  line-number  ; Line number in source file
  expanded     ; Whether node is expanded (for targets/classes/files)
  children     ; Child nodes (for targets/classes/files)
  parent       ; Parent node reference
  duration     ; Test duration in seconds (if available)
  message      ; Error/failure message (if any)
  display-name ; Swift Testing display name from @Test("...") or @Suite("...")
  func-name    ; Original function name (for Swift Testing tests)
  package-type ; 'spm for local SPM package, 'xcode for Xcode target, nil for unknown
  package-root) ; Root directory for local SPM packages

(defvar swift-test--report nil
  "Current test report - list of target nodes.")

(defvar swift-test--buffer-name "*Test Explorer*"
  "Name of the test explorer buffer.")

(defvar swift-test--line-to-node nil
  "Hash table mapping line numbers to nodes in the explorer buffer.")

(defvar swift-test--last-run-tests nil
  "List of test IDs from the last test run.")

(defvar swift-test--failed-tests nil
  "List of test IDs that failed in the last run.")

(defvar swift-test--running-tests nil
  "List of test IDs currently running.")

(defvar swift-test--animation-timer nil
  "Timer for animating running tests.")

(defvar swift-test--animation-frame 0
  "Current animation frame.")

(defvar swift-test--scheme-cache (make-hash-table :test 'equal)
  "Hash table mapping project root to cached scheme.")

(defvar swift-test--current-project-root nil
  "The project root for the current test explorer state.")

(defconst swift-test--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Animation frames for running tests.")

;;; Utility Functions

(defun swift-test--project-root ()
  "Get the project root directory."
  (or (and (fboundp 'xcode-project-project-root)
           (xcode-project-project-root))
      (when-let* ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun swift-test--notify (message &optional seconds)
  "Show notification with MESSAGE for SECONDS."
  (if (fboundp 'swift-notification-send)
      (swift-notification-send :message message :seconds (or seconds 2))
    (message "%s" message)))

(defun swift-test--make-test-id (target class &optional test)
  "Create a test ID from TARGET, CLASS, and optional TEST name."
  (if test
      (format "%s/%s/%s" target class test)
    (if class
        (format "%s/%s" target class)
      target)))

(defun swift-test--parse-test-id (id)
  "Parse a test ID into (target class test) list."
  (split-string id "/" t))

(defun swift-test--format-only-testing-arg (id)
  "Format ID as an -only-testing argument for xcodebuild.
ID is parsed into target/class/test components."
  (let ((parts (swift-test--parse-test-id id)))
    (if (nth 2 parts)
        ;; Full test path: target/class/test
        (format "\"-only-testing:%s/%s/%s\""
                (nth 0 parts)
                (nth 1 parts)
                (nth 2 parts))
      ;; Class only: target/class (for running all tests in a class)
      (format "\"-only-testing:%s/%s\""
              (nth 0 parts)
              (nth 1 parts)))))

(defun swift-test--format-error-message (msg)
  "Format error MSG for display, cleaning up multi-line comparison messages.
Extracts the key assertion info and makes it more readable."
  (when msg
    (let ((cleaned msg))
      ;; Replace literal newlines within quoted strings with a visual indicator
      ;; XCTAssertEqual shows: ("line1\n\nline2") is not equal to ("line3\n\nline4")
      (setq cleaned (replace-regexp-in-string "\n\n" " | " cleaned))
      (setq cleaned (replace-regexp-in-string "\n" " " cleaned))
      ;; Collapse multiple spaces
      (setq cleaned (replace-regexp-in-string "  +" " " cleaned))
      ;; Trim
      (string-trim cleaned))))

(defun swift-test--reset-state ()
  "Reset all test explorer state for a fresh start."
  (setq swift-test--report nil)
  (setq swift-test--last-run-tests nil)
  (setq swift-test--failed-tests nil)
  (setq swift-test--running-tests nil)
  (setq swift-test--line-to-node (make-hash-table))
  (swift-test--stop-animation))

(defun swift-test--check-project-changed ()
  "Check if project has changed and reset state if needed."
  (let ((current-root (swift-test--project-root)))
    (unless (equal current-root swift-test--current-project-root)
      ;; Project changed - reset everything
      (swift-test--reset-state)
      (setq swift-test--current-project-root current-root)
      t)))  ; Return t if project changed

;;; Test Discovery

(defun swift-test--find-test-files ()
  "Find all Swift test files in the project.
Excludes .build, DerivedData, and other build directories."
  (let* ((root (swift-test--project-root))
         (files '())
         ;; Directories to exclude
         (excluded-dirs '(".build" "DerivedData" "build" "Build" ".swiftpm" "Pods" "Carthage")))
    ;; Find files matching common test patterns (using regexp, not glob)
    (dolist (pattern '("Tests\\.swift$" "Test\\.swift$" "Spec\\.swift$"))
      (setq files (append files
                          (directory-files-recursively root pattern))))
    ;; Also search in any *Tests directories (e.g., testpreviewTests)
    (dolist (dir (directory-files root t "Tests$"))
      (when (file-directory-p dir)
        (setq files (append files
                            (directory-files-recursively dir "\\.swift$")))))
    ;; Filter out excluded directories
    (setq files (cl-remove-if
                 (lambda (f)
                   (cl-some (lambda (excluded)
                              (string-match-p (concat "/" excluded "/") f))
                            excluded-dirs))
                 files))
    ;; Remove duplicates and sort
    (sort (delete-dups files) #'string<)))

(defun swift-test--parse-swift-file-for-tests (filepath)
  "Parse FILEPATH for test functions.
Returns a list of plists with :class, :test, :line, :type, :display-name, :suite-name, :suite-path."
  (let ((tests '())
        (current-class nil)
        (current-class-type nil)  ; 'xctest or 'swift-testing
        (current-suite-name nil)  ; Display name from @Suite("...")
        (suite-stack '())         ; Stack of (struct-name . indent) for nested suites
        (swift-testing-lines (make-hash-table)))  ; Track lines with @Test
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      
      ;; First pass: find all @Test lines and the func lines they apply to
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
              (line-num (line-number-at-pos)))
          (when (string-match "^\\s-*@Test" line)
            ;; Mark this line and potentially the next as Swift Testing
            (puthash line-num t swift-testing-lines)
            (save-excursion
              (forward-line 1)
              (puthash (line-number-at-pos) t swift-testing-lines))))
        (forward-line 1))
      
      ;; Second pass: parse classes and tests
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
              (line-num (line-number-at-pos)))
          
          ;; Check for class declaration (XCTestCase)
          (when (string-match
                 "\\(?:class\\|final class\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*:\\s-*\\(?:.*\\)?XCTestCase"
                 line)
            (setq current-class (match-string 1 line))
            (setq current-class-type 'xctest)
            (setq current-suite-name nil))
          
          ;; Check for @Suite annotation (may be on line before struct/class)
          (when (string-match "@Suite(\"\\([^\"]+\\)\")" line)
            (setq current-suite-name (match-string 1 line)))
          
          ;; Check for struct/class with Swift Testing @Suite
          (when (string-match
                 "^\\(\\s-*\\)\\(?:struct\\|class\\|final class\\|actor\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
                 line)
            (let ((indent (length (match-string 1 line)))
                  (struct-name (match-string 2 line))
                  (found-suite nil)
                  (suite-display-name nil))
              ;; Check if @Suite is on previous line(s)
              (save-excursion
                (forward-line -1)
                (let ((prev-line (buffer-substring-no-properties
                                  (line-beginning-position) (line-end-position))))
                  (when (string-match "@Suite" prev-line)
                    (setq found-suite t)
                    ;; Extract display name if present
                    (when (string-match "@Suite(\"\\([^\"]+\\)\")" prev-line)
                      (setq suite-display-name (match-string 1 prev-line))))
                  (unless found-suite
                    (forward-line -1)
                    (setq prev-line (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))
                    (when (string-match "@Suite" prev-line)
                      (setq found-suite t)
                      (when (string-match "@Suite(\"\\([^\"]+\\)\")" prev-line)
                        (setq suite-display-name (match-string 1 prev-line)))))))
              (when found-suite
                ;; Pop suites that are at same or higher indent (we've exited them)
                (while (and suite-stack (<= indent (cdar suite-stack)))
                  (pop suite-stack))
                ;; Push this suite onto the stack
                (push (cons struct-name indent) suite-stack)
                (setq current-class struct-name)
                (setq current-class-type 'swift-testing)
                (setq current-suite-name suite-display-name))))
          
          ;; Check for Swift Testing @Test
          (when (string-match "^\\s-*@Test" line)
            (let ((test-display-name nil))
              ;; Extract display name from @Test("...")
              (when (string-match "@Test(\"\\([^\"]+\\)\")" line)
                (setq test-display-name (match-string 1 line)))
              (save-excursion
                (let ((func-line line)
                      (func-line-num line-num))
                  (unless (string-match "func\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" func-line)
                    (forward-line 1)
                    (setq func-line (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))
                    (setq func-line-num (line-number-at-pos)))
                  (when (string-match "func\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" func-line)
                    ;; Build the full suite path from the stack (reversed, innermost last)
                    (let ((suite-path (mapconcat #'car (reverse suite-stack) "/")))
                      (push (list :class (or current-class
                                             (file-name-sans-extension
                                              (file-name-nondirectory filepath)))
                                  :test (match-string 1 func-line)
                                  :line func-line-num
                                  :type :swift-testing
                                  :display-name test-display-name
                                  :suite-name current-suite-name
                                  :suite-path suite-path)  ; Full path for -only-testing
                            tests)))))))
          
          ;; Check for XCTest test function (only if not a @Test line)
          (when (and current-class
                     (eq current-class-type 'xctest)
                     (not (gethash line-num swift-testing-lines))
                     (string-match
                      "func\\s-+\\(test[A-Za-z0-9_]*\\)\\s-*(\\s-*)"
                      line))
            (push (list :class current-class
                        :test (match-string 1 line)
                        :line line-num
                        :type :xctest
                        :display-name nil
                        :suite-name nil)
                  tests)))
        (forward-line 1)))
    (nreverse tests)))

(defun swift-test--detect-package-type (filepath)
  "Detect if FILEPATH belongs to a local SPM package or Xcode target.
Returns a plist with :type ('spm or 'xcode) and :root (package root for SPM)."
  (let ((dir (file-name-directory filepath))
        (project-root (file-truename (swift-test--project-root))))
    (catch 'found
      ;; Search upward for Package.swift
      (while (and dir (not (string= dir "/")))
        (let ((package-file (expand-file-name "Package.swift" dir)))
          (if (file-exists-p package-file)
              ;; Found Package.swift
              (if (string= (file-truename dir) project-root)
                  ;; It's the main project - could be pure SPM or Xcode with Package.swift
                  (throw 'found (list :type 'xcode :root nil))
                ;; It's a local package
                (throw 'found (list :type 'spm :root dir)))
            (setq dir (file-name-directory (directory-file-name dir))))))
      ;; No Package.swift found - assume Xcode
      (list :type 'xcode :root nil))))

(defun swift-test--discover-tests ()
  "Discover all tests in the project and build the test report.
Structure: target -> file -> class/suite -> test"
  ;; Check if project changed and reset if needed
  (swift-test--check-project-changed)
  
  (let ((files (swift-test--find-test-files))
        (targets (make-hash-table :test 'equal))
        (root (swift-test--project-root)))
    
    (dolist (file files)
      ;; Determine target from file path
      ;; Look for directory containing test files - usually named *Tests
      ;; e.g., Bruce/BruceCore/Tests/BruceCoreTests/SomeTests.swift -> BruceCoreTests
      (let* ((relative-path (file-relative-name file root))
             (path-parts (split-string relative-path "/" t))
             ;; Find the directory that contains the test file (parent dir)
             (parent-dir (file-name-nondirectory (directory-file-name (file-name-directory file))))
             ;; Use parent directory if it ends with "Tests", otherwise look for *Tests in path
             (target (or (and (string-match-p "Tests$" parent-dir) parent-dir)
                         (cl-find-if (lambda (p) (string-match-p "Tests$" p)) (reverse path-parts))
                         (car path-parts)
                         "Tests"))
             (filename (file-name-nondirectory file))
             (tests (swift-test--parse-swift-file-for-tests file))
             ;; Detect package type for this file
             (pkg-info (swift-test--detect-package-type file))
             (pkg-type (plist-get pkg-info :type))
             (pkg-root (plist-get pkg-info :root)))
        
        ;; Only process files that have tests
        (when tests
          (let* (;; Get or create target node
                 (target-node (or (gethash target targets)
                                  (let ((node (make-swift-test-node
                                               :id target
                                               :kind 'target
                                               :name target
                                               :status 'not-run
                                               :expanded t
                                               :children (make-hash-table :test 'equal)
                                               :package-type pkg-type
                                               :package-root pkg-root)))
                                    (puthash target node targets)
                                    node)))
                 (target-children (swift-test-node-children target-node))
                 ;; Get or create file node
                 (file-id (format "%s/%s" target filename))
                 (file-node (or (gethash filename target-children)
                                (puthash filename
                                         (make-swift-test-node
                                          :id file-id
                                          :kind 'file
                                          :name filename
                                          :status 'not-run
                                          :filepath file
                                          :expanded nil
                                          :children (make-hash-table :test 'equal)
                                          :parent target-node)
                                         target-children)))
                 (file-children (swift-test-node-children file-node)))
            
            (dolist (test tests)
              (let* ((class-name (plist-get test :class))
                     (test-name (plist-get test :test))
                     (line-num (plist-get test :line))
                     (display-name (plist-get test :display-name))
                     (suite-name (plist-get test :suite-name))
                     (suite-path (plist-get test :suite-path))
                     ;; For Swift Testing, use suite-path for the test ID (for -only-testing)
                     ;; suite-path is like "KYCValidatorTests/NameValidationTests"
                     (class-key (or suite-path class-name))
                     ;; Get or create class/suite node under file
                     (class-id (swift-test--make-test-id target class-key))
                     (class-node (or (gethash class-key file-children)
                                     (puthash class-key
                                              (make-swift-test-node
                                               :id class-id
                                               :kind 'class
                                               :name class-name  ; Display name is still just the class
                                               :status 'not-run
                                               :filepath file
                                               :expanded nil
                                               :children '()
                                               :parent file-node
                                               :display-name suite-name)
                                              file-children)))
                     (test-id (swift-test--make-test-id target class-key test-name))
                     (test-node (make-swift-test-node
                                 :id test-id
                                 :kind 'test
                                 :name test-name
                                 :status 'not-run
                                 :filepath file
                                 :line-number line-num
                                 :parent class-node
                                 :display-name display-name
                                 :func-name test-name)))
                
                ;; Update class display-name if we have a suite-name and it's not set yet
                (when (and suite-name (not (swift-test-node-display-name class-node)))
                  (setf (swift-test-node-display-name class-node) suite-name))
                
                ;; Add test to class
                (setf (swift-test-node-children class-node)
                      (append (swift-test-node-children class-node) (list test-node)))))))))
    
    ;; Convert hash tables to lists (3 levels deep now)
    (setq swift-test--report
          (mapcar (lambda (target-pair)
                    (let ((target-node (cdr target-pair)))
                      ;; Convert file hash to list
                      (setf (swift-test-node-children target-node)
                            (mapcar (lambda (file-pair)
                                      (let ((file-node (cdr file-pair)))
                                        ;; Convert class hash to list
                                        (setf (swift-test-node-children file-node)
                                              (mapcar #'cdr
                                                      (hash-table-to-alist
                                                       (swift-test-node-children file-node))))
                                        file-node))
                                    (hash-table-to-alist
                                     (swift-test-node-children target-node))))
                      target-node))
                  (hash-table-to-alist targets)))
    
    ;; Sort by target name
    (setq swift-test--report
          (sort swift-test--report
                (lambda (a b)
                  (string< (swift-test-node-name a)
                           (swift-test-node-name b)))))
    
    swift-test--report))

(defun hash-table-to-alist (hash)
  "Convert HASH table to alist."
  (let ((result '()))
    (maphash (lambda (k v) (push (cons k v) result)) hash)
    result))

;;; Test Explorer Buffer

(defvar swift-test-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; Navigation - Emacs style (n/p from special-mode, plus explicit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    ;; Toggle/goto
    (define-key map (kbd "RET") #'swift-test-explorer-toggle-or-goto)
    (define-key map (kbd "TAB") #'swift-test-explorer-toggle-expand)
    (define-key map (kbd "<return>") #'swift-test-explorer-toggle-or-goto)
    (define-key map (kbd "<tab>") #'swift-test-explorer-toggle-expand)
    (define-key map (kbd "o") #'swift-test-explorer-goto-test)
    ;; Run tests - C-c prefix and x for quick access
    (define-key map (kbd "C-c C-c") #'swift-test-explorer-run-at-point)
    (define-key map (kbd "x") #'swift-test-explorer-run-at-point)
    (define-key map (kbd "C-c C-a") #'swift-test-explorer-run-all)
    (define-key map (kbd "X") #'swift-test-explorer-run-all)
    (define-key map (kbd "C-c C-r") #'swift-test-explorer-run-failed)
    (define-key map (kbd "r") #'swift-test-explorer-run-failed)
    ;; Expand/collapse
    (define-key map (kbd "E") #'swift-test-explorer-expand-all)
    (define-key map (kbd "C") #'swift-test-explorer-collapse-all)
    ;; Other actions
    (define-key map (kbd "R") #'swift-test-explorer-refresh)
    (define-key map (kbd "g") #'swift-test-explorer-refresh)
    (define-key map (kbd "c") #'swift-test-explorer-clear)
    (define-key map (kbd "S") #'swift-test-set-scheme)
    (define-key map (kbd "[") #'swift-test-explorer-prev-failed)
    (define-key map (kbd "]") #'swift-test-explorer-next-failed)
    (define-key map (kbd "q") #'swift-test-explorer-quit)
    (define-key map (kbd "?") #'swift-test-explorer-help)
    map)
  "Keymap for Test Explorer mode.")

;; Evil-mode integration - setup in mode hook to ensure proper load order
(declare-function evil-define-key* "evil-core")
(declare-function evil-set-initial-state "evil-core")

(defun swift-test-explorer--setup-evil-keys ()
  "Setup evil keybindings for test explorer mode."
  (when (bound-and-true-p evil-mode)
    (evil-define-key* 'motion swift-test-explorer-mode-map
      (kbd "RET") #'swift-test-explorer-toggle-or-goto
      (kbd "TAB") #'swift-test-explorer-toggle-expand
      (kbd "o") #'swift-test-explorer-goto-test
      (kbd "x") #'swift-test-explorer-run-at-point
      (kbd "X") #'swift-test-explorer-run-all
      (kbd "r") #'swift-test-explorer-run-failed
      (kbd "R") #'swift-test-explorer-refresh
      (kbd "gr") #'swift-test-explorer-refresh
      (kbd "c") #'swift-test-explorer-clear
      (kbd "E") #'swift-test-explorer-expand-all
      (kbd "C") #'swift-test-explorer-collapse-all
      (kbd "S") #'swift-test-set-scheme
      (kbd "[") #'swift-test-explorer-prev-failed
      (kbd "]") #'swift-test-explorer-next-failed
      (kbd "q") #'swift-test-explorer-quit
      (kbd "?") #'swift-test-explorer-help)
    (evil-set-initial-state 'swift-test-explorer-mode 'motion)))

(add-hook 'swift-test-explorer-mode-hook #'swift-test-explorer--setup-evil-keys)

(define-derived-mode swift-test-explorer-mode special-mode "TestExplorer"
  "Major mode for Swift Test Explorer.

\\{swift-test-explorer-mode-map}"
  :group 'swift-test-explorer
  :keymap swift-test-explorer-mode-map
  (setq-local line-move-visual t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local wrap-prefix "        ")  ; Indent wrapped lines for readability
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function #'swift-test-explorer-refresh)
  (visual-line-mode 1))

(defun swift-test--get-status-icon (status)
  "Get the icon for STATUS."
  (pcase status
    ('passed (swift-test-icon-passed))
    ('failed (swift-test-icon-failed))
    ('running (if swift-test-animate-running
                  (nth swift-test--animation-frame swift-test--spinner-frames)
                (swift-test-icon-running)))
    ('skipped (swift-test-icon-skipped))
    (_ (swift-test-icon-not-run))))

(defun swift-test--get-status-face (status)
  "Get the face for STATUS."
  (pcase status
    ('passed 'swift-test-explorer-passed-face)
    ('failed 'swift-test-explorer-failed-face)
    ('running 'swift-test-explorer-running-face)
    ('skipped 'swift-test-explorer-skipped-face)
    (_ 'swift-test-explorer-not-run-face)))

(defun swift-test--count-tests (node)
  "Count total and passed tests under NODE recursively.
Returns (total . passed)."
  (let ((total 0)
        (passed 0))
    (if (eq (swift-test-node-kind node) 'test)
        ;; It's a test node
        (progn
          (setq total 1)
          (when (eq (swift-test-node-status node) 'passed)
            (setq passed 1)))
      ;; It's a container - count children
      (dolist (child (swift-test-node-children node))
        (let ((counts (swift-test--count-tests child)))
          (setq total (+ total (car counts)))
          (setq passed (+ passed (cdr counts))))))
    (cons total passed)))

(defun swift-test--has-running-tests (node)
  "Check if NODE or any of its children have running tests."
  (if (eq (swift-test-node-kind node) 'test)
      (eq (swift-test-node-status node) 'running)
    ;; Container - check children
    (cl-some #'swift-test--has-running-tests
             (swift-test-node-children node))))

(defun swift-test--render-node (node depth)
  "Render NODE at DEPTH level. Returns list of formatted lines."
  (let* ((kind (swift-test-node-kind node))
         (name (swift-test-node-name node))
         (display-name (swift-test-node-display-name node))
         (status (swift-test-node-status node))
         (expanded (swift-test-node-expanded node))
         (children (swift-test-node-children node))
         (duration (swift-test-node-duration node))
         (error-message (swift-test-node-message node))
         (indent (make-string (* depth 2) ?\s))
         (status-icon (swift-test--get-status-icon status))
         (status-face (swift-test--get-status-face status))
         (lines '())
         ;; Use display-name if available, otherwise fall back to name
         ;; For files, strip .swift extension
         (shown-name (let ((base-name (or display-name name)))
                       (if (eq kind 'file)
                           (replace-regexp-in-string "\\.swift$" "" base-name)
                         base-name))))
    
    ;; Render this node
    (let* ((expand-icon (if (and children (not (eq kind 'test)))
                            (if expanded (swift-test-icon-expanded) (swift-test-icon-collapsed))
                          " "))
           ;; Package type icon for targets
           (package-type (swift-test-node-package-type node))
           (package-icon (when (eq kind 'target)
                           (pcase package-type
                             ('spm (swift-test-icon-spm))
                             ('xcode (swift-test-icon-xcode))
                             (_ nil))))
           ;; For targets, use package-type icon instead of generic target icon
           (kind-icon (pcase kind
                        ('target nil)  ; Will use package-icon instead
                        ('file (swift-test-icon-file))
                        ('class (swift-test-icon-class))
                        (_ "")))
           ;; Face for name based on kind (not status for tests)
           (name-face (pcase kind
                        ('target 'swift-test-explorer-target-face)
                        ('file 'swift-test-explorer-file-face)
                        ('class 'swift-test-explorer-class-face)
                        (_ 'swift-test-explorer-test-face)))
           ;; Count tests for containers
           (test-counts (unless (eq kind 'test)
                          (swift-test--count-tests node)))
           (total-tests (car test-counts))
           (passed-tests (cdr test-counts))
           ;; Build the line differently for tests vs containers
           (line (if (eq kind 'test)
                     ;; Test: [indent] [status-icon] name [duration]
                     (concat indent
                             "  "  ; space for alignment with expand icons
                             (propertize status-icon 'face status-face)
                             " "
                             (propertize shown-name 'face name-face)
                             (when duration
                               (propertize (format " (%.3fs)" duration) 'face 'shadow)))
                   ;; Target/File/Class: [indent] [expand-icon] [kind-icon] name [count] [status-icon]
                   (let* ((all-passed (and (> total-tests 0) (= passed-tests total-tests)))
                          (any-run (> passed-tests 0))
                          (count-str (format "(%d/%d)" passed-tests total-tests))
                          (count-face (cond
                                       (all-passed 'swift-test-explorer-passed-face)
                                       (any-run 'swift-test-explorer-failed-face)
                                       (t 'shadow)))
                          (result-icon (cond
                                        (all-passed (swift-test-icon-passed))
                                        (any-run (swift-test-icon-failed))
                                        (t nil))))
                     (let ((is-running (swift-test--has-running-tests node))
                           (spinner (nth swift-test--animation-frame swift-test--spinner-frames)))
                       (concat indent
                               (propertize expand-icon 'face 'shadow)
                               " "
                               ;; Show package type icon for targets, or kind-icon for other nodes
                               (when package-icon
                                 (concat package-icon " "))
                               (when (and kind-icon (not (string-empty-p kind-icon)))
                                 (concat kind-icon " "))  ; icon already has face from swift-test-icon-*
                               (propertize shown-name 'face name-face)
                               " "
                               (propertize count-str 'face count-face)
                                (cond
                                 (is-running
                                  (concat " " (propertize spinner 'face 'swift-test-explorer-running-face)))
                                 (result-icon
                                  (concat " " (propertize result-icon 'face count-face))))))))))
      
      (push (cons line node) lines)
      
      ;; Render error messages if test failed and has messages
      ;; error-message can be a string (legacy) or a list of strings
      ;; Multi-line messages are formatted into a single readable line
      ;; Visual-line-mode handles wrapping long messages nicely
      (when (and (eq kind 'test)
                 (eq status 'failed)
                 error-message)
        (let ((error-indent (make-string (+ (* depth 2) 4) ?\s))
              (messages (if (listp error-message) error-message (list error-message))))
          ;; Render each error on its own line
          (dolist (msg messages)
            ;; Format the message (handles multi-line, cleans up)
            (let ((formatted (swift-test--format-error-message msg)))
              (push (cons (concat error-indent
                                  (propertize "↳ " 'face 'swift-test-explorer-failed-face)
                                  (propertize formatted 'face 'swift-test-explorer-failed-face))
                          nil)
                    lines)))))
      
      ;; Reverse to get correct order (test line first, then error message)
      (setq lines (nreverse lines)))
    
    ;; Render children if expanded
    (when (and expanded children)
      (dolist (child children)
        (setq lines (append lines (swift-test--render-node child (1+ depth))))))
    
    lines))

(defun swift-test--render-buffer ()
  "Render the test explorer buffer."
  (let ((buf (get-buffer swift-test--buffer-name)))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pos (point))
              (win (get-buffer-window buf)))
          (erase-buffer)
          (setq swift-test--line-to-node (make-hash-table))
          
          (if (null swift-test--report)
              (progn
                (insert "\n")
                (insert (propertize "  No tests found.\n\n" 'face 'shadow))
                (insert (propertize "  Press " 'face 'shadow))
                (insert (propertize "R" 'face 'bold))
                (insert (propertize " to discover tests.\n" 'face 'shadow))
                (insert (propertize "  Press " 'face 'shadow))
                (insert (propertize "?" 'face 'bold))
                (insert (propertize " for help.\n" 'face 'shadow)))
            
            ;; Render all targets
            (let ((line-num 1))
              (dolist (target swift-test--report)
                (dolist (rendered (swift-test--render-node target 0))
                  (insert (car rendered))
                  (insert "\n")
                  (puthash line-num (cdr rendered) swift-test--line-to-node)
                  (cl-incf line-num)))))
          
          (goto-char (min pos (point-max)))
          ;; Force redisplay
          (when win
            (force-window-update win)))))))

(defun swift-test--walk-nodes (fn &optional nodes)
  "Walk all nodes calling FN on each. Start from NODES or root."
  (dolist (node (or nodes swift-test--report))
    (funcall fn node)
    (when-let* ((children (swift-test-node-children node)))
      (swift-test--walk-nodes fn children))))

;;; Interactive Commands

;;;###autoload
(defun swift-test-explorer-show ()
  "Show the test explorer window."
  (interactive)
  (when swift-test-auto-discover
    (swift-test--discover-tests))
  
  (let* ((buf (get-buffer-create swift-test--buffer-name))
         (win (get-buffer-window buf)))
    (unless win
      (let ((side (if (eq swift-test-explorer-window-position 'left) 'left 'right)))
        (setq win (display-buffer-in-side-window
                   buf
                   `((side . ,side)
                     (window-width . ,swift-test-explorer-window-width)
                     (slot . 0))))))
    (with-current-buffer buf
      (swift-test-explorer-mode)
      (swift-test--render-buffer))
    (select-window win)))

;;;###autoload
(defun swift-test-explorer-toggle ()
  "Toggle the test explorer window."
  (interactive)
  (if-let* ((buf (get-buffer swift-test--buffer-name))
            (win (get-buffer-window buf)))
      (delete-window win)
    (swift-test-explorer-show)))

(defun swift-test-explorer-quit ()
  "Close the test explorer window."
  (interactive)
  (when-let* ((win (get-buffer-window swift-test--buffer-name)))
    (delete-window win)))

(defun swift-test-explorer-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the test explorer."
  (interactive)
  (swift-test--discover-tests)
  (swift-test--render-buffer)
  (swift-test--notify "Test explorer refreshed"))

(defun swift-test-explorer-clear ()
  "Clear all test results (keeps test structure)."
  (interactive)
  (swift-test--walk-nodes
   (lambda (node)
     (setf (swift-test-node-status node) 'not-run)
     (setf (swift-test-node-message node) nil)
     (setf (swift-test-node-duration node) nil)))
  (setq swift-test--failed-tests nil)
  (setq swift-test--last-run-tests nil)
  (swift-test--render-buffer))

(defun swift-test-explorer-reset ()
  "Reset test explorer completely (clears everything and rediscovers)."
  (interactive)
  (swift-test--reset-state)
  (setq swift-test--current-project-root nil)
  (swift-test--discover-tests)
  (swift-test--render-buffer)
  (swift-test--notify "Test explorer reset"))

(defun swift-test-explorer-toggle-or-goto ()
  "Toggle node expansion or goto source at point.
For targets: toggle expansion.
For files/classes/tests: jump to source location."
  (interactive)
  (let* ((line (line-number-at-pos))
         (node (gethash line swift-test--line-to-node)))
    (if node
        (pcase (swift-test-node-kind node)
          ('target
           ;; Toggle expansion for targets
           (setf (swift-test-node-expanded node)
                 (not (swift-test-node-expanded node)))
           (swift-test--render-buffer))
          (_
           ;; For file/class/test - jump to source
           (swift-test-explorer-goto-test)))
      ;; No node - might be an error message line, do nothing
      nil)))

(defun swift-test-explorer-toggle-expand ()
  "Toggle expansion of node at point."
  (interactive)
  (when-let* ((node (gethash (line-number-at-pos) swift-test--line-to-node)))
    (when (swift-test-node-children node)
      (setf (swift-test-node-expanded node)
            (not (swift-test-node-expanded node)))
      (swift-test--render-buffer))))

(defun swift-test-explorer-expand-all ()
  "Expand all nodes in the test explorer."
  (interactive)
  (swift-test--walk-nodes
   (lambda (node)
     (when (swift-test-node-children node)
       (setf (swift-test-node-expanded node) t))))
  (swift-test--render-buffer))

(defun swift-test-explorer-collapse-all ()
  "Collapse all nodes in the test explorer."
  (interactive)
  (swift-test--walk-nodes
   (lambda (node)
     (when (swift-test-node-children node)
       (setf (swift-test-node-expanded node) nil))))
  (swift-test--render-buffer))

(defun swift-test-explorer-goto-test ()
  "Jump to the test source at point."
  (interactive)
  (when-let* ((node (gethash (line-number-at-pos) swift-test--line-to-node))
              (filepath (swift-test-node-filepath node)))
    (let ((line (swift-test-node-line-number node)))
      (other-window 1)
      (find-file filepath)
      (when line
        (goto-char (point-min))
        (forward-line (1- line))
        (recenter)))))

(defun swift-test-explorer-run-at-point ()
  "Run the test(s) at point."
  (interactive)
  (when-let* ((node (gethash (line-number-at-pos) swift-test--line-to-node)))
    (swift-test--run-node node)))

(defun swift-test-explorer-run-all ()
  "Run all tests using a scheme that includes all test targets.
Prompts for scheme selection - choose your main app scheme or a test plan scheme."
  (interactive)
  (let* ((root (swift-test--project-root))
         (cached-all-scheme (gethash "all-tests" swift-test--scheme-cache))
         (scheme (or cached-all-scheme
                     (let* ((schemes (swift-test--available-schemes))
                            (selected (completing-read "Select scheme for ALL tests: " schemes nil t)))
                       (puthash "all-tests" selected swift-test--scheme-cache)
                       selected))))
    (swift-test--run-all-with-scheme scheme)))

(defun swift-test--run-all-with-scheme (scheme)
  "Run all tests using SCHEME without any -only-testing filters."
  ;; Auto-show test explorer
  (unless (get-buffer-window swift-test--buffer-name)
    (swift-test-explorer-show))
  
  (setq swift-test--last-run-tests nil)
  (setq swift-test--failed-tests nil)
  ;; Mark all tests as running
  (swift-test--walk-nodes
   (lambda (node)
     (when (eq (swift-test-node-kind node) 'test)
       (setf (swift-test-node-status node) 'running))))
  (swift-test--render-buffer)
  (swift-test--start-animation)
  
  (let* ((root (swift-test--project-root))
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (project-file (car (directory-files root t "\\.xcodeproj$")))
         (project-arg (cond
                       (workspace-file (format "-workspace %s" (shell-quote-argument workspace-file)))
                       (project-file (format "-project %s" (shell-quote-argument project-file)))
                       (t "")))
         (destination (swift-test--get-destination))
         (command (mapconcat
                   #'identity
                   (list
                    "xcodebuild test"
                    (format "-scheme %s" (shell-quote-argument scheme))
                    project-arg
                    (format "-destination '%s'" destination)
                    "-disable-concurrent-destination-testing"
                    "-test-timeouts-enabled NO"
                    "-parallel-testing-enabled NO")
                   " ")))
    
    (swift-test--notify (format "Running all tests with scheme %s..." scheme))
    
    (let* ((output-buffer (generate-new-buffer " *swift-test-output*"))
           (process (start-process-shell-command
                     "swift-test"
                     output-buffer
                     command)))
      
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (swift-test--stop-animation)
           (let* ((output (with-current-buffer (process-buffer proc)
                            (buffer-string)))
                  (has-build-errors (swift-test--has-build-errors-p output)))
              ;; If there are build errors, send to error proxy for display
              (when has-build-errors
                (when (fboundp 'swift-error-proxy-parse-output)
                  (swift-error-proxy-parse-output output)))
              ;; Always try to parse test results
              (swift-test--parse-results output)
              (swift-test--render-buffer)
              (kill-buffer (process-buffer proc))
              ;; Notify based on results
              (cond
               (has-build-errors
                (swift-test--notify "Build failed - check error buffer for details"))
               ((> (length swift-test--failed-tests) 0)
                (swift-test--notify (format "Tests completed: %d failed"
                                            (length swift-test--failed-tests))))
               (t
                (swift-test--notify "All tests passed!"))))))))))

(defun swift-test-explorer-run-failed ()
  "Re-run failed tests."
  (interactive)
  (if swift-test--failed-tests
      (swift-test--run-tests swift-test--failed-tests)
    (swift-test--notify "No failed tests to re-run")))

(defun swift-test-explorer-prev-failed ()
  "Jump to previous failed test."
  (interactive)
  (swift-test--jump-to-failed -1))

(defun swift-test-explorer-next-failed ()
  "Jump to next failed test."
  (interactive)
  (swift-test--jump-to-failed 1))

(defun swift-test--jump-to-failed (direction)
  "Jump to failed test in DIRECTION (1 or -1)."
  (let ((current-line (line-number-at-pos))
        (found nil))
    (maphash (lambda (line node)
               (when (and (eq (swift-test-node-status node) 'failed)
                          (if (> direction 0)
                              (> line current-line)
                            (< line current-line)))
                 (when (or (null found)
                           (if (> direction 0)
                               (< line found)
                             (> line found)))
                   (setq found line))))
             swift-test--line-to-node)
    (if found
        (goto-char (point-min))
        (forward-line (1- found))
      (swift-test--notify "No more failed tests"))))

(defun swift-test-explorer-help ()
  "Show help for test explorer."
  (interactive)
  (message "Test Explorer: RET=toggle/run  o=goto  x=run  X=all  r=failed  R=refresh  S=scheme  c=clear  [/]=prev/next  q=quit"))

(defun swift-test-explorer-show-last-output ()
  "Show the last test output in a buffer for debugging."
  (interactive)
  (if swift-test--last-output
      (with-current-buffer (get-buffer-create "*swift-test-output*")
        (erase-buffer)
        (insert swift-test--last-output)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer)))
    (message "No test output recorded yet")))

(defun swift-test-explorer-debug-errors ()
  "Debug: show all failed tests and their error messages."
  (interactive)
  (let ((failures '()))
    (swift-test--walk-nodes
     (lambda (node)
       (when (and (eq (swift-test-node-kind node) 'test)
                  (eq (swift-test-node-status node) 'failed))
         (push (list :name (or (swift-test-node-display-name node)
                               (swift-test-node-name node))
                     :message (swift-test-node-message node))
               failures))))
    (if failures
        (with-current-buffer (get-buffer-create "*swift-test-debug*")
          (erase-buffer)
          (insert "Failed tests:\n\n")
          (dolist (f (nreverse failures))
            (insert (format "Test: %s\n" (plist-get f :name)))
            (insert (format "  Message: %s\n\n" (or (plist-get f :message) "(none)"))))
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))
      (message "No failed tests found"))))

;;; Test Running

(defun swift-test--collect-test-ids (node)
  "Collect all test IDs from NODE and its children."
  (let ((ids '()))
    (if (eq (swift-test-node-kind node) 'test)
        (push (swift-test-node-id node) ids)
      (dolist (child (swift-test-node-children node))
        (setq ids (append ids (swift-test--collect-test-ids child)))))
    ids))

(defun swift-test--run-node (node)
  "Run tests in NODE."
  (let ((ids (swift-test--collect-test-ids node)))
    (swift-test--run-tests ids)))

(defun swift-test--has-build-errors-p (output)
  "Check if OUTPUT contains build errors (not test failures).
Returns non-nil if there are compilation or build configuration errors.
Delegates to `swift-error-proxy-has-errors-p' for consolidated pattern matching."
  (if (fboundp 'swift-error-proxy-has-errors-p)
      (swift-error-proxy-has-errors-p output)
    ;; Fallback if proxy not loaded
    (and output
         (string-match-p "\\(?:error:\\|BUILD FAILED\\)" output))))

(defun swift-test--mark-tests-running (test-ids)
  "Mark TEST-IDS as running and clear previous error messages."
  (swift-test--walk-nodes
   (lambda (node)
     (when (and (eq (swift-test-node-kind node) 'test)
                (or (null test-ids)
                    (member (swift-test-node-id node) test-ids)))
       (setf (swift-test-node-status node) 'running)
       (setf (swift-test-node-message node) nil))))  ; Clear old error
  (setq swift-test--running-tests test-ids)
  (swift-test--render-buffer)
  (swift-test--start-animation))

(defun swift-test--start-animation ()
  "Start the running test animation."
  (when (and swift-test-animate-running (null swift-test--animation-timer))
    (setq swift-test--animation-timer
          (run-with-timer 0.1 0.1
                          (lambda ()
                            (setq swift-test--animation-frame
                                  (mod (1+ swift-test--animation-frame)
                                       (length swift-test--spinner-frames)))
                            (when (get-buffer swift-test--buffer-name)
                              (swift-test--render-buffer)))))))

(defun swift-test--stop-animation ()
  "Stop the running test animation."
  (when swift-test--animation-timer
    (cancel-timer swift-test--animation-timer)
    (setq swift-test--animation-timer nil)))

(defun swift-test--find-package-root-for-test (test-id)
  "Find the Package.swift directory for a test ID if it's in a local package.
Returns the package root directory or nil if not a local package."
  (when test-id
    (let* ((parts (swift-test--parse-test-id test-id))
           (target-name (car parts))
           (result nil))
      ;; Find the target node and check its package-root
      (swift-test--walk-nodes
       (lambda (node)
         (when (and (eq (swift-test-node-kind node) 'target)
                    (string= (swift-test-node-name node) target-name))
           (setq result (swift-test-node-package-root node)))))
      result)))

(defun swift-test--run-tests (test-ids)
  "Run tests with TEST-IDS (nil means all tests)."
  ;; Auto-show test explorer when running tests
  (unless (get-buffer-window swift-test--buffer-name)
    (swift-test-explorer-show))
  
  (setq swift-test--last-run-tests test-ids)
  (setq swift-test--failed-tests nil)
  (swift-test--mark-tests-running test-ids)
  
  (let* ((root (swift-test--project-root))
         (is-spm (file-exists-p (expand-file-name "Package.swift" root)))
         ;; Check if this is a local package test
         (local-package-root (and test-ids
                                  (not is-spm)
                                  (swift-test--find-package-root-for-test (car test-ids))))
         (command (cond
                   ;; Local SPM package inside Xcode project
                   (local-package-root
                    (swift-test--build-local-package-command test-ids local-package-root))
                   ;; Pure SPM project (Package.swift at root)
                   (is-spm
                    (swift-test--build-spm-command test-ids))
                   ;; Xcode project
                   (t
                    (swift-test--build-xcode-command test-ids)))))
    
    (swift-test--notify (format "Running %s..."
                                (if test-ids
                                    (format "%d test(s)" (length test-ids))
                                  "all tests")))
    
    (let* ((output-buffer (generate-new-buffer " *swift-test-output*"))
           (process (start-process-shell-command
                     "swift-test"
                     output-buffer
                     command)))
      
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (swift-test--stop-animation)
           (let* ((output (with-current-buffer (process-buffer proc)
                            (buffer-string)))
                  (has-build-errors (swift-test--has-build-errors-p output)))
              ;; If there are build errors, send to error proxy for display
              (when has-build-errors
                (when (fboundp 'swift-error-proxy-parse-output)
                  (swift-error-proxy-parse-output output)))
              ;; Always try to parse test results
              (swift-test--parse-results output)
              (swift-test--render-buffer)
              (kill-buffer (process-buffer proc))
              ;; Notify based on results
              (cond
               (has-build-errors
                (swift-test--notify "Build failed - check error buffer for details"))
               ((> (length swift-test--failed-tests) 0)
                (swift-test--notify (format "Tests completed: %d failed"
                                            (length swift-test--failed-tests))))
               (t
                (swift-test--notify "All tests passed!"))))))))))


(defun swift-test--build-spm-command (test-ids)
  "Build swift test command for TEST-IDS."
  (if test-ids
      (format "swift test --filter '%s'"
              (mapconcat (lambda (id)
                           (let ((parts (swift-test--parse-test-id id)))
                             (if (= (length parts) 3)
                                 (format "%s.%s" (nth 1 parts) (nth 2 parts))
                               (nth 1 parts))))
                         test-ids
                         "|"))
    "swift test"))

(defvar swift-test--package-scheme-cache (make-hash-table :test 'equal)
  "Cache mapping package names to their preferred test scheme.")

(defun swift-test--get-scheme-for-package (package-name test-target)
  "Get the scheme to use for testing PACKAGE-NAME with TEST-TARGET.
Priority:
1. User-selected test scheme from transient menu (swift-test--scheme-cache)
2. Package-specific cached scheme (swift-test--package-scheme-cache)
3. Package's own scheme if it supports testing
4. Prompt user to select"
  (let* ((root (swift-test--project-root))
         ;; First check if user has selected a global test scheme
         (global-scheme (gethash root swift-test--scheme-cache))
         (package-cache-key (format "%s::%s" root package-name))
         (package-cached-scheme (gethash package-cache-key swift-test--package-scheme-cache)))
    (cond
     ;; User has selected a global test scheme - use that
     (global-scheme
      global-scheme)
     ;; Package-specific scheme is cached
     (package-cached-scheme
      package-cached-scheme)
     ;; Try to find a working scheme automatically
     (t
      (let* ((available-schemes (swift-test--available-schemes))
             (test-scheme-name (format "%sTests" package-name))
             (package-scheme-name package-name)
             (package-scheme-works (and (member package-scheme-name available-schemes)
                                        (swift-test--scheme-supports-testing-p package-scheme-name)))
             (test-scheme-works (and (member test-scheme-name available-schemes)
                                     (swift-test--scheme-supports-testing-p test-scheme-name))))
        (cond
         ;; Package's test scheme works
         (test-scheme-works
          (puthash package-cache-key test-scheme-name swift-test--package-scheme-cache)
          test-scheme-name)
         ;; Package's own scheme works
         (package-scheme-works
          (puthash package-cache-key package-scheme-name swift-test--package-scheme-cache)
          package-scheme-name)
         ;; Neither works - ask user which scheme to use
         (t
          (let ((selected (completing-read
                           (format "Select scheme for %s tests (package scheme not configured): "
                                   package-name)
                           available-schemes nil t)))
            (puthash package-cache-key selected swift-test--package-scheme-cache)
            selected))))))))

(defun swift-test--build-local-package-command (test-ids package-root)
  "Build xcodebuild test command for TEST-IDS in local PACKAGE-ROOT.
Uses xcodebuild from the main project. Prompts for scheme if package's
own scheme is not configured for testing (common with Swift Testing).

Note: For Swift Testing (@Test/@Suite), -only-testing may not work properly
as it uses display names. In that case, run entire test target without filter."
  (let* ((root (swift-test--project-root))
         (destination (swift-test--get-destination))
         ;; Package name is the directory name (e.g., "BruceStyle")
         (package-name (file-name-nondirectory (directory-file-name package-root)))
         ;; Test target name
         (test-target (format "%sTests" package-name))
         ;; Get the scheme to use (cached or user-selected)
         (scheme (swift-test--get-scheme-for-package package-name test-target))
         ;; Find workspace or project file
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (project-file (car (directory-files root t "\\.xcodeproj$")))
         (project-arg (cond
                       (workspace-file (format "-workspace %s" (shell-quote-argument workspace-file)))
                       (project-file (format "-project %s" (shell-quote-argument project-file)))
                       (t "")))
         ;; For Swift Testing, -only-testing doesn't work well with display names
         ;; Check if tests are Swift Testing by looking at the first test node
         (is-swift-testing (swift-test--tests-are-swift-testing-p test-ids))
         ;; Only add filter for XCTest, not Swift Testing
         (filter (when (and test-ids (not is-swift-testing))
                   (mapconcat #'swift-test--format-only-testing-arg
                              test-ids
                              " ")))
         ;; For Swift Testing, filter by test target instead
         ;; Use -only-testing to include only this target (excludes others like BruceTests)
         (target-filter (format "-only-testing:%s" test-target)))
    ;; Log which scheme is being used
    (message "Running tests with scheme: %s (package: %s, target: %s, swift-testing: %s)"
             scheme package-name test-target is-swift-testing)
    ;; Run from project root using the selected scheme
    ;; Always use -only-testing to limit to this package's test target
    (format "xcodebuild test -scheme %s %s -destination '%s' -disable-concurrent-destination-testing -test-timeouts-enabled NO -parallel-testing-enabled NO %s%s"
            (shell-quote-argument scheme)
            project-arg
            destination
            (shell-quote-argument target-filter)
            (if filter (concat " " filter) ""))))

(defun swift-test--tests-are-swift-testing-p (test-ids)
  "Check if TEST-IDS contain Swift Testing tests (vs XCTest).
Returns t if any test uses @Test/@Suite, nil for XCTest."
  (when test-ids
    (let ((first-id (car test-ids))
          (result nil))
      ;; Find the test node and check its type
      (swift-test--walk-nodes
       (lambda (node)
         (when (and (eq (swift-test-node-kind node) 'test)
                    (string= (swift-test-node-id node) first-id))
           ;; Swift Testing tests have display-name from @Test("...")
           ;; or their parent has display-name from @Suite("...")
           (when (or (swift-test-node-display-name node)
                     (let ((parent (swift-test-node-parent node)))
                       (and parent (swift-test-node-display-name parent))))
             (setq result t)))))
      result)))

(defun swift-test-clear-scheme-cache ()
  "Clear the cached scheme selections for packages.
Use this if you want to re-select schemes for package tests."
  (interactive)
  (clrhash swift-test--package-scheme-cache)
  (swift-test--notify "Package scheme cache cleared"))

;;; Scheme Configuration for Testing

(defun swift-test--scheme-supports-testing-p (scheme)
  "Check if SCHEME is configured for test action by examining the scheme file."
  (let ((scheme-file (swift-test--find-scheme-file scheme)))
    (when scheme-file
      (with-temp-buffer
        (insert-file-contents scheme-file)
        ;; Check if TestAction has any Testables configured
        (and (re-search-forward "<TestAction" nil t)
             (re-search-forward "<Testables>" nil t)
             (re-search-forward "<TestableReference" nil t))))))

(defun swift-test--find-scheme-file (scheme)
  "Find the .xcscheme file for SCHEME in the project."
  (let* ((root (swift-test--project-root))
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (project-file (car (directory-files root t "\\.xcodeproj$")))
         (search-paths (list
                        ;; Shared schemes in workspace
                        (when workspace-file
                          (expand-file-name "xcshareddata/xcschemes" workspace-file))
                        ;; Shared schemes in project
                        (when project-file
                          (expand-file-name "xcshareddata/xcschemes" project-file))
                        ;; User schemes
                        (when project-file
                          (expand-file-name (format "xcuserdata/%s.xcuserdatad/xcschemes"
                                                    (user-login-name))
                                            project-file)))))
    (cl-some (lambda (dir)
               (when (and dir (file-directory-p dir))
                 (let ((scheme-file (expand-file-name (format "%s.xcscheme" scheme) dir)))
                   (when (file-exists-p scheme-file)
                     scheme-file))))
             search-paths)))

(defun swift-test--create-scheme-with-tests (scheme test-target package-root)
  "Create a new SCHEME file configured for TEST-TARGET in PACKAGE-ROOT.
Returns t on success, nil on failure."
  (let* ((root (swift-test--project-root))
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (schemes-dir (when workspace-file
                        (expand-file-name "xcshareddata/xcschemes" workspace-file)))
         (scheme-file (when schemes-dir
                        (expand-file-name (format "%s.xcscheme" scheme) schemes-dir)))
         ;; Container reference for the package
         (package-name (file-name-nondirectory (directory-file-name package-root)))
         (relative-package-path (file-relative-name package-root root)))
    (unless schemes-dir
      (swift-test--notify "Could not find workspace")
      (cl-return-from swift-test--create-scheme-with-tests nil))
    ;; Create xcschemes directory if it doesn't exist
    (unless (file-directory-p schemes-dir)
      (make-directory schemes-dir t))
    ;; Write the scheme file
    (with-temp-file scheme-file
      (insert (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<Scheme
   LastUpgradeVersion = \"1500\"
   version = \"1.7\">
   <BuildAction
      parallelizeBuildables = \"YES\"
      buildImplicitDependencies = \"YES\">
      <BuildActionEntries>
         <BuildActionEntry
            buildForTesting = \"YES\"
            buildForRunning = \"YES\"
            buildForProfiling = \"YES\"
            buildForArchiving = \"YES\"
            buildForAnalyzing = \"YES\">
            <BuildableReference
               BuildableIdentifier = \"primary\"
               BlueprintIdentifier = \"%s\"
               BuildableName = \"%s\"
               BlueprintName = \"%s\"
               ReferencedContainer = \"container:%s\">
            </BuildableReference>
         </BuildActionEntry>
      </BuildActionEntries>
   </BuildAction>
   <TestAction
      buildConfiguration = \"Debug\"
      selectedDebuggerIdentifier = \"Xcode.DebuggerFoundation.Debugger.LLDB\"
      selectedLauncherIdentifier = \"Xcode.DebuggerFoundation.Launcher.LLDB\"
      shouldUseLaunchSchemeArgsEnv = \"YES\"
      shouldAutocreateTestPlan = \"YES\">
      <Testables>
         <TestableReference
            skipped = \"NO\">
            <BuildableReference
               BuildableIdentifier = \"primary\"
               BlueprintIdentifier = \"%s\"
               BuildableName = \"%s.xctest\"
               BlueprintName = \"%s\"
               ReferencedContainer = \"container:%s\">
            </BuildableReference>
         </TestableReference>
      </Testables>
   </TestAction>
   <LaunchAction
      buildConfiguration = \"Debug\"
      selectedDebuggerIdentifier = \"Xcode.DebuggerFoundation.Debugger.LLDB\"
      selectedLauncherIdentifier = \"Xcode.DebuggerFoundation.Launcher.LLDB\"
      launchStyle = \"0\"
      useCustomWorkingDirectory = \"NO\"
      ignoresPersistentStateOnLaunch = \"NO\"
      debugDocumentVersioning = \"YES\"
      debugServiceExtension = \"internal\"
      allowLocationSimulation = \"YES\">
   </LaunchAction>
   <ProfileAction
      buildConfiguration = \"Release\"
      shouldUseLaunchSchemeArgsEnv = \"YES\"
      savedToolIdentifier = \"\"
      useCustomWorkingDirectory = \"NO\"
      debugDocumentVersioning = \"YES\">
   </ProfileAction>
   <AnalyzeAction
      buildConfiguration = \"Debug\">
   </AnalyzeAction>
   <ArchiveAction
      buildConfiguration = \"Release\"
      revealArchiveInOrganizer = \"YES\">
   </ArchiveAction>
</Scheme>
"
                      ;; Build target (the library)
                      package-name
                      package-name
                      package-name
                      relative-package-path
                      ;; Test target
                      test-target
                      test-target
                      test-target
                      relative-package-path)))
    (swift-test--notify (format "Created scheme %s with test target %s" scheme test-target))
    t))

(defun swift-test--add-test-target-to-scheme (scheme test-target)
  "Add TEST-TARGET to SCHEME's test action.
Returns t on success, nil on failure."
  (let ((scheme-file (swift-test--find-scheme-file scheme)))
    (if scheme-file
        ;; Existing scheme file - modify it
        (with-temp-buffer
          (insert-file-contents scheme-file)
          (goto-char (point-min))
          ;; Find the TestAction section
          (if (not (re-search-forward "<TestAction" nil t))
              (progn
                (swift-test--notify "Could not find TestAction in scheme")
                nil)
            ;; Check if Testables section exists
            (if (re-search-forward "<Testables>" nil t)
                ;; Add test target to existing Testables
                (progn
                  (insert (format "
         <TestableReference
            skipped = \"NO\">
            <BuildableReference
               BuildableIdentifier = \"primary\"
               BlueprintIdentifier = \"%s\"
               BuildableName = \"%s.xctest\"
               BlueprintName = \"%s\"
               ReferencedContainer = \"container:\">
            </BuildableReference>
         </TestableReference>"
                                  test-target
                                  test-target
                                  test-target))
                  (write-region (point-min) (point-max) scheme-file)
                  (swift-test--notify (format "Added %s to %s scheme" test-target scheme))
                  t)
              ;; Need to create Testables section
              (goto-char (point-min))
              (when (re-search-forward "<TestAction[^>]*>" nil t)
                (insert (format "
      <Testables>
         <TestableReference
            skipped = \"NO\">
            <BuildableReference
               BuildableIdentifier = \"primary\"
               BlueprintIdentifier = \"%s\"
               BuildableName = \"%s.xctest\"
               BlueprintName = \"%s\"
               ReferencedContainer = \"container:\">
            </BuildableReference>
         </TestableReference>
      </Testables>"
                                test-target
                                test-target
                                test-target))
                (write-region (point-min) (point-max) scheme-file)
                (swift-test--notify (format "Added %s to %s scheme" test-target scheme))
                t))))
      ;; No scheme file exists - need to tell user to create it
      nil)))

(defun swift-test--ensure-scheme-configured-for-test (scheme test-target &optional package-root)
  "Ensure SCHEME is configured to run TEST-TARGET.
PACKAGE-ROOT is the path to the local package if applicable.
Prompts user if configuration is needed."
  (if (swift-test--scheme-supports-testing-p scheme)
      t  ; Already configured
    ;; Ask user if they want to configure
    (if (y-or-n-p (format "Scheme '%s' is not configured for testing. Configure it now? " scheme))
        (let ((scheme-file (swift-test--find-scheme-file scheme)))
          (if scheme-file
              ;; Modify existing scheme
              (swift-test--add-test-target-to-scheme scheme test-target)
            ;; No scheme file - create new one
            (if package-root
                (swift-test--create-scheme-with-tests scheme test-target package-root)
              (progn
                (swift-test--notify "Cannot create scheme - no package root provided")
                nil))))
      (progn
        (swift-test--notify "Test cancelled - scheme not configured")
        nil))))

;; Forward declaration for ios-simulator
(declare-function ios-simulator-simulator-identifier "ios-simulator")

(defun swift-test--get-destination ()
  "Get the destination for xcodebuild test command.
Uses the same simulator as the main build system."
  (let ((sim-id (and (fboundp 'ios-simulator-simulator-identifier)
                     (ios-simulator-simulator-identifier))))
    (if sim-id
        (format "platform=iOS Simulator,id=%s" sim-id)
      ;; Fallback to any iOS Simulator
      "platform=iOS Simulator,name=Any iOS Simulator Device")))

(defun swift-test--get-scheme-for-target (target-name)
  "Try to find a scheme matching TARGET-NAME."
  (let ((schemes (swift-test--available-schemes)))
    (cl-find-if (lambda (s) (string= s target-name)) schemes)))

(defun swift-test--get-scheme (&optional target-name)
  "Get the scheme for testing.
If TARGET-NAME is provided, try to find a matching scheme first.
Falls back to saved :test-scheme from project settings, or prompts user.
The selected scheme is saved persistently to project settings."
  (let* ((root (swift-test--project-root))
         ;; Try to get saved test-scheme from project settings
         (saved-test-scheme (when (fboundp 'swift-project-settings-test-scheme)
                              (swift-project-settings-test-scheme root))))
    (or ;; First: if target specified, try to find matching scheme
        (and target-name (swift-test--get-scheme-for-target target-name))
        ;; Second: check our own cache for this specific target
        (and target-name (gethash target-name swift-test--scheme-cache))
        ;; Third: use saved test-scheme from project settings (new!)
        saved-test-scheme
        ;; Fourth: prompt user to select for this target
        (let ((schemes (swift-test--available-schemes)))
          (when schemes
            (let* ((test-schemes (cl-remove-if-not
                                  (lambda (s) (string-match-p "Test" s))
                                  schemes))
                   (other-schemes (cl-remove-if
                                   (lambda (s) (string-match-p "Test" s))
                                   schemes))
                   ;; Show all schemes, but put test schemes first
                   (candidates (append test-schemes other-schemes))
                   (prompt (if target-name
                               (format "Select scheme for %s: " target-name)
                             "Select test scheme: "))
                   (selected (if (= (length candidates) 1)
                                 (car candidates)
                               (completing-read prompt candidates nil t))))
              ;; Cache by target name if provided
              (when target-name
                (puthash target-name selected swift-test--scheme-cache))
              ;; Save to project settings for persistence (new!)
              (when (fboundp 'swift-project-settings-set-test-scheme)
                (swift-project-settings-set-test-scheme root selected))
              (swift-test--notify (format "Using scheme: %s" selected))
              selected))))))

(defun swift-test-set-scheme (scheme)
  "Set SCHEME as the test scheme for the current project.
Saves to both local cache and persistent project settings."
  (interactive
   (list (completing-read "Test scheme: "
                          (swift-test--available-schemes)
                          nil nil
                          (swift-test--get-scheme))))
  (let ((root (swift-test--project-root)))
    ;; Update local cache
    (puthash root scheme swift-test--scheme-cache)
    ;; Save to persistent project settings
    (when (fboundp 'swift-project-settings-set-test-scheme)
      (swift-project-settings-set-test-scheme root scheme))
    (message "Test scheme set to: %s (saved to project settings)" scheme)))

(defun swift-test--available-schemes ()
  "Get list of available schemes from the project."
  (let* ((root (swift-test--project-root))
         (project-file (car (directory-files root t "\\.xcodeproj$")))
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (project-arg (cond
                       (workspace-file (format "-workspace %s" (shell-quote-argument workspace-file)))
                       (project-file (format "-project %s" (shell-quote-argument project-file)))
                       (t nil))))
    (when project-arg
      (let* ((cmd (format "xcodebuild -list %s 2>/dev/null | grep -A 100 'Schemes:' | tail -n +2 | grep -v '^$' | sed 's/^[[:space:]]*//' | head -20"
                          project-arg))
             (output (swift-async-run-sync cmd :timeout 15)))
        (split-string (or output "") "\n" t)))))

(defun swift-test--is-local-package-target-p (target-name)
  "Check if TARGET-NAME is a local SPM package test target.
Returns non-nil if the target appears to be from a local package."
  (when target-name
    ;; Check if there's a node with this target that has package-type 'spm
    (let ((is-spm nil))
      (swift-test--walk-nodes
       (lambda (node)
         (when (and (eq (swift-test-node-kind node) 'target)
                    (string= (swift-test-node-name node) target-name)
                    (eq (swift-test-node-package-type node) 'spm))
           (setq is-spm t))))
      is-spm)))

(defun swift-test--build-xcode-command (test-ids)
  "Build xcodebuild test command for TEST-IDS."
  (let* ((root (swift-test--project-root))
         ;; Extract target name from first test-id to find matching scheme
         (target-name (when test-ids
                        (car (swift-test--parse-test-id (car test-ids)))))
         ;; For local packages, we need to use main app scheme but the package's test target
         (is-local-package (and target-name
                                (swift-test--is-local-package-target-p target-name)))
         ;; For local packages, don't pass target-name to get-scheme (use main app scheme)
         (scheme (swift-test--get-scheme (unless is-local-package target-name)))
         ;; Find workspace or project file
         (workspace-file (car (directory-files root t "\\.xcworkspace$")))
         (project-file (car (directory-files root t "\\.xcodeproj$")))
         (project-arg (cond
                       (workspace-file (format "-workspace %s" (shell-quote-argument workspace-file)))
                       (project-file (format "-project %s" (shell-quote-argument project-file)))
                       (t "")))
         (destination (swift-test--get-destination))
         ;; Build optimized test command
         (base-cmd (mapconcat
                    #'identity
                     (list
                      "xcodebuild test"
                      (format "-scheme %s" (shell-quote-argument (or scheme "")))
                      project-arg
                      (format "-destination '%s'" destination)
                      ;; Performance optimizations
                      "-disable-concurrent-destination-testing"  ; Don't clone simulator
                      "-test-timeouts-enabled NO"                ; Disable test timeouts
                      "-parallel-testing-enabled NO")            ; Run on same simulator
                    " ")))
    (if test-ids
        (let* ((unique-filters
                (delete-dups
                 (mapcar #'swift-test--format-only-testing-arg test-ids))))
          (format "%s %s" base-cmd (mapconcat #'identity unique-filters " ")))
      base-cmd)))

(defvar swift-test--last-output nil
  "Last test output for debugging.")

(defun swift-test--parse-results (output)
  "Parse test OUTPUT and update node statuses."
  ;; Save for debugging
  (setq swift-test--last-output output)
  
  ;; Mark all running tests as not-run first (in case some didn't report)
  (swift-test--walk-nodes
   (lambda (node)
     (when (eq (swift-test-node-status node) 'running)
       (setf (swift-test-node-status node) 'not-run)
       (setf (swift-test-node-message node) nil))))  ; Clear old error messages
  
  (setq swift-test--running-tests nil)
  
  ;; First pass: collect error messages
  ;; Hash table: test-identifier -> error-message
  (let ((error-messages (make-hash-table :test 'equal))
        (matched-count 0))
    
    (with-temp-buffer
      (insert output)
      
      ;; Collect XCTest error messages
      ;; Format: /path/to/File.swift:42: error: -[TestClass testMethod] : XCTAssertEqual failed: ("1") is not equal to ("2")
      ;; Error messages can span multiple lines when comparing multi-line strings
      ;; The message ends when we hit a new file:line: pattern or end of relevant section
      ;; Store as lists to support multiple errors per test
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([^:\n]+\\):\\([0-9]+\\): error: -\\[\\([^ ]+\\) \\([^]]+\\)\\] : "
              nil t)
        (let* ((file-path (match-string 1))
               (line-num (match-string 2))
               (full-class (match-string 3))
               (test (match-string 4))
               (msg-start (point))
               ;; Find the end of this error message
               ;; It ends at the next file:line: pattern or next test marker
               (msg-end (save-excursion
                          (if (re-search-forward
                               "^[^:\n]+:[0-9]+: \\(?:error\\|warning\\|note\\):\\|^Test \\(?:Case\\|case\\|Suite\\)" 
                               nil t)
                              (match-beginning 0)
                            (point-max))))
               (error-msg (string-trim (buffer-substring-no-properties msg-start msg-end)))
               ;; Truncate very long messages but keep them readable
               (error-msg (if (> (length error-msg) 500)
                              (concat (substring error-msg 0 497) "...")
                            error-msg))
               ;; Store with multiple key formats to ensure matching
               (class (if (string-match-p "\\." full-class)
                          (car (last (split-string full-class "\\.")))
                        full-class))
               (key1 (format "%s.%s" full-class test))
               (key2 (format "%s.%s" class test))
               (existing1 (gethash key1 error-messages))
               (existing2 (gethash key2 error-messages)))
          (message "Found XCTest error: %s.%s -> %s" class test (substring error-msg 0 (min 80 (length error-msg))))
          ;; Store as list - append to existing or create new
          (puthash key1 (append existing1 (list error-msg)) error-messages)
          (puthash key2 (append existing2 (list error-msg)) error-messages)))
      
      ;; Collect Swift Testing error messages - multiple formats
      ;; Store as lists to support multiple errors per test
      
      ;; Format 1: Error message on same line as "recorded an issue"
      ;; ✘ Test "Name" recorded an issue at File.swift:66:9: Expectation failed: ...
      ;; May have multiple issues per test - store as list
      (goto-char (point-min))
      (while (re-search-forward
              "✘ Test \"\\([^\"]+\\)\" recorded an issue[^:]*:[0-9]+:[0-9]+: \\(.+\\)$"
              nil t)
        (let* ((test-name (match-string 1))
               (error-msg (match-string 2))
               (existing (gethash test-name error-messages)))
          ;; Store as list - append to existing list or create new one
          (puthash test-name (append existing (list error-msg)) error-messages)
          (message "Found Swift Testing error (format 1): %s -> %s" test-name error-msg)))
      
      ;; Format 2: "✘ Test "Name" recorded an issue..." followed by "↳ message" on next line
      (goto-char (point-min))
      (while (re-search-forward
              "✘ Test \"\\([^\"]+\\)\" recorded an issue[^\n]*\n\\s-*↳ \\(.+\\)$"
              nil t)
        (let* ((test-name (match-string 1))
               (error-msg (match-string 2))
               (existing (gethash test-name error-messages)))
          ;; Only add if this exact message isn't already in the list
          (unless (member error-msg existing)
            (message "Found Swift Testing error (format 2): %s -> %s" test-name error-msg)
            (puthash test-name (append existing (list error-msg)) error-messages))))
      
      ;; Format 3: "✘ Expectation failed: ..." directly after test start
      ;; ◇ Test "Name" started.
      ;; ✘ Expectation failed: (value → x) == y
      (goto-char (point-min))
      (while (re-search-forward
              "◇ Test \"\\([^\"]+\\)\" started\\." nil t)
        (let ((test-name (match-string 1))
              (search-limit (save-excursion (forward-line 10) (point))))
          ;; Look for expectation failure
          (when (re-search-forward "✘ \\(Expectation failed: .+\\)$" search-limit t)
            (let* ((error-msg (match-string 1))
                   (existing (gethash test-name error-messages)))
              (unless (member error-msg existing)
                (message "Found Swift Testing error (format 3): %s -> %s" test-name error-msg)
                (puthash test-name (append existing (list error-msg)) error-messages))))))
      
      ;; Second pass: parse test results and apply error messages
      
      ;; Parse XCTest results - new Xcode format:
      ;; "Test case 'ClassName.testMethod()' passed on 'Device' (0.001 seconds)"
      (goto-char (point-min))
      (while (re-search-forward
              "Test case '\\([^.]+\\)\\.\\([^(']+\\)()' \\(passed\\|failed\\).*?(\\([0-9.]+\\) seconds)"
              nil t)
        (let* ((class (match-string 1))
               (test (match-string 2))
               (result (match-string 3))
               (duration (string-to-number (match-string 4)))
               (status (if (string= result "passed") 'passed 'failed))
               (error-msg (gethash (format "%s.%s" class test) error-messages)))
          (cl-incf matched-count)
          (swift-test--update-test-status class test status duration nil error-msg)))
      
      ;; Parse Swift Testing format from xcresult/structured output:
      ;; "Test case 'ClassName/testMethod()' passed on 'Device' (0.001 seconds)"
      (goto-char (point-min))
      (while (re-search-forward
              "Test case '\\([^/]+\\)/\\([^(']+\\)()' \\(passed\\|failed\\).*?(\\([0-9.]+\\) seconds)"
              nil t)
        (let* ((class (match-string 1))
               (test (match-string 2))
               (result (match-string 3))
               (duration (string-to-number (match-string 4)))
               (status (if (string= result "passed") 'passed 'failed))
               (error-msg (gethash (format "%s.%s" class test) error-messages)))
          (cl-incf matched-count)
          (swift-test--update-test-status class test status duration nil error-msg)))
      
      ;; Parse old XCTest format: "Test Case '-[ClassName testMethod]' passed (0.001 seconds)."
      (goto-char (point-min))
      (while (re-search-forward
              "Test Case '-\\[\\([^ ]+\\) \\([^]]+\\)\\]' \\(passed\\|failed\\) (\\([0-9.]+\\) seconds)"
              nil t)
        (let* ((full-class (match-string 1))
               (test (match-string 2))
               (result (match-string 3))
               (duration-str (match-string 4))
               (class (if (string-match-p "\\." full-class)
                          (car (last (split-string full-class "\\.")))
                        full-class))
               (duration (string-to-number duration-str))
               (status (if (string= result "passed") 'passed 'failed))
               (error-msg (gethash (format "%s.%s" full-class test) error-messages)))
          (cl-incf matched-count)
          (swift-test--update-test-status class test status duration nil error-msg)))
      
      ;; Parse Swift Testing console output with display names:
      ;; Format: ✔ Test "Display Name" passed after 0.001 seconds.
      ;; Format: ✘ Test "Display Name" failed after 0.001 seconds.
      (goto-char (point-min))
      (while (re-search-forward
              "[✔✘] Test \"\\([^\"]+\\)\" \\(passed\\|failed\\) after \\([0-9.]+\\) seconds"
              nil t)
        (let* ((display-name (match-string 1))
               (result (match-string 2))
               (duration (string-to-number (match-string 3)))
               (status (if (string= result "passed") 'passed 'failed))
               (error-msg (gethash display-name error-messages)))
          (cl-incf matched-count)
          (swift-test--update-test-status nil nil status duration display-name error-msg)))
      
      ;; Parse Swift Testing Suite results:
      ;; Format: ✔ Suite "Suite Name" passed after 0.001 seconds.
      (goto-char (point-min))
      (while (re-search-forward
              "[✔✘] Suite \"\\([^\"]+\\)\" \\(passed\\|failed\\) after \\([0-9.]+\\) seconds"
              nil t)
        (let* ((suite-name (match-string 1))
               (result (match-string 2))
               (status (if (string= result "passed") 'passed 'failed)))
          ;; Update class/suite status by display name
          (swift-test--update-suite-status suite-name status))))
    
    (message "Parsed %d test results from output (%d chars)" matched-count (length output)))
  
  ;; Update parent statuses
  (swift-test--update-parent-statuses))

(defun swift-test--update-test-status (class-name test-name status &optional duration display-name error-message)
  "Update status for test TEST-NAME in CLASS-NAME.
Optionally set DURATION in seconds.
If DISPLAY-NAME is provided, match by display name instead of function name.
If ERROR-MESSAGE is provided, store it for display."
  (let ((found nil))
    (swift-test--walk-nodes
     (lambda (node)
       (when (eq (swift-test-node-kind node) 'test)
         (let ((node-name (swift-test-node-name node))
               (node-display-name (swift-test-node-display-name node))
               (parent (swift-test-node-parent node)))
           ;; Match by display-name if provided, otherwise by function name
           (when (if display-name
                     ;; Match by display name
                     (and node-display-name
                          (string= node-display-name display-name))
                   ;; Match by function name and optionally class name
                   (and (string= node-name test-name)
                        (or (null class-name)
                            (string= (swift-test-node-name parent) class-name))))
             (setq found t)
             (setf (swift-test-node-status node) status)
             (when duration
               (setf (swift-test-node-duration node) duration))
             (when error-message
               (setf (swift-test-node-message node) error-message))
             (message "Updated test status: %s -> %s"
                      (or display-name (format "%s.%s" class-name test-name))
                      status)
             (when (eq status 'failed)
               (push (swift-test-node-id node) swift-test--failed-tests)))))))
    (unless found
      (message "Warning: Could not find test to update: %s"
               (or display-name (format "%s.%s" class-name test-name))))))

(defun swift-test--update-suite-status (suite-display-name status)
  "Update status for suite with SUITE-DISPLAY-NAME to STATUS."
  (swift-test--walk-nodes
   (lambda (node)
     (when (and (eq (swift-test-node-kind node) 'class)
                (swift-test-node-display-name node)
                (string= (swift-test-node-display-name node) suite-display-name))
       (setf (swift-test-node-status node) status)
       (message "Updated suite status: \"%s\" -> %s" suite-display-name status)))))

(defun swift-test--compute-status-from-children (children)
  "Compute aggregate status from CHILDREN nodes."
  (let ((has-passed nil)
        (has-failed nil)
        (has-running nil))
    (dolist (child children)
      (pcase (swift-test-node-status child)
        ('passed (setq has-passed t))
        ('failed (setq has-failed t))
        ('running (setq has-running t))))
    (cond (has-running 'running)
          (has-failed 'failed)
          (has-passed 'passed)
          (t 'not-run))))

(defun swift-test--update-parent-statuses ()
  "Update status of parent nodes based on children.
Structure: target -> file -> class -> test"
  (dolist (target swift-test--report)
    ;; Iterate over files
    (dolist (file (swift-test-node-children target))
      ;; Iterate over classes/suites within file
      (dolist (class (swift-test-node-children file))
        ;; Update class status from tests
        (setf (swift-test-node-status class)
              (swift-test--compute-status-from-children
               (swift-test-node-children class))))
      ;; Update file status from classes
      (setf (swift-test-node-status file)
            (swift-test--compute-status-from-children
             (swift-test-node-children file))))
    ;; Update target status from files
    (setf (swift-test-node-status target)
          (swift-test--compute-status-from-children
           (swift-test-node-children target)))))

;;; Commands for use outside the explorer

;;;###autoload
(defun swift-test-run-at-point ()
  "Run the test at point in the current buffer."
  (interactive)
  (let* ((test-info (swift-test--find-test-at-point))
         (class (plist-get test-info :class))
         (test (plist-get test-info :test)))
    (if (and class test)
        (progn
          (swift-test--discover-tests)
          (let ((test-id (swift-test--find-test-id class test)))
            (if test-id
                (swift-test--run-tests (list test-id))
              (swift-test--notify (format "Test not found: %s.%s" class test)))))
      (swift-test--notify "No test found at point"))))

;;;###autoload
(defun swift-test-run-class ()
  "Run all tests in the current class."
  (interactive)
  (let* ((class (swift-test--find-class-at-point)))
    (if class
        (progn
          (swift-test--discover-tests)
          (let ((class-id (swift-test--find-class-id class)))
            (if class-id
                (swift-test--run-tests
                 (swift-test--collect-test-ids
                  (swift-test--find-node-by-id class-id)))
              (swift-test--notify (format "Class not found: %s" class)))))
      (swift-test--notify "No test class found"))))

;;;###autoload
(defun swift-test-run-all ()
  "Run all tests in the project."
  (interactive)
  (swift-test--discover-tests)
  (swift-test--run-tests nil))

;;;###autoload
(defun swift-test-run-failed ()
  "Re-run failed tests."
  (interactive)
  (if swift-test--failed-tests
      (swift-test--run-tests swift-test--failed-tests)
    (swift-test--notify "No failed tests to re-run")))

(defun swift-test--find-test-at-point ()
  "Find the test function at point.
Returns plist with :class and :test."
  (save-excursion
    (let ((class nil)
          (test nil))
      ;; Find test function
      (beginning-of-line)
      (when (or (looking-at ".*func\\s-+\\(test[A-Za-z0-9_]*\\)")
                (progn
                  (re-search-backward "func\\s-+\\(test[A-Za-z0-9_]*\\)" nil t)))
        (setq test (match-string 1)))
      
      ;; Find class
      (when (re-search-backward
             "\\(?:class\\|struct\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
             nil t)
        (setq class (match-string 1)))
      
      (list :class class :test test))))

(defun swift-test--find-class-at-point ()
  "Find the test class at point."
  (save-excursion
    (when (re-search-backward
           "\\(?:class\\|struct\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)"
           nil t)
      (match-string 1))))

(defun swift-test--find-test-id (class test)
  "Find the test ID for CLASS and TEST."
  (let ((found nil))
    (swift-test--walk-nodes
     (lambda (node)
       (when (and (eq (swift-test-node-kind node) 'test)
                  (string= (swift-test-node-name node) test)
                  (string= (swift-test-node-name (swift-test-node-parent node)) class))
         (setq found (swift-test-node-id node)))))
    found))

(defun swift-test--find-class-id (class)
  "Find the class ID for CLASS."
  (let ((found nil))
    (swift-test--walk-nodes
     (lambda (node)
       (when (and (eq (swift-test-node-kind node) 'class)
                  (string= (swift-test-node-name node) class))
         (setq found (swift-test-node-id node)))))
    found))

(defun swift-test--find-node-by-id (id)
  "Find node by ID."
  (let ((found nil))
    (swift-test--walk-nodes
     (lambda (node)
       (when (string= (swift-test-node-id node) id)
         (setq found node))))
    found))

;;; Transient Menu

(require 'transient)

(defun swift-test--current-scheme-display ()
  "Get current test scheme for display in transient menu."
  (let* ((root (swift-test--project-root))
         (cached (gethash root swift-test--scheme-cache)))
    (if cached
        (propertize cached 'face 'font-lock-constant-face)
      (propertize "not set" 'face 'shadow))))

(defun swift-test--package-schemes-display ()
  "Get summary of package scheme mappings for display."
  (let ((count (hash-table-count swift-test--package-scheme-cache)))
    (if (> count 0)
        (propertize (format "%d configured" count) 'face 'font-lock-constant-face)
      (propertize "none" 'face 'shadow))))

(transient-define-suffix swift-test-select-scheme ()
  "Select the test scheme."
  :key "s"
  :description (lambda () (format "Test scheme: %s" (swift-test--current-scheme-display)))
  (interactive)
  (call-interactively #'swift-test-set-scheme)
  (transient-setup 'swift-test-transient))

(transient-define-suffix swift-test-select-all-tests-scheme ()
  "Select scheme for running ALL tests."
  :key "S"
  :description (lambda ()
                 (let ((scheme (gethash "all-tests" swift-test--scheme-cache)))
                   (format "All-tests scheme: %s"
                           (if scheme
                               (propertize scheme 'face 'font-lock-constant-face)
                             (propertize "not set" 'face 'shadow)))))
  (interactive)
  (let* ((schemes (swift-test--available-schemes))
         (selected (completing-read "Select scheme for ALL tests: " schemes nil t)))
    (puthash "all-tests" selected swift-test--scheme-cache)
    (message "All-tests scheme set to: %s" selected))
  (transient-setup 'swift-test-transient))

(transient-define-suffix swift-test-clear-package-schemes ()
  "Clear cached package scheme mappings."
  :key "x"
  :description (lambda () (format "Clear package schemes (%s)" (swift-test--package-schemes-display)))
  (interactive)
  (swift-test-clear-scheme-cache)
  (transient-setup 'swift-test-transient))

(defun swift-test-add-target-to-scheme ()
  "Add a test target to a scheme's test action.
Prompts for scheme and test target, then modifies the scheme file."
  (interactive)
  (let* ((schemes (swift-test--available-schemes))
         (scheme (completing-read "Add test target to scheme: " schemes nil t
                                  (gethash (swift-test--project-root) swift-test--scheme-cache)))
         ;; Find all test targets in the project
         (test-targets (swift-test--discover-test-targets))
         (target (completing-read "Test target to add: " test-targets nil t)))
    (if (swift-test--add-test-target-to-scheme scheme target)
        (message "Added %s to %s - reload Xcode to see changes" target scheme)
      (message "Failed to add %s to %s" target scheme))))

(defun swift-test--discover-test-targets ()
  "Discover all test targets in the project."
  (let ((targets '()))
    ;; Get from discovered tests
    (dolist (target-node swift-test--report)
      (let ((name (swift-test-node-name target-node)))
        (when (string-match-p "Tests$" name)
          (push name targets))))
    ;; Also scan for *Tests directories that might not have tests discovered yet
    (let* ((root (swift-test--project-root))
           (dirs (directory-files-recursively root "Tests$" t)))
      (dolist (dir dirs)
        (when (file-directory-p dir)
          (let ((name (file-name-nondirectory dir)))
            (unless (member name targets)
              (push name targets))))))
    (delete-dups targets)))

(transient-define-suffix swift-test-add-target-to-scheme-suffix ()
  "Add a test target to a scheme."
  :key "+"
  :description "Add test target to scheme"
  (interactive)
  (swift-test-add-target-to-scheme)
  (transient-setup 'swift-test-transient))

;;;###autoload (autoload 'swift-test-transient "swift-test-explorer" nil t)
(transient-define-prefix swift-test-transient ()
  "Swift Test menu."
  [:description
   (lambda () (format "Swift Tests [%s]" (swift-test--current-scheme-display)))
   ""]
  ["Test Explorer"
   ("e" "Show explorer" swift-test-explorer-show)
   ("E" "Toggle explorer" swift-test-explorer-toggle)]
  ["Run Tests"
   ("t" "Test at point" swift-test-run-at-point)
   ("c" "Test class" swift-test-run-class)
   ("a" "All tests" swift-test-run-all)
   ("r" "Re-run failed" swift-test-run-failed)]
  ["Scheme Settings"
   (swift-test-select-scheme)
   (swift-test-select-all-tests-scheme)
   (swift-test-add-target-to-scheme-suffix)
   (swift-test-clear-package-schemes)]
  ["Actions"
   ("R" "Refresh/discover" swift-test-explorer-refresh)
   ("C" "Clear results" swift-test-explorer-clear)]
  [("q" "Quit" transient-quit-one)])

(provide 'swift-test-explorer)
;;; swift-test-explorer.el ends here
