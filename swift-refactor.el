;;; swift-refactor.el --- Swift code refactoring utilities -*- lexical-binding: t -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, refactoring

;;; Commentary:

;; Provides refactoring tools for Swift code

;;; Code:

(require 'swift-notification nil t)
(require 'transient)

;; Forward declarations
(declare-function swift-error-proxy-parse-output "swift-error-proxy")
(declare-function swift-error-proxy-has-errors-p "swift-error-proxy")

(defgroup swift-refactor nil
  "Provides refactoring tools for Swift."
  :group 'tools
  :prefix "swift-refactor-")

(defun delete-to-next-closing-brace ()
  "Delete all text between the current line and the next closing brace }, but not including the brace itself."
  (interactive)
  (let ((start (point)))
    (when (re-search-forward "\\w?\s?{\\|\}" nil t)
      (forward-line 2)
      (delete-region start (point))
      (indent-region start (point)))))

(cl-defun swift-refactor-delete-until-balancing-char (&key opening-char &key closing-char)
  "Deletes the current line starting with an opening brace { and the matching closing brace } somewhere below it."
  (interactive)
  (let ((open-char opening-char)
        (close-char closing-char))
    (save-excursion
      (end-of-line)
      (when (search-backward open-char nil t)
        (let ((opening-postion (line-beginning-position)))
          (goto-char (point))
          (delete-line)
          ;; (delete-region (line-beginning-position) (1+ (point)))
          (let ((count 1))
            (while (and (> count 0) (search-forward-regexp (concat open-char "\\|" close-char) nil t))
              (when (string= (match-string 0) open-char)
                (setq count (1+ count)))
              (when (string= (match-string 0) close-char)
                (setq count (1- count))))
            (when (eq count 0)
              (goto-char (point))
              (delete-line) ;; Delete the whole line
              (indent-region opening-postion (point)))))))))

(defun swift-refactor-delete-current-line-with-matching-brace ()
  "Deletes the current line starting with '{' and the matching '}' brace somewhere below it."
  (interactive)
  (swift-refactor-delete-until-balancing-char
   :opening-char "{"
   :closing-char "}"))

(defun swift-refactor-insert-at (start end name)
  "Insert a an element with NAME and '{' and ending '}' using START AND END."
  (ignore-errors
    (save-excursion
      (goto-char start)
      (insert  (concat name " {\n"))
      (goto-char end)
      (forward-line)
      (insert "}\n")
      (indent-region (1- start) (line-end-position)))))

(defun swift-refactor-run-on-region (fn &optional args)
  "Run FN on the active region or the current line."
  (if (use-region-p)
      (funcall fn (region-beginning) (region-end))
    (funcall fn (line-beginning-position) (line-end-position) args)))

(defun swift-refactor-insert-around (name)
  "Insert element around selection (as NAME)."
  (interactive "sEnter element name: ")
  (let ((name (if (string-blank-p (string-trim-right name)) "Element" name)))
    (swift-refactor-run-on-region #'swift-refactor-insert-at name)))

(defun swift-refactor-wrap-in-block (name start end)
  "Wrap region from START to END in a block named NAME."
  (goto-char end)
  (insert "}\n")
  (goto-char start)
  (insert (format "%s {\n" name))
  (indent-region start (point)))

(defun swift-refactor-wrap-selection (name)
  "Wrap the selected region in a block named NAME."
  (interactive "sEnter block name: ")
  (swift-refactor-run-on-region
   (lambda (start end)
     (swift-refactor-wrap-in-block name start end))))

(defun swift-refactor-extract-function (method-name)
  "Extract active region to its own function (as METHOD-NAME)."
  (interactive "sEnter method name (optional): ")
  (let ((method-name (if (string-blank-p (string-trim-right method-name)) "extractedMethod" method-name)))
    (swift-refactor-run-on-region #'swift-refactor-extract-function-with method-name)))

(defun swift-refactor-tidy-up-constructor ()
  "Clean up constructor from Type.init() to Type()."
  (interactive)
  (swift-refactor-run-on-region #'swift-refactor-tidy-up-constructor-with))

(defun swift-refactor-extract-function-with (start end method-name)
  "Extracts the Swift code region between START and END into a new function with the given METHOD-NAME."
  (ignore-errors
    (let* ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (kill-region start end)
        (insert (concat method-name "()\n"))
        (beginning-of-defun)
        (insert (concat "\tprivate func " method-name "() {\n"))
        (insert content)
        (insert "\t}\n\n"))
      (indent-region (region-beginning) (region-end))
      (indent-according-to-mode))))

(defun swift-refactor-tidy-up-constructor-with (start end)
  "Clean up the constructor and removes .init from code."
  (ignore-errors
    (let* ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (kill-region start end)
        (insert (remove-init-from-string content))))))

(defun swift-refactor-add-try-catch ()
  "Add try catch."
  (interactive)
  (swift-refactor-run-on-region #'swift-refactor-add-try-catch-with))

(defun swift-refactor-add-try-catch-with (start end)
  "Extract region between START & END."
  (ignore-errors
    (let ((content (buffer-substring-no-properties start end)))
      (save-excursion
        (delete-region start end)
          (insert "do {\n")
          (insert content)
          (insert "} catch {\n print(error)\n}\n")
          (indent-region (region-beginning) (region-end))
          (indent-according-to-mode)))))

(defun remove-init-from-string (string)
  "Remove '.init' from the given STRING."
  (replace-regexp-in-string "\\.init" "" string))

;; Taken from  https://gitlab.com/woolsweater/dotemacs.d/-/blob/main/modules/my-swift-mode.el
;;;###autoload
(defun code-refactor-split-function-list ()
  "While on either the header of a function-like declaration or a call to a function, split each parameter/argument to its own line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (condition-case nil
        (atomic-change-group
          (search-forward "(")
          (let ((end))
            (while (not end)
              (newline-and-indent)
              (let ((parens 0)
                    (angles 0)
                    (squares 0)
                    (curlies 0)
                    (comma))
                (while (not (or comma end))
                  (re-search-forward
                   (rx (or ?\( ?\) ?< ?> ?\[ ?\] ?{ ?} ?\" ?,))
                   (line-end-position))
                  (pcase (match-string 0)
                    ("(" (cl-incf parens))
                    (")" (if (> parens 0)
                             (cl-decf parens)
                           (backward-char)
                           (newline-and-indent)
                           (setq end t)))
                    ;; Note; these could be operators in an expression;
                    ;; there's no obvious way to fully handle that.
                    ("<" (cl-incf angles))
                    ;; At a minimum we can skip greater-than and func arrows
                    (">" (unless (zerop angles)
                           (cl-decf angles)))
                    ("[" (cl-incf squares))
                    ("]" (cl-decf squares))
                    ("{" (cl-incf curlies))
                    ("}" (cl-decf curlies))
                    ("\"" (let ((string-end))
                            (while (not string-end)
                              (re-search-forward (rx (or ?\" (seq ?\\ ?\")))
                                                 (line-end-position))
                              (setq string-end (equal (match-string 0) "\"")))))
                    ("," (when (and (zerop parens) (zerop angles)
                                    (zerop squares) (zerop curlies))
                           (setq comma t)))))))))
      (error (user-error "Cannot parse function decl or call here")))))

;;;###autoload
(defun swift-refactor-functions-and-pragmas ()
    "Show swift file compressed functions and pragmas."
    (interactive)
    (let ((list-matching-lines-face nil))
      (occur "\\(#pragma mark\\)\\|\\(MARK:\\)")))

;;;###autoload
(defun swift-refactor-print-thing-at-point ()
    "Print thing at point."
    (interactive)
    (let ((word (thing-at-point 'word)))
      (end-of-line)
      (newline-and-indent)
      (insert (format "debugPrint(\"%s: \ \\(%s\)\")" word word))))

(defun swift-refactor-insert-and-indent (text)
  "Insert TEXT and indent the inserted region."
  (let ((start (point)))
    (insert text)
    (indent-region start (point))))

(defun swift-refactor-insert-and-goto-eol (text)
  "Insert TEXT at point, indent, and move to end of line."
  (swift-refactor-insert-and-indent text)
  (end-of-line))

;;;###autoload
(defun swift-refactor-insert-mark ()
    "Insert a mark at line."
    (interactive)
    (swift-refactor-insert-and-goto-eol "// MARK: - "))

;;;###autoload
(defun swift-refactor-insert-todo ()
    "Insert a Todo."
    (interactive)
    (swift-refactor-insert-and-goto-eol "// TODO: "))

;;; SwiftFormat integration

(defcustom swift-refactor-swiftformat-command "swiftformat"
  "Path to swiftformat executable."
  :type 'string
  :group 'swift-refactor)

;;;###autoload
(defun swift-refactor-format-buffer ()
  "Format current buffer with swiftformat."
  (interactive)
  (unless (derived-mode-p 'swift-mode 'swift-ts-mode)
    (user-error "Not a Swift buffer"))
  (when-let* ((file (buffer-file-name)))
    (unless (executable-find swift-refactor-swiftformat-command)
      (user-error "swiftformat not installed. Install with: brew install swiftformat"))
    (save-buffer)
    (let ((point-before (point))
          (window-start-before (window-start)))
      (if (zerop (call-process swift-refactor-swiftformat-command nil nil nil file))
          (progn
            (revert-buffer t t t)
            (goto-char (min point-before (point-max)))
            (set-window-start nil (min window-start-before (point-max)))
            (swift-notification-send :message "Formatted" :seconds 1))
        (swift-notification-send :message "Format failed" :seconds 2)))))

;;;###autoload
(defun swift-refactor-format-region (start end)
  "Format region from START to END with swiftformat."
  (interactive "r")
  (unless (derived-mode-p 'swift-mode 'swift-ts-mode)
    (user-error "Not a Swift buffer"))
  (unless (executable-find swift-refactor-swiftformat-command)
    (user-error "swiftformat not installed. Install with: brew install swiftformat"))
  (let ((temp-file (make-temp-file "swiftformat-region" nil ".swift")))
    (unwind-protect
        (progn
          (write-region start end temp-file)
          (when (zerop (call-process swift-refactor-swiftformat-command nil nil nil temp-file))
            (let ((formatted (with-temp-buffer
                               (insert-file-contents temp-file)
                               (buffer-string))))
              (delete-region start end)
              (goto-char start)
              (insert formatted)
              (swift-notification-send :message "Region formatted" :seconds 1))))
      (delete-file temp-file))))

(defcustom swift-refactor-swiftlint-command "swiftlint"
  "Command to run swiftlint."
  :type 'string
  :group 'swift-refactor)

;;;###autoload
(defun swift-refactor-lint-project ()
  "Run SwiftLint on the project and show results in periphery buffer."
  (interactive)
  (require 'periphery-swiftlint)
  (periphery-run-swiftlint))

;;;###autoload
(defun swift-refactor-lint-file ()
  "Run SwiftLint on the current file and show results in periphery buffer."
  (interactive)
  (unless (derived-mode-p 'swift-mode 'swift-ts-mode)
    (user-error "Not a Swift buffer"))
  (when-let* ((file (buffer-file-name)))
    (unless (executable-find swift-refactor-swiftlint-command)
      (user-error "swiftlint not installed. Install with: brew install swiftlint"))
    (save-buffer)
    (require 'periphery-helper nil t)
    (async-start-command-to-string
     :command (format "%s lint --path %s" swift-refactor-swiftlint-command (shell-quote-argument file))
     :callback (lambda (result)
                 (if (and result (not (string-empty-p (string-trim result))))
                     (swift-error-proxy-parse-output result)
                   (message-with-color
                    :tag "[Success]"
                    :text "No lint warnings or errors."
                    :attributes 'success))))
    (swift-notification-send :message (format "Linting %s..." (file-name-nondirectory file)) :seconds 1)))

;;;###autoload
(defun swift-refactor-fix-file ()
  "Auto-fix SwiftLint issues in the current file."
  (interactive)
  (unless (derived-mode-p 'swift-mode 'swift-ts-mode)
    (user-error "Not a Swift buffer"))
  (when-let* ((file (buffer-file-name)))
    (unless (executable-find swift-refactor-swiftlint-command)
      (user-error "swiftlint not installed. Install with: brew install swiftlint"))
    (save-buffer)
    (let ((point-before (point))
          (window-start-before (window-start)))
      (if (zerop (call-process swift-refactor-swiftlint-command nil nil nil
                               "lint" "--fix" "--path" file))
          (progn
            (revert-buffer t t t)
            (goto-char point-before)
            (set-window-start nil window-start-before)
            (swift-notification-send :message "File auto-fixed" :seconds 1))
        (swift-notification-send :message "Auto-fix failed" :seconds 2)))))

;;; Transient Menu

;;;###autoload
(transient-define-prefix swift-refactor-transient ()
  "Swift refactoring commands."
  [["Format"
    ("f" "Format buffer" swift-refactor-format-buffer)
    ("r" "Format region" swift-refactor-format-region)]
   ["Lint"
    ("l" "Lint file" swift-refactor-lint-file)
    ("L" "Lint project" swift-refactor-lint-project)
    ("x" "Fix file" swift-refactor-fix-file)]
   ["Extract"
    ("e" "Extract function" swift-refactor-extract-function)
    ("w" "Wrap selection" swift-refactor-wrap-selection)
    ("t" "Add try-catch" swift-refactor-add-try-catch)]
   ["Edit"
    ("s" "Split arguments" code-refactor-split-function-list)
    ("d" "Delete matching brace" swift-refactor-delete-current-line-with-matching-brace)
    ("c" "Clean .init()" swift-refactor-tidy-up-constructor)]
   ["Insert"
    ("m" "Insert MARK" swift-refactor-insert-mark)
    ("o" "Insert TODO" swift-refactor-insert-todo)
    ("p" "Print at point" swift-refactor-print-thing-at-point)]])

(provide 'swift-refactor)
;;; swift-refactor.el ends here
