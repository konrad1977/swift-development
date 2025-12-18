;;; localizeable-mode.el --- Major mode for editing .strings files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, localization

;;; Commentary:

;; A simple major mode for editing .strings files used in iOS/macOS localization

;;; Code:

(defvar localizeable-mode--map nil "Keymap for localizeable.")
(setq localizeable-mode--map (make-sparse-keymap))

(add-to-list 'auto-mode-alist '("\\.strings\\'" . localizeable-mode))

;;;###autoload
(define-derived-mode localizeable-mode fundamental-mode
  (setq indicate-empty-lines t            ;; Show empty lines
        indicate-unused-lines t           ;; Show unused lines
        mode-name "Localizable")
  (setq font-lock-defaults '(()))
  ;; Set up C-style comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (modify-syntax-entry ?/ ". 124b" localizeable-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" localizeable-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" localizeable-mode-syntax-table)
  (display-line-numbers-mode 1))

(font-lock-add-keywords
 'localizeable-mode '(
                      ;; Comments - match from // or /* to end of line/block
                      ("^[ \t]*\\(//.*\\)$" 0 'font-lock-comment-face t)
                      ("^[ \t]*\\(/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/\\)" 0 'font-lock-comment-face t)
                      ;; String pairs: "key" = "value";
                      ("\\(\"[^\"]+\"\\)\s+=\s+\\(\".+\"\\)\\(;\\)"
                       (1 'font-lock-type-face t)
                       (2 'font-lock-string-face t)
                       (3 'font-lock-delimiter-face t))
                      ;; Equal sign
                      ("\\(=\\)" 0 'font-lock-operator-face t)))

(provide 'localizeable-mode)

;;; localizeable-mode.el ends here

