;;; swift-development-mode.el --- Minor mode for Swift development -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that provides a unified keymap for Swift development across
;; multiple buffer types (Swift files, localizeable .strings files, and
;; iOS simulator output buffers).

;;; Code:

;; Ensure required packages are loaded
(require 'swift-refactor nil t)
(require 'ios-simulator nil t)
(require 'xcode-project nil t)

(defgroup swift-development-mode nil
  "Minor mode for Swift development."
  :group 'programming
  :prefix "swift-development-mode-")

(defvar swift-development-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Build & Run Commands
    (define-key map (kbd "C-c C-c") #'swift-development-compile-and-run)
    (define-key map (kbd "C-c C-b") #'swift-development-compile-app)
    (define-key map (kbd "M-r") #'swift-development-run)
    (define-key map (kbd "C-c C-x") #'swift-development-reset)
    (define-key map (kbd "C-c b s") #'swift-development-build-status)

    ;; SwiftUI Preview Commands
    (define-key map (kbd "C-c C-p") #'swiftui-preview-generate)
    (define-key map (kbd "C-c p h") #'swiftui-preview-generate-with-hot-reload)
    (define-key map (kbd "C-c p s") #'swiftui-preview-stop-hot-reload)
    (define-key map (kbd "C-c p r") #'swiftui-preview-refresh)

    ;; Test Commands
    (define-key map (kbd "C-c t m") #'swift-development-test-module-silent)
    (define-key map (kbd "C-c t p") #'swift-development-test-swift-package-from-file)

    ;; Simulator Commands
    (define-key map (kbd "M-s") #'ios-simulator-terminate-current-app)
    (define-key map (kbd "C-x s n") #'ios-simulator-send-notification)
    (define-key map (kbd "C-x s t") #'ios-simulator-toggle-buffer)
    (define-key map (kbd "C-x s l") #'ios-simulator-change-language)

    ;; Xcode Commands
    (define-key map (kbd "M-K") #'xcode-project-clean-build-folder)
    (define-key map (kbd "C-c C-d") #'xcode-project-start-debugging)
    (define-key map (kbd "C-c x t") #'xcode-project-toggle-device-choice)
    (define-key map (kbd "C-c x c") #'xcode-project-show-current-configuration)

    ;; Refactoring Commands
    (define-key map (kbd "M-t") #'swift-refactor-insert-todo)
    (define-key map (kbd "M-m") #'swift-refactor-insert-mark)
    (define-key map (kbd "C-c r a") #'swift-refactor-wrap-selection)
    (define-key map (kbd "C-c r d") #'swift-refactor-delete-current-line-with-matching-brace)
    (define-key map (kbd "C-c r i") #'swift-refactor-tidy-up-constructor)
    (define-key map (kbd "C-c r r") #'swift-refactor-extract-function)
    (define-key map (kbd "M-P") #'swift-refactor-print-thing-at-point)
    (define-key map (kbd "C-c r t") #'swift-refactor-add-try-catch)
    (define-key map (kbd "C-c r s") #'code-refactor-split-function-list)

    ;; Periphery & Search
    (define-key map (kbd "C-x p t") #'periphery-toggle-buffer)
    (define-key map (kbd "C-c C-f") #'periphery-search-dwiw-rg)

    map)
  "Keymap for swift-development-mode.")

;;;###autoload
(define-minor-mode swift-development-mode
  "Minor mode for Swift development with unified keybindings.

This minor mode provides a consistent set of keybindings across
different buffer types related to Swift development, including:
- Swift source files
- Localizeable .strings files
- iOS simulator output buffers

\\{swift-development-mode-map}"
  :lighter " SwiftDev"
  :keymap swift-development-mode-map
  :group 'swift-development-mode)

;;;###autoload
(defun swift-development-mode-enable ()
  "Enable swift-development-mode."
  (swift-development-mode 1))

;; Auto-enable for Swift files (both traditional and tree-sitter modes)
;;;###autoload
(add-hook 'swift-mode-hook #'swift-development-mode-enable)
;;;###autoload
(add-hook 'swift-ts-mode-hook #'swift-development-mode-enable)

;; Auto-enable for localizeable .strings files
;;;###autoload
(add-hook 'localizeable-mode-hook #'swift-development-mode-enable)

;; Auto-enable for iOS simulator buffers
;;;###autoload
(add-hook 'ios-simulator-mode-hook #'swift-development-mode-enable)

(provide 'swift-development-mode)
;;; swift-development-mode.el ends here
