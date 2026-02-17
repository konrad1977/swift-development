;;; swift-lsp.el --- Language Server Protocol (LSP) support for Swift -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, lsp

;;; Commentary:

;; LSP support and configuration for Swift development.
;; Uses cached SDK/arch values from ios-simulator.el when available.

;;; Code:
(require 'cl-lib)
(require 'swift-cache)
(require 'swift-async)

;; Optional dependency - use cached values when available
(require 'ios-simulator nil t)

(defconst swift-lsp--cache-ttl 3600
  "Cache TTL in seconds (1 hour).")

(defun swift-lsp--cached-sdk-path ()
  "Get the simulator SDK path (cached for 1 hour).
Returns nil if the SDK path cannot be resolved."
  (swift-cache-with "swift-lsp::sdk-path" swift-lsp--cache-ttl
    (let ((path (string-trim
                 (or (swift-async-run-sync
                      (list "xcrun" "--show-sdk-path" "--sdk" "iphonesimulator")
                      :timeout 5)
                     ""))))
      (if (string-empty-p path) nil path))))

(defun swift-lsp--cached-sourcekit-path ()
  "Get the sourcekit-lsp path (cached for 1 hour).
Returns nil if sourcekit-lsp cannot be found."
  (swift-cache-with "swift-lsp::sourcekit-path" swift-lsp--cache-ttl
    (let ((path (string-trim
                 (or (swift-async-run-sync
                      (list "xcrun" "--find" "sourcekit-lsp")
                      :timeout 5)
                     ""))))
      (if (string-empty-p path) nil path))))

;;;###autoload
(defun lsp-arguments ()
  "Get the lsp arguments to support UIKit.
Uses cached SDK values for better performance."
  (let* ((sdk (swift-lsp--cached-sdk-path))
         ;; Use ios-simulator-target if available (uses caching), otherwise compute locally
         (target (if (fboundp 'ios-simulator-target)
                     (ios-simulator-target)
                   ;; Fallback: compute target locally with caching
                   (swift-cache-with "swift-lsp::target" swift-lsp--cache-ttl
                     (let* ((arch-str (string-trim
                                       (or (swift-async-run-sync
                                            (list "clang" "-print-target-triple")
                                            :timeout 5)
                                           "")))
                            (components (split-string arch-str "-"))
                            (arch (nth 0 components))
                            (vendor (nth 1 components))
                            (version (string-trim
                                      (or (swift-async-run-sync
                                           (list "xcrun" "--sdk" "iphonesimulator"
                                                 "--show-sdk-version")
                                           :timeout 5)
                                          ""))))
                       (when (and arch vendor version
                                  (not (string-empty-p arch))
                                  (not (string-empty-p version)))
                         (format "%s-%s-ios%s-simulator" arch vendor version)))))))
    (when (and sdk target)
      (list
       "-Xswiftc" "-sdk"
       "-Xswiftc" sdk
       "-Xswiftc" "-target"
       "-Xswiftc" target
       "-Xcc" "-DSWIFT_PACKAGE=0"))))

;;;###autoload
(defun swift-lsp-eglot-server-contact (&optional _interactive _project)
  "Construct the list that eglot needs to start sourcekit-lsp.
INTERACTIVE is non-nil when called from `M-x eglot'.
PROJECT is the `project-find-functions' result.
Uses cached paths for better startup performance."
  (let ((arglist (lsp-arguments))
        (sourcekit-lsp-path (swift-lsp--cached-sourcekit-path)))
    (if sourcekit-lsp-path
        (cons sourcekit-lsp-path (or arglist '()))
      (error "Cannot find sourcekit-lsp.  Is Xcode installed?"))))

(defun swift-lsp-clear-cache ()
  "Clear LSP path caches.
Useful after updating Xcode."
  (interactive)
  (swift-cache-invalidate "swift-lsp::sdk-path")
  (swift-cache-invalidate "swift-lsp::sourcekit-path")
  (swift-cache-invalidate "swift-lsp::target")
  (message "Swift LSP cache cleared"))

(provide 'swift-lsp)
;;; swift-lsp.el ends here
