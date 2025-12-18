;;; apple-docs-query.el --- Query Apple Developer Documentation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, documentation

;;; Commentary:

;; Query Apple Developer Documentation from Emacs

;;; Code:

(defconst apple-developer-url "https://developer.apple.com"
  "Developer apple site.")

(defun apple-docs/query (query)
  "Query Apple Documentation for QUERY."
  (interactive "sQuery: ")
  (when query
    (browse-url (format "%s/search/?q=%s" apple-developer-url (url-hexify-string query)))))

(defun apple-docs/query-thing-at-point ()
  "Query thing at point."
  (interactive)
  (when-let ((word (thing-at-point 'word)))
    (apple-docs/query word)))

(provide 'apple-docs-query)
;;; apple-docs-query.el ends here
