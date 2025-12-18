;;; swift-notification.el --- Notification system for Swift development -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, notifications

;;; Commentary:

;; Unified notification system for Swift development tools.
;; Supports multiple backends: mode-line-hud, minibuffer messages, or custom functions.

;;; Code:

(defvar swift-notification--mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

(defgroup swift-notification nil
  "Notification settings for Swift development tools."
  :group 'programming
  :prefix "swift-notification-")

(defcustom swift-notification-backend 'mode-line-hud
  "Backend to use for displaying build progress and notifications.
Options:
- \\='mode-line-hud: Use mode-line-hud package (visual mode line updates)
- \\='message: Use Emacs message with colored propertized text
- \\='custom: Use custom function set in `swift-notification-function'"
  :type '(choice (const :tag "Mode Line HUD" mode-line-hud)
                 (const :tag "Minibuffer Messages" message)
                 (const :tag "Custom Function" custom))
  :group 'swift-notification)

(defvar swift-notification-function nil
  "Custom notification function to use when `swift-notification-backend' is \\='custom.
Function should accept keyword arguments :message, :delay, :seconds, and :reset.")

(defun swift-notification-send (&rest args)
  "Universal notification function that delegates to configured backend.
Accepts keyword arguments:
  :message - The message to display
  :delay - Optional delay before showing (for mode-line-hud)
  :seconds - How long to show notification
  :reset - Whether to reset after showing
  :face - Face to apply to message (for message backend)
  :no-redisplay - If t, skip the automatic redisplay (default nil)"
  (let ((message-text (plist-get args :message))
        (delay (plist-get args :delay))
        (seconds (plist-get args :seconds))
        (reset (plist-get args :reset))
        (face (plist-get args :face))
        (no-redisplay (plist-get args :no-redisplay))
        (minibuffer-active (active-minibuffer-window)))

    ;; Send to backend (but not if minibuffer is active - would disrupt input)
    (unless minibuffer-active
      (pcase swift-notification-backend
        ('mode-line-hud
         (when swift-notification--mode-line-hud-available-p
           (cond
            ;; Notification style (with seconds and reset)
            ((and seconds reset)
             (mode-line-hud:notification :message message-text :seconds seconds :reset reset))
            ;; Update with delay
            (delay
             (mode-line-hud:updateWith :message message-text :delay delay))
            ;; Simple update
            (t
             (mode-line-hud:update :message message-text)))))

        ('message
         ;; Use minibuffer messages with optional color
         (if face
             (message "%s" (propertize message-text 'face face))
           (message "%s" message-text)))

        ('custom
         ;; Call custom function if configured
         (when (functionp swift-notification-function)
           (apply swift-notification-function args)))

        (_
         ;; Fallback to message if backend unknown
         (message "%s" message-text)))

      ;; Force display update so notification is visible before any blocking operation
      ;; This is especially important before shell commands, file I/O, or user input
      (unless no-redisplay
        (redisplay t)))))

;; Compatibility aliases for existing code
(defalias 'swift-notification-update #'swift-notification-send)
(defalias 'swift-notification-notify #'swift-notification-send)

(provide 'swift-notification)
;;; swift-notification.el ends here
