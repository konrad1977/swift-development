;;; swift-notification.el --- Notification system for Swift development -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.7.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, notifications

;;; Commentary:

;; Unified notification system for Swift development tools.
;; Supports multiple backends: mode-line-hud, minibuffer messages, knockknock, or custom functions.
;; Also provides progress bar notifications for long-running operations.

;;; Code:

(defvar swift-notification--mode-line-hud-available-p (require 'mode-line-hud nil t)
  "Whether mode-line-hud is available.")

(defvar swift-notification--knockknock-available-p (require 'knockknock nil t)
  "Whether knockknock is available.")

(defgroup swift-notification nil
  "Notification settings for Swift development tools."
  :group 'programming
  :prefix "swift-notification-")

(defcustom swift-notification-backend 'mode-line-hud
  "Backend to use for displaying build progress and notifications.
Options:
- \\='mode-line-hud: Use mode-line-hud package (visual mode line updates)
- \\='knockknock: Use knockknock package (floating notifications with icons)
- \\='message: Use Emacs message with colored propertized text
- \\='custom: Use custom function set in `swift-notification-function'"
  :type '(choice (const :tag "Mode Line HUD" mode-line-hud)
                 (const :tag "Knockknock" knockknock)
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

        ('knockknock
         (when swift-notification--knockknock-available-p
           (knockknock-notify :message message-text
                              :icon "nf-cod-tools"
                              :duration (or seconds 3))))

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

;;; Progress bar notifications

(defun swift-notification-progress-start (&rest args)
  "Start a progress notification for a long-running operation.

ARGS is a property list with the following keys:
  :id      - Unique identifier for this progress (required)
  :title   - Title text (optional)
  :message - Message text (optional)
  :icon    - Nerd-icon name (optional, default: nf-dev-xcode)
  :total   - Total number of steps for step-based progress (optional)
  :percent - Initial percentage 0-100 for percent-based progress (optional)

Either :total or :percent must be provided.

If knockknock is not available, falls back to a simple message.

Examples:
  ;; Step-based progress
  (swift-notification-progress-start :id \\='build :title \"Building\" :total 5)

  ;; Percent-based progress
  (swift-notification-progress-start :id \\='compile :title \"Compiling\" :percent 0)"
  (let ((id (plist-get args :id))
        (title (plist-get args :title))
        (message-text (plist-get args :message))
        (icon (or (plist-get args :icon) "nf-dev-xcode"))
        (total (plist-get args :total))
        (percent (plist-get args :percent)))
    (if swift-notification--knockknock-available-p
        (knockknock-progress-create :id id
                                    :title title
                                    :message message-text
                                    :icon icon
                                    :total total
                                    :percent percent)
      ;; Fallback: show simple notification
      (swift-notification-send :message (or title message-text "Working...")))))

(defun swift-notification-progress-update (id &rest args)
  "Update a progress notification.

ID is the identifier passed to `swift-notification-progress-start'.

ARGS is a property list with the following keys:
  :current - Current step number (for step-based progress)
  :percent - Current percentage 0-100 (for percent-based progress)
  :message - Optional new message text
  :title   - Optional new title text

Examples:
  ;; Update step-based progress
  (swift-notification-progress-update \\='build :current 3)

  ;; Update percent-based progress with new message
  (swift-notification-progress-update \\='compile :percent 75 :message \"Linking...\")"
  (if swift-notification--knockknock-available-p
      (apply #'knockknock-progress-update id args)
    ;; Fallback: show simple notification with percent
    (let ((percent (plist-get args :percent))
          (current (plist-get args :current))
          (message-text (plist-get args :message)))
      (swift-notification-send
       :message (format "%s [%s]"
                        (or message-text "Working...")
                        (if percent
                            (format "%d%%" percent)
                          (format "%d" (or current 0))))))))

(defun swift-notification-progress-finish (id &optional message)
  "Finish a progress notification and show completion message.

ID is the identifier of the progress to close.
MESSAGE is an optional completion message to show briefly."
  (if swift-notification--knockknock-available-p
      (progn
        ;; Update to 100% first (triggers auto-close after duration)
        (knockknock-progress-update id :percent 100
                                    :message (or message "Complete"))
        ;; Note: knockknock will auto-close after knockknock-default-duration
        )
    ;; Fallback: show completion message
    (swift-notification-send :message (or message "Complete") :seconds 2)))

(defun swift-notification-progress-cancel (id)
  "Cancel/close a progress notification without showing completion.

ID is the identifier of the progress to close."
  (when swift-notification--knockknock-available-p
    (knockknock-progress-close id)))

(provide 'swift-notification)
;;; swift-notification.el ends here
