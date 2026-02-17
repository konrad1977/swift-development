;;; swiftui-preview.el --- SwiftUI preview support for swift-development -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: Swift, swiftui, preview, ios

;;; Commentary:

;; This module provides SwiftUI preview functionality for swift-development.el.
;; It uses dynamic target injection to build and capture SwiftUI previews
;; without requiring any additional packages in your project.
;;
;; Features:
;; - Zero configuration required
;; - Works with existing #Preview macros
;; - Supports Xcode projects, workspaces, and SPM packages
;; - Fast builds (only builds required modules)
;; - External screenshot capture via simctl
;;
;; Usage:
;; 1. Open a Swift file with a #Preview block
;; 2. M-x swiftui-preview-generate
;;
;; Requirements:
;; - Xcode command line tools
;; - Ruby with xcodeproj gem (install via M-x swiftui-preview-setup)
;; - iOS Simulator
;;
;; Directory Structure for saved previews:
;; .swift-development/
;; └── previews/
;;     ├── HomeView.png
;;     └── SettingsView.png

;;; Code:

(require 'cl-lib)

;; Forward declarations for functions from other modules
(declare-function swift-project-root "swift-project")
(declare-function swift-project-settings-get "swift-project-settings")
(declare-function xcode-project-project-root "xcode-project")
(declare-function xcode-project-scheme "xcode-project")
(declare-function xcode-project-build-folder "xcode-project")
(declare-function ios-simulator-simulator-name "ios-simulator")
(declare-function ios-simulator-simulator-id "ios-simulator")
(declare-function swiftui-preview-core-watch-for-file "swiftui-preview-core")

(defgroup swiftui-preview nil
  "SwiftUI preview support for swift-development."
  :group 'swift-development
  :prefix "swiftui-preview-")

(defcustom swiftui-preview-directory-name "swiftuipreview"
  "Directory name for preview images within .swift-development/."
  :type 'string
  :group 'swiftui-preview)

(defcustom swiftui-preview-poll-interval 0.5
  "Interval in seconds to poll for preview image."
  :type 'number
  :group 'swiftui-preview)

(defcustom swiftui-preview-timeout 30
  "Timeout in seconds to wait for preview image."
  :type 'number
  :group 'swiftui-preview)

(defcustom swiftui-preview-buffer-name "*SwiftUI Preview*"
  "Buffer name for displaying preview images."
  :type 'string
  :group 'swiftui-preview)

(defcustom swiftui-preview-overwrite-mode t
  "If non-nil, always overwrite 01.png. Otherwise create numbered files."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-debug nil
  "Enable debug messages for SwiftUI preview."
  :type 'boolean
  :group 'swiftui-preview)



(defcustom swiftui-preview-window-width 0.25
  "Width of preview window as fraction of frame width (0.0 to 1.0)."
  :type 'number
  :group 'swiftui-preview)



(defcustom swiftui-preview-auto-show-on-open t
  "If non-nil, automatically show existing preview when opening Swift file.
Preview files are named after the Swift file (e.g., HomeView.swift -> HomeView.png)."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-auto-generate-on-open nil
  "If non-nil, automatically generate preview if it doesn't exist when opening Swift file.
This will build and run the app automatically to create the preview.
Only works for files with .setupSwiftDevelopmentPreview() modifier."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-use-file-based-naming t
  "If non-nil, name preview files after the Swift file instead of using numbers.
E.g., HomeView.swift generates HomeView.png instead of 01.png."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-auto-update-on-save t
  "If non-nil, automatically regenerate preview when saving Swift file.
Only triggers if preview buffer is currently visible.
Disable this on slower machines to avoid frequent rebuilds."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-show-notifications t
  "If non-nil, show progress notifications during preview generation.
When nil, preview generation runs silently without progress updates.
This avoids potential UI overhead from frequent `redisplay' calls
during the preview pipeline.  Toggle interactively with
`swiftui-preview-toggle-notifications'."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-detect-setup-modifier t
  "If non-nil, detect .setupSwiftDevelopmentPreview() modifier for auto-generation."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-detect-preview-macro t
  "If non-nil, detect #Preview macros for auto-generation."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-detect-preview-provider t
  "If non-nil, detect PreviewProvider protocol for auto-generation."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-pin-mode nil
  "If non-nil, pin the preview window to the current preview.
When enabled, switching buffers will not hide or update the
preview window.  Toggle interactively with
`swiftui-preview-toggle-pin-mode'."
  :type 'boolean
  :group 'swiftui-preview)


(defcustom swiftui-preview-fit-to-window t
  "If non-nil, scale preview image to fit the window.
When enabled, the image is scaled to fit within the preview window's
dimensions while maintaining aspect ratio."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-max-width nil
  "Maximum width for preview image in pixels.
If nil and `swiftui-preview-fit-to-window' is t, uses window width.
If nil and `swiftui-preview-fit-to-window' is nil, uses 1000 pixels."
  :type '(choice (const :tag "Auto (fit to window)" nil)
                 (integer :tag "Max width in pixels"))
  :group 'swiftui-preview)

(defcustom swiftui-preview-max-height nil
  "Maximum height for preview image in pixels.
If nil and `swiftui-preview-fit-to-window' is t, uses window height.
If nil and `swiftui-preview-fit-to-window' is nil, no height limit."
  :type '(choice (const :tag "Auto (fit to window)" nil)
                 (integer :tag "Max height in pixels"))
  :group 'swiftui-preview)



(defvar swiftui-preview--poll-timer nil
  "Timer for polling preview image.")

(defvar swiftui-preview--poll-start-time nil
  "Start time for preview generation.")

(defvar swiftui-preview--current-preview-path nil
  "Path to the current preview image being generated.")

(defvar swiftui-preview--current-simulator-id nil
  "Simulator ID used for current preview.")

(defvar swiftui-preview--current-app-identifier nil
  "App identifier for current preview.")



(defvar swiftui-preview--last-preview-mtime nil
  "Last modification time of preview image.")

(defvar swiftui-preview--last-buffer nil
  "Last buffer that was active when preview was shown.")

(defvar swiftui-preview--last-display-time nil
  "Timestamp of last preview display operation.
Used to debounce buffer-change reactions after auto-show.")

(defvar swiftui-preview--last-build-failed nil
  "Non-nil if last preview build failed.
Used to retry preview generation after fixing errors.")

(defvar swiftui-preview--generate-all-active nil
  "Non-nil while `swiftui-preview-generate-all' is running.
Suppresses individual `swiftui-preview--display-image' calls so
that the final stacked display is not overwritten.")

(defvar-local swiftui-preview--image-paths nil
  "List of (NAME . PATH) pairs for multi-image preview display.
Set by `swiftui-preview--display-images', used by
`swiftui-preview--revert-multi-buffer' and `swiftui-preview--on-buffer-change'.")

;;; Directory Management

(defun swiftui-preview--directory (project-root)
  "Get the swiftuipreview directory path for PROJECT-ROOT."
  (expand-file-name
   swiftui-preview-directory-name
   (expand-file-name ".swift-development" project-root)))

(defun swiftui-preview--ensure-directory (project-root)
  "Ensure swiftuipreview directory exists for PROJECT-ROOT.
Returns the full path to the directory."
  (let ((dir (swiftui-preview--directory project-root)))
    (unless (file-exists-p dir)
      (make-directory dir t)
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Created directory: %s" dir)))
    dir))

(defun swiftui-preview--get-preview-name-for-buffer ()
  "Get preview filename for current buffer.
If swiftui-preview-use-file-based-naming is t, uses buffer filename without extension.
Otherwise returns nil to use numbered naming."
  (when (and swiftui-preview-use-file-based-naming
             (buffer-file-name))
    (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))


(defun swiftui-preview--next-preview-number (project-root)
  "Find the next available preview number in PROJECT-ROOT.
Returns 1 if swiftui-preview-overwrite-mode is t, otherwise finds next unused number."
  (if swiftui-preview-overwrite-mode
      1
    (let ((dir (swiftui-preview--directory project-root))
          (max-number 0))
      (when (file-exists-p dir)
        (dolist (file (directory-files dir))
          (when (string-match "^\\([0-9]+\\)\\.png$" file)
            (let ((num (string-to-number (match-string 1 file))))
              (setq max-number (max max-number num))))))
      (1+ max-number))))

(defun swiftui-preview--get-preview-path (project-root &optional preview-identifier)
  "Get the full path for preview in PROJECT-ROOT.
PREVIEW-IDENTIFIER can be either a number or a string (filename).
If nil, uses file-based naming if enabled, otherwise uses number."
  (let* ((preview-name (or preview-identifier
                          (swiftui-preview--get-preview-name-for-buffer)
                          (swiftui-preview--next-preview-number project-root)))
         (filename (if (numberp preview-name)
                      (format "%02d.png" preview-name)
                    (format "%s.png" preview-name))))
    (expand-file-name filename (swiftui-preview--directory project-root))))


;;; Preview Generation (New Dynamic Approach)

;; Load required modules
(require 'swiftui-preview-setup nil t)
(require 'swiftui-preview-dynamic nil t)
(require 'swiftui-preview-standalone nil t)

;;;###autoload
(defun swiftui-preview-generate (&optional preview-body preview-name)
  "Generate SwiftUI preview using the best available method.
When PREVIEW-BODY is provided, generate that specific preview body
instead of auto-detecting from the buffer.

PREVIEW-NAME, when non-nil, is used to create a unique output filename
\(e.g., \"Light\" produces \"HomeView-Light.png\").

Automatically detects project type and uses:
- Dynamic target injection for Xcode projects
- SPM project creation for Swift packages
- Standalone build for single Swift files

This is the new zero-config approach that doesn't require
any setup or additional packages in your project."
  (interactive)
  ;; Check dependencies
  (when (fboundp 'swiftui-preview-setup-check)
    (unless (swiftui-preview-setup-check)
      (if (fboundp 'swiftui-preview-setup-wizard)
          (swiftui-preview-setup-wizard)
        (user-error "SwiftUI Preview setup incomplete"))))
  
  ;; Dispatch to appropriate generator
  (cond
   ((fboundp 'swiftui-preview-dynamic-generate)
    (swiftui-preview-dynamic-generate preview-body preview-name))
   ((fboundp 'swiftui-preview-standalone-generate)
    (swiftui-preview-standalone-generate))
   (t
    (user-error "No preview generator available. Check your installation"))))

;;;###autoload
(defun swiftui-preview-generate-all ()
  "Generate previews for all #Preview blocks in current file.
Generates each preview sequentially, then displays all stacked
vertically in the preview buffer."
  (interactive)
  (let ((previews (swiftui-preview--detect-preview-definitions)))
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] generate-all: detected %d definitions"
               (length previews))
      (dolist (p previews)
        (message "[SwiftUI Preview]   type=%s name=%s body-len=%d"
                 (plist-get p :type)
                 (plist-get p :name)
                 (length (or (plist-get p :body) "")))))
    (if (null previews)
        (user-error "No #Preview blocks found")
      (let ((macro-previews (cl-remove-if-not
                             (lambda (p) (eq (plist-get p :type) 'preview-macro))
                             previews)))
        (if (<= (length macro-previews) 1)
            (swiftui-preview-generate)
          (message "Found %d previews. Generating all..." (length macro-previews))
          ;; Suppress individual display-image calls during chain
          (setq swiftui-preview--generate-all-active t)
          (when swiftui-preview-debug
            (message "[SwiftUI Preview] generate-all: flag SET, starting chain"))
          ;; Generate sequentially: each preview builds asynchronously,
          ;; so we chain them using timers to avoid overlapping builds.
          ;; After the last one, display all images stacked.
          (swiftui-preview--generate-chain macro-previews 0))))))

(defun swiftui-preview--generate-chain (previews index &optional source-buffer image-alist)
  "Generate preview at INDEX in PREVIEWS list, then chain to next.
Each preview generation is async.  After kicking off a build, we poll
for the output file to appear before starting the next preview.  This
avoids the problem where Preview N+1's capture pipeline terminates
Preview N's running app (they share the same bundle ID).

Each preview is saved with a unique filename based on its name
\(e.g., \"HomeView-Preview-1.png\").
SOURCE-BUFFER is the Swift buffer to generate from; defaults to
`current-buffer' on the first call and is captured for timer callbacks.
IMAGE-ALIST accumulates (NAME . PATH) pairs for final stacked display."
  (let ((buf (or source-buffer (current-buffer)))
        (images (or image-alist '())))
    (when (< index (length previews))
      (let* ((preview (nth index previews))
             (body (plist-get preview :body))
             (name (or (plist-get preview :name)
                       (format "Preview %d" (1+ index))))
             (sanitized (replace-regexp-in-string "[[:space:]]+" "-" name))
             (total (length previews))
             ;; Compute expected output path using the same function
             ;; that the build pipeline uses
             (expected-path
              (with-current-buffer buf
                (when-let* ((project-root (swift-project-root))
                            (filename (file-name-nondirectory
                                       (buffer-file-name))))
                  (if (fboundp 'swiftui-preview-dynamic--get-output-path)
                      (swiftui-preview-dynamic--get-output-path
                       project-root filename name)
                    ;; Fallback: mirror the dynamic path manually
                    (let ((preview-dir (expand-file-name
                                        ".swift-development/previews"
                                        project-root))
                          (base (file-name-sans-extension filename)))
                      (make-directory preview-dir t)
                      (expand-file-name
                       (format "%s-%s.png" base sanitized)
                       preview-dir)))))))
        (when swiftui-preview-debug
          (message "[SwiftUI Preview] chain[%d/%d]: name=%s sanitized=%s"
                   (1+ index) total name sanitized)
          (message "[SwiftUI Preview] chain[%d/%d]: expected-path=%s"
                   (1+ index) total expected-path)
          (message "[SwiftUI Preview] chain[%d/%d]: body-len=%d"
                   (1+ index) total (length (or body ""))))
        ;; Delete old output file so we can detect when the new one appears
        (when (and expected-path (file-exists-p expected-path))
          (delete-file expected-path)
          (when swiftui-preview-debug
            (message "[SwiftUI Preview] chain[%d/%d]: deleted old file"
                     (1+ index) total)))
        (message "Generating preview %d/%d: %s" (1+ index) total name)
        (with-current-buffer buf
          (swiftui-preview-generate body name))
        ;; Record expected output for final display
        (when expected-path
          (push (cons name expected-path) images))
        (if (< (1+ index) total)
            ;; More previews: wait for this one's output file before starting next
            (progn
              (when swiftui-preview-debug
                (message "[SwiftUI Preview] chain: waiting for %s before next"
                         (file-name-nondirectory (or expected-path "?"))))
              (swiftui-preview--wait-then-chain
               expected-path previews (1+ index) buf images))
          ;; Last preview kicked off -- watch for all images to appear
          (let ((final-images (nreverse images)))
            (when swiftui-preview-debug
              (message "[SwiftUI Preview] chain: last preview kicked off, watching for %d files"
                       (length final-images)))
            (swiftui-preview--watch-for-all-images
             final-images total buf)))))))

(defun swiftui-preview--wait-then-chain
    (expected-path previews next-index source-buffer image-alist)
  "Wait for EXPECTED-PATH to appear, then chain to NEXT-INDEX in PREVIEWS.
Uses OS file notifications via `swiftui-preview-core-watch-for-file'
for near-instant detection.  Falls back after `swiftui-preview-timeout'.
SOURCE-BUFFER and IMAGE-ALIST are passed through to `--generate-chain'."
  (let ((timeout (or swiftui-preview-timeout 30))
        (start-time (float-time)))
    (swiftui-preview-core-watch-for-file
     expected-path timeout
     (lambda (found-path)
       (let ((elapsed (- (float-time) start-time)))
         (cond
          ;; File appeared -- chain to next preview
          (found-path
           (when swiftui-preview-debug
             (message "[SwiftUI Preview] chain: file ready after %.1fs, starting next"
                      elapsed))
           ;; Small delay to let capture cleanup finish
           (run-with-timer
            1 nil
            (lambda ()
              (when (buffer-live-p source-buffer)
                (swiftui-preview--generate-chain
                 previews next-index source-buffer image-alist)))))
          ;; Timeout -- chain anyway so we don't get stuck
          (t
           (when swiftui-preview-debug
             (message "[SwiftUI Preview] chain: TIMEOUT waiting for %s, proceeding"
                      (file-name-nondirectory (or expected-path "?"))))
           (when (buffer-live-p source-buffer)
             (swiftui-preview--generate-chain
              previews next-index source-buffer image-alist)))))))))


(defun swiftui-preview--clear-generate-all-state ()
  "Clear generate-all flag and recompile cache after a delay."
  (run-with-timer 3 nil
                  (lambda ()
                    (setq swiftui-preview--generate-all-active nil)
                    (when (boundp 'swiftui-preview-dynamic--recompile-cache)
                      (when-let* ((cache swiftui-preview-dynamic--recompile-cache)
                                  (dir (plist-get cache :cache-dir)))
                        (when (file-directory-p dir)
                          (delete-directory dir t)))
                      (setq swiftui-preview-dynamic--recompile-cache nil))
                    (when swiftui-preview-debug
                      (message "[SwiftUI Preview] generate-all flag CLEARED")))))

(defun swiftui-preview--watch-for-all-images (image-alist total source-buffer)
  "Watch for all images in IMAGE-ALIST via OS file notifications.
TOTAL is the preview count for the completion message.
SOURCE-BUFFER is the Swift buffer to record as last-buffer.
Sets up one `swiftui-preview-core-watch-for-file' watcher per image.
When all have arrived (or timeout), displays the stacked result."
  (let* ((timeout (or swiftui-preview-timeout 30))
         (remaining (length image-alist))
         (arrived '())
         (done nil))
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] watch-all: starting, %d files, timeout=%ds"
               remaining timeout))
    (dolist (item image-alist)
      (let ((name (car item))
            (path (cdr item)))
        (swiftui-preview-core-watch-for-file
         path timeout
         (lambda (found-path)
           (unless done
             (if found-path
                 (progn
                   (push (cons name path) arrived)
                   (when swiftui-preview-debug
                     (message "[SwiftUI Preview] watch-all: %s arrived (%d/%d)"
                              name (length arrived) total))
                   ;; Check if all images have arrived
                   (when (= (length arrived) total)
                     (setq done t)
                     (when swiftui-preview-debug
                       (message "[SwiftUI Preview] watch-all: ALL files found"))
                     (swiftui-preview--display-images image-alist source-buffer)
                     (message "All %d previews generated" total)
                     (swiftui-preview--clear-generate-all-state)))
               ;; This particular file timed out
               (progn
                 (setq remaining (1- remaining))
                 (when swiftui-preview-debug
                   (message "[SwiftUI Preview] watch-all: %s TIMEOUT (%d remaining)"
                            name remaining))
                 ;; If all watchers resolved (arrived + timed out), show what we have
                 (when (= (+ (length arrived) (- total remaining)) total)
                   (setq done t)
                   (if arrived
                       (let ((existing (cl-remove-if-not
                                        (lambda (it) (file-exists-p (cdr it)))
                                        image-alist)))
                         (swiftui-preview--display-images existing source-buffer)
                         (message "%d/%d previews generated (some timed out)"
                                  (length existing) total))
                     (message "Preview generation timed out"))
                    (swiftui-preview--clear-generate-all-state)))))))))))


;;;###autoload
(defun swiftui-preview-select ()
  "Select which #Preview to generate when multiple exist."
  (interactive)
  (if (fboundp 'swiftui-preview-dynamic-select)
      (swiftui-preview-dynamic-select)
    (swiftui-preview-generate)))

;;; Polling and Display

(defun swiftui-preview--create-scaled-image (image-path &optional window)
  "Create an image from IMAGE-PATH scaled to fit WINDOW.
Reads file data directly to bypass Emacs file-based image cache.
Uses `swiftui-preview-fit-to-window', `swiftui-preview-max-width',
and `swiftui-preview-max-height' to determine scaling."
  (let* ((win (or window (selected-window)))
         (win-width (when (window-live-p win)
                      (- (window-body-width win t) 20)))  ; pixels, with margin
         (win-height (when (window-live-p win)
                       (- (window-body-height win t) 20)))
         (max-w (cond
                 (swiftui-preview-max-width swiftui-preview-max-width)
                 (swiftui-preview-fit-to-window (or win-width 400))
                 (t 1000)))
         (max-h (cond
                 (swiftui-preview-max-height swiftui-preview-max-height)
                 (swiftui-preview-fit-to-window (or win-height 800))
                 (t nil)))
         ;; Read raw file data to bypass file-path image caching
         (image-data (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally image-path)
                       (buffer-string))))
    (if max-h
        (create-image image-data nil t :max-width max-w :max-height max-h)
      (create-image image-data nil t :max-width max-w))))

(defun swiftui-preview--display-image (image-path)
  "Display preview IMAGE-PATH in Emacs.
Updates the current preview path to track which preview is being shown.
Skipped when `swiftui-preview--generate-all-active' is non-nil or
when the preview buffer is showing multi-image content."
  ;; During generate-all, suppress individual display calls
  (when swiftui-preview--generate-all-active
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] display-image SKIPPED (generate-all active): %s"
               (file-name-nondirectory image-path))))
  ;; Also skip if preview buffer is currently showing multi-image
  (when (and (not swiftui-preview--generate-all-active)
             (let ((pbuf (get-buffer swiftui-preview-buffer-name)))
               (and pbuf (buffer-local-value 'swiftui-preview--image-paths pbuf))))
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] display-image SKIPPED (multi-image shown): %s"
               (file-name-nondirectory image-path))))
  (unless (or swiftui-preview--generate-all-active
              (let ((pbuf (get-buffer swiftui-preview-buffer-name)))
                (and pbuf (buffer-local-value 'swiftui-preview--image-paths pbuf))))
    (let ((buffer (get-buffer-create swiftui-preview-buffer-name))
          (mtime (file-attribute-modification-time
                  (file-attributes image-path))))
      ;; Update current preview path to match what we're displaying
      (setq swiftui-preview--current-preview-path image-path)
      ;; Remember which buffer this preview is for
      (setq swiftui-preview--last-buffer (current-buffer))
      ;; Record display time for debouncing
      (setq swiftui-preview--last-display-time (current-time))

      ;; Display buffer first to get correct window dimensions
      (display-buffer buffer
                      `(display-buffer-in-side-window
                        (side . right)
                        (slot . 0)
                        (window-width . ,swiftui-preview-window-width)
                        (preserve-size . (t . nil))))

      ;; Now populate buffer with correctly scaled image
      (let ((preview-window (get-buffer-window buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; Clear image cache to force reload
            (clear-image-cache t)
            (insert-image (swiftui-preview--create-scaled-image
                           image-path preview-window))
            ;; Use special-mode as base instead of image-mode
            (special-mode)
            (setq-local swiftui-preview--image-path image-path)
            (setq-local swiftui-preview--last-mtime mtime)
            (setq-local revert-buffer-function
                        #'swiftui-preview--revert-buffer)
            ;; Set up keybindings
            (local-set-key (kbd "g") 'swiftui-preview-generate)
            (local-set-key (kbd "r") 'swiftui-preview--refresh-current)
            (local-set-key (kbd "q") 'quit-window)))))

    ;; Set up window resize hook for this buffer
    (add-hook 'window-size-change-functions
              #'swiftui-preview--on-window-resize)))

(defface swiftui-preview-label-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for preview name labels in multi-preview display."
  :group 'swiftui-preview)

(defun swiftui-preview--create-scaled-image-for-count
    (image-path window count)
  "Create an image from IMAGE-PATH scaled to fit WINDOW shared with COUNT images.
Like `swiftui-preview--create-scaled-image' but divides the available
height by COUNT so all previews fit without scrolling."
  (let* ((win (or window (selected-window)))
         (win-width (when (window-live-p win)
                      (- (window-body-width win t) 20)))
         (win-height (when (window-live-p win)
                       (- (window-body-height win t) 20)))
         (max-w (cond
                 (swiftui-preview-max-width swiftui-preview-max-width)
                 (swiftui-preview-fit-to-window (or win-width 400))
                 (t 1000)))
         ;; Divide height by count, minus space for labels (~30px each)
         (per-image-height (when win-height
                             (max 100 (/ (- win-height (* count 30))
                                         count))))
         (max-h (cond
                 (swiftui-preview-max-height swiftui-preview-max-height)
                 (swiftui-preview-fit-to-window (or per-image-height 400))
                 (t nil)))
         (image-data (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally image-path)
                       (buffer-string))))
    (if max-h
        (create-image image-data nil t :max-width max-w :max-height max-h)
      (create-image image-data nil t :max-width max-w))))

(defun swiftui-preview--display-images (image-alist &optional source-buffer)
  "Display multiple preview images stacked vertically.
IMAGE-ALIST is a list of (NAME . PATH) cons cells where NAME is
the display label and PATH is the image file path.  Only existing
files are shown.  Images are scaled to share the window height.
SOURCE-BUFFER, when non-nil, is recorded as `swiftui-preview--last-buffer'
so that `on-buffer-change' does not immediately hide the preview."
  (let* ((existing (cl-remove-if-not (lambda (item) (file-exists-p (cdr item)))
                                     image-alist))
         (count (length existing))
         (buffer (get-buffer-create swiftui-preview-buffer-name)))
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] display-images: %d images to show" count)
      (dolist (item existing)
        (message "[SwiftUI Preview]   %s -> %s" (car item) (cdr item))))
    (when existing
      ;; Record display time for debouncing
      (setq swiftui-preview--last-display-time (current-time))
      (setq swiftui-preview--last-buffer (or source-buffer (current-buffer)))

      ;; Display buffer first to get correct window dimensions
      (display-buffer buffer
                      `(display-buffer-in-side-window
                        (side . right)
                        (slot . 0)
                        (window-width . ,swiftui-preview-window-width)
                        (preserve-size . (t . nil))))

      (let ((preview-window (get-buffer-window buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (clear-image-cache t)
            (dolist (item existing)
              (let ((name (car item))
                    (path (cdr item)))
                ;; Insert label
                (insert (propertize (format " %s" name)
                                   'face 'swiftui-preview-label-face))
                (insert "\n")
                ;; Insert image scaled to share window height
                (insert-image
                 (swiftui-preview--create-scaled-image-for-count
                  path preview-window count))
                (insert "\n\n")))
            (special-mode)
            (setq-local swiftui-preview--image-paths existing)
            (setq-local revert-buffer-function
                        #'swiftui-preview--revert-multi-buffer)
            (local-set-key (kbd "g") 'swiftui-preview-generate)
            (local-set-key (kbd "r") 'swiftui-preview--refresh-current)
            (local-set-key (kbd "q") 'quit-window)
            (goto-char (point-min)))))

      (add-hook 'window-size-change-functions
                #'swiftui-preview--on-window-resize))))

(defun swiftui-preview--revert-multi-buffer (&rest _)
  "Revert the preview buffer when showing multiple images."
  (when-let* ((paths (buffer-local-value 'swiftui-preview--image-paths
                                         (current-buffer))))
    (let ((inhibit-read-only t)
          (win (get-buffer-window (current-buffer)))
          (count (length paths)))
      (erase-buffer)
      (clear-image-cache t)
      (dolist (item paths)
        (let ((name (car item))
              (path (cdr item)))
          (when (file-exists-p path)
            (insert (propertize (format " %s" name)
                                'face 'swiftui-preview-label-face))
            (insert "\n")
            (insert-image (swiftui-preview--create-scaled-image-for-count
                           path win count))
            (insert "\n\n"))))
      (goto-char (point-min)))))

(defun swiftui-preview--revert-buffer (&rest _)
  "Revert the preview buffer by reloading the image."
  ;; Check if this is a multi-image buffer
  (if (bound-and-true-p swiftui-preview--image-paths)
      (swiftui-preview--revert-multi-buffer)
    (when-let* ((image-path (buffer-local-value 'swiftui-preview--image-path
                                                (current-buffer))))
      (let ((inhibit-read-only t)
            (win (get-buffer-window (current-buffer))))
        (erase-buffer)
        (clear-image-cache t)
        (insert-image (swiftui-preview--create-scaled-image image-path win))
        (setq-local swiftui-preview--last-mtime
                    (file-attribute-modification-time
                     (file-attributes image-path)))))))

(defun swiftui-preview--refresh-current ()
  "Refresh the current preview image to fit window."
  (interactive)
  (revert-buffer))

(defvar swiftui-preview--resize-timer nil
  "Timer for debouncing window resize.")

(defun swiftui-preview--on-window-resize (frame)
  "Handle window resize for FRAME by refreshing preview if visible."
  (when-let* ((buffer (get-buffer swiftui-preview-buffer-name)))
    (when (get-buffer-window buffer frame)
      ;; Debounce resize events
      (when swiftui-preview--resize-timer
        (cancel-timer swiftui-preview--resize-timer))
      (setq swiftui-preview--resize-timer
            (run-with-idle-timer
             0.3 nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (bound-and-true-p swiftui-preview--image-path)
                     (swiftui-preview--revert-buffer))))))))))


(defun swiftui-preview--cleanup ()
  "Clean up preview resources."
  (when (and swiftui-preview--current-simulator-id
             swiftui-preview--current-app-identifier)
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] Terminating app: %s" swiftui-preview--current-app-identifier))
    (call-process "xcrun" nil nil nil "simctl" "terminate"
                 swiftui-preview--current-simulator-id
                 swiftui-preview--current-app-identifier)))



;;;###autoload
(defun swiftui-preview-cleanup ()
  "Manually clean up any injected preview targets."
  (interactive)
  (if (fboundp 'swiftui-preview-dynamic-cleanup)
      (swiftui-preview-dynamic-cleanup)
    (message "No cleanup needed")))

;;;###autoload
(defun swiftui-preview-toggle-debug ()
  "Toggle debug mode for SwiftUI preview."
  (interactive)
  (setq swiftui-preview-debug (not swiftui-preview-debug))
  (message "SwiftUI Preview debug mode: %s" (if swiftui-preview-debug "ON" "OFF")))

;;;###autoload
(defun swiftui-preview-toggle-pin-mode ()
  "Toggle pin mode for the preview window.
When enabled, the preview stays fixed when switching buffers."
  (interactive)
  (setq swiftui-preview-pin-mode (not swiftui-preview-pin-mode))
  (message "SwiftUI Preview pin mode: %s"
           (if swiftui-preview-pin-mode "ON (pinned)" "OFF")))

;;; Utility Commands

;;;###autoload
(defun swiftui-preview-clean-temp-files ()
  "Clean all generated *.PreviewWrappers.swift files in current project."
  (interactive)
  (let* ((project-root (swift-project-root))
         (count 0))
    (when project-root
      (dolist (file (directory-files-recursively project-root "\\.PreviewWrappers\\.swift$"))
        (condition-case err
            (progn
              (delete-file file)
              (setq count (1+ count))
              (when swiftui-preview-debug
                (message "[SwiftUI Preview] Deleted wrapper file: %s" file)))
          (file-error
           (when swiftui-preview-debug
             (message "[SwiftUI Preview] Could not delete %s: %s" file (error-message-string err))))))
      (if (> count 0)
          (message "Cleaned %d wrapper file(s)" count)
        (message "No wrapper files found")))))

;;;###autoload
(defun swiftui-preview-clear ()
  "Clear all preview images and wrapper files in current project."
  (interactive)
  (let* ((project-root (swift-project-root))
         (preview-dir (swiftui-preview--directory project-root)))
    (when (and project-root
               (y-or-n-p "Delete all previews and wrapper files? "))
      ;; Clear preview images
      (when (file-exists-p preview-dir)
        (dolist (file (directory-files preview-dir t "\\.png$"))
          (condition-case nil
              (delete-file file)
            (file-error nil))))
      ;; Clear wrapper files (*.PreviewWrappers.swift)
      (dolist (file (directory-files-recursively project-root "\\.PreviewWrappers\\.swift$"))
        (condition-case err
            (progn
              (delete-file file)
              (when swiftui-preview-debug
                (message "[SwiftUI Preview] Deleted wrapper file: %s" file)))
          (file-error
           (when swiftui-preview-debug
             (message "[SwiftUI Preview] Could not delete %s: %s" file (error-message-string err))))))
      (message "Cleared all preview images and wrapper files"))))

;;;###autoload
(defun swiftui-preview-show-directory ()
  "Open the preview directory in Dired."
  (interactive)
  (let* ((project-root (swift-project-root))
         (preview-dir (swiftui-preview--directory project-root)))
    (if (file-exists-p preview-dir)
        (dired preview-dir)
      (message "Preview directory does not exist yet: %s" preview-dir))))

;;;###autoload
(defun swiftui-preview-refresh ()
  "Refresh the currently displayed preview image."
  (interactive)
  (let ((buffer (get-buffer swiftui-preview-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (when (fboundp 'revert-buffer)
            (revert-buffer nil t)))
      (message "No preview buffer found"))))

;;;###autoload
(defun swiftui-preview-show-existing ()
  "Show existing preview for current Swift file if it exists."
  (interactive)
  (let* ((project-root (swift-project-root))
         (preview-path (swiftui-preview--get-preview-path project-root)))
    (if (and preview-path (file-exists-p preview-path))
        (progn
          (swiftui-preview--display-image preview-path)
          (message "Showing preview: %s" (file-name-nondirectory preview-path)))
      (message "No preview found for %s"
               (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 "this buffer")))))


(defun swiftui-preview--parse-balanced-braces (start-pos)
  "Parse balanced braces starting from START-POS.
Returns the position after the closing brace, or nil if unbalanced.
Handles nested braces, string literals (including multi-line), and comments."
  (save-excursion
    (goto-char start-pos)
    (let ((depth 1)
          (limit (point-max)))
      (forward-char 1) ; Skip opening brace
      (while (and (> depth 0) (< (point) limit))
        (cond
         ;; Multi-line string literal """..."""
         ((looking-at "\"\"\"")
          (forward-char 3)
          (while (and (not (looking-at "\"\"\"")) (< (point) limit))
            (if (looking-at "\\\\")
                (forward-char 2)
              (forward-char 1)))
          (when (looking-at "\"\"\"")
            (forward-char 3)))
         ;; Single-line string literal "..."
         ((looking-at "\"")
          (forward-char 1)
          (while (and (not (looking-at "\"")) (< (point) limit))
            (if (looking-at "\\\\")
                (forward-char 2)
              (forward-char 1)))
          (when (< (point) limit)
            (forward-char 1)))
         ;; Line comment //...
         ((looking-at "//")
          (end-of-line)
          (when (< (point) limit)
            (forward-char 1)))
         ;; Block comment /*...*/
         ((looking-at "/\\*")
          (forward-char 2)
          (while (and (not (looking-at "\\*/")) (< (point) limit))
            (forward-char 1))
          (when (looking-at "\\*/")
            (forward-char 2)))
         ;; Opening brace
         ((looking-at "{")
          (setq depth (1+ depth))
          (forward-char 1))
         ;; Closing brace
         ((looking-at "}")
          (setq depth (1- depth))
          (forward-char 1))
         ;; Any other character
         (t
          (forward-char 1))))
      (if (= depth 0)
          (point)
        nil))))

(defun swiftui-preview--extract-preview-body (start-pos)
  "Extract the body of a #Preview macro starting at opening brace START-POS.
Returns the body text without the surrounding braces, properly dedented.
Removes common leading whitespace from all lines (like Ruby's dedent)."
  (let ((end-pos (swiftui-preview--parse-balanced-braces start-pos)))
    (when end-pos
      (let* ((raw-body (buffer-substring-no-properties (1+ start-pos) (1- end-pos)))
             (lines (split-string raw-body "\n"))
             ;; Remove leading/trailing empty lines
             (trimmed-lines (progn
                              (while (and lines (string-empty-p (string-trim (car lines))))
                                (setq lines (cdr lines)))
                              (setq lines (nreverse lines))
                              (while (and lines (string-empty-p (string-trim (car lines))))
                                (setq lines (cdr lines)))
                              (nreverse lines))))
        (when trimmed-lines
          ;; Find minimum indentation (excluding empty lines)
          (let ((min-indent most-positive-fixnum))
            (dolist (line trimmed-lines)
              (unless (string-empty-p (string-trim line))
                (let ((indent (- (length line) (length (string-trim-left line)))))
                  (setq min-indent (min min-indent indent)))))
            ;; Dedent all lines
            (when (and (> min-indent 0) (< min-indent most-positive-fixnum))
              (setq trimmed-lines
                    (mapcar (lambda (line)
                              (if (>= (length line) min-indent)
                                  (substring line min-indent)
                                line))
                            trimmed-lines)))
            (string-join trimmed-lines "\n")))))))

(defun swiftui-preview--detect-preview-definitions ()
  "Detect all preview definitions in current buffer.
Returns a list of preview info plists with :type, :name, :body, and :traits keys.
Example: ((:type preview-macro :name \"Light\" :body \"HomeView()\" :traits nil))
Respects configuration variables for which types to detect.

Supports all #Preview variants:
- #Preview { ... }
- #Preview(\"Name\") { ... }
- #Preview(\"Name\", traits: .sizeThatFitsLayout) { ... }
- #Preview(traits: .landscapeLeft) { ... }"
  (save-excursion
    (let ((previews '()))

      ;; Detect .setupSwiftDevelopmentPreview() modifier
      (when swiftui-preview-detect-setup-modifier
        (goto-char (point-min))
        (when (re-search-forward "\\.setupSwiftDevelopmentPreview" nil t)
          (push (list :type 'setup-modifier :name nil :body nil :traits nil) previews)))

      ;; Detect #Preview macros with all parameter variants
      (when swiftui-preview-detect-preview-macro
        (goto-char (point-min))
        (let ((preview-counter 0))
          ;; Match #Preview followed by optional params and opening brace
          (while (re-search-forward "#Preview[[:space:]]*" nil t)
            (let ((preview-name nil)
                  (preview-traits nil)
                  (brace-start nil)
                  (body nil))
              ;; Check for parameters in parentheses
              (when (looking-at "(")
                (let ((params-start (point))
                      (params-end (save-excursion
                                    (forward-sexp)
                                    (point))))
                  (let ((params-str (buffer-substring-no-properties
                                     (1+ params-start) (1- params-end))))
                    ;; Extract name if present (first string literal)
                    (when (string-match "\"\\([^\"]+\\)\"" params-str)
                      (setq preview-name (match-string 1 params-str)))
                    ;; Extract traits if present
                    (when (string-match "traits:[[:space:]]*\\([^,)]+\\)" params-str)
                      (setq preview-traits (string-trim (match-string 1 params-str)))))
                  (goto-char params-end)))
              ;; Skip whitespace to find opening brace
              (skip-chars-forward " \t\n")
              (when (looking-at "{")
                (setq brace-start (point))
                (setq body (swiftui-preview--extract-preview-body brace-start))
                (when body
                  (setq preview-counter (1+ preview-counter))
                  (push (list :type 'preview-macro
                             :name (or preview-name
                                       (format "Preview %d" preview-counter))
                             :body (string-trim body)
                             :traits preview-traits)
                        previews)))))))

      ;; Detect PreviewProvider protocol
      (when swiftui-preview-detect-preview-provider
        (goto-char (point-min))
        (when (re-search-forward "struct +\\([A-Za-z0-9_]+\\)[^:]*:[^{]*PreviewProvider" nil t)
          (push (list :type 'preview-provider
                     :name (match-string 1)
                     :body nil
                     :traits nil)
                previews)))

      (nreverse previews))))

(defun swiftui-preview--count-previews ()
  "Count number of preview definitions in current buffer.
Returns the count of detected previews."
  (length (swiftui-preview--detect-preview-definitions)))

(defun swiftui-preview--get-first-preview-body ()
  "Get the body of the last #Preview macro in current buffer.
When multiple #Preview blocks exist, the last one is typically the most
complete (e.g., with NavigationView wrappers, color scheme modifiers).
Returns nil if no #Preview found."
  (let ((previews (swiftui-preview--detect-preview-definitions))
        (last-body nil))
    (dolist (preview previews)
      (when (eq (plist-get preview :type) 'preview-macro)
        (setq last-body (plist-get preview :body))))
    last-body))

(defun swiftui-preview--has-any-preview-p ()
  "Check if current buffer has any type of preview definition.
Checks for .setupSwiftDevelopmentPreview(), #Preview macros, or PreviewProvider."
  (> (swiftui-preview--count-previews) 0))

(defun swiftui-preview--auto-show-on-open ()
  "Automatically show preview if it exists for current file.
If preview doesn't exist and auto-generate is enabled, generate it.
Called from swift-mode-hook when swiftui-preview-auto-show-on-open is t."
  (when (and (or swiftui-preview-auto-show-on-open
                swiftui-preview-auto-generate-on-open)
             (buffer-file-name)
             (string-match-p "\\.swift$" (buffer-file-name)))
    ;; Use a longer delay to ensure project is fully initialized
    (run-with-idle-timer 1.0 nil
                         (lambda (buffer-name filename)
                           (when (buffer-live-p (get-buffer buffer-name))
                             (with-current-buffer buffer-name
                               (condition-case err
                                   (when-let* ((project-root (swift-project-root))
                                              (preview-path (swiftui-preview--get-preview-path project-root)))
                                     (cond
                                      ;; Preview exists - show it
                                      ((file-exists-p preview-path)
                                       (when swiftui-preview-auto-show-on-open
                                         (when swiftui-preview-debug
                                           (message "[SwiftUI Preview] Auto-showing existing preview for %s"
                                                    (file-name-nondirectory filename)))
                                         ;; Make sure we're in the right buffer before displaying
                                         (with-current-buffer buffer-name
                                           (swiftui-preview--display-image preview-path))))

                                      ;; Preview doesn't exist - generate if enabled and has any preview definition
                                      ((and swiftui-preview-auto-generate-on-open
                                            (swiftui-preview--has-any-preview-p))
                                       (when swiftui-preview-debug
                                         (message "[SwiftUI Preview] Auto-generating preview for %s (detected %d preview(s))"
                                                  (file-name-nondirectory filename)
                                                  (swiftui-preview--count-previews)))
                                       (swiftui-preview-generate))))
                                 (error
                                  (when swiftui-preview-debug
                                    (message "[SwiftUI Preview] Error in auto-show: %s"
                                             (error-message-string err))))))))
                         (buffer-name)
                         (buffer-file-name))))

(defun swiftui-preview--auto-update-on-save ()
  "Automatically regenerate preview when saving Swift file.
Only triggers if preview buffer is currently visible, a live preview
is running in the simulator, OR if the last build failed (to allow
automatic retry after fixing errors)."
  (when (and swiftui-preview-auto-update-on-save
             (buffer-file-name)
             (string-match-p "\\.swift$" (buffer-file-name))
             (or (get-buffer-window swiftui-preview-buffer-name)
                 ;; Live mode: preview runs in simulator, no Emacs buffer needed
                 (and (boundp 'swiftui-preview-dynamic-live-mode)
                      swiftui-preview-dynamic-live-mode
                      (boundp 'swiftui-preview-dynamic--live-simulator-udid)
                      swiftui-preview-dynamic--live-simulator-udid)
                 swiftui-preview--last-build-failed))
    ;; Capture current buffer context immediately
    (let* ((saved-buffer (current-buffer))
           (saved-file (buffer-file-name))
           (project-root (swift-project-root nil t))
           (expected-preview (when project-root
                              (swiftui-preview--get-preview-path project-root)))
           (preview-buffer (get-buffer swiftui-preview-buffer-name))
           ;; Check if the displayed preview matches current file
           (needs-update (or (not preview-buffer)
                            (not expected-preview)
                            (not (equal swiftui-preview--current-preview-path expected-preview)))))
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Auto-update on save:")
        (message "  Saved buffer: %s" (buffer-name saved-buffer))
        (message "  Saved file: %s" (file-name-nondirectory saved-file))
        (message "  Expected preview: %s" expected-preview)
        (message "  Displayed preview: %s" swiftui-preview--current-preview-path)
        (message "  Needs update: %s" needs-update))

      ;; Invalidate the recompile cache so the changed source file
      ;; is actually recompiled (the cache is keyed on source-file and
      ;; would otherwise skip recompilation for the same file).
      (when (boundp 'swiftui-preview-dynamic--recompile-cache)
        (setq swiftui-preview-dynamic--recompile-cache nil))

      ;; Generate from the saved buffer context.
      ;; If preview buffer is showing multi-image (from generate-all),
      ;; regenerate all previews to keep the stacked display.
      (let ((showing-multi (and preview-buffer
                                (buffer-local-value
                                 'swiftui-preview--image-paths
                                 preview-buffer))))
        (with-current-buffer saved-buffer
          (when swiftui-preview-debug
            (message "  Generating from buffer: %s (file: %s) multi: %s"
                     (buffer-name)
                     (file-name-nondirectory (buffer-file-name))
                     (if showing-multi "YES" "NO")))
          (if showing-multi
              (swiftui-preview-generate-all)
            (swiftui-preview-generate)))))))

(defun swiftui-preview--on-buffer-change ()
  "Automatically switch or hide preview when buffer change.
Switches preview if new buffer has one, hides preview window if it doesn't.
This is always enabled as it's fundamental preview behavior.
Does nothing while `swiftui-preview--generate-all-active' is non-nil.
Does not hide multi-image (generate-all) previews.
Skipped entirely when `swiftui-preview-pin-mode' is non-nil."
  (unless (or swiftui-preview--generate-all-active
              swiftui-preview-pin-mode)
  (let* ((preview-window (get-buffer-window swiftui-preview-buffer-name))
         (preview-buf (get-buffer swiftui-preview-buffer-name))
         (showing-multi (and preview-buf
                             (buffer-local-value 'swiftui-preview--image-paths
                                                 preview-buf)))
         (current-buf (current-buffer))
         (time-since-display (when swiftui-preview--last-display-time
                              (float-time (time-subtract (current-time)
                                                        swiftui-preview--last-display-time)))))
    (when (and swiftui-preview-debug preview-window)
      (message "[SwiftUI Preview] on-buffer-change: buf=%s showing-multi=%s flag=%s since=%.1fs"
               (buffer-name current-buf)
               (if showing-multi "YES" "NO")
               (if swiftui-preview--generate-all-active "YES" "NO")
               (or time-since-display -1.0)))
    (when (and preview-window
               ;; Don't touch multi-image previews
               (not showing-multi)
               ;; Only act if we're in a different buffer than last time
               (not (eq current-buf swiftui-preview--last-buffer))
               ;; Don't act if we're IN the preview buffer itself
               (not (eq current-buf preview-buf))
               ;; Don't act if we're in a minibuffer or special buffer
               (not (minibufferp current-buf))
               (not (string-prefix-p " " (buffer-name current-buf)))
               ;; Debounce: Don't act within 0.5s of last display
               (or (not time-since-display) (> time-since-display 0.5)))
      (if (and (buffer-file-name)
               (string-match-p "\\.swift$" (buffer-file-name)))
          ;; We're in a Swift file, check if it has a preview
          (let* ((project-root (swift-project-root nil t))
                 (preview-path (when project-root
                                (swiftui-preview--get-preview-path project-root))))
            (if (and preview-path (file-exists-p preview-path))
                ;; Preview exists, display it
                (progn
                  (when swiftui-preview-debug
                    (message "[SwiftUI Preview] Buffer changed, switching to preview for %s"
                             (file-name-nondirectory (buffer-file-name))))
                  (swiftui-preview--display-image preview-path))
              ;; No preview for this Swift file, hide window
              (when swiftui-preview-debug
                (message "[SwiftUI Preview] No preview for %s, hiding window"
                         (file-name-nondirectory (buffer-file-name))))
              (ignore-errors (delete-window preview-window))))
        ;; Not a Swift file, hide preview window
        (when swiftui-preview-debug
          (message "[SwiftUI Preview] Not a Swift file, hiding preview window"))
        (ignore-errors (delete-window preview-window)))))))

;;;###autoload
(defun swiftui-preview-test-auto-show ()
  "Test auto-show functionality in current buffer."
  (interactive)
  (if (not (and (buffer-file-name) (string-match-p "\\.swift$" (buffer-file-name))))
      (message "Not in a Swift file")
    (let* ((project-root (swift-project-root))
           (preview-path (when project-root
                          (swiftui-preview--get-preview-path project-root))))
      (message "Project root: %s" project-root)
      (message "Preview path: %s" preview-path)
      (message "Preview exists: %s" (and preview-path (file-exists-p preview-path)))
      (when (and preview-path (file-exists-p preview-path))
        (swiftui-preview--display-image preview-path)
        (message "Preview displayed!")))))

;;;###autoload
(defun swiftui-preview-enable-auto-show ()
  "Enable automatic preview display when opening Swift files."
  (interactive)
  (add-hook 'swift-development-mode-hook #'swiftui-preview--auto-show-on-open)
  (message "SwiftUI Preview auto-show enabled via swift-development-mode-hook"))

;;;###autoload
(defun swiftui-preview-disable-auto-show ()
  "Disable automatic preview display when opening Swift files."
  (interactive)
  (remove-hook 'swift-development-mode-hook #'swiftui-preview--auto-show-on-open)
  (message "SwiftUI Preview auto-show disabled"))

;;;###autoload
(defun swiftui-preview-enable-auto-update ()
  "Enable automatic preview update when saving Swift files."
  (interactive)
  (setq swiftui-preview-auto-update-on-save t)
  (message "SwiftUI Preview auto-update on save enabled"))

;;;###autoload
(defun swiftui-preview-disable-auto-update ()
  "Disable automatic preview update when saving Swift files."
  (interactive)
  (setq swiftui-preview-auto-update-on-save nil)
  (message "SwiftUI Preview auto-update on save disabled"))

;;;###autoload
(defun swiftui-preview-toggle-notifications ()
  "Toggle whether preview progress notifications are shown.
When disabled, preview generation runs silently without calling
`swift-notification-progress-start/update/finish/cancel'."
  (interactive)
  (setq swiftui-preview-show-notifications
        (not swiftui-preview-show-notifications))
  (message "SwiftUI Preview notifications: %s"
           (if swiftui-preview-show-notifications "ON" "OFF")))

;;;###autoload
(defun swiftui-preview-enable-auto-generate ()
  "Enable automatic preview generation when opening Swift files without previews."
  (interactive)
  (setq swiftui-preview-auto-generate-on-open t)
  (message "SwiftUI Preview auto-generate on open enabled"))

;;;###autoload
(defun swiftui-preview-disable-auto-generate ()
  "Disable automatic preview generation when opening Swift files."
  (interactive)
  (setq swiftui-preview-auto-generate-on-open nil)
  (message "SwiftUI Preview auto-generate on open disabled"))

;; Enable auto-show by default if configured
;; Use swift-development-mode-hook for unified hook across swift-mode and swift-ts-mode
(when swiftui-preview-auto-show-on-open
  (add-hook 'swift-development-mode-hook #'swiftui-preview--auto-show-on-open))

;; Always enable auto-update and auto-switch hooks (controlled by their respective variables)
(add-hook 'after-save-hook #'swiftui-preview--auto-update-on-save)
(add-hook 'buffer-list-update-hook #'swiftui-preview--on-buffer-change)

;;;###autoload
(defun swiftui-preview-setup ()
  "Open the SwiftUI Preview setup wizard.
Checks dependencies and guides through installation if needed."
  (interactive)
  (if (fboundp 'swiftui-preview-setup-wizard)
      (swiftui-preview-setup-wizard)
    (message "Setup wizard not available. Ensure swiftui-preview-setup.el is loaded.")))

;;; External Simulator Screenshot Capture

(defcustom swiftui-preview-capture-delay 2.0
  "Delay in seconds before capturing screenshot after app launch.
Used when capturing via simctl to allow the app to render."
  :type 'number
  :group 'swiftui-preview)

(defun swiftui-preview--find-simulator-udid (_simulator-name)
  "Find the UDID for a simulator.
_SIMULATOR-NAME is accepted for compatibility but ignored.
Delegates to `ios-simulator-simulator-identifier'."
  (swiftui-preview-core-find-simulator))

(defun swiftui-preview--get-booted-simulator ()
  "Get the UDID of the first booted simulator.
Delegates to `ios-simulator-booted-simulator'."
  (if (fboundp 'ios-simulator-booted-simulator)
      (ios-simulator-booted-simulator)
    (swiftui-preview-core-find-simulator)))

;;;###autoload
(defun swiftui-preview-capture-simulator (&optional output-path simulator-id)
  "Capture screenshot of current simulator state.
OUTPUT-PATH is the destination file (defaults to /tmp/simulator-screenshot.png).
SIMULATOR-ID is the simulator UDID (defaults to first booted simulator).
Returns the path to the captured screenshot or nil on failure."
  (interactive)
  (let* ((sim-id (or simulator-id
                     (swiftui-preview--get-booted-simulator)
                     "booted"))
         (output (or output-path
                     (expand-file-name "simulator-screenshot.png" temporary-file-directory))))
    (if (equal sim-id "booted")
        (unless (swiftui-preview--get-booted-simulator)
          (user-error "No simulator is currently booted"))
      (unless sim-id
        (user-error "Could not find simulator")))

    ;; Ensure output directory exists
    (make-directory (file-name-directory output) t)

    ;; Capture screenshot
    (let ((exit-code (call-process "xcrun" nil nil nil
                                   "simctl" "io" sim-id "screenshot" output)))
      (if (and (= exit-code 0) (file-exists-p output))
          (progn
            (message "Screenshot captured: %s" output)
            output)
        (message "Failed to capture simulator screenshot")
        nil))))

;;;###autoload
(defun swiftui-preview-capture-after-delay (&optional delay output-path simulator-id)
  "Wait DELAY seconds then capture simulator screenshot.
DELAY defaults to `swiftui-preview-capture-delay'.
OUTPUT-PATH and SIMULATOR-ID are passed to `swiftui-preview-capture-simulator'.
Displays the captured image in preview buffer."
  (interactive)
  (let ((delay-time (or delay swiftui-preview-capture-delay))
        (out-path (or output-path
                      (let ((project-root (swift-project-root)))
                        (when project-root
                          (swiftui-preview--get-preview-path project-root)))))
        (sim-id (or simulator-id
                    (swiftui-preview--get-booted-simulator))))

    (unless out-path
      (setq out-path (expand-file-name "preview-capture.png" temporary-file-directory)))

    (message "Capturing screenshot in %.1f seconds..." delay-time)
    (run-with-timer
     delay-time nil
     (lambda ()
       (let ((captured-path (swiftui-preview-capture-simulator out-path sim-id)))
         (when captured-path
           (swiftui-preview--display-image captured-path)
           (message "Preview captured and displayed")))))))

;;;###autoload
(defun swiftui-preview-capture-current ()
  "Immediately capture the current simulator state and display it.
Useful for debugging or checking the current UI state."
  (interactive)
  (let* ((project-root (swift-project-root))
         (output-path (if project-root
                         (expand-file-name
                          "capture.png"
                          (swiftui-preview--directory project-root))
                       (expand-file-name "capture.png" temporary-file-directory))))
    (when project-root
      (swiftui-preview--ensure-directory project-root))
    (let ((captured (swiftui-preview-capture-simulator output-path)))
      (when captured
        (swiftui-preview--display-image captured)))))

;;; Transient Menu

(require 'transient)

(defun swiftui-preview--live-mode-description ()
  "Return description for live mode toggle showing current state."
  (format "Toggle live mode (%s)"
          (if (bound-and-true-p swiftui-preview-dynamic-live-mode)
              "ON" "OFF")))

(defun swiftui-preview--pin-mode-description ()
  "Return description for pin mode toggle showing current state."
  (format "Toggle pin mode (%s)"
          (if swiftui-preview-pin-mode "ON" "OFF")))

;;;###autoload
(transient-define-prefix swiftui-preview-transient ()
  "SwiftUI Preview actions."
  ["Generate Preview"
   [("p" "Generate preview" swiftui-preview-generate)
    ("a" "Generate all in file" swiftui-preview-generate-all)
    ("s" "Select preview..." swiftui-preview-select)]]
  ["Capture Simulator"
   [("c" "Capture now" swiftui-preview-capture-current)
    ("C" "Capture after delay" swiftui-preview-capture-after-delay)]]
  ["View"
   [("r" "Refresh" swiftui-preview-refresh)
    ("e" "Show existing" swiftui-preview-show-existing)
    ("d" "Open directory" swiftui-preview-show-directory)]]
  ["Live Mode"
   [("L" swiftui-preview-dynamic-toggle-live-mode
     :description swiftui-preview--live-mode-description)
    ("R" "Refresh live" swiftui-preview-dynamic-refresh-live)
    ("K" "Stop live" swiftui-preview-dynamic-stop-live)]]
  ["Manage"
   [("x" "Clear window" swiftui-preview-clear)
    ("X" "Cleanup targets" swiftui-preview-cleanup)]]
  ["Settings"
   [("S" "Setup wizard" swiftui-preview-setup)
    ("g" "Toggle debug" swiftui-preview-toggle-debug)
    ("n" "Toggle notifications" swiftui-preview-toggle-notifications)
    ("P" swiftui-preview-toggle-pin-mode
     :description swiftui-preview--pin-mode-description)]]
  [("q" "Quit" transient-quit-one)])

(provide 'swiftui-preview)
;;; swiftui-preview.el ends here
