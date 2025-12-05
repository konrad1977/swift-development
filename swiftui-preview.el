;;; swiftui-preview.el --- SwiftUI preview support for swift-development -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Mikael Konradsson
;; Keywords: Swift, swiftui, preview, ios
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides SwiftUI preview functionality for swift-development.el.
;; It builds your app, launches it with a special flag, and displays a snapshot
;; of a specific view in Emacs.
;;
;; Usage:
;; 1. Add the SwiftDevelopmentPreview package to your project
;; 2. Add .setupSwiftDevelopmentPreview() to views you want to preview
;; 3. M-x swiftui-preview-generate (or C-c C-p)
;;
;; Directory Structure:
;; .swift-development/
;; └── swiftuipreview/
;;     ├── 01.png
;;     ├── 02.png
;;     └── 03.png

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

(defcustom swiftui-preview-use-separate-build-folder nil
  "If non-nil, use a separate build folder for preview builds.
This prevents preview builds from invalidating the main build cache.
Currently not working properly with xcodebuild."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-debug nil
  "Enable debug messages for SwiftUI preview."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-hot-reload-enabled nil
  "Enable hot reload mode to avoid rebuilding on every change.
When enabled, the app stays running and automatically updates
when code changes via Inject/InjectionIII."
  :type 'boolean
  :group 'swiftui-preview)

(defcustom swiftui-preview-hot-reload-refresh-interval 1.0
  "Interval in seconds to check for updated preview images during hot reload."
  :type 'number
  :group 'swiftui-preview)

(defcustom swiftui-preview-window-width 0.25
  "Width of preview window as fraction of frame width (0.0 to 1.0)."
  :type 'number
  :group 'swiftui-preview)

(defcustom swiftui-preview-hide-compilation-on-success t
  "If non-nil, keep compilation buffer hidden during builds.
Only shows the buffer if build fails with errors."
  :type 'boolean
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

(defcustom swiftui-preview-scale nil
  "Image scale factor for preview rendering (e.g., 1.0, 2.0, 3.0).
If nil, uses device's native scale. Lower values = smaller file size.
Examples: 1.0 = @1x, 2.0 = @2x (Retina), 3.0 = @3x (iPhone Pro)"
  :type '(choice (const :tag "Device native scale" nil)
                 (number :tag "Custom scale (1.0-3.0)"))
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

(defvar swiftui-preview--hot-reload-active nil
  "Whether hot reload is currently active.")

(defvar swiftui-preview--image-refresh-timer nil
  "Timer for refreshing preview image during hot reload.")

(defvar swiftui-preview--last-preview-mtime nil
  "Last modification time of preview image.")

(defvar swiftui-preview--last-buffer nil
  "Last buffer that was active when preview was shown.")

(defvar swiftui-preview--last-display-time nil
  "Timestamp of last preview display operation.
Used to debounce buffer-change reactions after auto-show.")

;;; Directory Management

(defun swiftui-preview--directory (project-root)
  "Get the swiftuipreview directory path for PROJECT-ROOT."
  (expand-file-name
   swiftui-preview-directory-name
   (swift-project-settings--directory project-root)))

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


;;; Preview Generation

;;;###autoload
(defun swiftui-preview-generate (&optional enable-hot-reload)
  "Generate SwiftUI preview for current project.
With prefix argument ENABLE-HOT-RELOAD, keep app running for hot reload.
Always generates preview for the currently active Swift buffer."
  (interactive "P")
  (let* ((project-root (swift-project-root))
         (hot-reload (or enable-hot-reload swiftui-preview-hot-reload-enabled))
         ;; Capture current buffer info immediately
         (current-buffer-name (buffer-name))
         (current-file-name (buffer-file-name)))
    (unless project-root
      (user-error "No Swift project found"))

    (unless current-file-name
      (user-error "Current buffer is not visiting a file"))

    ;; Cancel any existing poll timer
    (when swiftui-preview--poll-timer
      (cancel-timer swiftui-preview--poll-timer)
      (setq swiftui-preview--poll-timer nil))

    ;; Save all buffers
    (save-some-buffers t)

    ;; === ZERO-CONFIG: Auto-install package and create registry ===
    (let ((view-name (file-name-sans-extension (file-name-nondirectory current-file-name))))
      ;; Ensure package is installed (copies template and adds to Package.swift if needed)
      (swiftui-preview--ensure-package-installed project-root)

      ;; Ensure PreviewRegistry.swift exists (creates if missing)
      (swiftui-preview--auto-create-registry-if-missing project-root view-name))

    ;; Ensure directory exists
    (swiftui-preview--ensure-directory project-root)

    ;; Get preview path based on CURRENT buffer (not any cached value)
    (setq swiftui-preview--current-preview-path
          (swiftui-preview--get-preview-path project-root)
          swiftui-preview--hot-reload-active hot-reload)

    (when swiftui-preview-debug
      (message "[SwiftUI Preview] Current file: %s" (file-name-nondirectory current-file-name))
      (message "[SwiftUI Preview] Preview path: %s" swiftui-preview--current-preview-path)
      (message "[SwiftUI Preview] Hot reload: %s" hot-reload))

    ;; Delete old preview file if it exists
    (when (file-exists-p swiftui-preview--current-preview-path)
      (condition-case nil
          (delete-file swiftui-preview--current-preview-path)
        (file-error nil)))

    ;; Get view name for dynamic registry update
    (let* ((view-name (file-name-sans-extension
                       (file-name-nondirectory swiftui-preview--current-preview-path))))

      (when swiftui-preview-debug
        (message "[SwiftUI Preview] View name: %s" view-name))

      ;; Update PreviewRegistry.swift BEFORE rebuild check
      (swiftui-preview--update-preview-registry-for-current-view
       project-root view-name nil)

      (if hot-reload
          (message "Updating preview for %s with hot reload..."
                   (file-name-nondirectory current-file-name))
        (message "Updating preview for %s..."
                 (file-name-nondirectory current-file-name)))

      ;; Small delay to ensure file system timestamp is updated
      ;; This ensures rebuild detection sees the PreviewRegistry.swift change
      ;; IMPORTANT: Capture buffer and buffer-local variables before going async
      (let ((preview-path swiftui-preview--current-preview-path)
            (captured-project-root project-root)
            (captured-buffer (current-buffer)))
        (run-with-timer 0.1 nil
                        (lambda ()
                          ;; Restore buffer context for buffer-local variables (scheme, config, etc.)
                          (with-current-buffer captured-buffer
                            ;; Build project with captured variables
                            (swiftui-preview--build-project
                             :project-root captured-project-root
                             :callback (lambda ()
                                         ;; Restore context and launch with correct preview path
                                         (with-current-buffer captured-buffer
                                           (let ((swiftui-preview--current-preview-path preview-path))
                                             (swiftui-preview--launch-all-previews captured-project-root))))))))))))

(cl-defun swiftui-preview--build-project (&key project-root callback)
  "Build project for preview with PROJECT-ROOT.
Calls CALLBACK on successful build.
Uses fast rebuild detection to skip unnecessary rebuilds."
  (let* ((default-directory project-root)
         (app-path (swift-development-get-built-app-path)))

    ;; Check if rebuild is needed using fast detection
    (if (and app-path
             (file-exists-p app-path)
             (not (swift-development-needs-rebuild-p)))
        ;; App is up-to-date, skip build
        (progn
          (when swiftui-preview-debug
            (message "[SwiftUI Preview] App is up-to-date, skipping rebuild"))
          (message "Preview: App up-to-date, launching...")
          (funcall callback))

      ;; Need to rebuild
      (let* ((simulator-id (or current-simulator-id
                              (ios-simulator-simulator-identifier)))
             ;; Set linker flags for hot reload only if needed
             (xcode-build-config-other-ld-flags
              (if swiftui-preview--hot-reload-active
                  '("-Xlinker" "-interposable")
                xcode-build-config-other-ld-flags))
             ;; Keep cached build command for speed
             (build-command (xcode-build-config-build-app-command
                            :sim-id simulator-id)))

        (when swiftui-preview-debug
          (message "[SwiftUI Preview] Rebuild needed")
          (message "[SwiftUI Preview] Build command: %s" build-command))

        ;; Temporarily configure compilation buffer to not show window
        (let ((display-buffer-alist
               (if swiftui-preview-hide-compilation-on-success
                   (cons '("\\*compilation\\*" (display-buffer-no-window))
                         display-buffer-alist)
                 display-buffer-alist)))
          (compile build-command))

        ;; Wait for compilation to finish
        (let ((compilation-buffer (get-buffer "*compilation*")))
          (when compilation-buffer
            (with-current-buffer compilation-buffer
              (add-hook 'compilation-finish-functions
                        (lambda (buffer status)
                          (when (string-match "finished" status)
                            (message "Preview updated, launching...")
                            (funcall callback))
                          (when (string-match "exited abnormally" status)
                            (message "Build failed. Check *compilation* buffer for details.")
                            ;; Show compilation buffer on failure
                            (display-buffer buffer)))
                        nil t))))))))

(defun swiftui-preview--update-preview-registry-for-current-view (project-root view-name &optional wrapper-names)
  "Update PreviewRegistry.swift to register view(s) in PROJECT-ROOT.
If WRAPPER-NAMES is provided, registers all wrapper views.
Otherwise registers VIEW-NAME directly."
  (let* ((swift-files (condition-case err
                          (directory-files-recursively
                           project-root
                           "\\.swift$"
                           nil
                           (lambda (dir)
                             ;; Skip build directories and hidden directories
                             (not (string-match-p "\\(?:\\.build\\|DerivedData\\|Pods\\|/\\..+\\)" dir))))
                        (file-missing
                         ;; If we get a file-missing error, try to continue with what we can find
                         (when swiftui-preview-debug
                           (message "[SwiftUI Preview] Warning: Could not access some directories: %s" (error-message-string err)))
                         ;; Return empty list and try to find PreviewRegistry manually
                         nil)))
         (registry-file nil))
    ;; Find PreviewRegistry.swift
    (if swift-files
        ;; Use directory scan if it succeeded
        (dolist (file swift-files)
          (when (string-match-p "PreviewRegistry\\.swift$" file)
            (setq registry-file file)))
      ;; Fallback: manually check common locations
      (let ((possible-paths
             (list
              (expand-file-name "PreviewRegistry.swift" project-root)
              (expand-file-name "testpreview/PreviewRegistry.swift" project-root)
              ;; Add more common paths as needed
              )))
        (dolist (path possible-paths)
          (when (and (not registry-file) (file-exists-p path))
            (setq registry-file path)))))

    (when registry-file
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Updating PreviewRegistry at: %s" registry-file)
        (message "[SwiftUI Preview] Wrapper names to register: %S" wrapper-names))

      (let ((registration-code
             (if wrapper-names
                 ;; Register all wrapper views with setupSwiftDevelopmentPreview
                 ;; The closure must return the same instance (self), not a new one
                 (mapconcat
                  (lambda (wrapper-name)
                    (format "    ViewRegistry.shared.register(name: \"%s\") {\n        let view = %s()\n        return view.setupSwiftDevelopmentPreview() { view }\n    }"
                            wrapper-name wrapper-name))
                  wrapper-names
                  "\n")
               ;; Register single view
               (format "    ViewRegistry.shared.register(name: \"%s\") { %s() }"
                       view-name view-name))))

        (with-temp-file registry-file
          (insert (format "//
//  PreviewRegistry.swift
//  Auto-generated by swiftui-preview-generate
//
//  This file dynamically registers the currently previewed view(s).
//

import SwiftUI
import SwiftDevelopmentPreview

/// Register view(s) for preview
func registerAllViewsForPreview() {
    guard SwiftDevelopmentPreview.isInPreview else { return }

%s
}
" registration-code)))
        (when swiftui-preview-debug
          (if wrapper-names
              (message "[SwiftUI Preview] Updated PreviewRegistry.swift with wrapper views: %s"
                       (string-join wrapper-names ", "))
            (message "[SwiftUI Preview] Updated PreviewRegistry.swift for view: %s" view-name)))))))


(defun swiftui-preview--launch-all-previews (project-root)
  "Launch app for preview in current buffer for PROJECT-ROOT."
  ;; Simple single preview launch
  (swiftui-preview--launch-preview project-root))


(defun swiftui-preview--launch-preview (project-root)
  "Launch app with preview flag for PROJECT-ROOT."
  (condition-case err
      (let* ((default-directory project-root)
             (simulator-id (or current-simulator-id
                              (ios-simulator-simulator-identifier)))
             (app-identifier (or xcode-project--current-app-identifier
                                (xcode-project-fetch-or-load-app-identifier)))
             ;; Get build folder from settings
             (settings (swift-project-settings-load project-root))
             (build-folder (or (plist-get settings :build-folder)
                              (expand-file-name
                               (format ".build/Build/Products/Debug-iphonesimulator/")
                               project-root)))
             (app-name (file-name-base (xcode-project-scheme)))
             (target-preview-path swiftui-preview--current-preview-path)
             ;; Get view name from the preview path (e.g., HomeView.png -> HomeView)
             (view-name (file-name-sans-extension
                        (file-name-nondirectory target-preview-path)))
             (preview-args (append (list "--swift-development-preview"
                                        (format "--preview-path=%s" target-preview-path)
                                        (format "--preview-view=%s" view-name))
                                  ;; Add scale argument if configured
                                  (when swiftui-preview-scale
                                    (list (format "--preview-scale=%s" swiftui-preview-scale))))))

        (when swiftui-preview-debug
          (message "[SwiftUI Preview] Launching with view name: %s" view-name))

        (setq swiftui-preview--current-simulator-id simulator-id
              swiftui-preview--current-app-identifier app-identifier)

        (when swiftui-preview-debug
          (message "[SwiftUI Preview] Installing app: %s from build-folder: %s"
                   app-name build-folder)
          (message "[SwiftUI Preview] Simulator ID: %s" simulator-id)
          (message "[SwiftUI Preview] View name: %s" view-name)
          (message "[SwiftUI Preview] Preview args: %s" preview-args))

        ;; Use ios-simulator-install-app for proper installation
        (ios-simulator-install-app
         :simulatorID simulator-id
         :build-folder build-folder
         :appname app-name
         :callback (lambda ()
                     (when swiftui-preview-debug
                       (message "[SwiftUI Preview] Installation complete, launching with preview args..."))

                     ;; Launch app with preview flags
                     (let ((launch-command (append (list "xcrun" "simctl" "launch"
                                                        "--terminate-running-process"
                                                        simulator-id
                                                        app-identifier)
                                                  preview-args)))
                       (when swiftui-preview-debug
                         (message "[SwiftUI Preview] Launch command: %s"
                                  (mapconcat #'identity launch-command " ")))

                       (apply #'start-process "swiftui-preview-launch" nil launch-command)

                       ;; Start polling for preview image
                       (setq swiftui-preview--poll-start-time (current-time))
                       (message "Waiting for preview generation...")
                       (swiftui-preview--start-polling)))))
    (error
     (message "Error launching preview: %s" (error-message-string err)))))

;;; Polling and Display

(defun swiftui-preview--start-polling ()
  "Start polling for preview image."
  (setq swiftui-preview--poll-timer
        (run-with-timer swiftui-preview-poll-interval
                       swiftui-preview-poll-interval
                       #'swiftui-preview--poll-for-image)))

(defun swiftui-preview--poll-for-image ()
  "Poll for preview image. Called by timer."
  (let ((elapsed (float-time (time-subtract (current-time)
                                            swiftui-preview--poll-start-time))))
    (cond
     ;; Timeout
     ((> elapsed swiftui-preview-timeout)
      (cancel-timer swiftui-preview--poll-timer)
      (setq swiftui-preview--poll-timer nil)
      (swiftui-preview--cleanup)
      (message "Preview generation timed out after %d seconds. Check that .setupSwiftDevelopmentPreview() is added to your view."
               swiftui-preview-timeout))

     ;; Image found
     ((file-exists-p swiftui-preview--current-preview-path)
      (cancel-timer swiftui-preview--poll-timer)
      (setq swiftui-preview--poll-timer nil)
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Preview image found after %.1f seconds" elapsed))

      ;; Display the preview image
      (swiftui-preview--display-image swiftui-preview--current-preview-path)

      ;; Handle hot reload or cleanup
      (if swiftui-preview--hot-reload-active
          (progn
            (swiftui-preview--start-hot-reload-timer)
            (message "Preview generated! Hot reload active - edit code and save to see updates."))
        (swiftui-preview--cleanup)
        (message "Preview generated successfully: %s" swiftui-preview--current-preview-path)))

     ;; Keep waiting
     (t
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Polling... %.1fs elapsed" elapsed))))))

(defun swiftui-preview--create-scaled-image (image-path &optional window)
  "Create an image from IMAGE-PATH scaled to fit WINDOW.
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
                 (t nil))))
    (if max-h
        (create-image image-path nil nil :max-width max-w :max-height max-h)
      (create-image image-path nil nil :max-width max-w))))

(defun swiftui-preview--display-image (image-path)
  "Display preview IMAGE-PATH in Emacs.
Updates the current preview path to track which preview is being shown."
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
          (insert-image (swiftui-preview--create-scaled-image image-path preview-window))
          ;; Use special-mode as base instead of image-mode to avoid resize conflicts
          (special-mode)
          (setq-local swiftui-preview--image-path image-path)
          (setq-local swiftui-preview--last-mtime mtime)
          (setq-local revert-buffer-function #'swiftui-preview--revert-buffer)
          ;; Set up keybindings
          (local-set-key (kbd "g") 'swiftui-preview-generate)
          (local-set-key (kbd "r") 'swiftui-preview--refresh-current)
          (local-set-key (kbd "q") 'quit-window)))))

  ;; Set up window resize hook for this buffer
  (add-hook 'window-size-change-functions #'swiftui-preview--on-window-resize))

(defun swiftui-preview--revert-buffer (&rest _)
  "Revert the preview buffer by reloading the image."
  (when-let ((image-path (buffer-local-value 'swiftui-preview--image-path (current-buffer))))
    (let ((inhibit-read-only t)
          (win (get-buffer-window (current-buffer))))
      (erase-buffer)
      (clear-image-cache t)
      (insert-image (swiftui-preview--create-scaled-image image-path win))
      (setq-local swiftui-preview--last-mtime
                  (file-attribute-modification-time
                   (file-attributes image-path))))))

(defun swiftui-preview--refresh-current ()
  "Refresh the current preview image to fit window."
  (interactive)
  (revert-buffer))

(defvar swiftui-preview--resize-timer nil
  "Timer for debouncing window resize.")

(defun swiftui-preview--on-window-resize (frame)
  "Handle window resize for FRAME by refreshing preview if visible."
  (when-let ((buffer (get-buffer swiftui-preview-buffer-name)))
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
  "Clean up preview resources.
Don't terminate app if hot reload is active."
  (when (and swiftui-preview--current-simulator-id
             swiftui-preview--current-app-identifier
             (not swiftui-preview--hot-reload-active))
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] Terminating app: %s" swiftui-preview--current-app-identifier))
    (call-process "xcrun" nil nil nil "simctl" "terminate"
                 swiftui-preview--current-simulator-id
                 swiftui-preview--current-app-identifier)))

;;; Hot Reload Functions

(defun swiftui-preview--start-hot-reload-timer ()
  "Start timer to refresh preview image during hot reload."
  (when swiftui-preview--image-refresh-timer
    (cancel-timer swiftui-preview--image-refresh-timer))

  (setq swiftui-preview--image-refresh-timer
        (run-with-timer swiftui-preview-hot-reload-refresh-interval
                       swiftui-preview-hot-reload-refresh-interval
                       #'swiftui-preview--refresh-image-if-changed))

  (when swiftui-preview-debug
    (message "[SwiftUI Preview] Hot reload timer started (interval: %.1fs)"
             swiftui-preview-hot-reload-refresh-interval)))

(defun swiftui-preview--refresh-image-if-changed ()
  "Refresh preview image if file has been modified."
  (when (and swiftui-preview--current-preview-path
             (file-exists-p swiftui-preview--current-preview-path))
    (let* ((buffer (get-buffer swiftui-preview-buffer-name))
           (file-mtime (file-attribute-modification-time
                       (file-attributes swiftui-preview--current-preview-path)))
           (last-mtime (when buffer
                        (buffer-local-value 'swiftui-preview--last-mtime buffer))))
      (when (and buffer last-mtime (time-less-p last-mtime file-mtime))
        (when swiftui-preview-debug
          (message "[SwiftUI Preview] Image updated, refreshing..."))
        (with-current-buffer buffer
          (revert-buffer nil t))
        (message "Preview updated!")))))

;;;###autoload
(defun swiftui-preview-generate-with-hot-reload ()
  "Generate SwiftUI preview with hot reload enabled.
Keep app running and automatically update preview when code changes."
  (interactive)
  (swiftui-preview-generate t))

;;;###autoload
(defun swiftui-preview-toggle-debug ()
  "Toggle debug mode for SwiftUI preview."
  (interactive)
  (setq swiftui-preview-debug (not swiftui-preview-debug))
  (message "SwiftUI Preview debug mode: %s" (if swiftui-preview-debug "ON" "OFF")))

;;;###autoload
(defun swiftui-preview-stop-hot-reload ()
  "Stop hot reload and terminate app."
  (interactive)
  (when swiftui-preview--image-refresh-timer
    (cancel-timer swiftui-preview--image-refresh-timer)
    (setq swiftui-preview--image-refresh-timer nil))
  (setq swiftui-preview--hot-reload-active nil)
  ;; Now cleanup will terminate the app
  (when (and swiftui-preview--current-simulator-id
             swiftui-preview--current-app-identifier)
    (when swiftui-preview-debug
      (message "[SwiftUI Preview] Terminating app: %s" swiftui-preview--current-app-identifier))
    (call-process "xcrun" nil nil nil "simctl" "terminate"
                 swiftui-preview--current-simulator-id
                 swiftui-preview--current-app-identifier))
  (message "Hot reload stopped"))

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


(defun swiftui-preview--has-preview-setup-p ()
  "Check if current buffer has .setupSwiftDevelopmentPreview() modifier."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "setupSwiftDevelopmentPreview" nil t)))

(defun swiftui-preview--parse-balanced-braces (start-pos)
  "Parse balanced braces starting from START-POS.
Returns the position after the closing brace, or nil if unbalanced."
  (save-excursion
    (goto-char start-pos)
    (let ((depth 1)
          (limit (point-max)))
      (forward-char 1) ; Skip opening brace
      (while (and (> depth 0) (< (point) limit))
        (cond
         ((looking-at "{")
          (setq depth (1+ depth))
          (forward-char 1))
         ((looking-at "}")
          (setq depth (1- depth))
          (forward-char 1))
         ((looking-at "\"")
          ;; Skip string literals
          (forward-char 1)
          (while (and (not (looking-at "\"")) (< (point) limit))
            (if (looking-at "\\\\")
                (forward-char 2)
              (forward-char 1)))
          (forward-char 1))
         (t
          (forward-char 1))))
      (if (= depth 0)
          (point)
        nil))))

(defun swiftui-preview--extract-preview-body (start-pos)
  "Extract the body of a #Preview macro starting at opening brace START-POS.
Returns the body text without the surrounding braces."
  (let ((end-pos (swiftui-preview--parse-balanced-braces start-pos)))
    (when end-pos
      (buffer-substring-no-properties (1+ start-pos) (1- end-pos)))))

(defun swiftui-preview--detect-preview-definitions ()
  "Detect all preview definitions in current buffer.
Returns a list of preview info plists with :type, :name, and :body keys.
Example: ((:type preview-macro :name \"Light\" :body \"HomeView()\"))
Respects configuration variables for which types to detect."
  (save-excursion
    (let ((previews '()))

      ;; Detect .setupSwiftDevelopmentPreview() modifier
      (when swiftui-preview-detect-setup-modifier
        (goto-char (point-min))
        (when (re-search-forward "\\.setupSwiftDevelopmentPreview" nil t)
          (push (list :type 'setup-modifier :name nil :body nil) previews)))

      ;; Detect #Preview macros with optional names and extract body
      (when swiftui-preview-detect-preview-macro
        (goto-char (point-min))
        (while (re-search-forward "#Preview\\(?:[[:space:]]*(\"\\([^\"]+\\)\")\\)?[[:space:]]*{" nil t)
          (let* ((preview-name (match-string 1))
                 (brace-start (1- (point)))
                 (body (swiftui-preview--extract-preview-body brace-start)))
            (when body
              (push (list :type 'preview-macro
                         :name (or preview-name "Preview")
                         :body (string-trim body))
                    previews)))))

      ;; Detect PreviewProvider protocol
      (when swiftui-preview-detect-preview-provider
        (goto-char (point-min))
        (when (re-search-forward "struct +\\([A-Za-z0-9_]+\\)[^:]*:[^{]*PreviewProvider" nil t)
          (push (list :type 'preview-provider
                     :name (match-string 1)
                     :body nil)
                previews)))

      (nreverse previews))))

(defun swiftui-preview--count-previews ()
  "Count number of preview definitions in current buffer.
Returns the count of detected previews."
  (length (swiftui-preview--detect-preview-definitions)))

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
Only triggers if preview buffer is currently visible."
  (when (and swiftui-preview-auto-update-on-save
             (buffer-file-name)
             (string-match-p "\\.swift$" (buffer-file-name))
             (get-buffer-window swiftui-preview-buffer-name))
    ;; Capture current buffer context immediately
    (let* ((saved-buffer (current-buffer))
           (saved-file (buffer-file-name))
           (project-root (swift-project-root))
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

      ;; Always generate from the saved buffer context
      (with-current-buffer saved-buffer
        (when swiftui-preview-debug
          (message "  Generating from buffer: %s (file: %s)"
                   (buffer-name)
                   (file-name-nondirectory (buffer-file-name))))
        (swiftui-preview-generate)))))

(defun swiftui-preview--on-buffer-change ()
  "Automatically switch or hide preview when buffer change.
Switches preview if new buffer has one, hides preview window if it doesn't.
This is always enabled as it's fundamental preview behavior."
  (let ((preview-window (get-buffer-window swiftui-preview-buffer-name))
        (current-buf (current-buffer))
        (time-since-display (when swiftui-preview--last-display-time
                             (float-time (time-subtract (current-time)
                                                       swiftui-preview--last-display-time)))))
    (when (and preview-window
               ;; Only act if we're in a different buffer than last time
               (not (eq current-buf swiftui-preview--last-buffer))
               ;; Don't act if we're IN the preview buffer itself
               (not (eq current-buf (get-buffer swiftui-preview-buffer-name)))
               ;; Don't act if we're in a minibuffer or special buffer
               (not (minibufferp current-buf))
               (not (string-prefix-p " " (buffer-name current-buf)))
               ;; Debounce: Don't act within 0.5s of last display
               (or (not time-since-display) (> time-since-display 0.5)))
      (if (and (buffer-file-name)
               (string-match-p "\\.swift$" (buffer-file-name)))
          ;; We're in a Swift file, check if it has a preview
          (let* ((project-root (swift-project-root))
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
        (ignore-errors (delete-window preview-window))))))

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
(defun swiftui-preview-update-view-registry ()
  "Automatically update PreviewRegistry.swift based on all View structs in project."
  (interactive)
  (let* ((project-root (swift-project-root))
         (swift-files (directory-files-recursively project-root "\\.swift$"))
         (view-names nil)
         (registry-file nil))

    (unless project-root
      (user-error "No Swift project found"))

    ;; Find PreviewRegistry.swift
    (dolist (file swift-files)
      (when (string-match-p "PreviewRegistry\\.swift$" file)
        (setq registry-file file)))

    (unless registry-file
      (user-error "PreviewRegistry.swift not found. Create it first in your project."))

    ;; Find all view struct names
    (dolist (file swift-files)
      (unless (or (string-match-p "/\\.build/" file)
                  (string-match-p "/SwiftDevelopmentPreview/" file)
                  (string-match-p "Tests\\.swift$" file)
                  (string-match-p "PreviewRegistry\\.swift$" file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^struct \\([A-Z][a-zA-Z0-9]*\\).*:.*View" nil t)
            (let ((name (match-string 1)))
              (unless (member name '("App")) ; Skip App struct
                (push name view-names)))))))

    (setq view-names (sort (delete-dups view-names) #'string<))

    (when view-names
      (let ((registry-code
             (mapconcat (lambda (name)
                         (format "    ViewRegistry.shared.register(name: \"%s\") { %s() }" name name))
                       view-names
                       "\n")))
        ;; Update PreviewRegistry.swift
        (with-temp-file registry-file
          (insert (format "//
//  PreviewRegistry.swift
//  Auto-generated by swiftui-preview-update-view-registry
//
//  This file registers all views for the preview system.
//  Run M-x swiftui-preview-update-view-registry in Emacs to update automatically.
//

import SwiftUI
import SwiftDevelopmentPreview

/// Triggers auto-registration of all views
func registerAllViewsForPreview() {
    guard SwiftDevelopmentPreview.isInPreview else { return }

    // Create view instances to trigger auto-registration via .setupSwiftDevelopmentPreview()
%s
}
" registry-code)))
        (message "✓ Updated PreviewRegistry.swift with %d views: %s"
                 (length view-names)
                 (string-join view-names ", "))))))

;;; Zero-Config Auto-Installation and Registry Generation

(defun swiftui-preview--package-exists-p (project-root)
  "Check if SwiftDevelopmentPreview package is in Package.swift for PROJECT-ROOT."
  (let ((package-file (expand-file-name "Package.swift" project-root)))
    (when (file-exists-p package-file)
      (with-temp-buffer
        (insert-file-contents package-file)
        (goto-char (point-min))
        (re-search-forward "SwiftDevelopmentPreview" nil t)))))

(defun swiftui-preview--add-package-dependency (project-root)
  "Automatically add SwiftDevelopmentPreview package to Package.swift in PROJECT-ROOT."
  (let ((package-file (expand-file-name "Package.swift" project-root)))
    (unless (file-exists-p package-file)
      (user-error "No Package.swift found in project root"))

    (message "Adding SwiftDevelopmentPreview package...")

    ;; Read current Package.swift
    (with-temp-buffer
      (insert-file-contents package-file)
      (let ((content (buffer-string)))

        ;; Add to dependencies array if not present
        (unless (string-match-p "SwiftDevelopmentPreview" content)
          (goto-char (point-min))

          ;; Find dependencies array
          (if (re-search-forward "dependencies:[[:space:]]*\\[" nil t)
              (progn
                ;; Insert at beginning of dependencies array
                (insert "\n        .package(path: \"SwiftDevelopmentPreview\"),")

                ;; Write back
                (write-region (point-min) (point-max) package-file)
                (message "✓ Added SwiftDevelopmentPreview to Package.swift"))
            (user-error "Could not find dependencies array in Package.swift")))))))

(defun swiftui-preview--ensure-package-installed (project-root)
  "Ensure SwiftDevelopmentPreview package is installed for PROJECT-ROOT.
If package directory doesn't exist, copy from templates."
  (let ((package-dir (expand-file-name "SwiftDevelopmentPreview" project-root))
        (template-dir (expand-file-name
                       "templates/SwiftDevelopmentPreview"
                       (file-name-directory (locate-library "swift-development")))))

    ;; Copy package template if it doesn't exist
    (unless (file-exists-p package-dir)
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] Copying SwiftDevelopmentPreview template..."))
      (copy-directory template-dir package-dir nil t t)
      (message "✓ Installed SwiftDevelopmentPreview package"))

    ;; Add to Package.swift if it exists (SPM projects only)
    (when (file-exists-p (expand-file-name "Package.swift" project-root))
      (unless (swiftui-preview--package-exists-p project-root)
        (swiftui-preview--add-package-dependency project-root)))

    ;; For Xcode projects, the local package reference is already in project.pbxproj
    ;; So we just need to ensure the directory exists (done above)
    ))

(defun swiftui-preview--auto-create-registry-if-missing (project-root view-name)
  "Auto-create PreviewRegistry.swift if it doesn't exist in PROJECT-ROOT for VIEW-NAME."
  (let* ((swift-files (condition-case err
                          (directory-files-recursively
                           project-root
                           "\\.swift$"
                           nil
                           (lambda (dir)
                             (not (string-match-p "\\(?:\\.build\\|DerivedData\\|Pods\\|/\\..+\\)" dir))))
                        (file-missing nil)))
         (registry-file nil)
         (app-target-dir nil))

    ;; Find PreviewRegistry.swift
    (dolist (file swift-files)
      (when (string-match-p "PreviewRegistry\\.swift$" file)
        (setq registry-file file)))

    ;; If not found, create it
    (unless registry-file
      (when swiftui-preview-debug
        (message "[SwiftUI Preview] PreviewRegistry.swift not found, creating it..."))

      ;; Try to find the app target directory (look for *App.swift)
      (dolist (file swift-files)
        (when (and (not app-target-dir)
                   (string-match-p "App\\.swift$" file)
                   (not (string-match-p "SwiftDevelopmentPreview" file)))
          (setq app-target-dir (file-name-directory file))))

      ;; Fallback: use project root
      (unless app-target-dir
        (setq app-target-dir project-root))

      (setq registry-file (expand-file-name "PreviewRegistry.swift" app-target-dir))

      ;; Create initial registry with current view
      (with-temp-file registry-file
        (insert (format "//
//  PreviewRegistry.swift
//  Auto-generated by swift-development
//

import SwiftUI
import SwiftDevelopmentPreview

/// Register view(s) for preview
func registerAllViewsForPreview() {
    guard SwiftDevelopmentPreview.isInPreview else { return }

    ViewRegistry.shared.register(name: \"%s\") { %s() }
}
" view-name view-name)))

      (message "✓ Created PreviewRegistry.swift"))

    registry-file))

;;;###autoload
(defun swiftui-preview-auto-setup ()
  "Automatically setup SwiftUI preview for current project (zero-config).
Installs package and creates registry if needed."
  (interactive)
  (let* ((project-root (swift-project-root))
         (view-name (when (buffer-file-name)
                      (file-name-sans-extension
                       (file-name-nondirectory (buffer-file-name))))))

    (unless project-root
      (user-error "No Swift project found"))

    (unless view-name
      (user-error "Current buffer is not visiting a file"))

    (message "Setting up SwiftUI Preview (zero-config)...")

    ;; 1. Ensure package is installed
    (swiftui-preview--ensure-package-installed project-root)

    ;; 2. Create or update registry
    (swiftui-preview--auto-create-registry-if-missing project-root view-name)

    (message "✓ SwiftUI Preview setup complete! Run M-x swiftui-preview-generate to create preview.")))

(provide 'swiftui-preview)
;;; swiftui-preview.el ends here
