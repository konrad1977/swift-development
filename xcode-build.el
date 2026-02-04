;;; xcode-build.el --- Build and run Swift apps using Xcode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, xcode, build

;;; Commentary:

;; DEPRECATED: This module is not currently integrated with the main build system.
;; It provides functions to control Xcode.app via AppleScript, but the main build
;; workflow uses xcodebuild directly (see swift-development.el).
;;
;; These functions are kept for users who prefer building through Xcode.app UI.
;; WARNING: All functions use synchronous shell commands which block Emacs.

;;; Code:

(require 'cl-lib)
(require 'swift-notification nil t)

(defcustom xcode-build-use-optimized-settings t
  "Whether to use optimized build settings when building with Xcode."
  :type 'boolean
  :group 'xcode-build)

(defun xcode-build-apply-optimized-settings ()
  "Apply optimized build settings to Xcode."
  (when xcode-build-use-optimized-settings
    (shell-command-to-string
     "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'tell targetProject' -e 'set activeRunDestination to active run destination' -e 'set buildSettings to build settings of active target of project 1' -e 'set value of buildSettings for key \"SWIFT_OPTIMIZATION_LEVEL\" to \"-Osize\"' -e 'set value of buildSettings for key \"COMPILER_INDEX_STORE_ENABLE\" to \"NO\"' -e 'set value of buildSettings for key \"DEBUG_INFORMATION_FORMAT\" to \"dwarf\"' -e 'set value of buildSettings for key \"SWIFT_COMPILATION_MODE\" to \"incremental\"' -e 'end tell' -e 'end tell'")))

(defun xcode-build-build()
  "Start a build using Xcode with optimized settings."
  (interactive)
  (save-some-buffers t)
  (xcode-build-apply-optimized-settings)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'")
  (swift-notification-send :message "Building project using Xcode..." :seconds 2))

(defun xcode-build-stop()
  "Stop application from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'end tell'")
  (swift-notification-send :message "Stopping simulator..." :seconds 2))

(defun xcode-build-run()
  "Run application from Xcode with optimized settings."
  (interactive)
  (save-some-buffers t)
  (xcode-build-apply-optimized-settings)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'")
  (swift-notification-send :message "Running project using Xcode..." :seconds 2))

(defun xcode-build-test()
  "Run current test scheme from Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'")
  (swift-notification-send :message "Testing project using Xcode..." :seconds 2))

(defun xcode-build-clean()
  "Clean the project in Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'clean targetProject' -e 'end tell'")
  (swift-notification-send :message "Cleaning project in Xcode..." :seconds 2))

(defun xcode-build-clean-build-folder()
  "Clean the build folder in Xcode."
  (interactive)
  (save-some-buffers t)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'tell targetProject' -e 'clean build folder' -e 'end tell' -e 'end tell'")
  (swift-notification-send :message "Cleaning build folder in Xcode..." :seconds 2))

(provide 'xcode-build)
;;; xcode-build.el ends here
