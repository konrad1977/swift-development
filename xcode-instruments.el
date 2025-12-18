;;; xcode-instruments.el --- Xcode Instruments integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, profiling, instruments

;;; Commentary:

;; Integration with Xcode Instruments for profiling iOS/macOS apps.
;; Provides functions to launch profiling sessions and view trace files.

;;; Code:

(require 'transient)
(require 'xcode-project nil t)
(require 'ios-simulator nil t)
(require 'ios-device nil t)

(defgroup xcode-instruments nil
  "Xcode Instruments integration for profiling."
  :group 'swift-development
  :tag "Xcode Instruments")

(defcustom xcode-instruments-trace-directory
  (expand-file-name "~/Library/Developer/Xcode/Instruments/")
  "Directory where Instruments trace files are stored."
  :type 'directory
  :group 'xcode-instruments)

(defcustom xcode-instruments-templates
  '(("Time Profiler" . "Time Profiler")
    ("Allocations" . "Allocations")
    ("Leaks" . "Leaks")
    ("Network" . "Network")
    ("File Activity" . "File Activity")
    ("Core Data" . "Core Data")
    ("Energy Log" . "Energy Log")
    ("System Trace" . "System Trace")
    ("Animation Hitches" . "Animation Hitches")
    ("SwiftUI" . "SwiftUI"))
  "Available Instruments templates.
Each entry is (DISPLAY-NAME . TEMPLATE-NAME)."
  :type '(alist :key-type string :value-type string)
  :group 'xcode-instruments)

(defcustom xcode-instruments-output-limit 30
  "Time limit in seconds for profiling session.
Set to 0 for unlimited recording."
  :type 'integer
  :group 'xcode-instruments)

(defvar xcode-instruments--current-process nil
  "Current Instruments recording process.")

(defvar xcode-instruments--last-trace-file nil
  "Path to the last recorded trace file.")

;;;###autoload
(defun xcode-instruments-list-templates ()
  "List available Instruments templates."
  (interactive)
  (let ((output (shell-command-to-string "xcrun xctrace list templates 2>/dev/null")))
    (with-current-buffer (get-buffer-create "*Instruments Templates*")
      (erase-buffer)
      (insert "Available Instruments Templates:\n")
      (insert "================================\n\n")
      (insert output)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun xcode-instruments-run (template)
  "Run Instruments with TEMPLATE on current app.
The app must be built and the device/simulator must be running."
  (interactive
   (list (completing-read "Instruments template: "
                          (mapcar #'car xcode-instruments-templates)
                          nil t nil nil "Time Profiler")))
  (let* ((template-name (or (cdr (assoc template xcode-instruments-templates))
                            template))
         (device-id (or (and (fboundp 'ios-device-identifier)
                             (ios-device-identifier))
                        (and (fboundp 'ios-simulator-simulator-identifier)
                             (ios-simulator-simulator-identifier))))
         (app-identifier (when (fboundp 'xcode-project-app-identifier)
                           (xcode-project-app-identifier)))
         (output-dir (expand-file-name
                      (format-time-string "%Y%m%d-%H%M%S.trace")
                      xcode-instruments-trace-directory)))
    (unless device-id
      (user-error "No device or simulator selected"))
    (unless app-identifier
      (user-error "No app identifier found. Build the project first"))
    ;; Ensure output directory exists
    (unless (file-exists-p xcode-instruments-trace-directory)
      (make-directory xcode-instruments-trace-directory t))
    (setq xcode-instruments--last-trace-file output-dir)
    ;; Build the xctrace command
    (let* ((time-limit (if (> xcode-instruments-output-limit 0)
                           (list "--time-limit" (format "%ds" xcode-instruments-output-limit))
                         nil))
           (cmd (append (list "xcrun" "xctrace" "record"
                              "--device" device-id
                              "--template" template-name
                              "--output" output-dir
                              "--attach" app-identifier)
                        time-limit)))
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (format "Starting %s profiling..." template)))
      (setq xcode-instruments--current-process
            (make-process
             :name "xcode-instruments"
             :buffer "*Instruments*"
             :command cmd
             :sentinel #'xcode-instruments--process-sentinel))
      (message "Instruments recording started. Press C-c C-c in *Instruments* buffer to stop."))))

(defun xcode-instruments--process-sentinel (process event)
  "Handle PROCESS completion EVENT for Instruments."
  (when (string-match-p "\\(?:finished\\|exited\\)" event)
    (setq xcode-instruments--current-process nil)
    (if (and xcode-instruments--last-trace-file
             (file-exists-p xcode-instruments--last-trace-file))
        (progn
          (when (fboundp 'xcode-project-notify)
            (xcode-project-notify
             :message (propertize "Profiling complete" 'face 'success)
             :seconds 3
             :reset t))
          (when (y-or-n-p "Open trace file in Instruments? ")
            (xcode-instruments-open-trace xcode-instruments--last-trace-file)))
      (when (fboundp 'xcode-project-notify)
        (xcode-project-notify
         :message (propertize "Profiling stopped" 'face 'warning)
         :seconds 2
         :reset t)))))

;;;###autoload
(defun xcode-instruments-stop ()
  "Stop the current Instruments recording."
  (interactive)
  (if (and xcode-instruments--current-process
           (process-live-p xcode-instruments--current-process))
      (progn
        (interrupt-process xcode-instruments--current-process)
        (message "Stopping Instruments recording..."))
    (message "No active Instruments recording")))

;;;###autoload
(defun xcode-instruments-open-trace (&optional trace-file)
  "Open TRACE-FILE in Instruments app.
If TRACE-FILE is nil, prompt for a trace file."
  (interactive)
  (let ((file (or trace-file
                  (read-file-name "Open trace file: "
                                  xcode-instruments-trace-directory
                                  nil t nil
                                  (lambda (f) (string-suffix-p ".trace" f))))))
    (if (file-exists-p file)
        (start-process "open-instruments" nil "open" "-a" "Instruments" file)
      (user-error "Trace file not found: %s" file))))

;;;###autoload
(defun xcode-instruments-open-latest-trace ()
  "Open the most recent trace file in Instruments."
  (interactive)
  (let* ((traces (directory-files xcode-instruments-trace-directory t "\\.trace$"))
         (sorted (sort traces #'file-newer-than-file-p))
         (latest (car sorted)))
    (if latest
        (xcode-instruments-open-trace latest)
      (user-error "No trace files found in %s" xcode-instruments-trace-directory))))

;;;###autoload
(defun xcode-instruments-clean-traces ()
  "Delete old trace files to free up disk space."
  (interactive)
  (let* ((traces (directory-files xcode-instruments-trace-directory t "\\.trace$"))
         (count (length traces)))
    (if (zerop count)
        (message "No trace files to clean")
      (when (y-or-n-p (format "Delete %d trace file(s)? " count))
        (dolist (trace traces)
          (delete-directory trace t))
        (message "Deleted %d trace file(s)" count)))))

;;;###autoload
(defun xcode-instruments-quick-profile ()
  "Quick profile the current app with Time Profiler for 10 seconds."
  (interactive)
  (let ((xcode-instruments-output-limit 10))
    (xcode-instruments-run "Time Profiler")))

;;;###autoload
(defun xcode-instruments-memory-profile ()
  "Profile memory allocations of the current app."
  (interactive)
  (xcode-instruments-run "Allocations"))

;;;###autoload
(defun xcode-instruments-leaks-profile ()
  "Check for memory leaks in the current app."
  (interactive)
  (xcode-instruments-run "Leaks"))

;;; Transient Menu

;;;###autoload
(transient-define-prefix xcode-instruments-transient ()
  "Xcode Instruments profiling actions."
  ["Profile App"
   [("t" "Time Profiler" xcode-instruments-quick-profile)
    ("m" "Memory (Allocations)" xcode-instruments-memory-profile)
    ("l" "Leaks" xcode-instruments-leaks-profile)
    ("r" "Run custom template..." xcode-instruments-run)]]
  ["Recording Control"
   [("s" "Stop recording" xcode-instruments-stop)]]
  ["Trace Files"
   [("o" "Open trace file..." xcode-instruments-open-trace)
    ("O" "Open latest trace" xcode-instruments-open-latest-trace)
    ("c" "Clean old traces" xcode-instruments-clean-traces)]]
  ["Info"
   [("i" "List templates" xcode-instruments-list-templates)]]
  [("q" "Quit" transient-quit-one)])

(provide 'xcode-instruments)
;;; xcode-instruments.el ends here
