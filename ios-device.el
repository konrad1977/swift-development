;;; ios-device.el --- iOS physical device management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.5.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, ios, device

;;; Commentary:

;; Package for handling iOS physical devices - installation, launching, and logs

;;; Code:

(require 'periphery-helper nil t)
(require 'cl-lib)
(require 'json)

(defgroup ios-device nil
  "Customization group for ios-device package."
  :group 'tools)

(defcustom ios-device-buffer-name "*ios-device-"
  "Base name of the buffer used for iOS device operations."
  :type 'string
  :group 'ios-device)

(defcustom ios-device-debug t
  "Enable debug output for ios-device operations."
  :type 'boolean
  :group 'ios-device)

(defvar ios-device--current-buffer-name nil)
(defvar ios-device--current-device-id nil)
(defvar ios-device--current-device-udid nil
  "Cached UDID for xcodebuild (different from CoreDevice UUID).")
(defvar ios-device--current-install-command nil)
(defvar ios-device--current-app-identifier nil)
(defvar ios-device--current-buffer nil)

(defun ios-device-format-id (id)
  "Format device id (as ID)."
  (if id
      (if (not
           (string-match-p (regexp-quote "-") id))
          (concat (substring id 0 8) "-" (substring id 6))
        id)))

(cl-defun ios-device-install-app (&key buildfolder appIdentifier)
  "Install app on device (BUILDFOLDER APPIDENTIFIER)."
  (let* ((default-directory buildfolder)
         (appname (ios-device-app-name-from :buildfolder buildfolder))
         (buffer-name (concat ios-device-buffer-name appname "*"))
         (buffer (get-buffer-create buffer-name))
         (device-id (ios-device-identifier))
         (command (ios-device-install-cmd :identifier device-id :appname appname)))

    (when ios-device-debug
      (message "install-app: %s" command)
      (message "build-folder: %s" buildfolder)
      (message "app-name: %s" appname))

    (with-current-buffer buffer
      (erase-buffer)
      (setq-local mode-line-format nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0)
      (setq-local ios-device--current-device-id device-id)
      (setq-local ios-device--current-app-identifier appIdentifier)
      ;; (add-hook 'kill-buffer-hook #'ios-device-cleanup nil t)
      )

    (async-start-command-to-string
     :command command
     :callback (lambda (output)
                 (let ((run-command (ios-device-run-cmd :identifier device-id :appIdentifier appIdentifier)))
                   (with-current-buffer buffer
                     (erase-buffer)
                     (insert output)
                     (insert "\n\nRunning app...\n\n")
                     (inhibit-sentinel-messages #'async-shell-command
                                                run-command
                                                buffer)))))

    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(defun ios-device-identifier ()
  "Get iOS device identifier."
  (if ios-device--current-device-id
      ios-device--current-device-id
    (setq ios-device--current-device-id (ios-device-choose-device))
    ios-device--current-device-id))

(defun ios-device-connected-device-id ()
  "Get the id of the connected device."
  (let ((result
         (string-trim
          (shell-command-to-string "system_profiler SPUSBDataType | sed -n -E -e '/(iPhone|iPad)/,/Serial/s/ *Serial Number: *(.+)/\\1/p'"))))
    (if (string= result "") nil result)))

(defun ios-device-kill-buffer ()
  "Kill the ios-device buffer."
  (when (and ios-device--current-buffer-name (get-buffer ios-device--current-buffer-name))
    (kill-buffer ios-device--current-buffer-name)))

(cl-defun ios-device-install-cmd (&key identifier appname)
  "Install app on device (IDENTIFIER APPNAME)."
  (format "xcrun devicectl device install app --device %s '%s.app'"
          identifier
          appname))

(cl-defun ios-device-run-cmd (&key identifier appIdentifier)
  "Generate run command for device (IDENTIFIER APPIDENTIFIER)."
  (when ios-device-debug
    (message "xcrun devicectl device process launch --terminate-existing --device %s %s" identifier appIdentifier))
  (format "xcrun devicectl device process launch --terminate-existing --console --device %s %s"
          identifier
          appIdentifier))

(defun ios-device-reset()
  "Reset the iOS device."
  (ios-device-kill-buffer)
  (setq ios-device--current-buffer-name nil)
  (setq ios-device--current-device-id nil)
  (setq ios-device--current-device-udid nil)
  (setq ios-device--current-install-command nil)
  (setq ios-device--current-app-identifier nil)
  (setq ios-device--current-buffer nil))


(defun ios-device-cleanup ()
  "Cleanup function to terminate the app when the buffer is closed."
  (when (and ios-device--current-device-id ios-device--current-app-identifier)
    (message "Cleaning up iOS device session...")
    (let ((cleanup-command
           (format "xcrun devicectl device process terminate --device %s %s"
                   ios-device--current-device-id
                   ios-device--current-app-identifier)))
      (message "Executing cleanup command: %s" cleanup-command)
      (shell-command cleanup-command)
      (message "Cleanup completed for device %s and app %s"
               ios-device--current-device-id
               ios-device--current-app-identifier))
    (setq ios-device--current-device-id nil)
    (setq ios-device--current-app-identifier nil)))

(cl-defun ios-device-install-app-async (&key buildfolder &key appIdentifier)
  "Install app on device from BUILDFOLDER with APPIDENTIFIER."
  (ios-device-kill-buffer)
  (let* ((default-directory buildfolder)
         (identifier (ios-device-identifier))
         (appname (ios-device-app-name-from :buildfolder buildfolder))
         (app-path (expand-file-name (concat appname ".app") buildfolder))
         (buffer (get-buffer-create (concat ios-device-buffer-name appname "*")))
         (command (format "xcrun devicectl device install app --device %s %s"
                          (shell-quote-argument identifier)
                          (shell-quote-argument app-path))))
    (setq ios-device--current-buffer-name buffer)
    (setq ios-device--current-app-identifier appIdentifier)
    (async-shell-command command buffer)))

(cl-defun ios-device-app-name-from (&key buildfolder)
  "Get compiled app name from (BUILDFOLDER)."
  (if (file-exists-p buildfolder)
      (let ((binary-name (directory-files buildfolder nil "\\.app$")))
        (file-name-sans-extension (car binary-name)))
    nil))

;; Debug function to see raw output
(defun ios-device-debug-output ()
  "Debug function to see what devicectl actually return."
  (shell-command-to-string "xcrun devicectl list devices"))

;; Parse all devices from devicectl output
(defun ios-device-parse-devices ()
  "Parse devicectl output and return a list of all devices.
Each device is represented as a plist with :name, :hostname, :identifier, :state, and :model."
  (let ((output (shell-command-to-string "xcrun devicectl list devices"))
        (devices '()))
    (dolist (line (split-string output "\n"))
      (when (and (not (string-match "^Name\\|^-" line))
                 (not (string= (string-trim line) ""))
                 (string-match "[0-9A-F-]\\{36\\}" line)) ; Contains UUID
        ;; Split on multiple spaces (2 or more)
        (let ((parts (split-string line "[ ]\\{2,\\}" t)))
          (when (>= (length parts) 5)
            (push (list :name (string-trim (nth 0 parts))
                        :hostname (string-trim (nth 1 parts))
                        :identifier (string-trim (nth 2 parts))
                        :state (string-trim (nth 3 parts))
                        :model (string-trim (nth 4 parts)))
                  devices)))))
    (reverse devices)))

;; Interactive device selection by name only
(defun ios-device-choose-device ()
  "Select a device by name and return its UUID identifier."
  (let ((devices (ios-device-parse-devices)))
    (cond
     ((null devices)
      (message "No devices found")
      nil)
     ((= (length devices) 1)
      ;; Only one device, return its UUID directly
      (plist-get (car devices) :identifier))
     (t
      ;; Multiple devices, let user choose by name only
      (let* ((device-names (mapcar (lambda (device)
                                       (plist-get device :name))
                                     devices))
             (selected-name (completing-read "Select device: " device-names nil t)))
        (when selected-name
          (let ((selected-device (seq-find (lambda (device)
                                             (string= (plist-get device :name) selected-name))
                                           devices)))
            (when selected-device
              (plist-get selected-device :identifier)))))))))

(defun ios-device-parse-xctrace-devices ()
  "Parse xctrace output and return a list of physical iOS devices with UDIDs.
Each device is a plist with :name, :version, and :udid.
This returns the UDID format that xcodebuild expects."
  (let ((output (shell-command-to-string "xcrun xctrace list devices 2>/dev/null"))
        (devices '())
        (in-devices-section nil))
    (dolist (line (split-string output "\n"))
      ;; Track sections - only parse "== Devices ==" section
      (cond
       ((string-match "^== Devices ==" line)
        (setq in-devices-section t))
       ((string-match "^== " line)
        (setq in-devices-section nil))
       ;; Parse device lines: "Name (version) (UDID)"
       ((and in-devices-section
             (string-match "^\\(.+\\) (\\([0-9.]+\\)) (\\([0-9A-Fa-f-]+\\))$" line))
        (let ((name (string-trim (match-string 1 line)))
              (version (match-string 2 line))
              (udid (match-string 3 line)))
          ;; Only include iPhone/iPad devices (exclude Mac, Apple Watch)
          (when (string-match-p "iPhone\\|iPad" name)
            (push (list :name name :version version :udid udid) devices))))))
    (nreverse devices)))

(defun ios-device-get-udid-for-name (device-name)
  "Get the UDID (xcodebuild format) for DEVICE-NAME."
  (let ((devices (ios-device-parse-xctrace-devices)))
    (when-let ((device (seq-find (lambda (d)
                                   (string= (plist-get d :name) device-name))
                                 devices)))
      (plist-get device :udid))))

(defun ios-device-udid ()
  "Get the UDID for xcodebuild destination.
This returns the UDID format that xcodebuild expects, not the CoreDevice UUID."
  (or ios-device--current-device-udid
      (let* ((devices (ios-device-parse-xctrace-devices)))
        (cond
         ((null devices)
          (message "No iOS devices found")
          nil)
         ((= (length devices) 1)
          (setq ios-device--current-device-udid (plist-get (car devices) :udid)))
         (t
          ;; Multiple devices - let user choose
          (let* ((device-names (mapcar (lambda (d) (plist-get d :name)) devices))
                 (selected-name (completing-read "Select device for build: " device-names nil t)))
            (when selected-name
              (setq ios-device--current-device-udid
                    (ios-device-get-udid-for-name selected-name)))))))))

;; Get first iPhone UUID
(defun ios-device-get-iphone-id ()
  "Get the UUID of the first iPhone device found."
  (let ((devices (ios-device-parse-devices)))
    (let ((iphone (seq-find (lambda (device)
                              (string-match "iPhone" (plist-get device :model)))
                            devices)))
      (when iphone
        (plist-get iphone :identifier)))))

;; Alternative awk approach for debugging
(defun ios-device-awk-test ()
  "Test different awk approaches to see what works."
  (let ((cmd1 "xcrun devicectl list devices | awk '/iPhone/ {print NF, $0}'")
        (cmd2 "xcrun devicectl list devices | awk '/iPhone/ {for(i=1;i<=NF;i++) print i, $i}'"))
    (list :field-count (shell-command-to-string cmd1)
          :all-fields (shell-command-to-string cmd2))))

;; Simple function using sed instead of awk
(defun ios-device-get-iphone-uuid-sed ()
  "Get iPhone UUID using sed instead of awk."
  (let ((result (string-trim
                 (shell-command-to-string
                  "xcrun devicectl list devices | grep iPhone | sed -E 's/.*([0-9A-F]{8}-([0-9A-F]{4}-){3}[0-9A-F]{12}).*/\\1/'"))))
    (if (string= result "") nil result)))

(cl-defun ios-device-launch-for-debug (&key device-id app-identifier)
  "Launch app on device in stopped state for debugging.
DEVICE-ID is the device UUID, APP-IDENTIFIER is the bundle ID.
Returns the process ID (PID) on success, nil on failure."
  (let* ((json-file (make-temp-file "devicectl-launch" nil ".json"))
         (command (format "xcrun devicectl device process launch --start-stopped --device %s %s --json-output %s"
                          (shell-quote-argument device-id)
                          (shell-quote-argument app-identifier)
                          (shell-quote-argument json-file)))
         (output (shell-command-to-string command))
         (pid nil))
    (when ios-device-debug
      (message "Device launch command: %s" command)
      (message "Device launch output: %s" output))
    ;; Parse the JSON output to get the PID
    (when (file-exists-p json-file)
      (condition-case err
          (let* ((json-data (json-read-file json-file))
                 (result (cdr (assoc 'result json-data)))
                 (process-info (cdr (assoc 'process result))))
            (setq pid (cdr (assoc 'processIdentifier process-info)))
            (when ios-device-debug
              (message "Launched app with PID: %s" pid)))
        (error
         (message "Failed to parse devicectl JSON output: %s" (error-message-string err))))
      (delete-file json-file))
    pid))

(defun ios-device-get-device-name (device-id)
  "Get the device name for DEVICE-ID."
  (let ((devices (ios-device-parse-devices)))
    (when-let ((device (seq-find (lambda (d)
                                   (string= (plist-get d :identifier) device-id))
                                 devices)))
      (plist-get device :name))))

(provide 'ios-device)
;;; ios-device.el ends here
