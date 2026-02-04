;;; swift-async.el --- Robust async process utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: swift, processes, async
;; URL: https://github.com/konrad1977/swift-development

;;; Commentary:

;; Provides robust async process utilities for Swift/Xcode development tools.
;; Key features:
;; - Configurable timeout (prevents hangs from blocking processes)
;; - Separate stderr capture (better error diagnostics)
;; - Automatic buffer cleanup (no memory leaks)
;; - Output size guard (prevents huge allocations)
;; - Integration with swift-cache.el
;; - Process tracking for debugging
;;
;; Usage:
;; Prefer list-form commands over shell strings for better safety:
;;   (swift-async-run '("xcrun" "simctl" "list") #'my-callback)
;; Shell strings are supported but go through shell interpretation:
;;   (swift-async-run "echo hello" #'my-callback)

;;; Code:

(require 'cl-lib)
(require 'json)

;; Optional dependency - will use if available
(declare-function swift-cache-get "swift-cache" (key &optional default))
(declare-function swift-cache-set "swift-cache" (key value &optional ttl))

;;; Customization

(defgroup swift-async nil
  "Async process utilities for Swift development."
  :group 'programming
  :prefix "swift-async-")

(defcustom swift-async-default-timeout 15
  "Default timeout in seconds for async processes."
  :type 'integer
  :group 'swift-async)

(defcustom swift-async-sync-timeout 5
  "Default timeout in seconds for sync processes with timeout."
  :type 'integer
  :group 'swift-async)

(defcustom swift-async-max-output-size (* 10 1024 1024)
  "Maximum output size in bytes (default 10MB).
Processes exceeding this will be killed."
  :type 'integer
  :group 'swift-async)

(defcustom swift-async-debug nil
  "Enable debug logging for async operations."
  :type 'boolean
  :group 'swift-async)

;;; Internal variables

(defvar swift-async--active-processes (make-hash-table :test 'equal)
  "Hash table tracking active async processes by key.")

(defvar swift-async--timers (make-hash-table :test 'equal)
  "Hash table tracking timeout timers by process key.")

(defvar swift-async--process-state (make-hash-table :test 'equal)
  "Hash table tracking process state (for overflow detection, errors, etc.).")

;;; Debug helpers

(defun swift-async--log (format-string &rest args)
  "Log message with FORMAT-STRING and ARGS when debug is enabled."
  (when swift-async-debug
    (apply #'message (concat "[swift-async %s] " format-string)
           (swift-async--timestamp) args)))

(defun swift-async--timestamp ()
  "Return current timestamp string for debugging."
  (format-time-string "%H:%M:%S.%3N"))

(defun swift-async--log-proc (proc format-string &rest args)
  "Log PROC status with FORMAT-STRING and ARGS. Always includes process info."
  (when swift-async-debug
    (let ((status (if (processp proc) (process-status proc) 'invalid))
          (exit-code (if (and (processp proc)
                              (memq (process-status proc) '(exit signal)))
                         (process-exit-status proc)
                       nil)))
      (apply #'message
             (concat "[swift-async %s] PROC=%s STATUS=%s%s | " format-string)
             (swift-async--timestamp)
             (if (processp proc) (process-name proc) "nil")
             status
             (if exit-code (format " EXIT=%d" exit-code) "")
             args))))

;;; Safe callback invocation

(defun swift-async--safe-funcall (callback &rest args)
  "Safely call CALLBACK with ARGS, catching any errors."
  (when callback
    (condition-case err
        (apply callback args)
      (error
       (swift-async--log "Callback error: %S" err)
       nil))))

;;; JSON parsing with consistent settings

(defun swift-async--parse-json (string)
  "Parse JSON STRING with consistent settings.
Returns parsed data or nil on error."
  (when (and string (> (length string) 0))
    (let ((json-object-type 'alist)
          (json-array-type 'vector)
          (json-key-type 'symbol))
      (condition-case err
          (json-read-from-string string)
        (error
         (swift-async--log "JSON parse failed: %s" err)
         nil)))))

;;; Process tracking and state

(defun swift-async--state-put (key prop val)
  "Set property PROP to VAL in state for KEY."
  (let ((state (gethash key swift-async--process-state)))
    (puthash key (plist-put state prop val) swift-async--process-state)))

(defun swift-async--state-get (key prop)
  "Get property PROP from state for KEY."
  (plist-get (gethash key swift-async--process-state) prop))

;;;###autoload
(defun swift-async-status ()
  "Display status of all active async processes."
  (interactive)
  (let ((count (hash-table-count swift-async--active-processes))
        (now (current-time)))
    (if (zerop count)
        (when (called-interactively-p 'any)
          (message "[swift-async] No active processes"))
      (message "[swift-async] Active processes: %d" count)
      (maphash (lambda (key proc)
                 (let* ((started (swift-async--state-get key :started))
                        (elapsed (if started
                                     (float-time (time-subtract now started))
                                   0)))
                   (message "  [%s] PROC=%s STATUS=%s elapsed=%.1fs%s%s"
                            key
                            (process-name proc)
                            (process-status proc)
                            elapsed
                            (if (swift-async--state-get key :overflow) " OVERFLOW" "")
                            (if (swift-async--state-get key :timeout) " TIMEOUT" ""))))
               swift-async--active-processes))))

(defun swift-async-running-p (key)
  "Return t if process with KEY is currently running."
  (when-let* ((proc (gethash key swift-async--active-processes)))
    (process-live-p proc)))

(defun swift-async-last-error (key)
  "Return last error for process KEY, or nil."
  (swift-async--state-get key :error))

(defun swift-async--cancel-timer (key)
  "Cancel timer for KEY if it exists and is valid."
  (when-let* ((timer (gethash key swift-async--timers)))
    (when (timerp timer)
      (cancel-timer timer))
    (remhash key swift-async--timers)))

(defun swift-async-cancel (key)
  "Cancel async process with KEY."
  (swift-async--cancel-timer key)
  (let ((proc (gethash key swift-async--active-processes)))
    (when (and proc (process-live-p proc))
      (swift-async--log "Cancelling process: %s" key)
      (delete-process proc))
    (remhash key swift-async--active-processes)
    (remhash key swift-async--process-state)
    (and proc t)))

;;;###autoload
(defun swift-async-cancel-all ()
  "Emergency kill all active async processes."
  (interactive)
  (let ((count 0))
    (maphash (lambda (key _proc)
               (when (swift-async-cancel key)
                 (cl-incf count)))
             (copy-hash-table swift-async--active-processes))
    (clrhash swift-async--active-processes)
    (clrhash swift-async--timers)
    (clrhash swift-async--process-state)
    (message "Cancelled %d async processes" count)))

;;; Main async function

;;;###autoload
(cl-defun swift-async-run (command callback &key
                                   (timeout swift-async-default-timeout)
                                   error-callback
                                   cache-key
                                   (cache-ttl 300)
                                   (max-output-size swift-async-max-output-size)
                                   parse-json
                                   process-key)
  "Run COMMAND asynchronously and call CALLBACK with result.

COMMAND can be:
- A list of strings (program and arguments) - RECOMMENDED
- A string (shell command) - goes through shell interpretation

CALLBACK receives the output string (or parsed JSON if PARSE-JSON is t).

Keyword arguments:
- TIMEOUT: Seconds before killing process (default `swift-async-default-timeout')
- ERROR-CALLBACK: Called with error message on failure
- CACHE-KEY: If provided, results are cached using swift-cache
- CACHE-TTL: Cache TTL in seconds (default 300)
- MAX-OUTPUT-SIZE: Kill process if output exceeds this (default 10MB)
- PARSE-JSON: If t, parse output as JSON before calling CALLBACK
- PROCESS-KEY: Key for tracking/cancelling (defaults to CACHE-KEY or generated)

Returns the process object."
  ;; Check cache first
  (when (and cache-key (fboundp 'swift-cache-get))
    (let ((cached (swift-cache-get cache-key)))
      (when cached
        (swift-async--log "CACHE HIT key=%s" cache-key)
        (swift-async--safe-funcall callback cached)
        (cl-return-from swift-async-run nil))))

  ;; Generate process key if not provided
  (let ((key (or process-key cache-key (make-temp-name "swift-async-"))))
    ;; Kill any existing process with same key
    (swift-async-cancel key)

    ;; Validate and normalize command
    (let* ((cmd-list (cond
                      ((listp command) command)
                      ((stringp command)
                       (list shell-file-name shell-command-switch command))
                      (t (error "Invalid command type: %S" command))))
           (out-buf nil)
           (err-buf nil)
           (start-time (current-time))
           (output-size 0)
           (killed-for-overflow nil)
           (proc nil)
           (timer nil))

      ;; Initialize state
      (puthash key (list :started start-time) swift-async--process-state)

      (swift-async--log "Starting: %s (timeout: %ds, key: %s)"
                        (if (stringp command) command (string-join cmd-list " "))
                        timeout
                        key)

      ;; Create buffers and process with unwind-protect for cleanup on error
      (condition-case err
          (progn
            (setq out-buf (generate-new-buffer " *swift-async-out*"))
            (setq err-buf (generate-new-buffer " *swift-async-err*"))

            (setq proc
                  (make-process
                   :name (format "swift-async:%s" key)
                   :buffer out-buf
                   :stderr err-buf
                   :command cmd-list
                   :coding 'utf-8-unix
                   :connection-type 'pipe
                   :noquery t
                   :filter
                   (lambda (process output)
                     ;; Track output size and kill if exceeded
                     (cl-incf output-size (length output))
                     (if (> output-size max-output-size)
                         (progn
                           (swift-async--log-proc process "OVERFLOW size=%d max=%d - killing" output-size max-output-size)
                           (setq killed-for-overflow t)
                           (swift-async--state-put key :overflow t)
                           (delete-process process))
                       ;; Use internal-default-process-filter to insert into buffer
                       (internal-default-process-filter process output)))
                   :sentinel
                   (lambda (process event)
                     (swift-async--log-proc process "event=%s elapsed=%.2fs key=%s"
                                            (string-trim event)
                                            (float-time (time-subtract (current-time) start-time))
                                            key)

                     ;; Cancel timeout timer (with race guard)
                     (swift-async--cancel-timer key)

                     ;; Remove from active processes
                     (remhash key swift-async--active-processes)

                      (when (memq (process-status process) '(exit signal))
                        (unwind-protect
                            (cond
                             ;; Killed for overflow
                             ((swift-async--state-get key :overflow)
                              (let ((err-msg "Process killed: output exceeded size limit"))
                                (swift-async--state-put key :error err-msg)
                                (if error-callback
                                    (swift-async--safe-funcall error-callback err-msg)
                                  (swift-async--safe-funcall callback nil))))

                             ;; Killed for timeout (error-callback already called in timer)
                             ((swift-async--state-get key :timeout)
                              (swift-async--safe-funcall callback nil))

                             ;; Success
                             ((and (eq (process-status process) 'exit)
                                   (= (process-exit-status process) 0))
                              (let ((result nil))
                                (when (buffer-live-p out-buf)
                                  (with-current-buffer out-buf
                                    (let ((txt (buffer-substring-no-properties
                                                (point-min) (point-max))))
                                      (setq result (if parse-json
                                                       (swift-async--parse-json txt)
                                                     txt)))))
                                ;; Handle JSON parse failure
                                (when (and parse-json (null result))
                                  (swift-async--state-put key :error "JSON parse error")
                                  (swift-async--safe-funcall error-callback "JSON parse error"))
                                ;; Cache the result
                                (when (and cache-key result (fboundp 'swift-cache-set))
                                  (swift-cache-set cache-key result cache-ttl))
                                (swift-async--safe-funcall callback result)))

                             ;; Failure
                             (t
                              (let ((errtxt (when (buffer-live-p err-buf)
                                              (with-current-buffer err-buf
                                                (buffer-substring-no-properties
                                                 (point-min) (point-max))))))
                                (swift-async--log-proc process "FAILED stderr=%s"
                                                       (truncate-string-to-width (or errtxt event) 200))
                                (swift-async--state-put key :error (or errtxt event))
                                (if error-callback
                                    (swift-async--safe-funcall error-callback (or errtxt event))
                                  (swift-async--safe-funcall callback nil)))))
                         ;; Cleanup buffers
                         (when (buffer-live-p out-buf) (kill-buffer out-buf))
                         (when (buffer-live-p err-buf) (kill-buffer err-buf)))))))

            ;; Setup timeout timer
            (when (and timeout (> timeout 0))
              (setq timer
                    (run-with-timer
                     timeout nil
                     (lambda ()
                       ;; Guard against race with sentinel
                       (when (and (gethash key swift-async--active-processes)
                                  (process-live-p proc))
                         (swift-async--log-proc proc "TIMEOUT after %ds - killing" timeout)
                         (swift-async--state-put key :timeout t)
                         (remhash key swift-async--timers)
                         (swift-async--safe-funcall error-callback
                                                    (format "Process timed out after %ds" timeout))
                         (delete-process proc)))))
              (puthash key timer swift-async--timers))

            ;; Track the process
            (puthash key proc swift-async--active-processes)

            proc)

        ;; Cleanup on make-process failure
        (error
         (swift-async--log "Failed to start process: %s" err)
         (when (buffer-live-p out-buf) (kill-buffer out-buf))
         (when (buffer-live-p err-buf) (kill-buffer err-buf))
         (remhash key swift-async--process-state)
         (swift-async--safe-funcall error-callback (format "Failed to start process: %s" err))
         nil)))))

;;; Synchronous with timeout

;;;###autoload
(cl-defun swift-async-run-sync (command &key
                                        (timeout swift-async-sync-timeout)
                                        parse-json)
  "Run COMMAND synchronously with TIMEOUT protection.

COMMAND can be a list (recommended) or string (shell command).
Returns output string (or parsed JSON if PARSE-JSON is t), or nil on error/timeout.

This is useful for commands that should be fast (<1s) but need timeout protection."
  (let* ((start-time (current-time))
         (result nil)
         (timed-out nil)
         (cmd-list (cond
                    ((listp command) command)
                    ((stringp command)
                     (list shell-file-name shell-command-switch command))
                    (t (error "Invalid command type: %S" command)))))
    (with-temp-buffer
      (let ((proc (make-process
                   :name (format "swift-sync:%s" (if (stringp command)
                                                     (car (last (split-string command)))
                                                   (car (last cmd-list))))
                   :buffer (current-buffer)
                   :command cmd-list
                   :coding 'utf-8-unix
                   :noquery t
                   :connection-type 'pipe)))
        ;; Wait for process to finish or timeout (with redisplay)
        (while (and (process-live-p proc)
                    (< (float-time (time-subtract (current-time) start-time)) timeout))
          (accept-process-output proc 0.1 nil t))

        (if (process-live-p proc)
            (progn
              ;; Timeout - kill the process
              (delete-process proc)
              (setq timed-out t)
              (swift-async--log "Sync command timed out after %ds: %s"
                                timeout
                                (if (stringp command) command (car command))))
          ;; Process finished - get output
          (when (= (process-exit-status proc) 0)
            (setq result (buffer-string))))))

    ;; Parse JSON if requested
    (when (and result parse-json (not timed-out))
      (setq result (swift-async--parse-json result)))

    result))

;;; Cached sync command (for rarely-changing values)

;;;###autoload
(defun swift-async-cached-sync (cache-key ttl command &optional parse-json)
  "Run COMMAND synchronously but cache result with CACHE-KEY and TTL.

Uses `swift-cache' if available, otherwise runs command every time.
PARSE-JSON if t, parses output as JSON.

Useful for commands that rarely change (like xcode-select -p)."
  (if (fboundp 'swift-cache-get)
      (let ((cached (swift-cache-get cache-key)))
        (if cached
            (progn
              (swift-async--log "Cache hit for sync: %s" cache-key)
              cached)
          (let ((result (swift-async-run-sync command :parse-json parse-json)))
            (when result
              (swift-cache-set cache-key result ttl))
            result)))
    ;; No cache available, just run
    (swift-async-run-sync command :parse-json parse-json)))

;;; Shell command variants (convenience)

(cl-defun swift-async-shell (command callback &rest args)
  "Run shell COMMAND asynchronously.
CALLBACK and ARGS are passed to `swift-async-run'.
Note: Prefer list-form commands when possible for better safety."
  (apply #'swift-async-run
         (list shell-file-name shell-command-switch command)
         callback
         args))

(defun swift-async-shell-sync (command &optional timeout)
  "Run shell COMMAND synchronously with TIMEOUT.
Returns output string or nil on error/timeout.
Note: Prefer list-form commands when possible for better safety."
  (swift-async-run-sync
   (list shell-file-name shell-command-switch command)
   :timeout (or timeout swift-async-sync-timeout)))

;;; Cleanup on Emacs exit

(defun swift-async--cleanup ()
  "Cleanup all async processes and timers."
  (swift-async-cancel-all))

(add-hook 'kill-emacs-hook #'swift-async--cleanup)

(provide 'swift-async)
;;; swift-async.el ends here
