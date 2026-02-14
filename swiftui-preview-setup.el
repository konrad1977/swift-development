;;; swiftui-preview-setup.el --- Setup wizard for SwiftUI Preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Mikael Konradsson
;; Author: Mikael Konradsson
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/konrad1977/swift-development
;; Keywords: swift, swiftui, preview, ios

;;; Commentary:

;; This module provides a setup wizard for SwiftUI Preview functionality.
;; It checks for required dependencies (Ruby, xcodeproj gem) and guides
;; the user through installation if needed.
;;
;; Usage:
;; M-x swiftui-preview-setup-wizard
;; M-x swiftui-preview-setup-check

;;; Code:

(require 'cl-lib)

(defgroup swiftui-preview-setup nil
  "Setup and dependencies for SwiftUI Preview."
  :group 'swiftui-preview
  :prefix "swiftui-preview-setup-")

(defvar swiftui-preview-setup--wizard-buffer "*SwiftUI Preview Setup*"
  "Buffer name for setup wizard.")

(defvar swiftui-preview-setup--deps-checked nil
  "Cache for dependency check results.")

(defvar swiftui-preview-setup--last-check-time nil
  "Time of last dependency check.")

(defconst swiftui-preview-setup--check-interval 300
  "Seconds between dependency rechecks (5 minutes).")

;;; Dependency Checks

(defun swiftui-preview-setup--check-xcode-cli ()
  "Check if Xcode command line tools are installed.
Returns t if installed, nil otherwise."
  (= 0 (call-process "xcode-select" nil nil nil "-p")))

(defun swiftui-preview-setup--check-ruby ()
  "Check if Ruby is available.
Returns a plist with :installed, :version, and :path."
  (let ((ruby-path (executable-find "ruby")))
    (if ruby-path
        (let ((version (string-trim
                        (shell-command-to-string "ruby --version 2>/dev/null"))))
          (list :installed t
                :version version
                :path ruby-path))
      (list :installed nil :version nil :path nil))))

(defun swiftui-preview-setup--check-xcodeproj-gem ()
  "Check if xcodeproj Ruby gem is installed.
Returns a plist with :installed and :version."
  (let* ((check-cmd "ruby -e \"require 'xcodeproj'; puts Xcodeproj::VERSION\" 2>/dev/null")
         (result (string-trim (shell-command-to-string check-cmd))))
    (if (and result (not (string-empty-p result)) (not (string-match-p "error\\|cannot load" result)))
        (list :installed t :version result)
      ;; Try with user gem path
      (let* ((home (getenv "HOME"))
             (gem-paths (list
                         (format "%s/.gem/ruby/4.0.0" home)
                         (format "%s/.gem/ruby/3.3.0" home)
                         (format "%s/.gem/ruby/3.2.0" home)
                         (format "%s/.gem/ruby/3.1.0" home)
                         (format "%s/.gem/ruby/3.0.0" home)
                         (format "%s/.gem/ruby/2.6.0" home)))
             (found nil))
        (dolist (gem-path gem-paths)
          (unless found
            (let* ((check-cmd2 (format "GEM_HOME='%s' ruby -e \"require 'xcodeproj'; puts Xcodeproj::VERSION\" 2>/dev/null" gem-path))
                   (result2 (string-trim (shell-command-to-string check-cmd2))))
              (when (and result2 (not (string-empty-p result2)) (not (string-match-p "error\\|cannot load" result2)))
                (setq found (list :installed t :version result2 :gem-home gem-path))))))
        (or found (list :installed nil :version nil))))))

(defun swiftui-preview-setup--check-simulator ()
  "Check if iOS Simulator is available and has devices.
Returns a plist with :available and :booted-count."
  (let ((output (shell-command-to-string "xcrun simctl list devices available -j 2>/dev/null")))
    (condition-case nil
        (let* ((json (json-read-from-string output))
               (devices (alist-get 'devices json))
               (total 0)
               (booted 0))
          (dolist (runtime-devices devices)
            (when (string-match-p "iOS" (symbol-name (car runtime-devices)))
              (dolist (device (cdr runtime-devices))
                (setq total (1+ total))
                (when (equal (alist-get 'state device) "Booted")
                  (setq booted (1+ booted))))))
          (list :available (> total 0)
                :total-count total
                :booted-count booted))
      (error (list :available nil :total-count 0 :booted-count 0)))))

(defun swiftui-preview-setup-check-all ()
  "Check all dependencies and return results.
Returns a plist with status of each dependency."
  (list :xcode-cli (swiftui-preview-setup--check-xcode-cli)
        :ruby (swiftui-preview-setup--check-ruby)
        :xcodeproj (swiftui-preview-setup--check-xcodeproj-gem)
        :simulator (swiftui-preview-setup--check-simulator)))

;;;###autoload
(defun swiftui-preview-setup-check (&optional force)
  "Check if all dependencies are satisfied.
Returns t if ready, nil otherwise.
With FORCE, skip cache and recheck."
  (interactive "P")
  ;; Use cache if recent
  (if (and (not force)
           swiftui-preview-setup--deps-checked
           swiftui-preview-setup--last-check-time
           (< (- (float-time) swiftui-preview-setup--last-check-time)
              swiftui-preview-setup--check-interval))
      swiftui-preview-setup--deps-checked
    ;; Need to check
    (let* ((deps (swiftui-preview-setup-check-all))
           (xcode-ok (plist-get deps :xcode-cli))
           (ruby-ok (plist-get (plist-get deps :ruby) :installed))
           (gem-ok (plist-get (plist-get deps :xcodeproj) :installed))
           (all-ok (and xcode-ok ruby-ok gem-ok)))
      
      ;; Cache results
      (setq swiftui-preview-setup--deps-checked all-ok
            swiftui-preview-setup--last-check-time (float-time))
      
      (when (called-interactively-p 'any)
        (if all-ok
            (message "SwiftUI Preview: All dependencies satisfied!")
          (message "SwiftUI Preview: Missing dependencies. Run M-x swiftui-preview-setup-wizard")))
      
      all-ok)))

;;; Installation Functions

(defun swiftui-preview-setup--install-xcodeproj-gem ()
  "Install xcodeproj gem using user install.
Returns t on success, nil on failure."
  (interactive)
  (message "Installing xcodeproj gem...")
  (let* ((cmd "gem install xcodeproj --user-install 2>&1")
         (output (shell-command-to-string cmd))
         (success (string-match-p "Successfully installed\\|xcodeproj" output)))
    (if success
        (progn
          (message "xcodeproj gem installed successfully!")
          ;; Clear cache
          (setq swiftui-preview-setup--deps-checked nil)
          t)
      (message "Failed to install xcodeproj: %s" output)
      nil)))

(defun swiftui-preview-setup--install-xcode-cli ()
  "Trigger Xcode command line tools installation."
  (interactive)
  (message "Opening Xcode CLI tools installer...")
  (call-process "xcode-select" nil nil nil "--install")
  (message "Follow the system dialog to complete installation."))

;;; Setup Wizard

(defun swiftui-preview-setup--insert-status (label ok &optional details)
  "Insert a status line with LABEL and OK indicator.
DETAILS is optional extra information."
  (insert (format "  %s %s"
                  (if ok "[OK]" "[X]")
                  label))
  (when details
    (insert (format " (%s)" details)))
  (insert "\n"))

(defun swiftui-preview-setup--wizard-refresh ()
  "Refresh the wizard buffer with current status."
  (let ((deps (swiftui-preview-setup-check-all))
        (inhibit-read-only t))
    (erase-buffer)
    
    ;; Header
    (insert "\n")
    (insert "  ╔═══════════════════════════════════════════════════════╗\n")
    (insert "  ║           SwiftUI Preview Setup Wizard                ║\n")
    (insert "  ╚═══════════════════════════════════════════════════════╝\n")
    (insert "\n")
    
    ;; Dependencies status
    (insert "  Checking dependencies...\n\n")
    
    ;; Xcode CLI
    (let ((xcode-ok (plist-get deps :xcode-cli)))
      (swiftui-preview-setup--insert-status
       "Xcode command line tools" xcode-ok))
    
    ;; Ruby
    (let* ((ruby-info (plist-get deps :ruby))
           (ruby-ok (plist-get ruby-info :installed))
           (ruby-version (plist-get ruby-info :version)))
      (swiftui-preview-setup--insert-status
       "Ruby" ruby-ok
       (when ruby-ok (car (split-string ruby-version " " t)))))
    
    ;; xcodeproj gem
    (let* ((gem-info (plist-get deps :xcodeproj))
           (gem-ok (plist-get gem-info :installed))
           (gem-version (plist-get gem-info :version)))
      (swiftui-preview-setup--insert-status
       "xcodeproj gem" gem-ok
       (when gem-ok (format "v%s" gem-version))))
    
    ;; Simulator
    (let* ((sim-info (plist-get deps :simulator))
           (sim-ok (plist-get sim-info :available))
           (booted (plist-get sim-info :booted-count)))
      (swiftui-preview-setup--insert-status
       "iOS Simulator" sim-ok
       (when sim-ok (format "%d booted" booted))))
    
    (insert "\n")
    
    ;; Actions based on status
    (let ((xcode-ok (plist-get deps :xcode-cli))
          (ruby-ok (plist-get (plist-get deps :ruby) :installed))
          (gem-ok (plist-get (plist-get deps :xcodeproj) :installed))
          (sim-ok (plist-get (plist-get deps :simulator) :available)))
      
      (cond
       ;; All OK
       ((and xcode-ok ruby-ok gem-ok sim-ok)
        (insert "  All dependencies satisfied! You're ready to use SwiftUI Preview.\n\n")
        (insert "  Usage:\n")
        (insert "    1. Open a Swift file with a #Preview block\n")
        (insert "    2. Run M-x swiftui-preview-generate\n")
        (insert "    3. Or use the transient menu: M-x swiftui-preview-transient\n"))
       
       ;; Missing Xcode CLI
       ((not xcode-ok)
        (insert "  Xcode command line tools are required.\n\n")
        (insert "  Press [x] to install Xcode CLI tools\n"))
       
       ;; Missing Ruby (unlikely on macOS)
       ((not ruby-ok)
        (insert "  Ruby is required but not found.\n\n")
        (insert "  Install Ruby via Homebrew:\n")
        (insert "    brew install ruby\n"))
       
       ;; Missing xcodeproj gem
       ((not gem-ok)
        (insert "  The 'xcodeproj' Ruby gem is required for Xcode project\n")
        (insert "  integration. This gem allows us to dynamically inject\n")
        (insert "  preview targets into your project.\n\n")
        (insert "  Press [i] to install xcodeproj gem\n"))
       
       ;; No simulator
       ((not sim-ok)
        (insert "  No iOS Simulator found. Open Xcode and download\n")
        (insert "  a simulator runtime.\n"))))
    
    (insert "\n")
    (insert "  ─────────────────────────────────────────────────────────\n")
    (insert "  [r] Refresh   [i] Install gem   [x] Install Xcode CLI   [q] Quit\n")
    
    (goto-char (point-min))))

(defvar swiftui-preview-setup-wizard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'swiftui-preview-setup-wizard-refresh)
    (define-key map (kbd "i") #'swiftui-preview-setup-wizard-install-gem)
    (define-key map (kbd "x") #'swiftui-preview-setup-wizard-install-xcode)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'swiftui-preview-setup-wizard-action)
    map)
  "Keymap for SwiftUI Preview Setup Wizard.")

(define-derived-mode swiftui-preview-setup-wizard-mode special-mode "SwiftUI-Setup"
  "Major mode for SwiftUI Preview Setup Wizard."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun swiftui-preview-setup-wizard-refresh ()
  "Refresh the setup wizard display."
  (interactive)
  (swiftui-preview-setup--wizard-refresh))

(defun swiftui-preview-setup-wizard-install-gem ()
  "Install xcodeproj gem from wizard."
  (interactive)
  (when (y-or-n-p "Install xcodeproj gem? ")
    (swiftui-preview-setup--install-xcodeproj-gem)
    (swiftui-preview-setup--wizard-refresh)))

(defun swiftui-preview-setup-wizard-install-xcode ()
  "Install Xcode CLI tools from wizard."
  (interactive)
  (when (y-or-n-p "Install Xcode command line tools? ")
    (swiftui-preview-setup--install-xcode-cli)))

(defun swiftui-preview-setup-wizard-action ()
  "Perform context-sensitive action."
  (interactive)
  (let* ((deps (swiftui-preview-setup-check-all))
         (gem-ok (plist-get (plist-get deps :xcodeproj) :installed)))
    (unless gem-ok
      (swiftui-preview-setup-wizard-install-gem))))

;;;###autoload
(defun swiftui-preview-setup-wizard ()
  "Open the SwiftUI Preview setup wizard.
Checks dependencies and guides through installation."
  (interactive)
  (let ((buffer (get-buffer-create swiftui-preview-setup--wizard-buffer)))
    (with-current-buffer buffer
      (swiftui-preview-setup-wizard-mode)
      (swiftui-preview-setup--wizard-refresh))
    (pop-to-buffer buffer)))

;;;###autoload
(defun swiftui-preview-ensure-ready ()
  "Ensure SwiftUI Preview is ready to use.
Runs setup wizard if dependencies are missing.
Returns t if ready, nil if setup was cancelled."
  (if (swiftui-preview-setup-check)
      t
    (swiftui-preview-setup-wizard)
    ;; Wait for user to complete setup
    (swiftui-preview-setup-check t)))

(provide 'swiftui-preview-setup)
;;; swiftui-preview-setup.el ends here
