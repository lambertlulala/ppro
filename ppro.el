;;; ppro.el --- Functionality for Windows PowerPro  -*- lexical-binding:t -*-

;;;###autoload
(defun ppro-get-org-entries (file)
  "Get all information from an org file."
  (with-temp-buffer
    (org-mode)
    (when (file-exists-p file)
      (insert-file-contents file nil nil nil 'replace)
      (goto-char (point-min))
      (org-map-entries #'(lambda ()
			   (list
			    (org-entry-get nil "TODO")
			    (org-entry-get nil "ITEM")
			    (org-entry-get nil "SCHEDULED")))))))

;;;###autoload
(defun ppro-export-org-agenda-to-ini (file pproini)
  "Export org agenda from file to pproini file."
  (let ((entry-list (ppro-get-org-entries file))
        (cnt 0))
    (when entry-list
      (with-temp-file pproini
        (dolist (entry entry-list)
          (let ((state (nth 0 entry))
                (item (nth 1 entry))
                (timestamp (nth 2 entry)))
            (when (and timestamp (string-equal state "TODO"))
              (setq cnt (1+ cnt))
              (insert (format "[%d]\nEvent = %s\nCmd1 = %s\nWork1 = %s\nHow1 = %s\n"
                              cnt (format-time-string "%Y%m%d %H%M" (org-read-date nil t timestamp nil))
                              "*Message" item "topmost")))))))))

(defvar ppro-path (concat (getenv "programfiles(x86)") "\\PowerPro\\Powerpro.exe")
  "The path to PowerPro.")

;;;###autoload
(defun ppro-execute-command(parameters)
  "Execute PowerPro command."
  (interactive (list (read-shell-command "PowerPro command: ")))
  (when parameters
    (w32-shell-execute "open" ppro-path parameters 1)
    (message "Execute PowerPro command: %s" parameters)))

;;;###autoload
(defun ppro-import-org-agenda ()
  "The main routine that exports agenda to an ini file and calls PowerPro to import it."
  (interactive)
  (let* ((tmpini (concat (make-temp-file "sched") ".ini"))
         (parameters (format ".ImportScheduledEvents@importEvents(\"%s\", %s)" tmpini "1")))
    (require 'org)
    (ppro-export-org-agenda-to-ini org-default-notes-file tmpini)
    (ppro-execute-command parameters)))

(provide 'ppro)
