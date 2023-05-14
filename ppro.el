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

(defvar pproconf-path (concat (getenv "programfiles(x86)") "\\PowerPro\\pproconf.exe")
  "The path to pproconf.")

(defvar ppro-script-path (concat (getenv "APPDATA") "\\PowerPro\\scripts")
  "The path to ppro scripts.")

;;;###autoload
(defun ppro-generate-command-list ()
  "Generate a PowerPro command list from the PowerPro script path."
  (let ((regexp "^function \\(.*\\)$")
	(file-list (directory-files ppro-script-path nil ".powerpro$"))
	(command-list nil))
    (with-temp-buffer
      (dolist (file file-list)
	(let ((file-path (format "%s\\%s" ppro-script-path file)))
	  (when (file-exists-p file-path)
	    (insert-file-contents file-path nil nil nil 'replace)
	    (goto-char (point-min))
	    (while (re-search-forward regexp nil t)
	      (let ((function (match-string 1)))
		(when (not (string-match-p "_autoinit_" function))
		  (push (format ".%s@%s" (file-name-sans-extension file) (match-string 1)) command-list))))))))
    command-list))

;;;###autoload
(defun ppro-execute-command(parameters)
  "Execute PowerPro command."
  (interactive (list (completing-read "PowerPro command: " (ppro-generate-command-list))))
  (when parameters
    (w32-shell-execute "open" ppro-path parameters 1)
    (message "Execute PowerPro command: %s" parameters)))

;;;###autoload
(defun ppro-configure ()
  "Configure PowerPro."
  (interactive)
  (let* ((ppro-data-path (concat (getenv "APPDATA") "\\PowerPro"))
	 (conf-list (directory-files ppro-data-path nil ".pcf$"))
	 (conf-file (format "%s\\%s" ppro-data-path (completing-read "Select a PowerPro configuratuon file: " conf-list))))
    (w32-shell-execute "open" pproconf-path conf-file 1)))

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
