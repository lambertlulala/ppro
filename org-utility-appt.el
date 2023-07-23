;;; org-utility-appt.el --- Some scripts for org appt  -*- lexical-binding:t -*-

;;;###autoload
(defun org-utility-org-agenda-to-appt ()
  "Clear `appt-time-msg-list' and run `org-agenda-to-appt'."
  (interactive)
  (setq appt-time-msg-list nil)
  (when (equal major-mode 'org-mode)
    (org-agenda-to-appt)))

;;;###autoload
(defun org-utility-save-org-buffer (&optional arg)
  "Save the buffer after `org-agenda-to-appt'."
  (interactive "p")
  (save-buffer arg)
  (org-utility-org-agenda-to-appt))

;;;###autoload
(defun org-utility-initialize(auto-update)
  "Initialize org-utility. If auto-update is not nil, automatically convert agendas in
the directory of `org-default-notes-file' to appt."
  (require 'appt)
  (setq appt-message-warning-time 0
	appt-display-format 'window)

  (require 'org)
  (org-utility-org-agenda-to-appt)
  (appt-activate 1)

  (when auto-update
    (advice-add 'save-buffer :before #'(lambda (&optional arg)
					 (when (and (buffer-modified-p)
						    (string-match-p
                                                     (file-name-directory org-default-notes-file)
						     (or buffer-file-name org-default-notes-file)))
					   (org-utility-org-agenda-to-appt))))))

(provide 'org-utility-appt)
