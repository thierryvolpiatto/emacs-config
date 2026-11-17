;;; ofp.el --- Offlineimap UI for Gnus -*- lexical-binding: t -*-

;;; Code:

(defvar ofp--progress-reporter nil)
(defvar ofp-update-pre-hook nil)
(defvar ofp-command "offlineimap -u basic")
(defvar ofp-event-hooks nil)
(defvar ofp-quick-counter 10) ; Ensure a full update on startup.
(defvar ofp-max-number-quick-update 10)
(defvar ofp-command-full "offlineimap -u basic")
(defvar ofp-command-quick "offlineimap -q -u basic")

;;;###autoload
(defun ofp (&optional arg)
  "Run OfflineIMAP.
With a prefix arg don't run `ofp-event-hooks'."
  (interactive "P" gnus-group-mode)
  (let* ((buffer (get-buffer-create "*OfflineImap*"))
         (process-connection-type t)
         (num 0))
    (if (get-buffer-process buffer)
        (message "Offlineimap currently running")
      (run-hooks 'ofp-update-pre-hook)
      (setq ofp--progress-reporter
            (make-progress-reporter
             (format "Retrieving mails...")))
      (let ((process (start-process-shell-command
                      "offlineimap"
                      buffer
                      ofp-command)))
        (set-process-filter
         process
         (lambda (proc str)
           (when ofp--progress-reporter
             (progress-reporter-update ofp--progress-reporter))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert str)
             (when (string-match "\\`Copy message" str)
               (incf num)))))
        (set-process-sentinel
         process
         (lambda (proc _event)
           (if (or (not (eq (process-status proc) 'exit))
                   (/= (process-exit-status proc) 0))
               (message "Offlineimap returned with non-zero exit code")
             (when ofp--progress-reporter
               (progress-reporter-done ofp--progress-reporter))
             (unless (zerop num)
               (sit-for 1)
               (message "[%d] incoming message(s)" num))
             (sit-for 1)
             (unless arg (run-hooks 'ofp-event-hooks)))))))))

;; Make a full update all the
;; `ofp-max-number-offlineimap-quick-update' mail retrievals,
;; otherwise make only quick synchronizations.
(defun ofp-update-mail-quick-or-full ()
  (if (>= ofp-quick-counter ofp-max-number-quick-update)
      (setq ofp-command ofp-command-full
            ofp-quick-counter 0)
    (setq ofp-command ofp-command-quick)
    (cl-incf ofp-quick-counter)))


(provide 'ofp)

;;; ofp.el ends here
