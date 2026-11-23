;;; ofp.el --- Mail sync UI for Gnus -*- lexical-binding: t -*-

;;; commentary: Usable with offlineimap and mbsync.

;;; Code:

(defvar ofp-command "mbsync -q -V posteo")
(defvar ofp-update-pre-hook nil)
(defvar ofp-event-hooks nil)
(defvar ofp-buffer "*Mbsync*")

;; internals
(defvar ofp--progress-reporter nil)

;; Specific to offlineimap, allow running offlineimap -q
;; `ofp-max-number-quick-update' times before running a full update
;; without -q.
(defvar ofp-quick-counter 10) ; Ensure a full update on startup.
(defvar ofp-max-number-quick-update 10)
(defvar ofp-command-full "offlineimap -u basic")
(defvar ofp-command-quick "offlineimap -q -u basic")

;;;###autoload
(defun ofp (&optional arg)
  "Run OfflineIMAP.
With a prefix arg don't run `ofp-event-hooks'."
  (interactive "P" gnus-group-mode)
  (let* ((buffer (get-buffer-create ofp-buffer))
         (process-connection-type t)
         (cmd-name (car (split-string ofp-command)))
         (num 0)
         process)
    (if (get-buffer-process buffer)
        (message "%s currently running" cmd-name)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (propertize (format "*** %s: Running %s\n"
                                    (format-time-string "%c") cmd-name)
                            'face 'font-lock-variable-name-face)))
      (run-hooks 'ofp-update-pre-hook)
      (setq ofp--progress-reporter
            (make-progress-reporter
             (format "Retrieving mails...")))
      (setq process (start-process-shell-command
                     cmd-name
                     buffer
                     ofp-command))
      (set-process-filter
       process
       (lambda (proc str)
         (when ofp--progress-reporter
           (progress-reporter-update ofp--progress-reporter))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           ;; Clean the mbsync counter.
           (if (string-match ".*\r\\(.*\\)" str)
               (progn
                 (delete-region (pos-bol) (pos-eol))
                 (insert (match-string 1 str)))
             (insert str))
           (when (and (string= cmd-name "offlineimap")
                      (string-match "\\`Copy message" str))
             (incf num)))))
      (set-process-sentinel
       process
       (lambda (proc _event)
         (if (or (not (eq (process-status proc) 'exit))
                 (/= (process-exit-status proc) 0))
             (message "%s returned with non-zero exit code" cmd-name)
           (when ofp--progress-reporter
             (progress-reporter-done ofp--progress-reporter))
           (unless (zerop num)
             (sit-for 1)
             (message "[%d] incoming message(s)" num))
           (sit-for 1)
           (unless arg (run-hooks 'ofp-event-hooks))))))))

;; Specific to Offlineimap. Make a full update all the
;; `ofp-max-number-quick-update' mail retrievals,
;; otherwise make only quick synchronizations.
(defun ofp-update-mail-quick-or-full ()
  "Make a full or quick offlineimap update.
The full update is done all the `ofp-max-number-quick-update'
seconds, otherwise a quick update is done, that is the -q offlineimap
arg is used.  Should run before running offlineimap in `ofp-update-pre-hook'."
  (if (>= ofp-quick-counter ofp-max-number-quick-update)
      (setq ofp-command ofp-command-full
            ofp-quick-counter 0)
    (setq ofp-command ofp-command-quick)
    (incf ofp-quick-counter)))


(provide 'ofp)

;;; ofp.el ends here
