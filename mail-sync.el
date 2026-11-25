;;; mail-sync.el --- Mail sync UI for Gnus -*- lexical-binding: t -*-

;;; commentary: Usable with offlineimap and mbsync.

;;; Code:

(defvar mail-sync-command "mbsync -q -V posteo")
(defvar mail-sync-update-pre-hook nil)
(defvar mail-sync-event-hooks nil)
(defvar mail-sync-buffer "*Mbsync*")

;; internals
(defvar mail-sync--progress-reporter nil)

;; Specific to offlineimap, allow running offlineimap -q
;; `mail-sync-max-number-quick-update' times before running a full update
;; without -q.
(defvar mail-sync-quick-counter 10) ; Ensure a full update on startup.
(defvar mail-sync-max-number-quick-update 10)
(defvar mail-sync-command-full "offlineimap -u basic")
(defvar mail-sync-command-quick "offlineimap -q -u basic")

;;;###autoload
(defun mail-sync (&optional arg)
  "Sync mails with `mail-sync-command'.
With a prefix arg don't run `mail-sync-event-hooks'."
  (interactive "P" gnus-group-mode)
  (let* ((buffer (get-buffer-create mail-sync-buffer))
         (process-connection-type t)
         (cmd-name (car (split-string mail-sync-command)))
         process-connection-type
         (num 0)
         process)
    (if (get-buffer-process buffer)
        (message "%s currently running" cmd-name)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (propertize (format "*** %s: Running %s\n"
                                    (format-time-string "%c") cmd-name)
                            'face 'font-lock-variable-name-face)))
      (run-hooks 'mail-sync-update-pre-hook)
      (setq mail-sync--progress-reporter
            (make-progress-reporter
             (format "Retrieving mails...")))
      (setq process (start-process-shell-command
                     cmd-name
                     buffer
                     mail-sync-command))
      (set-process-filter
       process
       (lambda (proc str)
         (when mail-sync--progress-reporter
           (progress-reporter-update mail-sync--progress-reporter))
         (with-current-buffer (process-buffer proc)
           (goto-char (point-max))
           (insert str)
           (when (and (string= cmd-name "offlineimap")
                      (string-match "\\`Copy message" str))
             (incf num)))))
      (set-process-sentinel
       process
       (lambda (proc _event)
         (if (or (not (eq (process-status proc) 'exit))
                 (/= (process-exit-status proc) 0))
             (message "%s returned with non-zero exit code" cmd-name)
           (when mail-sync--progress-reporter
             (progress-reporter-done mail-sync--progress-reporter))
           ;; Mbsync incomings mails.
           (when (string= cmd-name "mbsync")
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (save-excursion
                 (when (re-search-backward "Near: \\+\\([[:digit:]]+\\)" nil t)
                   (setq num (string-to-number (match-string 1)))))))
           (unless (zerop num)
             (sit-for 1)
             (message "[%d] incoming message(s)" num))
           (sit-for 1)
           (unless arg (run-hooks 'mail-sync-event-hooks))))))))

;; Specific to Offlineimap. Make a full update all the
;; `mail-sync-max-number-quick-update' mail retrievals,
;; otherwise make only quick synchronizations.
(defun mail-sync-update-mail-quick-or-full ()
  "Make a full or quick offlineimap update.
The full update is done all the `mail-sync-max-number-quick-update'
seconds, otherwise a quick update is done, that is the -q offlineimap
arg is used.  Should run before running offlineimap in
`mail-sync-update-pre-hook'."
  (if (>= mail-sync-quick-counter mail-sync-max-number-quick-update)
      (setq mail-sync-command mail-sync-command-full
            mail-sync-quick-counter 0)
    (setq mail-sync-command mail-sync-command-quick)
    (incf mail-sync-quick-counter)))


(provide 'mail-sync)

;;; mail-sync.el ends here
