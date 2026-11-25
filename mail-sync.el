;;; mail-sync.el --- Mbsync UI for Gnus -*- lexical-binding: t -*-

;;; Commentary: None.

;;; Code:

(defvar mail-sync-command "mbsync -q -V posteo")
(defvar mail-sync-update-pre-hook nil)
(defvar mail-sync-event-hook nil)
(defvar mail-sync-buffer "*Mbsync*")

;; internals
(defvar mail-sync--progress-reporter nil)

;;;###autoload
(defun mail-sync (&optional arg)
  "Sync mails with `mail-sync-command'.
With a prefix arg don't run `mail-sync-event-hook'."
  (interactive "P" gnus-group-mode)
  (let* ((buffer (get-buffer-create mail-sync-buffer))
         (cmd-name (car (split-string mail-sync-command)))
         (num 0)
         ;; Ensure the mbsync -q really disable its reporter.
         (process-connection-type nil)
         process)
    (if (get-buffer-process buffer)
        (message "%s currently running" cmd-name)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "*** %s: Running %s\n"
                                      (format-time-string "%c") cmd-name)
                              'face 'font-lock-variable-name-face))))
      (run-hooks 'mail-sync-update-pre-hook)
      (setq mail-sync--progress-reporter
            (make-progress-reporter "Retrieving mails..."))
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
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert str)))))
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when mail-sync--progress-reporter
           (progress-reporter-done mail-sync--progress-reporter))
         (if (or (not (eq (process-status proc) 'exit))
                 (/= (process-exit-status proc) 0))
             (message "%s returned with non-zero exit code" cmd-name)
           ;; Mbsync incomings mails.
           (when (string= cmd-name "mbsync")
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (save-excursion
                 (when (re-search-backward "Near: \\+\\([[:digit:]]+\\)" nil t)
                   (setq num (string-to-number (match-string 1)))))
               (special-mode)))
           (unless arg (run-hooks 'mail-sync-event-hook))
           (unless (zerop num)
             (message "[%d] incoming message(s)" num))))))))


(provide 'mail-sync)

;;; mail-sync.el ends here
