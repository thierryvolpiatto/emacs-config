;;; eshell-command-advice.el --- Improve eshell-command output -*- lexical-binding: t -*-

(require 'eshell)

(defvar eshell-non-interactive-p)

(defcustom eshell-command-show-output '(sync)
  "When to show output of eshell-command.
This is a list like \\='(sync async), if the list is empty no output
is shown."
  :type '(set
          (const :tag "Show output for async commands" async)
          (const :tag "Show output for sync commands" sync))
  :group 'eshell
  :version "32.1")

(defun tv:advice-eshell-command (command &optional output-target error-target)
  "Execute the Eshell command string COMMAND.
If OUTPUT-TARGET is t (interactively, with the prefix argument), write
the command's standard output to the current buffer at point.  If nil,
write the output to a new output buffer.  For any other value, output to
that Eshell target (see `eshell-get-target').

ERROR-TARGET is similar to OUTPUT-TARGET, except that it controls where
to write standard error, and a nil value means to write standard error
to the same place as standard output.  (To suppress standard error, you
can write to the Eshell virtual target \"/dev/null\".)

When \"&\" is added at end of command, the command is async and its
output appears in a specific buffer.  You can customize
`eshell-command-async-buffer' to specify what to do when this output
buffer is already taken by another running shell command."
  (interactive (list (eshell-read-command)
                     (not (not current-prefix-arg))))
  (save-excursion
    (let ((stdout (cond ((eq output-target t) (current-buffer))
                        ((not output-target) t)
                        (t output-target)))
          (stderr (if (eq error-target t) (current-buffer) error-target))
          (buf (set-buffer (generate-new-buffer " *eshell cmd*")))
	  (eshell-non-interactive-p t))
      (eshell-mode)
      (let* ((proc (eshell-eval-command
                    `(eshell-with-handles (',stdout 'insert ',stderr 'insert)
                       (let ((eshell-current-subjob-p))
		         ,(eshell-parse-command command)))
                    command))
             (async (eq (car-safe proc) :eshell-background))
             (bufname (cond
                       ((not (eq stdout t)) nil)
                       (async eshell-command-buffer-name-async)
                       (t eshell-command-buffer-name-sync)))
             (show-output
              (cond ((string= bufname eshell-command-buffer-name-async)
                     (memq 'async eshell-command-show-output))
                    ((string= bufname eshell-command-buffer-name-sync)
                     (memq 'sync eshell-command-show-output))))
             unique)
        (when bufname
          (when (buffer-live-p (get-buffer bufname))
            (cond
             ((with-current-buffer bufname
                (and (null eshell-foreground-command)
                     (null eshell-background-commands)))
              ;; The old buffer is done executing; kill it so we can
              ;; take its place.
              (kill-buffer bufname))
             ((eq eshell-command-async-buffer 'confirm-kill-process)
              (shell-command--same-buffer-confirm "Kill it")
              (with-current-buffer bufname
                ;; Stop all the processes in the old buffer (there may
                ;; be several).
                (eshell-round-robin-kill))
              (kill-buffer bufname))
             ((eq eshell-command-async-buffer 'confirm-new-buffer)
              (shell-command--same-buffer-confirm "Use a new buffer")
              (setq unique t))
             ((eq eshell-command-async-buffer 'new-buffer)
              (setq unique t))
             ((eq eshell-command-async-buffer 'confirm-rename-buffer)
              (shell-command--same-buffer-confirm "Rename it")
              (with-current-buffer bufname
                (rename-uniquely)))
             ((eq eshell-command-async-buffer 'rename-buffer)
              (with-current-buffer bufname
                (rename-uniquely)))))
          (rename-buffer bufname unique))
	;; things get a little coarse here, since the desire is to
	;; make the output as attractive as possible, with no
	;; extraneous newlines
        (unless async
	  (funcall #'eshell-wait-for-processes (cadr eshell-foreground-command))
	  (cl-assert (not eshell-foreground-command))
	  (goto-char (point-max))
	  (while (and (bolp) (not (bobp)))
	    (delete-char -1)))
	(cl-assert (and buf (buffer-live-p buf)))
        (when show-output
          (let ((len (if async 2
		       (count-lines (point-min) (point-max)))))
	    (cond
	     ((= len 0)
	      (message "(There was no command output)")
	      (kill-buffer buf))
	     ((= len 1)
	      (message "%s" (buffer-string))
	      (kill-buffer buf))
	     (t
	      (save-selected-window
		(select-window (display-buffer buf))
		(goto-char (point-max))
		;; cause the output buffer to take up as little screen
		;; real-estate as possible, if temp buffer resizing is
		;; enabled
                (and (not async) temp-buffer-resize-mode
		     (resize-temp-buffer-window)))))))))))


(provide 'eshell-command-advice)

;;; eshell-command-advice.el ends here
