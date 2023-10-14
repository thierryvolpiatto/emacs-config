;;; register-preview.el --- Enhance vanilla emacs register-preview -*- lexical-binding: t -*-

;;; code:

(defun register-preview-forward-line (arg)
  (let ((fn (if (> arg 0) #'eobp #'bobp))
        (posfn (if (> arg 0)
                   #'point-min
                 (lambda () (1- (point-max)))))
        str)
    (with-current-buffer "*Register Preview*"
      (let ((ovs (overlays-in (point-min) (point-max)))
            pos)
        (goto-char (if ovs
                       (overlay-start (car ovs))
                     (point-min)))
        (setq pos (point))
        (and ovs (forward-line arg))
        (when (and (funcall fn)
                   (or (> arg 0) (eql pos (point))))
          (goto-char (funcall posfn)))
        (setq str (buffer-substring-no-properties
                   (pos-bol) (1+ (pos-bol))))
        (remove-overlays)
        (with-selected-window (minibuffer-window)
          (delete-minibuffer-contents)
          (insert str))))))

(defun register-preview-next ()
  (interactive)
  (register-preview-forward-line 1))

(defun register-preview-previous ()
  (interactive)
  (register-preview-forward-line -1))

(defun register-type (register)
  (pcase (cdr register)
    ((pred stringp) 'string)
    ((pred markerp) 'marker)
    ((pred numberp) 'number)
    ((and reg (pred consp) (guard (window-configuration-p (car reg)))) 'window)
    ((and reg (pred consp) (guard (frameset-p (car reg)))) 'frame)
    (_ 'unknow)))

(defun register-of-type-alist (types)
  (if (memq 'all types)
      register-alist
    (cl-loop for register in register-alist
           when (memq (register-type register) types)
           collect register)))

(defun register-preview-1 (buffer &optional show-empty types)
  "Pop up a window showing the registers preview in BUFFER.
If SHOW-EMPTY is non-nil, show the window even if no registers.
Format of each entry is controlled by the variable `register-preview-function'."
  (when (or show-empty (consp register-alist))
    (with-current-buffer-window
     buffer
     (cons 'display-buffer-below-selected
	   '((window-height . fit-window-to-buffer)
	     (preserve-size . (nil . t))))
     nil
     (with-current-buffer standard-output
       (setq cursor-in-non-selected-windows nil)
       (mapc (lambda (elem)
               (when (get-register (car elem))
                 (insert (funcall register-preview-function elem))))
             (register-of-type-alist (or types '(all))))))))

(defun advice--register-read-with-preview (prompt)
  "Read and return a register name, possibly showing existing registers.
Prompt with the string PROMPT.
If `help-char' (or a member of `help-event-list') is pressed,
display such a window regardless."
  (let* ((buffer "*Register Preview*")
         (pat "")
         (map (let ((m (make-sparse-keymap)))
                (set-keymap-parent m minibuffer-local-map)
                m))
         types msg result timer)
    (pcase this-command
      (`insert-register (setq types '(string number)
                              msg   "Insert register `%s'"))
      (`jump-to-register (setq types '(window frame marker)
                               msg   "Jump to register `%s'"))
      (`_ (setq types '(all)
                msg   "Overwrite register `%s'")))
    (dolist (k (cons help-char help-event-list))
      (define-key map
          (vector k) (lambda ()
                       (interactive)
                       (unless (get-buffer-window buffer)
                         (with-selected-window (minibuffer-selected-window)
                           (register-preview-1 buffer 'show-empty types))))))
    (define-key map (kbd "<down>") 'register-preview-next)
    (define-key map (kbd "<up>")   'register-preview-previous)
    (define-key map (kbd "C-n")    'register-preview-next)
    (define-key map (kbd "C-p")    'register-preview-previous)
    (unless executing-kbd-macro
      (register-preview-1 buffer nil types))
    (unwind-protect
         (progn
           (minibuffer-with-setup-hook
               (lambda ()
                 (setq timer
                       (run-with-idle-timer
                        0.01 'repeat
                        (lambda ()
                          (with-selected-window (minibuffer-window)
                            (let ((input (minibuffer-contents)))
                              (when (> (length input) 1)
                                (setq input (substring input 0 1))
                                (delete-minibuffer-contents)
                                (insert input))
                              (when (not (string= input pat))
                                (setq pat input))))
                          (if (get-buffer-window buffer)
                              (with-current-buffer buffer
                                (let ((ov (make-overlay (point-min) (point-min))))
                                  (goto-char (point-min))
                                  (if (string= pat "")
                                      (remove-overlays)
                                    (if (re-search-forward (concat "^" pat) nil t)
                                        (progn (move-overlay
                                                ov
                                                (match-beginning 0) (pos-eol))
                                               (overlay-put ov 'face 'match)
                                               (with-selected-window (minibuffer-window)
                                                 (minibuffer-message msg pat)))
                                      (with-selected-window (minibuffer-window)
                                        (minibuffer-message
                                         "Register `%s' contains no text" pat))))))
                            (unless (string= pat "")
                              (let ((strs (mapcar (lambda (x)
                                                    (string (car x)))
                                                  register-alist)))
                                (if (member pat strs)
                                    (with-selected-window (minibuffer-window)
                                      (minibuffer-message msg pat))
                                  (with-selected-window (minibuffer-window)
                                    (minibuffer-message
                                     "Register `%s' contains no text" pat))))))))))
             (setq result (read-from-minibuffer prompt nil map)))
           (cl-assert (and result (not (string= result "")))
                      nil "No register specified")
           (string-to-char result))
      (when timer (cancel-timer timer))
      (let ((w (get-buffer-window buffer)))
        (and (window-live-p w) (delete-window w)))
      (and (get-buffer buffer) (kill-buffer buffer)))))
(advice-add 'register-read-with-preview :override #'advice--register-read-with-preview)

(provide 'register-preview)

;;; register-preview ends here
