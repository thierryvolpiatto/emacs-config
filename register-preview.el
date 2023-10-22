;;; register-preview.el --- Enhance vanilla emacs register-preview -*- lexical-binding: t -*-

;;; code:

(eval-when-compile (require 'cl-lib))
(require 'register)

(declare-function frameset-p "frameset")

(defvar register-preview-default-keys '("a" "b" "c" "d" "e" "f" "g"
                                        "h" "i" "j" "k" "l" "m" "n"
                                        "o" "p" "q" "r" "s" "t" "u"
                                        "v" "w" "x" "y" "z")
  "The keys to use for setting a new register.")

(defvar register-use-preview t
  "Always show register preview when non nil.")

(defun register-preview-forward-line (arg)
  "Move to next or previous line in register preview buffer.
If ARG is positive goto next line, if negative to previous.
Do nothing when defining or executing kmacros."
  ;; Ensure user enter manually key in minibuffer when recording a macro.
  (unless (or defining-kbd-macro executing-kbd-macro
              (not (get-buffer-window "*Register Preview*" 'visible)))
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
            (insert str)))))))

(defun register-preview-next ()
  "Goto next line in register preview buffer."
  (interactive)
  (register-preview-forward-line 1))

(defun register-preview-previous ()
  "Goto previous line in register preview buffer."
  (interactive)
  (register-preview-forward-line -1))

(defun register-type (register)
  "Return REGISTER type.
One of string, marker, number, window or frame.
Returns unknow if REGISTER doesn't belong to one of these types."
  (require 'frameset)
  (pcase (cdr register)
    ((pred stringp) 'string)
    ((pred markerp) 'marker)
    ((pred numberp) 'number)
    ((and reg (pred consp) (guard (window-configuration-p (car reg)))) 'window)
    ((and reg (pred consp) (guard (frameset-p (car reg)))) 'frame)
    (_ 'unknow)))

(defun register-of-type-alist (types)
  "Filter `register-alist' according to TYPES."
  (if (memq 'all types)
      register-alist
    (cl-loop for register in register-alist
             when (memq (register-type register) types)
             collect register)))

(defun register-preview-1 (buffer &optional show-empty types)
  "Pop up a window showing the registers preview in BUFFER.
If SHOW-EMPTY is non-nil, show the window even if no registers.
Argument TYPES (a list) specify the types of register to show, when nil show all
registers, see `register-type' for suitable types. 
Format of each entry is controlled by the variable `register-preview-function'."
  (let ((registers (register-of-type-alist (or types '(all)))))
    (when (or show-empty (consp registers))
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
                registers))))))

(defun register-preview-get-defaults (action)
  "Returns available keys in `register-preview-default-keys'.
It is the keys not already token in `register-alist' according to ACTION."
  (unless (memq action '(insert jump))
    (cl-loop for s in register-preview-default-keys
             unless (assoc (string-to-char s) register-alist)
             collect s)))

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
         types msg result timer act win strs)
    (cl-case this-command
      (insert-register (setq types '(string number)
                             msg   "Insert register `%s'"
                             act   'insert))
      (jump-to-register (setq types '(window frame marker)
                              msg   "Jump to register `%s'"
                              act   'jump))
      (t (setq types '(all)
               msg   "Overwrite register `%s'"
               act   'set)))
    (setq strs (mapcar (lambda (x)
                         (string (car x)))
                       (register-of-type-alist types)))
    (when (and (memq act '(insert jump)) (null strs))
      (error "No register suitable for `%s'" act))
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
    (unless (or executing-kbd-macro (null register-use-preview))
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
                          (if (setq win (get-buffer-window buffer))
                              (with-selected-window win
                                (let ((ov (make-overlay (point-min) (point-min))))
                                  (goto-char (point-min))
                                  (remove-overlays)
                                  (unless (string= pat "")
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
                              (if (member pat strs)
                                  (with-selected-window (minibuffer-window)
                                    (minibuffer-message msg pat))
                                (with-selected-window (minibuffer-window)
                                  (minibuffer-message
                                   "Register `%s' contains no text" pat)))))))))
             (setq result (read-from-minibuffer
                           prompt nil map nil nil (register-preview-get-defaults act))))
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
