;;; tv-utils.el --- Some useful functions for Emacs. -*- lexical-binding: t -*-
;; 

;;; Code:

(require 'cl-lib)

(declare-function helm-find-files-1             "ext:helm-files.el")
(declare-function mailcap-extension-to-mime     "mailcap.el")
(declare-function calendar-exit                 "calendar.el")
(declare-function helm-region-active-p          "ext:helm-lib.el")
(declare-function helm-basename                 "ext:helm-lib.el")
(declare-function helm-read-file-name           "ext:helm-mode.el")
(declare-function common-lisp-indent-function-1 "cl-indent.el")
(declare-function tv/get-disk-info              "ext:dired-extension.el")
(declare-function iterator:circular             "ext:iterator.el")
(declare-function iterator:next                 "ext:iterator.el")
(declare-function helm-fast-remove-dups         "ext:helm-lib.el")
(declare-function auth-source-search            "auth-source.el")
(declare-function eshell-interactive-process    "esh-cmd.el")
(declare-function which-function                "which-func.el")
(declare-function tramp-get-completion-function "tramp")
(declare-function help--symbol-completion-table "help-fns.el")
(declare-function org-agenda-mode "org-agenda.el")
(declare-function message-goto-to       "message.el")
(declare-function message-insert-header "message.el")
(declare-function message-goto-subject  "message.el")
(declare-function help-fns-short-filename          "help-fns.el")
(declare-function help-fns--run-describe-functions "help-fns.el")

(defvar tramp-methods)
(defvar help-fns-describe-variable-functions)
(defvar Info-current-file)
(defvar Info-current-node)
;;; Sshfs
;;
;;
;;;###autoload
(defun tv/mount-sshfs (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let* ((user (if arg
                  (read-string "User name: ")
                (getenv "USER")))
         (host (completing-read
                "Host: "
                (delete-dups
                 (cl-loop with all-methods = (mapcar 'car tramp-methods)
                          for (f . h) in (tramp-get-completion-function "ssh")
                          append (cl-loop for e in (funcall f (car h))
                                          for host = (and (consp e) (cadr e))
                                          ;; On emacs-27 host may be
                                          ;; ("root" t) in sudo method.
                                          when (and (stringp host)
                                                    (not (member host all-methods)))
                                          collect host)))))
         (fs (concat host ":/home/" user))
         (mp (concat "~/sshfs/" user)))
    (unless (file-directory-p mp)
      (make-directory mp t))
    (if (> (length (directory-files
                    mp nil directory-files-no-dot-files-regexp))
           0)
        (message "Directory %s is busy, mountsshfs aborted" mp)
      (if (= (call-process-shell-command
              (format "sshfs %s %s" fs mp))
             0)
          (message "%s Mounted successfully on %s" fs mp)
        (message "Failed to mount remote filesystem %s on %s" fs mp)))))

;;;###autoload
(defun tv/umount-sshfs ()
  (interactive)
  (let ((mp (read-directory-name "Mount point: " "~/sshfs")))
    (if (file-equal-p default-directory mp)
        (message "Filesystem is busy can't umount!")
      (if (>= (length (cddr (directory-files mp))) 0)
          (if (= (call-process-shell-command
                  (format "fusermount -u %s" mp))
                 0)
              (message "%s Successfully unmounted" mp)
            (message "Failed to unmount %s" mp))
        (message "No existing remote filesystem to unmount!")))))

;;; move-to-window-line 
;;
;;;###autoload
(defun screen-top (&optional n)
  "Move the point to the top of the screen."
  (interactive "p")
  (move-to-window-line (or n 0)))

;;;###autoload
(defun screen-bottom (&optional n)
  "Move the point to the bottom of the screen."
  (interactive "P")
  (move-to-window-line (- (prefix-numeric-value n))))

;;; switch-other-window 
;;
;;;###autoload
(defun other-window-backward (&optional n)
  "Move backward to other window or frame."
  (interactive "p")
  (other-window (- n) 0)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun other-window-forward (&optional n)
  "Move to other window or frame.
With a prefix arg move N window forward or backward
depending the value of N is positive or negative."
  (interactive "p")
  (other-window n 0)
  (select-frame-set-input-focus (selected-frame)))

;;; Eval-region
;;
;;
;;;###autoload
(defun tv/eval-region (beg end)
  (interactive "r")
  (let ((str (buffer-substring beg end))
        expr
        store)
    (with-temp-buffer
      (save-excursion
        (insert str))
      (condition-case _err
          (while (setq expr (read (current-buffer)))
            (push (eval expr) store))
        (end-of-file nil)))
    (message "Evaluated in Region:\n- %s"
             (mapconcat 'identity
                        (mapcar #'(lambda (x)
                                    (format "`%s'" x))
                                (reverse store))
                        "\n- "))))

;; key-for-calendar 
(defvar tv/calendar-alive nil)
;;;###autoload
(defun tv/toggle-calendar ()
  (interactive)
  (if tv/calendar-alive
      (when (get-buffer "*Calendar*")
        (with-current-buffer "diary" (save-buffer)) 
        (calendar-exit)) ; advice reset win conf
      ;; In case calendar were called without toggle command
      (unless (get-buffer-window "*Calendar*")
        (setq tv/calendar-alive (current-window-configuration))
        (calendar))))

(defadvice calendar-exit (after reset-win-conf activate)
  (when tv/calendar-alive
    (set-window-configuration tv/calendar-alive)
    (setq tv/calendar-alive nil)))

;;; Insert-pairs 
;;
(setq parens-require-spaces t)

;;;###autoload
(defun tv/insert-double-quote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))

;;;###autoload
(defun tv/insert-double-backquote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\` (if (or (eq major-mode 'emacs-lisp-mode)
                               (eq major-mode 'lisp-interaction-mode))
                           ?\' ?\`)))

;;;###autoload
(defun tv/insert-vector (&optional arg)
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

;;;###autoload
(defun tv/move-pair-forward (beg end)
  (interactive "r")
  (if (region-active-p)
      (progn (goto-char beg) (insert "(")
             (goto-char (1+ end)) (insert ")"))
    (let ((timer (run-with-idle-timer
                  5 nil (lambda () (keyboard-quit))))
          action kb com)
      (unwind-protect
           (catch 'break
             (while t
               (setq action (read-key
                             (propertize
                              "`(': Enclose forward, (any key to exit)."
                              'face 'minibuffer-prompt)))
               (cl-case action
                 (?\(
                  (skip-chars-forward " \n")
                  (insert "(")
                  (forward-sexp 1)
                  (insert ")"))
                 (t (setq kb  (this-command-keys-vector))
                    (setq com (lookup-key (current-local-map) kb))
                    (if (commandp com)
                        (call-interactively com)
                      (setq unread-command-events
                            (nconc (mapcar 'identity
                                           (this-single-command-raw-keys))
                                   unread-command-events)))
                    (throw 'break nil)))))
        (cancel-timer timer)))))

;;;###autoload
(defun tv/insert-pair-and-close-forward (beg end)
  (interactive "r")
  (if (region-active-p)
      (progn (goto-char beg) (insert "(")
             (goto-char (1+ end)) (insert ")"))
    (let ((timer (run-with-idle-timer
                  5 nil (lambda () (keyboard-quit))))
          action kb com)
      (unless (looking-back "(" (1- (point))) (insert "("))
      (unwind-protect
           (catch 'break
             (while t
               (setq action (read-key
                             (propertize
                              "`)': Move forward, (any key to exit)."
                              'face 'minibuffer-prompt)))
               (cl-case action
                 (?\)
                  (unless (looking-back "(" (1- (point)))
                    (delete-char -1))
                  (skip-chars-forward " ")
                  (if (looking-at "(")
                      (forward-sexp 1) (forward-symbol 1))
                  ;; move forward in a list of strings
                  (skip-chars-forward "\"")
                  (insert ")"))
                 (t (setq kb  (this-command-keys-vector))
                    (setq com (lookup-key (current-local-map) kb))
                    (if (commandp com)
                        (call-interactively com)
                      (setq unread-command-events
                            (nconc (mapcar 'identity
                                           (this-single-command-raw-keys))
                                   unread-command-events)))
                    (throw 'break nil)))))
        (cancel-timer timer)))))

;;;###autoload
(defun tv/insert-double-quote-and-close-forward (beg end)
  (interactive "r")
  (if (region-active-p)
      (progn (goto-char beg) (insert "\"")
             (goto-char (1+ end)) (insert "\""))
      (let (action kb com
            (prompt (and (not (minibufferp))
                         "\": Insert, (any key to exit).")))
        (unless prompt (message "\": Insert, (any key to exit)."))
        (catch 'break
          (while t
            (setq action (read-key prompt))
            (cl-case action
              (?\"
               (skip-chars-forward " \n")
               (insert "\"")
               (forward-sexp 1)
               (insert "\""))
              (t (setq kb  (this-command-keys-vector))
                 (setq com (lookup-key (current-local-map) kb))
                 (if (commandp com)
                     (call-interactively com)
                     (setq unread-command-events
                           (nconc (mapcar 'identity
                                          (this-single-command-raw-keys))
                                  unread-command-events)))
                 (throw 'break nil))))))))

(defun tv/view-echo-area-messages (old--fn &rest args)
  (let ((win (get-buffer-window (messages-buffer) 'visible)))
    (cond ((and win (one-window-p))
           (quit-window nil win))
          (win
           (delete-other-windows win))
          (t (apply old--fn args)))))

(defun tv/quit-echo-area-messages ()
  (interactive)
  (with-selected-window (get-buffer-window (messages-buffer))
    (quit-window)))

;; Kill-backward
;;;###autoload
(defun tv/kill-whole-line (&optional arg)
  "Similar to `kill-whole-line' but don't kill new line.
Also alow killing whole line in a shell prompt without trying
to kill prompt.
When called non interactively, do not delete empty line.
Can be used from any place in the line."
  (interactive "p")
  (end-of-line)
  (let ((end (point)) beg)
    (forward-line 0)
    (while (get-text-property (point) 'read-only)
      (forward-char 1))
    (setq beg (point)) (kill-region beg end))
  (when (and arg (eq (point-at-bol) (point-at-eol)))
    (delete-blank-lines) (skip-chars-forward " ")))

;; Kill-line
;;;###autoload
(defun tv/kill-line ()
  "Like kill-line but when at eol delete whole line.
Ignore text read-only at bol i.e. prompts."
  (interactive)
  (if (eolp)
      (tv/kill-whole-line)
    (kill-line)))

;; Delete-char-or-region
;;;###autoload
(defun tv/delete-char (arg)
  (interactive "p")
  (if (helm-region-active-p)
      (delete-region (region-beginning) (region-end))
      (delete-char arg)))

;; Easypg
(defvar epa-armor)
;;;###autoload
(defun epa-sign-to-armored ()
  "Create a .asc file."
  (interactive)
  (let ((epa-armor t))
    (call-interactively 'epa-sign-file)))

;; Same as above but usable as alias in eshell
;;;###autoload
(defun gpg-sign-to-armored (file)
  "Create a .asc file."
  (let ((epa-armor t))
    (epa-sign-file file nil nil)))

;; Usable from eshell as alias
;;;###autoload
(defun gpg-sign-to-sig (file)
  "Create a .sig file."
  (epa-sign-file file nil 'detached))

;;;###autoload
(defun tv/gpg-verify-file (gpg-file)
  "Meant to be used from eshell alias.
    alias gpg-verify tv/gpg-verify-file $1"
  (let ((data-file (directory-files
                    (file-name-directory (expand-file-name gpg-file)) t
                    (concat (regexp-quote (helm-basename gpg-file t)) "$"))))
    (cl-assert (member (file-name-extension gpg-file) '("gpg" "sig" "asc"))
               nil "Please select the signed file not the data file")
    (cl-assert (null (cdr data-file)) nil "Failed to find data-file")
    (setq data-file (car data-file))
    (with-temp-buffer
      (if (= (call-process "gpg" nil t nil "--verify" gpg-file data-file) 0)
          (buffer-string)
        "Gpg error while verifying signature"))))

;; Switch indenting lisp style.
;;;###autoload
(defun toggle-lisp-indent ()
  (interactive)
  (if (memq lisp-indent-function '(common-lisp-indent-function
                                   common-lisp-indent-function-1))
      (progn
        (setq lisp-indent-function #'lisp-indent-function)
        (message "Switching to Emacs lisp indenting style."))
    (setq lisp-indent-function #'common-lisp-indent-function-1)
    (message "Switching to Common lisp indenting style.")))

;; Permutations (Too slow)

(cl-defun permutations (bag &key result-as-string print)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (when (stringp bag) (setq bag (split-string bag "" t)))
  (let ((result
         (if (null bag)
             '(())
             ;; Otherwise, take an element, e, out of the bag.
             ;; Generate all permutations of the remaining elements,
             ;; And add e to the front of each of these.
             ;; Do this for all possible e to generate all permutations.
             (cl-loop for e in bag append
                      (cl-loop for p in (permutations (remove e bag))
                               collect (cons e p))))))
    (when (or result-as-string print)
      (setq result (cl-loop for i in result collect (mapconcat 'identity i ""))))
    (if print
        (with-current-buffer (get-buffer-create "*permutations*")
          (erase-buffer)
          (cl-loop for i in result
                   do (insert (concat i "\n")))
          (pop-to-buffer (current-buffer)))
        result)))

;; Check paren errors
;;;###autoload
(defun tv/check-paren-error ()
  (interactive)
  (let (pos-err)
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (forward-list 9999)
        (error
         (setq pos-err (cl-caddr err))
         nil)))
    (if pos-err
        (message "Paren error found in sexp starting at %s"
                 (goto-char pos-err))
      (message "No paren error found"))))

;;; Generate strong passwords.
;;
(defun tv/shuffle-vector (vector)
  "Shuffle VECTOR."
  (cl-loop with len = (1- (length vector))
           while (>= len 0)
           for rand = (random (1+ len))
           for old = (aref vector rand)
           do (progn
                (aset vector rand (aref vector len))
                (aset vector len old)
                (setq len (1- len)))
           finally return vector))

(defun tv/shuffle-sequence (seq)
  (cl-loop for i from (1- (length seq)) downto 1
           do (cl-rotatef (elt seq i) (elt seq (random i)))
           finally return seq))

;;;###autoload
(cl-defun genpasswd (&optional (limit 12))
  "Generate strong password of length LIMIT.
LIMIT should be a number divisible by 2, otherwise
the password will be of length (floor LIMIT)."
  (cl-loop with alph = ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                        "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                        "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G"
                        "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                        "S" "T" "U" "V" "W" "X" "Y" "Z" "#" "!" "$"
                        "&" "~" "-" "_" "@" "%" "*"]
           ;; Divide by 2 because collecting 2 list.
           for i from 1 to (floor (/ limit 2))
           for rand1 = (int-to-string (random 9))
           for alphaindex = (random (length alph))
           for rand2 = (aref (tv/shuffle-sequence alph) alphaindex)
           ;; Collect a random number between O-9
           concat rand1 into ls
           ;; collect a random alpha between a-zA-Z.
           concat rand2 into ls
           finally return ls))

;;;###autoload
(defun tv/generate-passwd (arg)
  "Generate a random password of (max 8 ARG) chars.
Use a prefix arg to specify ARG."
  (interactive "p")
  (message "New pwd `%s' saved to kill ring"
           (kill-new (genpasswd (max 8 arg)))))

;;;###autoload
(defun tv/gen-socgen-passwd ()
  (interactive)
  (let ((code (mapconcat (lambda (x) (number-to-string x))
                         (cl-loop with randoms = nil
                                  while (not (= (length randoms) 6))
                                  for random = (random 9)
                                  unless (member random randoms)
                                  do (push random randoms)
                                  finally return randoms)
                         "")))
    (kill-new code)
    (message "`%s' copied to kill-ring" code)))

;;; Toggle split window vertically/horizontally
;;
(defvar helm-alive-p)
;;;###autoload
(defun tv/toggle-window-split ()
  (interactive)
  (unless helm-alive-p
    (if (= (length (window-list)) 2)
        (let ((buf (current-buffer))
              before-height) 
          (with-current-buffer buf
            (setq before-height (window-height))
            (delete-window)
            (set-window-buffer
             (select-window (if (= (window-height) before-height)
                                (split-window-vertically)
                              (split-window-horizontally)))
             buf)))
      (user-error "Can toggle split only with two windows"))))
(global-set-key (kbd "C-x C-'") 'tv/toggle-window-split)

;;; Rotate windows
;;
;;
;;;###autoload
(defun tv/rotate-windows ()
  (interactive)
  (require 'iterator)
  (cl-assert (> (length (window-list)) 1)
          nil "Error: Can't rotate with a single window")
  (unless helm-alive-p
    (cl-loop with wlist1 = (iterator:circular (window-list))
             with wlist2 = (iterator:circular (cdr (window-list))) 
             with len = (length (window-list))
             for count from 1
             for w1 = (iterator:next wlist1)
             for b1 = (window-buffer w1)
             for s1 = (window-start w1)
             for w2 = (iterator:next wlist2)
             for b2 = (window-buffer w2)
             for s2 = (window-start w2)
             while (< count len)
             do (progn (set-window-buffer w1 b2)
                       (set-window-start w1 s2)
                       (set-window-buffer w2 b1)
                       (set-window-start w2 s1)))))
(global-set-key (kbd "C-c -") 'tv/rotate-windows)

;;;###autoload
(defun tv/pp-sexp (&optional arg)
  (interactive "p")
  (when (and (looking-at "(")
             (> (point-at-eol) (+ (point) 50)))
    (save-excursion
      (forward-char 1)
      (while (and (ignore-errors (forward-sexp) t)
                  (not (looking-at ")")))
        (when (> (current-column) (or goal-column fill-column))
          (if arg ; interactive
              (newline-and-indent)
            (newline)))))))

;; Stollen somewhere.
;;;###autoload
(defun tv/kill-kbd (key)
  (interactive "kKill `kbd' form: ")
  (kill-new (message "(kbd \"%s\")" (help-key-description key nil)))
  (message nil))

;;;###autoload
(defun tv/insert-kbd-at-point (key)
  (interactive "kInsert `kbd' form: ")
  (insert (format "(kbd \"%s\")" (help-key-description key nil)))
  (message nil))

(cl-defun tv/get-passwd-from-auth-sources (host &key user port)
  "Retrieve a password for auth-info file.
Arg `host' is machine in auth-info file."
  (let* ((token (auth-source-search :host host :port port :user user))
         (secret (plist-get (car token) :secret)))
    (if (functionp secret) (funcall secret) secret)))

;; Avoid typing password for sudo in eshell
(defun tv/advice--eshell-send-invisible ()
  (interactive) ; Don't pass str as argument, to avoid snooping via C-x ESC ESC
  (let ((str (read-passwd
	      (format "%s Password: "
		      (process-name (eshell-interactive-process)))
              nil (tv/get-passwd-from-auth-sources
                   "default" :user "root" :port "sudo"))))
    (if (stringp str)
	(process-send-string (eshell-interactive-process)
			     (concat str "\n"))
      (message "Warning: text will be echoed"))))

;;; Scroll functions
;;;###autoload
(defun tv/scroll-down ()
  (interactive)
  (scroll-down -1))
;;;###autoload
(defun tv/scroll-up ()
  (interactive)
  (scroll-down 1))
;;;###autoload
(defun tv/scroll-other-down ()
  (interactive)
  (scroll-other-window 1))
;;;###autoload
(defun tv/scroll-other-up ()
  (interactive)
  (scroll-other-window -1))

;;;###autoload
(defun tv/restore-scratch-buffer ()
  (unless (buffer-file-name (get-buffer "*scratch*"))
    (and (get-buffer "*scratch*") (kill-buffer "*scratch*")))
  (with-current-buffer (find-file-noselect "~/.emacs.d/save-scratch.el")
    (rename-buffer "*scratch*")
    (lisp-interaction-mode)
    (setq lexical-binding t)
    (use-local-map lisp-interaction-mode-map))
  (when (or (eq (point-min) (point-max))
            ;; For some reason the scratch buffer have not a zero size.
            (<= (buffer-size) 2))
    (insert ";;; -*- coding: utf-8; mode: lisp-interaction; lexical-binding: t -*-\n;;\n;; SCRATCH BUFFER\n;; ==============\n\n")))

;;;###autoload
(defun tv/insert-info-command-from-current-node-at-point ()
  (interactive)
  (require 'info)
  (let ((buf (get-buffer "*info*")))
    (when (and buf (buffer-live-p buf))
      (insert
       (with-current-buffer "*info*"
         (format "(info \"(%s) %s\")"
                 (file-name-nondirectory Info-current-file)
                 Info-current-node))))))

(defun tv/get-headers-from-string (str)
  "Return a list of headers from a mailto link."
  (with-temp-buffer
    (let (result
          (beg 1))
      (save-excursion
        (insert
         (url-unhex-string str)))
      (while (re-search-forward "\\?\\|&" nil t)
        (push (buffer-substring-no-properties beg (match-beginning 0))
              result)
        (setq beg (match-end 0)))
      (unless (eobp)
        (push (buffer-substring-no-properties beg (point-max))
              result))
      (nreverse result))))

(defun tv/insert-headers-from-string (str)
  "Add headers from STR in message buffer.
Used by the Mailto script used from firefox."
  (require 'message)
  (cl-loop for header in (tv/get-headers-from-string str)
           for h = (split-string header "=")
           if (cdr h)
           do (let ((fn (intern (format "message-goto-%s" (car h)))))
                (if (fboundp fn)
                    (progn (funcall fn)
                           (insert (format "%s" (cadr h))))
                  (insert "\n")
                  (message-insert-header (intern (car h)) (format "%s" (cadr h)))))
           else
           do (progn (message-goto-to)
                     (insert (format "%s" (car h)))))
  (message-goto-subject))

(defun tv/diametre-plateau (holes dist)
  "Return the diameter of crankset with HOLES number separated by DIST mm.
  E.g. TROUS=5 and DIST=64.7 => 110"
  (let ((L2 (/ dist 2)))
    (floor
     (* 2 (sqrt
           (+ (expt L2 2)
              (expt (/ L2 (tan (degrees-to-radians (/ 180 holes)))) 2)))))))

;;;###autoload
(defun tv/align-let ()
  "Align let forms."
  (interactive)
  (let ((sexp       (thing-at-point 'sexp t))
        (bounds     (bounds-of-thing-at-point 'sexp))
        (let-regexp "(?\\(([^ ]*\\)\\(\\s-*\\).*$"))
    (when sexp
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (let ((max-len 0))
          (save-excursion
            (while (re-search-forward let-regexp nil t)
              (setq max-len (max (length (match-string 1)) max-len))
              (goto-char (match-end 1))
              (skip-chars-forward " ")
              (when (looking-at "(") (forward-sexp 1))))
          (while (re-search-forward let-regexp nil t)
            (let (bol)
              (goto-char (match-end 1))
              (setq bol (bolp))
              (skip-chars-forward "\n \t")
              (unless bol
                (replace-match
                 (make-string (1+ (- max-len (length (match-string 1)))) ? )
                 t t nil 2)))
            (when (looking-at "(") (forward-sexp 1))))
        (goto-char (point-min))
        (indent-region (point-min) (point-max))))))

;;; Set extend attr on faces if needed
;;
(defun tv/extend-faces-matching (regexp)
  "Allow setting `extend' attribute on faces matching REGEXP."
  (cl-loop for f in (face-list)
           for face = (symbol-name f)
           when (and (string-match regexp face)
                     (eq (face-attribute f :extend t 'default)
                         'unspecified))
           do (set-face-attribute f nil :extend t)))

;;; Replace dots in filenames
;;
(defun tv/collect-file-exts-in-buffer ()
  (let ((exts '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((ext (file-name-extension
                         (buffer-substring-no-properties
                          (point-at-bol) (point-at-eol)))))
          (unless (member ext exts)
            (push ext exts)))
        (forward-line 1)))
    exts))

(defun tv/normalize-fnames-1 (strings)
  (let ((regexp (regexp-opt strings)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[. ]" nil t)
        (unless (looking-at-p regexp) (replace-match "_"))))))

(defun tv/normalize-fnames (&optional edit-exts)
  "Replace dots and spaces in filenames by \"_\".
Ignore dots preceding file extension.

Meant to be used in buffers containg only filenames e.g. wfnames buffers.
With a prefix arg prompt to edit file extensions."
  (interactive "P")
  (cl-assert (memq major-mode '(wfnames-mode wdired-mode)) nil
             "No filenames to modify in this buffer")
  (let* ((defs    (tv/collect-file-exts-in-buffer))
         (strings (and edit-exts
                       (read-from-minibuffer
                        "Strings: "
                        nil nil nil nil
                        (mapconcat #'identity defs " ")))))
    (when (or defs (and strings (not (string= strings ""))))
      (tv/normalize-fnames-1 (if strings
                                 (split-string strings)
                               defs)))))

(provide 'tv-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; tv-utils.el ends here
