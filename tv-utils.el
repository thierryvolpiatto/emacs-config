;;; tv-utils.el --- Some useful functions for Emacs. -*- lexical-binding: t -*- 
;; 

;;; Code:

(require 'cl-lib)

(declare-function helm-find-files-1             "ext:helm-files.el")
(declare-function mailcap-extension-to-mime     "mailcap.el")
(declare-function htmlize-file                  "htmlize.el")
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
(declare-function helm--ansi-color-apply        "ext:helm-lib.el")
(declare-function tramp-get-completion-function "tramp")
(defvar tramp-methods)

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
                (cl-loop with all-methods = (mapcar 'car tramp-methods)
                         for (f . h) in (tramp-get-completion-function "ssh")
                         append (cl-loop for e in (funcall f (car h))
                                         for host = (and (consp e) (cadr e))
                                         ;; On emacs-27 host may be
                                         ;; ("root" t) in sudo method.
                                         when (and (stringp host)
                                                   (not (member host all-methods)))
                                         collect host))))
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

;; get-ip 
;; get my external ip
;;;###autoload
(defun get-external-ip ()
  (interactive)
  (with-current-buffer (url-retrieve-synchronously "http://checkip.dyndns.org/")
    (let ((data (xml-parse-region (point-min) (point-max))))
      (car (last
            (split-string
             (car (last (assoc 'body (assoc 'html data))))))))))

;; network-info 
(defun tv/network-info (network)
  (let ((info (cl-loop for (i . n) in (network-interface-list)
                       when (string= network i)
                       return (network-interface-info i))))
    (when info
      (cl-destructuring-bind (address broadcast netmask mac state)
          info
        (list :address address :broadcast broadcast
              :netmask netmask :mac (cdr mac) :state state)))))

;;;###autoload
(defun tv/network-state (network &optional arg)
  (interactive (list (read-string "Network: " "wlan0")
                     "\np"))
  (let* ((info (car (last (cl-getf (tv/network-info network) :state))))
         (state (if info (symbol-name info) "down")))
    (if arg (message "%s is %s" network state) state)))

;; Benchmark
(defmacro tv/time (&rest body)
  "Return a list (time result) of time execution of BODY and result of BODY."
  (declare (indent 0))
  `(let ((tm (float-time)))
     (reverse
      (list
       ,@body
       (- (float-time) tm)))))

;; Show-current-face 
;;;###autoload
(defun whatis-face ()
  (interactive)
  (message "CurrentFace: %s"
           (get-text-property (point) 'face)))

;; mcp 
;;;###autoload
(defun tv/mcp (file &optional dests)
  "Copy FILE in DESTS directories."
  (interactive "fFile: ")
  (unless dests
    (setq dests
          (helm-read-file-name "Directory: "
                               :marked-candidates t
                               :test 'file-directory-p
                               :noret t)))
  (cl-loop for dir in dests
           do
           (copy-file file (file-name-as-directory dir) t)))

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

;;; Stardict
;;
;;;###autoload
(defun translate-at-point (arg)
  (interactive "P")
  (let* ((word (if arg
                   (read-string "Translate Word: ")
                   (thing-at-point 'word)))
         (tooltip-hide-delay 30)
         (result
          (condition-case nil
              (shell-command-to-string (format "LC_ALL=\"fr_FR.UTF-8\" sdcv -n %s" word))
            (error nil))))
    (setq result (replace-regexp-in-string "^\\[ color=\"blue\">\\|</font>\\|\\]" "" result))
    (if result
        (with-current-buffer (get-buffer-create "*Dict*")
          (erase-buffer)
          (save-excursion
            (insert result) (fill-region (point-min) (point-max)))
          ;; Assume dict buffer is in `special-display-buffer-names'.
          (switch-to-buffer-other-frame "*Dict*")
          (view-mode 1))
        (message "Nothing found."))))

;;; Get-mime-type-of-file
;;
;;;###autoload
(defun file-mime-type (fname &optional arg)
  "Get the mime-type of fname"
  (interactive "fFileName: \np")
  (if arg
      (message "%s" (mailcap-extension-to-mime (file-name-extension fname t)))
      (mailcap-extension-to-mime (file-name-extension fname t))))

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

;;; Time-functions 
(cl-defun tv/time-date-in-n-days (days &key (separator "-") french)
  "Return the date in string form in n +/-DAYS."
  (let* ((days-in-sec       (* 3600 (* (+ days) 24)))
         (interval-days-sec (if (< days 0)
                                (+ (float-time (current-time)) days-in-sec)
                                (- (float-time (current-time)) days-in-sec)))
         (sec-to-time       (seconds-to-time interval-days-sec))
         (time-dec          (decode-time sec-to-time))
         (year              (int-to-string (nth 5 time-dec)))
         (month             (if (= (% (nth 4 time-dec) 10) 0)
                                (int-to-string (nth 4 time-dec))
                                (substring (int-to-string (/ (float (nth 4 time-dec)) 100)) 2)))
         (day-str           (if (= (% (nth 3 time-dec) 10) 0)
                                (int-to-string (nth 3 time-dec))
                                (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2)))
         (day               (if (< (length day-str) 2) (concat day-str "0") day-str))
         (result            (list year month day)))
    (if french
        (mapconcat 'identity (reverse result) separator)
        (mapconcat 'identity result separator))))

;; mapc-with-progress-reporter 
(defmacro mapc-with-progress-reporter (message func seq)
  `(let* ((max               (length ,seq))
          (progress-reporter (make-progress-reporter (message ,message) 0 max))
          (count             0))
     (mapc #'(lambda (x)
               (progress-reporter-update progress-reporter count)
               (funcall ,func x)
               (cl-incf count))
           ,seq)
     (progress-reporter-done progress-reporter)))

;; Send current buffer htmlized to web browser.
;;;###autoload
(defun tv/htmlize-buffer-to-browser ()
  (interactive)
  (let* ((fname           (concat "/tmp/" (symbol-name (cl-gensym "emacs2browser"))))
         (html-fname      (concat fname ".html"))
         (buffer-contents (buffer-substring (point-min) (point-max))))
    (with-current-buffer (find-file-noselect fname)
      (insert buffer-contents)
      (save-buffer)
      (kill-buffer))
    (htmlize-file fname html-fname)
    (browse-url (format "file://%s" html-fname))))

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
                  delay nil (lambda () (keyboard-quit))))
          action kb com)
      (unwind-protect
           (catch 'break
             (while t
               (setq action (read-key (propertize "`(': Enclose forward, (any key to exit)."
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
                  1.5 nil (lambda () (keyboard-quit))))
          action kb com)
      (insert "(")
      (unwind-protect
           (catch 'break
             (while t
               (setq action (read-key (propertize "`)': Move forward, (any key to exit)."
                                                  'face 'minibuffer-prompt)))
               (cl-case action
                 (?\)
                  (unless (looking-back "(" (1- (point)))
                    (delete-char -1))
                  (skip-chars-forward " ")
                  (forward-symbol 1)
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

;;; Insert-an-image-at-point
;;;###autoload
(defun tv/insert-image-at-point (image)
  (interactive (list (read-file-name "Image: " "~/Images")))
  (let* ((win (selected-window))
         (img (save-match-data
                (apply #'create-image image
                       (and (image-type-available-p 'imagemagick)
                            `(imagemagick nil :height ,(* (- (window-height win) 1)
                                                          (frame-char-height))))))))
    (insert-image img)))

;;;###autoload
(defun tv/show-img-from-fname-at-point ()
  (interactive)
  (let ((img (thing-at-point 'sexp)))
    (forward-line)
    (tv/insert-image-at-point img)))

(defun tv/view-echo-area-messages (old--fn &rest args)
  (let ((win (get-buffer-window (messages-buffer) 'visible)))
    (if win
        (quit-window nil win)
      (apply old--fn args))))

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

;; Insert-log-from-patch
;;;###autoload
(defun tv/insert-log-from-patch (patch)
  (interactive (list (helm-read-file-name
                      "Patch: "
                      :preselect ".*[Pp]atch.*")))
  (let (beg end data)
    (with-current-buffer (find-file-noselect patch)
      (goto-char (point-min))
      (while (re-search-forward "^#" nil t) (forward-line 1))
      (setq beg (point))
      (when (re-search-forward "^diff" nil t)
        (forward-line 0) (skip-chars-backward "\\s*|\n*")
        (setq end (point)))
      (setq data (buffer-substring beg end))
      (kill-buffer))
    (insert data)
    (delete-file patch)))

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

;; C-mode conf
(defvar c-mode-map)
;;;###autoload
(defun tv/cc-this-file ()
  (interactive)
  (when (eq major-mode 'c-mode)
    (let* ((iname (buffer-file-name (current-buffer)))
           (oname (file-name-sans-extension iname)))
      (compile (format "make -k %s" oname)))))
(add-hook 'c-mode-hook #'(lambda ()
                           (declare (special c-mode-map))
                           (define-key c-mode-map (kbd "C-c C-c") 'tv/cc-this-file)))

;; Insert line numbers in region
;;;###autoload
(defun tv/insert-lineno-in-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^.*$" nil t)
             for count from 1 do
             (replace-match
              (concat (format "%d " count) (match-string 0))))))

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

;; Verlan.
;;;###autoload
(defun tv/reverse-chars-in-region (beg end)
  "Verlan region. Unuseful but funny"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((bl (point-at-bol))
             (el (point-at-eol))
             (cur-line (buffer-substring bl el))
             (split (cl-loop for i across cur-line collect i)))
        (delete-region bl el)
        (cl-loop for i in (reverse split) do (insert i)))
      (forward-line 1))))

;; Interface to df command-line.
;;
;;;###autoload
(defun dfh (directory)
  "Interface to df -h command line.
If a prefix arg is given choose directory, otherwise use `default-directory'."
  (interactive (list (if current-prefix-arg
                         (helm-read-file-name
                          "Directory: " :test 'file-directory-p)
                         default-directory)))
  (require 'dired-extension) ; for tv/get-disk-info
  (let ((df-info (tv/get-disk-info directory t)))
    (pop-to-buffer (get-buffer-create "*df info*"))
    (erase-buffer)
    (insert (format "*Volume Info for `%s'*\n\nDevice: %s\nMaxSize: \
%s\nUsed: %s\nAvailable: %s\nCapacity in use: %s\nMount point: %s"
                    directory
                    (cl-getf df-info :device)
                    (cl-getf df-info :blocks)
                    (cl-getf df-info :used)
                    (cl-getf df-info :available)
                    (cl-getf df-info :capacity)
                    (cl-getf df-info :mount-point)))
    (view-mode 1)))

;; Interface to du (directory size)
;;;###autoload
(defun duh (directory)
  (interactive "DDirectory: ")
  (let* ((lst
          (with-temp-buffer
            (apply #'call-process "du" nil t nil
                   (list "-h" (expand-file-name directory)))
            (split-string (buffer-string) "\n" t)))
         (result (mapconcat 'identity
                            (reverse (split-string (car (last lst))
                                                   " \\|\t")) " => ")))
    (if (called-interactively-p 'interactive) 
        (message "%s" result) result)))

;; Euro million
;;;###autoload
(defun euro-million ()
  (interactive)
  (let* ((star-num #'(lambda (limit)
                       ;; Get a random number between 1 to 12.
                       (let ((n 0))
                         (while (= n 0) (setq n (random limit)))
                         n)))
         (get-stars #'(lambda ()
                        ;; Return a list of 2 differents numbers from 1 to 12.
                        (let* ((str1 (number-to-string (funcall star-num 12)))
                               (str2 (let ((n (number-to-string (funcall star-num 12))))
                                       (while (string= n str1)
                                         (setq n (number-to-string (funcall star-num 12))))
                                       n)))
                          (list str1 str2))))      
         (result #'(lambda ()
                     ;; Collect random numbers without  dups.
                     (cl-loop repeat 5
                              for r = (funcall star-num 51)
                              if (not (member r L))
                              collect r into L
                              else
                              collect (let ((n (funcall star-num 51)))
                                        (while (memq n L)
                                          (setq n (funcall star-num 51)))
                                        n) into L
                                        finally return L)))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*Euro million*")
      (erase-buffer)
      (insert "Grille aléatoire pour l'Euro Million\n\n")
      (cl-loop with ls = (cl-loop repeat 5 collect (funcall result))  
               for i in ls do
               (progn
                 (insert (mapconcat #'(lambda (x)
                                        (let ((elm (number-to-string x)))
                                          (if (= (length elm) 1) (concat elm " ") elm)))
                                    i " "))
                 (insert " Stars: ")
                 (insert (mapconcat 'identity (funcall get-stars) " "))
                 (insert "\n"))
               finally do (progn (pop-to-buffer "*Euro million*")
                                 (special-mode))))))

;; Just an example to use `url-retrieve'
;;;###autoload
(defun tv/download-file-async (url &optional noheaders to)
  (let ((noheaders noheaders) (to to))
    (url-retrieve url #'(lambda (status)
                          (if (plist-get status :error)
                              (signal (car status) (cadr status))
                              (switch-to-buffer (current-buffer))
                              (let ((inhibit-read-only t))
                                (goto-char (point-min))
                                ;; remove headers
                                (when noheaders
                                  (save-excursion
                                    (re-search-forward "^$")
                                    (forward-line 1)
                                    (delete-region (point-min) (point))))
                                (when to
                                  (write-file to)
                                  (kill-buffer (current-buffer)))))))))

;; Tool to take all sexps matching regexps in buffer and bring
;; them at point. Useful to reorder defvar, defcustoms etc...
;;;###autoload
(defun tv/group-sexp-matching-regexp-at-point (arg regexp)
  "Take all sexps matching REGEXP and put them at point.
The sexps are searched after point, unless ARG.
In this case, sexps are searched before point."
  (interactive "P\nsRegexp: ")
  (let ((pos (point))
        (fun (if arg 're-search-backward 're-search-forward))
        (sep (and (y-or-n-p "Separate sexp with newline? ") "\n")))
    (cl-loop while (funcall fun regexp nil t)
             do (progn
                  (beginning-of-defun)
                  (let ((beg (point))
                        (end (save-excursion (end-of-defun) (point))))
                    (save-excursion
                      (forward-line -1)
                      (when (search-forward "###autoload" (point-at-eol) t)
                        (setq beg (point-at-bol))))
                    (kill-region beg end)
                    (delete-blank-lines))
                  (save-excursion
                    (goto-char pos)
                    (yank)
                    (insert (concat "\n" sep))
                    (setq pos (point))))
             finally do (goto-char pos))))

;; Check paren errors
;;;###autoload
(defun tv/check-paren-error ()
  (interactive)
  (let (pos-err)
    (save-excursion
      (goto-char (point-min))
      (catch 'error
        (condition-case err
            (forward-list 9999)
          (error
           (throw 'error
             (setq pos-err (cl-caddr err)))))))
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
(defun tv/delete-duplicate-lines (beg end &optional arg)
  "Delete duplicate lines in region omiting new lines.
With a prefix arg remove new lines."
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((lines (helm-fast-remove-dups
                    (split-string (buffer-string) "\n" arg)
                    :test 'equal)))
        (delete-region (point-min) (point-max))
        (cl-loop for l in lines do (insert (concat l "\n")))))))

;;;###autoload
(defun tv/break-long-string-list-at-point (arg)
  (interactive "p")
  (when (and (looking-at "(")
             (> (point-at-eol) (+ (point) 50)))
    (save-excursion
      (while (and (re-search-forward "\"[^\"]*\"" nil t arg)
                  (not (looking-at ")")))
        (newline-and-indent)))))

;; Stollen somewhere.
;;;###autoload
(defun tv/generate-kbd (key)
  (interactive "kGenerate and kill `kbd' form for key: ")
  (kill-new (message "(kbd \"%s\")" (help-key-description key nil)))
  (message nil))

;;;###autoload
(defun tv/insert-key-name-at-point (key)
  (interactive "kGenerate and kill `kbd' form for key: ")
  (insert (format "(kbd \"%s\")" (help-key-description key nil)))
  (message nil))

;; some tar fn to use in eshell aliases.
;;;###autoload
(defun tar-gunzip (file)
  (shell-command
   (format "tar czvf $(basename %s).tar.gz $(basename %s)"
           file file)))

;;;###autoload
(defun tar-bunzip (file)
  (shell-command
   (format "tar cjvf $(basename %s).tar.bz $(basename %s)"
           file file)))

;;;###autoload
(defun tar-xz (file)
  (shell-command
   (format "tar cJvf $(basename %s).tar.xz $(basename %s)"
           file file)))

;;;###autoload
(defun tv/resize-img (input-file percent-size output-file)
  (interactive (let* ((in (read-file-name "Input file: " "~/Images"))
                      (pcge (read-string "Resize percentage: " "25"))
                      (of (read-file-name "Output file: " nil in nil in)))
                 (list in pcge of)))
  (shell-command (format "convert %s -resize %s%% %s"
                         input-file
                         percent-size
                         output-file)))

;;;###autoload
(defun tv/split-freeboxvpn-config (file dir)
  (interactive (list (helm-read-file-name
                      "ConfigFile: "
                      :initial-input "~/Téléchargements/"
                      :must-match t
                      :preselect ".*\\.ovpn")
                     (read-directory-name
                      "SplitToDirectory: " "~/openvpn/")))
  (unless (file-directory-p dir) (mkdir dir t))
  (let ((ca (expand-file-name "ca.crt" dir))
        (client (expand-file-name "client.crt" dir))
        (key (expand-file-name "client.key" dir))
        (newfile (expand-file-name (helm-basename file) dir))
        ca-crt cli-crt key-key cfg beg end)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (when (re-search-forward "^<ca>" nil t)
        (setq cfg (buffer-substring-no-properties
                   (point-min) (point-at-bol)))
        (forward-line 1) (setq beg (point))
        (re-search-forward "^</ca>" nil t)
        (forward-line 0) (setq end (point))
        (setq ca-crt (buffer-substring-no-properties beg end)))
      (when (re-search-forward "^<cert>" nil t)
        (forward-line 1) (setq beg (point))
        (re-search-forward "^</cert>" nil t)
        (forward-line 0) (setq end (point))
        (setq cli-crt (buffer-substring-no-properties beg end)))
      (when (re-search-forward "^<key>" nil t)
        (forward-line 1) (setq beg (point))
        (re-search-forward "^</key>" nil t)
        (forward-line 0) (setq end (point))
        (setq key-key (buffer-substring-no-properties beg end)))
      (kill-buffer))
    (cl-loop for f in `(,ca ,client ,key)
             for c in `(,ca-crt ,cli-crt ,key-key)
             do
             (with-current-buffer (find-file-noselect f)
               (erase-buffer)
               (insert c)
               (save-buffer)
               (kill-buffer)))
    (with-current-buffer (find-file-noselect newfile)
      (erase-buffer)
      (insert cfg
              "ca ca.crt\n"
              "cert client.crt\n"
              "key client.key\n")
      (save-buffer)
      (kill-buffer))))


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
(advice-add 'eshell-send-invisible :override #'tv/advice--eshell-send-invisible)

(defvar tv/freesms-default-url
  "https://smsapi.free-mobile.fr/sendmsg?user=%s&pass=%s&msg=%s")
;;;###autoload
(defun tv/freesms-notify (login msg)
  (interactive (list
                (completing-read "User: " '("thierry" "rachel"))
                (read-string "Message: ")))
  (setq msg (url-hexify-string msg))
  (let* ((host  (format "freesms%s" login))
         (user (plist-get (car (auth-source-search :host host)) :user))
         (pwd   (tv/get-passwd-from-auth-sources host :user user)))
    (with-current-buffer (url-retrieve-synchronously
                          (format tv/freesms-default-url user pwd msg))
      (goto-char (point-min))
      (let* ((rcode (nth 1 (split-string (buffer-substring-no-properties
                                          (point-at-bol) (point-at-eol)))))
             (rcode-msg
              (cond ((string= "200" rcode) "Le SMS a été envoyé sur votre mobile.")
                    ((string= "400" rcode) "Un des paramètres obligatoires est manquant.")
                    ((string= "402" rcode) "Trop de SMS ont été envoyés en trop peu de temps.")
                    ((string= "403" rcode) "Le service n'est pas activé sur l'espace abonné, ou login / clé incorrect.")
                    ((string= "500" rcode) "Erreur côté serveur. Veuillez réessayer ultérieurement.")
                    (t "Unknow error"))))
        (if (string= rcode-msg "200")
            (message rcode-msg)
            (error rcode-msg))))))

;;; Scroll functions
(defun tv/scroll-down ()
  (interactive)
  (scroll-down -1))

(defun tv/scroll-up ()
  (interactive)
  (scroll-down 1))

(defun tv/scroll-other-down ()
  (interactive)
  (scroll-other-window 1))

(defun tv/scroll-other-up ()
  (interactive)
  (scroll-other-window -1))

(defun tv/update-helm-only-symbol (dir)
  (cl-loop for f in (directory-files dir t "\\.el\\'")
           do (with-current-buffer (find-file-noselect f)
                (save-excursion
                  (goto-char (point-min))
                  (let (fun)
                    (while (re-search-forward "(with-helm-alive-p" nil t)
                      (when (setq fun (which-function))
                        (end-of-defun)
                        (unless (looking-at "(put")
                          (insert (format "(put '%s 'helm-only t)\n" fun))))))))))

(defun tv/thing-at-point-number ()
  (save-excursion
    (when (re-search-forward "[0-9]\\{1,6\\}" (min (+ (point) 6) (point-at-eol)) t)
      (string-to-number (match-string-no-properties 0)))))

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

;;; wttr.in weather report
;;
(defvar wttr-weather-history nil)
(defvar wttr-weather-default-location "Guillestre")
(defvar wttr-weather-last-location nil)
;;;###autoload
(defun wttr-weather (place)
  "Weather forecast with wttr.in.
With a prefix arg refresh buffer if some.
See <https://github.com/chubin/wttr.in>."
  (interactive (list (read-string (format "Place (%s): "
                                          wttr-weather-default-location)
                                  nil
                                  'wttr-weather-history
                                  wttr-weather-default-location)))
  (require 'helm-lib)
  (let ((buf (get-buffer-create (format "*wttr.in %s*" place))))
    (switch-to-buffer buf)
    (when current-prefix-arg
      (set (make-local-variable 'wttr-weather-last-location) nil))
    (unless wttr-weather-last-location
      (wttr-weather-update place)
      (wttr-weather-mode)
      (set (make-local-variable 'wttr-weather-last-location) place))))

(defun wttr-weather-update (place)
  (let* ((inhibit-read-only t)
         ansi
         (data
          (with-temp-buffer
            (call-process
             "curl" nil '(t t) nil
             "-s" (format "fr.wttr.in/~%s?m" (shell-quote-argument place)))
            (goto-char (point-min))
            (while (re-search-forward "38;5;\\([0-9]+\\)m" nil t)
              ;; If we have ansi sequences, that's mean we had weather
              ;; output, otherwise we have a simple message notifying
              ;; weather report is not available.
              (setq ansi t)
              ;; Need a 256 color ansi library, emacs supports only basic
              ;; ansi colors as now, so replace all 38;5 foreground
              ;; specs by simple ansi sequences.
              (replace-match (pcase (match-string 1)
                               ("154" "32")
                               ("190" "31")
                               ("118" "32")
                               ("208" "37")
                               ("202" "34")
                               ("214" "35")
                               ("220" "36")
                               ("226" "33")
                               (r     r))
                             t t nil 1))
            (helm--ansi-color-apply (buffer-string)))))
    (erase-buffer)
    (save-excursion
      (if data
          (insert data)
        ;; Probaly check error status instead (it is 52).
        (insert "Empy reply from server"))
      (forward-line -1)
      (when (and ansi ; Keep notification when no weather report.
                 (re-search-backward "^$" nil t))
        (delete-region (point) (point-max))))
    (while (re-search-forward "\\s\\" (point-at-eol) t) (replace-match ""))
    (goto-char (point-at-eol))
    (insert (format-time-string " le %d/%m/%Y à %H:%M:%S"))))

(defun wttr-weather-revert-fn (_ignore-auto _no_confirm)
  (wttr-weather-update wttr-weather-last-location))

(define-derived-mode wttr-weather-mode special-mode "wttr"
  (make-local-variable 'wttr-weather-last-location)
  (set (make-local-variable 'revert-buffer-function) 'wttr-weather-revert-fn))

;;;###autoload
(defun tv/insert-info-command-from-current-node-at-point ()
  (interactive)
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

(defun message-goto-in-reply-to ()
  "Move point to the In-Reply-To header."
  (interactive)
  (push-mark)
  (message-position-on-field "In-Reply-To" "Subject"))

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

(defun tv/sum-region (beg end)
  (interactive "r")
  (let ((data (buffer-substring beg end))
        result)
    (with-temp-buffer
      (save-excursion (insert data))
      (setq result
            (cl-loop while (re-search-forward "\\([0-9]+[.]?[0-9]*\\)" nil t)
                     concat (concat (replace-regexp-in-string " " "" (match-string 0)) "+") into op
                     finally return (calc-eval (replace-regexp-in-string "+$" "" op))
                     )))
    (kill-new result)
    (message "result: %s" result)))
(global-set-key (kbd "C-M-+") 'tv/sum-region)

(defun tv/get-group-prefix (group)
  "Extract :prefix value from defgroup GROUP definition."
  (get (intern-soft group) 'custom-prefix))

(defun tv/describe-variable (variable &optional buffer frame)
      (interactive
       (let ((v (variable-at-point))
	     (enable-recursive-minibuffers t)
             (orig-buffer (current-buffer))
	     val)
         (setq val (completing-read
                    (if (symbolp v)
                        (format
                         "Describe variable (default %s): " v)
                      "Describe variable: ")
                    #'help--symbol-completion-table
                    (lambda (vv)
                      ;; In case the variable only exists in the buffer
                      ;; the command we switch back to that buffer before
                      ;; we examine the variable.
                      (with-current-buffer orig-buffer
                        (or (get vv 'variable-documentation)
                            (and (boundp vv) (not (keywordp vv))))))
                    t nil nil
                    (if (symbolp v) (symbol-name v))))
         (list (if (equal val "")
	           v (intern val)))))
      (let (file-name)
        (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
        (unless (frame-live-p frame) (setq frame (selected-frame)))
        (if (not (symbolp variable))
	    (message "You did not specify a variable")
          (save-excursion
	    (let ((valvoid (not (with-current-buffer buffer (boundp variable))))
	          (permanent-local (get variable 'permanent-local))
	          val val-start-pos locus)
              ;; Extract the value before setting up the output buffer,
              ;; in case `buffer' *is* the output buffer.
	      (unless valvoid
	        (with-selected-frame frame
	          (with-current-buffer buffer
		    (setq val (symbol-value variable)
		          locus (variable-binding-locus variable)))))
	      (help-setup-xref (list #'describe-variable variable buffer)
			       (called-interactively-p 'interactive))
	      (with-help-window (help-buffer)
	        (with-current-buffer buffer
	          (prin1 variable)
	          (setq file-name (find-lisp-object-file-name variable 'defvar))

	          (if file-name
		      (progn
		        (princ (format-message
                                " is a variable defined in `%s'.\n"
                                (if (eq file-name 'C-source)
                                    "C source code"
                                  (file-name-nondirectory file-name))))
		        (with-current-buffer standard-output
		          (save-excursion
			    (re-search-backward (substitute-command-keys
                                                 "`\\([^`']+\\)'")
                                                nil t)
			    (help-xref-button 1 'help-variable-def
					      variable file-name)))
		        (if valvoid
			    (princ "It is void as a variable.")
		          (princ "Its ")))
		    (if valvoid
		        (princ " is void as a variable.")
		      (princ (substitute-command-keys "'s ")))))
	        (unless valvoid
	          (with-current-buffer standard-output
		    (setq val-start-pos (point))
		    (princ "value is")
		    (let ((line-beg (line-beginning-position))
		          (print-rep
		           (let ((rep
			          (let ((print-quoted t)
                                        (print-circle t))
				    (cl-prin1-to-string val))))
			     (if (and (symbolp val) (not (booleanp val)))
			         (format-message "`%s'" rep)
			       rep))))
		      (if (< (+ (length print-rep) (point) (- line-beg)) 68)
		          (insert " " print-rep)
		        (terpri)
                        ;; >>>>>>>>>>>adviced block
                        (cl-letf (((symbol-function 'pp)
                                   (lambda (object &optional stream)
                                     (let ((fn (lambda (ob &optional stream)
                                                 (princ (pp-to-string ob)
                                                        (or stream standard-output))
                                                 (terpri)))
                                           (print-circle t)
                                           prefix suffix map-fn looping)
                                       (cond ((ring-p object)
                                              (setq looping nil))
                                             ((consp object)
                                              (setq prefix "\n("
                                                    suffix ")"
                                                    map-fn 'mapc
                                                    looping t))
                                             ((vectorp object)
                                              (setq prefix "\n["
                                                    suffix "]"
                                                    map-fn 'mapc
                                                    looping t))
                                             ((hash-table-p object)
                                              (setq prefix (format "#s(hash-table size %s test %s rehash-size %s rehash-threshold %s data\n"
                                                                   (hash-table-size object)
                                                                   (hash-table-test object)
                                                                   (hash-table-rehash-size object)
                                                                   (hash-table-rehash-threshold object))
                                                    suffix ")"
                                                    map-fn 'maphash
                                                    fn `(lambda (k v &optional stream)
                                                          (funcall ,fn k stream)
                                                          (funcall ,fn v stream))
                                                    looping t)))
                                       (if looping
                                           (progn
                                             (insert prefix)
                                             (funcall map-fn fn object)
                                             (cl-letf (((point) (1- (point))))
                                               (insert suffix)))
                                         (funcall fn object stream))))))

                          (pp val))
                          ;; >>>>>>>>>>>>>>>>>>>>>>>>>
                          ;; Remove trailing newline.
                          (and (= (char-before) ?\n) (delete-char -1)))
		      (let* ((sv (get variable 'standard-value))
			     (origval (and (consp sv)
				           (condition-case nil
					       (eval (car sv))
					     (error :help-eval-error))))
                             from)
		        (when (and (consp sv)
                                   (not (equal origval val))
                                   (not (equal origval :help-eval-error)))
		          (princ "\nOriginal value was \n")
		          (setq from (point))
                          (cl-prin1 origval)
                          (save-restriction
                            (narrow-to-region from (point))
                            (save-excursion (pp-buffer)))
		          (if (< (point) (+ from 20))
			      (delete-region (1- from) from)))))))
	        (terpri)
	        (when locus
	          (cond
                    ((bufferp locus)
                     (princ (format "Local in buffer %s; "
                                    (buffer-name buffer))))
                    ((terminal-live-p locus)
                     (princ (format "It is a terminal-local variable; ")))
                    (t
                     (princ (format "It is local to %S" locus))))
	          (if (not (default-boundp variable))
		      (princ "globally void")
		    (let ((global-val (default-value variable)))
		      (with-current-buffer standard-output
		        (princ "global value is ")
		        (if (eq val global-val)
			    (princ "the same.")
		          (terpri)
                          ;; Fixme: pp can take an age if you happen to
                          ;; ask for a very large expression.  We should
                          ;; probably print it raw once and check it's a
                          ;; sensible size before prettyprinting.  -- fx
		          (let ((from (point)))
                            (cl-prin1 global-val)
                            (save-restriction
                              (narrow-to-region from (point))
                              (save-excursion (pp-buffer)))
                            ;; See previous comment for this function.
                            ;; (help-xref-on-pp from (point))
			    (if (< (point) (+ from 20))
			        (delete-region (1- from) from)))))))
                  (terpri))

                ;; If the value is large, move it to the end.
	        (with-current-buffer standard-output
	          (when (> (count-lines (point-min) (point-max)) 10)
                    ;; Note that setting the syntax table like below
                    ;; makes forward-sexp move over a `'s' at the end
                    ;; of a symbol.
		    (set-syntax-table emacs-lisp-mode-syntax-table)
		    (goto-char val-start-pos)
                    ;; The line below previously read as
                    ;; (delete-region (point) (progn (end-of-line) (point)))
                    ;; which suppressed display of the buffer local value for
                    ;; large values.
		    (when (looking-at "value is") (replace-match ""))
		    (save-excursion
		      (insert "\n\nValue:")
		      (set (make-local-variable 'help-button-cache)
		           (point-marker)))
		    (insert "value is shown ")
		    (insert-button "below"
			           'action help-button-cache
			           'follow-link t
			           'help-echo "mouse-2, RET: show value")
		    (insert ".\n")))
                (terpri)

                (let* ((alias (condition-case nil
                                  (indirect-variable variable)
                                (error variable)))
                       (obsolete (get variable 'byte-obsolete-variable))
                       (watchpoints (get-variable-watchers variable))
		       (use (car obsolete))
		       (safe-var (get variable 'safe-local-variable))
                       (doc (or (documentation-property
                                 variable 'variable-documentation)
                                (documentation-property
                                 alias 'variable-documentation)))
                       (extra-line nil))

                  ;; Mention if it's a local variable.
	          (cond
	            ((and (local-variable-if-set-p variable)
		          (or (not (local-variable-p variable))
			      (with-temp-buffer
			        (local-variable-if-set-p variable))))
                     (setq extra-line t)
                     (princ "  Automatically becomes ")
		     (if permanent-local
		         (princ "permanently "))
		     (princ "buffer-local when set.\n"))
	            ((not permanent-local))
	            ((bufferp locus)
		     (setq extra-line t)
		     (princ
		      (substitute-command-keys
		       "  This variable's buffer-local value is permanent.\n")))
	            (t
		     (setq extra-line t)
                     (princ (substitute-command-keys
			     "  This variable's value is permanent \
if it is given a local binding.\n"))))

                  ;; Mention if it's an alias.
                  (unless (eq alias variable)
                    (setq extra-line t)
                    (princ (format-message
                            "  This variable is an alias for `%s'.\n"
                            alias)))

                  (when obsolete
                    (setq extra-line t)
                    (princ "  This variable is obsolete")
                    (if (nth 2 obsolete)
                        (princ (format " since %s" (nth 2 obsolete))))
		    (princ (cond ((stringp use) (concat ";\n  " use))
			         (use (format-message ";\n  use `%s' instead."
                                                      (car obsolete)))
			         (t ".")))
                    (terpri))

                  (when watchpoints
                    (setq extra-line t)
                    (princ "  Calls these functions when changed: ")
                    (princ watchpoints)
                    (terpri))

	          (when (member (cons variable val)
                                (with-current-buffer buffer
                                  file-local-variables-alist))
		    (setq extra-line t)
		    (if (member (cons variable val)
                                (with-current-buffer buffer
                                  dir-local-variables-alist))
		        (let ((file (and (buffer-file-name buffer)
                                         (not (file-remote-p
                                               (buffer-file-name buffer)))
                                         (dir-locals-find-file
                                          (buffer-file-name buffer))))
                              (is-directory nil))
		          (princ (substitute-command-keys
			          "  This variable's value is directory-local"))
                          (when (consp file) ; result from cache
                            ;; If the cache element has an mtime, we
                            ;; assume it came from a file.
                            (if (nth 2 file)
                                ;; (car file) is a directory.
                                (setq file (dir-locals--all-files (car file)))
                              ;; Otherwise, assume it was set directly.
                              (setq file (car file)
                                    is-directory t)))
                          (if (null file)
                              (princ ".\n")
                            (princ ", set ")
                            (princ (substitute-command-keys
                                    (cond
                                      (is-directory "for the directory\n  `")
                                      ;; Many files matched.
                                      ((and (consp file) (cdr file))
                                       (setq file (file-name-directory (car file)))
                                       (format "by one of the\n  %s files in the directory\n  `"
                                               dir-locals-file))
                                      (t (setq file (car file))
                                         "by the file\n  `"))))
			    (with-current-buffer standard-output
			      (insert-text-button
			       file 'type 'help-dir-local-var-def
                               'help-args (list variable file)))
			    (princ (substitute-command-keys "'.\n"))))
		      (princ (substitute-command-keys
			      "  This variable's value is file-local.\n"))))

	          (when (memq variable ignored-local-variables)
		    (setq extra-line t)
		    (princ "  This variable is ignored as a file-local \
variable.\n"))

                  ;; Can be both risky and safe, eg auto-fill-function.
	          (when (risky-local-variable-p variable)
		    (setq extra-line t)
		    (princ "  This variable may be risky if used as a \
file-local variable.\n")
		    (when (assq variable safe-local-variable-values)
		      (princ (substitute-command-keys
                              "  However, you have added it to \
`safe-local-variable-values'.\n"))))

	          (when safe-var
                    (setq extra-line t)
		    (princ "  This variable is safe as a file local variable ")
		    (princ "if its value\n  satisfies the predicate ")
		    (princ (if (byte-code-function-p safe-var)
			       "which is a byte-compiled expression.\n"
			     (format-message "`%s'.\n" safe-var))))

                  (if extra-line (terpri))
	          (princ "Documentation:\n")
	          (with-current-buffer standard-output
		    (insert (or doc "Not documented as a variable."))))

                ;; Make a link to customize if this variable can be customized.
	        (when (custom-variable-p variable)
	          (let ((customize-label "customize"))
		    (terpri)
		    (terpri)
		    (princ (concat "You can " customize-label " this variable."))
		    (with-current-buffer standard-output
		      (save-excursion
		        (re-search-backward
		         (concat "\\(" customize-label "\\)") nil t)
		        (help-xref-button 1 'help-customize-variable variable))))
                  ;; Note variable's version or package version.
	          (let ((output (describe-variable-custom-version-info variable)))
		    (when output
		      (terpri)
		      (terpri)
		      (princ output))))

	        (with-current-buffer standard-output
                  ;; Return the text we displayed.
	          (buffer-string))))))))

;; Start org-agenda asynchronously.
;; Just a proof of concept for async.el.
(defun tv/org-agenda-async ()
  (interactive)
  (require 'org-agenda)
  (async-start
   `(lambda ()
      (require 'org-agenda)
      (setq org-agenda-files ',org-agenda-files)
      (org-agenda-list)
      (buffer-string))
   (lambda (result)
     (switch-to-buffer-other-window "*Org Agenda*")
     (mapc 'find-file-noselect (org-agenda-files))
     (with-current-buffer "*Org Agenda*"
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (car result))
         (map-text-properties (cdr result)))
       (org-agenda-mode)))))

(defun map-text-properties (props)
  (let ((plist (caddr props)))
    (while plist
      (put-text-property (1+ (nth 0 props))
                         (1+ (nth 1 props))
                         (car plist)
                         (let ((value (cadr plist)))
                           (cond ((and (consp value)
                                       (stringp (car value)))
                                  (with-temp-buffer
                                    (insert (car value))
                                    (map-text-properties (cdr value))
                                    (buffer-string)))
                                 ((and (consp value) (memq 'marker value))
                                  (let ((marker (set-marker
                                                 (make-marker)
                                                 (cl-loop for i in value
                                                          thereis (and (numberp i) i))
                                                 (get-buffer (mapconcat 'symbol-name (last value) "")))))
                                    (set-marker-insertion-type marker t)
                                    marker))
                                 (t value))))
      (setq plist (cddr plist)))
    (when props
      (map-text-properties (nthcdr 3 props)))))

(provide 'tv-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; tv-utils.el ends here
