;;; tv-utils.el --- Some useful functions for Emacs. 
;; 
;; Author: ThierryVolpiatto
;; Maintainer: ThierryVolpiatto
;; 
;; Created: mar jan 20 21:49:07 2009 (+0100)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl)


;;; Sshfs
;;
;;
;;;###autoload
(defun mount-sshfs (fs mp)
  (interactive (list (read-string "FileSystem: "
                                  "thievol:/home/thierry")
                     (expand-file-name
                      (read-directory-name "MountPoint: "
                                           "/home/thierry/"
                                           "/home/thierry/sshfs/"
                                           t
                                           "sshfs"))))
  (if (> (length (directory-files
                  mp nil directory-files-no-dot-files-regexp)) 0)
      (message "Directory %s is busy, mountsshfs aborted" mp)
      (if (= (call-process-shell-command "sshfs" nil t nil
                                         (format "%s %s" fs mp)) 0)
          (message "%s Mounted successfully on %s" fs mp)
          (message "Failed to mount remote filesystem %s on %s" fs mp))))

;;;###autoload
(defun umount-sshfs (mp)
  (interactive (list (expand-file-name
                      (read-directory-name "MountPoint: "
                                           "/home/thierry/"
                                           "/home/thierry/sshfs/"
                                           t
                                           "sshfs"))))
  (if (equal (pwd) (format "Directory %s" mp))
      (message "Filesystem is busy can't umount!")
      (progn
        (if (>= (length (cddr (directory-files mp))) 0)
            (if (= (call-process-shell-command "fusermount" nil t nil
                                               (format "-u %s" mp)) 0)
                (message "%s Successfully unmounted" mp)
                (message "Failed to unmount %s" mp))
            (message "No existing remote filesystem to unmount!")))))

;;;###autoload
(defun sshfs-connect ()
  "sshfs mount of thievol."
  (interactive)
  (mount-sshfs "thievol:" "~/sshfs")
  (helm-find-files-1 "~/sshfs"))

;;;###autoload
(defun sshfs-disconnect ()
  "sshfs umount of thievol."
  (interactive)
  (umount-sshfs "~/sshfs"))

;; find-file-as-root 
;;;###autoload
(defun find-file-as-root (file)
  (interactive "fFindFileAsRoot: ")
  (find-file (concat "/su::" (expand-file-name file))))

;; get-ip 
;; get my external ip (need my python script)
;;;###autoload
(defun tv-get-ip ()
  "get my ip."
  (interactive)
  (let ((my-ip (with-temp-buffer
                 (call-process "get_IP.py" nil t nil)
                 (buffer-string))))
    (message "%s" (replace-regexp-in-string "\n" "" my-ip))))

;; network-info 
(defun tv-network-info (network)
  (let ((info (loop for (i . n) in (network-interface-list)
                    when (string= network i)
                    return (network-interface-info i))))
    (when info
      (destructuring-bind (address broadcast netmask mac state)
          info
        (list :address address :broadcast broadcast
              :netmask netmask :mac (cdr mac) :state state)))))

(defun tv-network-state (network &optional arg)
  (interactive (list (read-string "Network: " "wlan0")
                     "\np"))
  (let* ((info (car (last (getf (tv-network-info network) :state))))
         (state (if info (symbol-name info) "down")))
    (if arg (message "%s is %s" network state) state)))

;; Crontab 
;;;###autoload
(defun crontab (min hr month-day month week-day progr)
  "Insert interactively crontab line at point.
Run first crontab -e in shell and when emacsclient popup run M-x crontab."
  (interactive "sMin (0 to 59 or 0-59/every_x_mn or *): \
\nsHour (0 to 23 or 0-59/every_x_ hr or *): \
\nsDayOfMonth (1 to 31 or *): \
\nsMonth (1 to 12 or *): \
\nsDayOfWeek (0 to 7 or x,y,z or *): \nsCommand: ")
  (let ((abs-prog (with-temp-buffer
                    (call-process "which" nil t nil (format "%s" progr))
                    (buffer-string))))
    (insert
     (concat min " " hr " " month-day " " month " " week-day " " abs-prog))))

;; tv-yank-from-screen 

(defvar screen-exchange-file "~/.screen_exchange")
;;;###autoload
(defun tv-yank-from-screen (arg)
  "Yank text copied in a GNU/screen session."
  (interactive "P")
  (let* ((content  (with-current-buffer
                       (find-file-noselect screen-exchange-file)
                     (prog1
                         (buffer-substring (point-min) (point-max))
                       (kill-buffer))))
         (len      (length content))
         (one-line (replace-regexp-in-string "\n" "" content)))
    ;; With prefix arg don't remove new lines.
    (if arg (insert content) (insert one-line))
    (forward-char len)))

;;;###autoload
(defun tv-copy-for-screen (arg beg end)
  "Copy region without newlines in `screen-exchange-file'."
  (interactive "P\nr")
  (let ((region (buffer-substring-no-properties beg end))
        (require-final-newline nil))
    (with-current-buffer
        (find-file-noselect screen-exchange-file)
      (goto-char (point-min))
      (erase-buffer)
      (if arg
          ;; With prefix arg don't remove new lines.
          (insert region)
          (insert (replace-regexp-in-string "\n" "" region)))
      (save-buffer)
      (shell-command "screen -X readbuf > /dev/null")
      (kill-buffer))))

;; Benchmark
(defmacro tv-time (&rest body)
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
(defun mcp (file &optional list-of-dir)
  "Copy `file' in multi directory.
At each prompt of directory add + to input
to be prompt for next directory.
When you do not add a + to directory name
input is finish and function executed"
  (interactive "fFile: ")
  (let* ((dest-list nil)
         (final-list
          (if list-of-dir
              list-of-dir
              (multi-read-name 'read-directory-name))))
    (loop for i in final-list
          do
          (copy-file file i t))))

;; Multi-read-name 
;;;###autoload
(defun* multi-read-name (&optional (fn 'read-string))
  "Prompt as many time you add + to end of prompt.
Return a list of all inputs in `var'.
You can specify input function to use."
  (let (var)
    (labels ((multiread ()
               (let ((stock)
                     (str (funcall fn (cond ((eq fn 'read-string)
                                             "String(add + to repeat): ")
                                            ((eq fn 'read-directory-name)
                                             "Directory(add + to repeat): ")
                                            (t
                                             "File(add + to repeat): ")))))
                 (push (replace-regexp-in-string "\+" "" str) stock)
                 (cond ((string-match "\+" str)
                        (push (car stock) var)
                        (multiread))
                       (t
                        (push (car stock) var)
                        (nreverse var))))))
      
      (multiread))))


;;; move-to-window-line 

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

;;;###autoload
(defun other-window-backward (&optional n)
  "Move backward to other window or frame."
  (interactive "p")
  (other-window (- n) t)
  (select-frame-set-input-focus (selected-frame)))

;;;###autoload
(defun other-window-forward (&optional n)
  "Move to other window or frame.
With a prefix arg move N window forward or backward
depending the value of N is positive or negative."
  (interactive "p")
  (other-window n t)
  (select-frame-set-input-focus (selected-frame)))

;;; Persistent-scratch

;;;###autoload
(defun go-to-scratch ()
  (interactive)
  (unless (buffer-file-name (get-buffer "*scratch*"))
    (and (get-buffer "*scratch*") (kill-buffer "*scratch*")))
  (if (and (get-buffer "*scratch*")
           (buffer-file-name (get-buffer "*scratch*")))
      (progn (switch-to-buffer "*scratch*") (lisp-interaction-mode))
      (find-file "~/.emacs.d/save-scratch.el")
      (rename-buffer "*scratch*")
      (lisp-interaction-mode)
      (use-local-map lisp-interaction-mode-map))
  (when (or (eq (point-min) (point-max))
            ;; For some reason the scratch buffer have not a zero size.
            (<= (buffer-size) 2))
    (insert ";; SCRATCH BUFFER\n;; ==============\n\n"))
  (current-buffer))

;;; registers-config 
;; Redefine append-to-register with a "\n"

;;;###autoload
(defun tv-append-to-register (register start end &optional delete-flag)
  "Append region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (let ((reg  (get-register register))
        (text (filter-buffer-substring start end)))
    (set-register
     register (cond ((not reg) text)
                    ((stringp reg) (concat reg "\n" text))
                    (t (error "Register does not contain text")))))
  (if delete-flag (delete-region start end)))

;;; Stardict
;;
(defun translate-at-point ()
  (interactive)
  (let* ((word (or (thing-at-point 'word) (read-string "Translate Word: ")))
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
(defun file-mime-type (fname &optional arg)
  "Get the mime-type of fname"
  (interactive "fFileName: \np")
  (if arg
      (message "%s" (mailcap-extension-to-mime (file-name-extension fname t)))
      (mailcap-extension-to-mime (file-name-extension fname t))))

;; Eval-region 
(defun tv-eval-region (beg end)
  (interactive "r")
  (eval-region beg end t))

;; String-processing 

;; like `make-string' :-)
(defmacro *string (str num)
  `(let ((str-lis))
     (dotimes (n ,num)
       (push ,str str-lis))
     (mapconcat #'(lambda (i) i) str-lis "")))

(defmacro +string (str-0 str-1)
  `(let ((str-lis (list ,str-0 ,str-1)))
     (mapconcat #'(lambda (i) i) str-lis "")))

;;; Time-functions 
(defun* tv-time-date-in-n-days (days &key (separator "-") french)
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


(defun* tv-date-string (&key (separator "-") french (date (current-time)))
  "Return DATE under string form."
  (let* ((year      (nth 5 (decode-time date)))
         (month     (nth 4 (decode-time date)))
         (month-str (substring (int-to-string (/ (float month) 100)) 2))
         (day       (nth 3 (decode-time (current-time))))
         (day-str   (substring (int-to-string (/ (float day) 100)) 2))
         (result    (list (int-to-string year)
                          (if (< (length month-str) 2) (concat month-str "0") month-str)
                          (if (< (length day-str) 2) (concat day-str "0") day-str))))
    (if french
        (mapconcat 'identity (reverse result) separator)
        (mapconcat 'identity result separator))))

(defun* tv-cur-date-string (&key (separator "-") french)
  "Return current date under string form."
  (if french
      (tv-date-string :separator separator :french t)
      (tv-date-string :separator separator)))

;; mapc-with-progress-reporter 
(defmacro mapc-with-progress-reporter (message func seq)
  `(let* ((max               (length ,seq))
          (progress-reporter (make-progress-reporter (message ,message) 0 max))
          (count             0))
     (mapc #'(lambda (x)
               (progress-reporter-update progress-reporter count)
               (funcall ,func x)
               (incf count))
           ,seq)
     (progress-reporter-done progress-reporter)))

;; Send current buffer htmlized to web browser. 
(defun tv-htmlize-buffer-to-browser ()
  (interactive)
  (let* ((fname           (concat "/tmp/" (symbol-name (gensym "emacs2browser"))))
         (html-fname      (concat fname ".html"))
         (buffer-contents (buffer-substring (point-min) (point-max))))
    (with-current-buffer (find-file-noselect fname)
      (insert buffer-contents)
      (save-buffer)
      (kill-buffer))
    (htmlize-file fname html-fname)
    (browse-url (format "file://%s" html-fname))))

;; key-for-calendar 
(defvar tv-calendar-alive nil)
(defun tv-toggle-calendar ()
  (interactive)
  (if tv-calendar-alive
      (when (get-buffer "*Calendar*")
        (with-current-buffer "diary" (save-buffer)) 
        (calendar-exit)) ; advice reset win conf
      ;; In case calendar were called without toggle command
      (unless (get-buffer-window "*Calendar*")
        (setq tv-calendar-alive (current-window-configuration))
        (calendar))))

(defadvice calendar-exit (after reset-win-conf activate)
  (when tv-calendar-alive
    (set-window-configuration tv-calendar-alive)
    (setq tv-calendar-alive nil)))

;; Cvs-update-current-directory-and-compile-it 
;; <2009-04-17 Ven. 16:15>
(require 'pcvs)
(defun update-cvs-dir-and-compile ()
  "Cvs update current dir and compile it."
  (interactive)
  (let ((dir default-directory))
    (cvs-update dir nil)
    (while (not (equal cvs-mode-line-process "exit"))
      (sit-for 1))
    (message "Wait compiling %s..." dir)
    (shell-command "make")))

;;; Insert-pairs 
;;
(setq parens-require-spaces t)

(defun tv-insert-double-quote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))


(defun tv-insert-double-backquote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\` ?\'))


(defun tv-insert-vector (&optional arg)
  (interactive "P")
  (insert-pair arg ?\[ ?\]))


(defun tv-move-pair-forward ()
  (interactive)
  (let (action)
    (catch 'break
      (while t
        (setq action (read-key "`(': Insert, (any key to exit)."))
        (case action
          ('?\(
           (skip-chars-forward " ")
           (insert "(")
           (forward-sexp 1)
           (insert ")"))
          (t
           (throw 'break nil)))))))


(defun tv-insert-double-quote-and-close-forward ()
  (interactive)
  (let (action
        (prompt (and (not (minibufferp))
                     "\": Insert, (any key to exit).")))
    (unless prompt (message "\": Insert, (any key to exit)."))
    (catch 'break
      (while t
        (setq action (read-key prompt))
        (case action
          ('?\"
           (skip-chars-forward " \n")
           (insert "\"")
           (forward-sexp 1)
           (insert "\""))
          (t
           (throw 'break (when (characterp action) (insert (string action))))))))))


(defun tv-insert-pair-and-close-forward ()
  (interactive)
  (let (action)
    (insert "(")
    (catch 'break
      (while t
        (setq action (read-key "`)': Insert, (any key to exit)."))
        (case action
          ('?\)
           (unless (looking-back "(")
             (delete-char -1))
           (skip-chars-forward " ")
           (forward-symbol 1)
           (insert ")"))
          (t
           (forward-char -1)
           (throw 'break nil)))))))


;;; Open-file-in-gimp 
;; <2009-08-13 Jeu. 10:29>
(defun tv-gimp-open-file (file)
  (interactive
   (list (helm-comp-read "Image: "
                         (loop
                               with f = (cddr (directory-files default-directory))
                               with img = '("jpg" "png" "gif" "jpeg")
                               for i in f
                               if (member (file-name-extension i) img)
                               collect i))))
  (message "Starting Gimp...") (sit-for 0.2)
  (start-process "Gimp" nil "gimp" (expand-file-name file))
  (set-process-sentinel (get-process "Gimp")
                        #'(lambda (process event)
                            (message
                             "%s process is %s"
                             process
                             event))))


;;; Insert-an-image-at-point 
(defun tv-insert-image-at-point (image)
  (interactive "fImage: ")
  (let ((img (create-image image)))
    (insert-image img)))

(defun tv-show-img-from-fname-at-point ()
  (interactive)
  (let ((img (thing-at-point 'sexp)))
    (forward-line)
    (tv-insert-image-at-point img)))

;;; Show-message-buffer-a-few-seconds 
(autoload 'View-scroll-to-buffer-end "view")
(defun tv-tail-echo-area-messages ()
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (pop-to-buffer (get-buffer-create "*Messages*") t)
    (View-scroll-to-buffer-end)
    (sit-for 10)))

;;; Align-for-sections-in-loop 
(defun align-loop-region-for (beg end)
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\) = " 1 1 nil)
  (indent-region beg end))

(define-key lisp-interaction-mode-map (kbd "C-M-&") 'align-loop-region-for)
(define-key lisp-mode-map (kbd "C-M-&") 'align-loop-region-for)
(define-key emacs-lisp-mode-map (kbd "C-M-&") 'align-loop-region-for)

;;; Persistent-objects
;;
;; Main function to save objects.
(defun dump-object-to-file (obj file)
  "Save symbol object OBJ to the byte compiled version of FILE.
OBJ can be any lisp object, list, hash-table, etc...
Windows configurations and markers are not supported.
FILE must be an elisp file with ext \"*.el\" (NOT \"*.elc\").
Loading the *.elc file will restitute object.
That may not work with Emacs versions <=23.1 for hash tables."
  (require 'cl) ; Be sure we use the CL version of `eval-when-compile'.
  (assert (not (file-exists-p file)) nil
          (format "dump-object-to-file: File `%s' already exists, please remove it." file))
  (let ((print-length nil)
        (print-level nil)
        (print-circle t))
    (with-temp-file file
      (prin1 `(setq ,obj (eval-when-compile ,obj)) (current-buffer)))
    (byte-compile-file file) (delete-file file)
    (message "`%s' dumped to %sc" obj file)))

(defvar elisp-objects-default-directory "~/.emacs.d/elisp-objects/")
(defvar object-to-save-alist '((ioccur-history . "ioccur-history.el")
                               (extended-command-history . "extended-command-history.el")
                               (helm-external-command-history . "helm-external-command-history.el")
                               (helm-surfraw-engines-history . "helm-surfraw-engines-history.el")
                               (tv-save-buffers-alist . "tv-save-buffers-alist.el")
                               (helm-ff-history . "helm-ff-history.el")
                               (helm-c-grep-history . "helm-c-grep-history.el")
                               (kill-ring . "kill-ring.el")
                               (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
                               (register-alist . "register-alist.el")
                               ))

(defun dump-object-to-file-save-alist ()
  (when object-to-save-alist
    (loop for (o . f) in object-to-save-alist
          for abs = (expand-file-name f elisp-objects-default-directory)
          ;; Registers are treated specially.
          if (and (eq o 'register-alist)
                  (eval o))
          do (tv-dump-object-save-register-alist f)
          else do
          ;; Don't dump object when it is nil
          (and (eval o) (dump-object-to-file o abs)))))

(defun* restore-objects-from-directory
    (&optional (dir elisp-objects-default-directory))
  (let ((file-list (cddr (directory-files dir t))))
    (mapc 'load file-list)))

(defun* tv-dump-object-save-register-alist (&optional (file "register-alist.el"))
  "Save `register-alist' but only supported objects."
  (let ((register-alist (loop for (char . val) in register-alist
                              unless (or (markerp val)
                                         (vectorp val)
                                         (and (consp val) (window-configuration-p (car val))))
                              collect (cons char val)))
        (def-file (expand-file-name file elisp-objects-default-directory)))
    (dump-object-to-file 'register-alist def-file)))

;;; Persistents-buffer 
;;
;;
(defvar tv-save-buffers-unwanted-buffers-regexp ".*[.]org\\|diary\\|[.]newsticker-cache$")
(defun tv-save-some-buffers ()
  (loop with dired-blist = (loop for (f . b) in dired-buffers
                                 when (buffer-name b)
                                 collect b)
        with blist = (append (buffer-list) dired-blist)
        for b in blist
        for buf-fname = (or (buffer-file-name b) (car (rassoc b dired-buffers)))
        for place = (with-current-buffer b (point))
        when (and buf-fname
                  (not (string-match tramp-file-name-regexp buf-fname))
                  (not (string-match  tv-save-buffers-unwanted-buffers-regexp
                                      buf-fname))
                  (file-exists-p buf-fname))
        collect (cons buf-fname place)))

(defvar tv-save-buffers-alist nil)
(defun tv-dump-some-buffers-to-list ()
  (setq tv-save-buffers-alist (tv-save-some-buffers)))

(defun tv-restore-some-buffers ()
  (let* ((max (length tv-save-buffers-alist))
         (progress-reporter (make-progress-reporter "Restoring buffers..." 0 max)))
    (loop for (f . p) in tv-save-buffers-alist
          for count from 0
          do
          (with-current-buffer (find-file-noselect f 'nowarn)
            (goto-char p)
            (progress-reporter-update progress-reporter count)))
    (progress-reporter-done progress-reporter)))

(defun* tv-set-emacs-session-backup (&key enable)
  (if enable
      (unless (or (memq 'dump-object-to-file-save-alist kill-emacs-hook)
                  (memq 'tv-dump-some-buffers-to-list kill-emacs-hook)
                  (memq 'restore-objects-from-directory emacs-startup-hook)
                  (memq 'tv-restore-some-buffers emacs-startup-hook))
        (add-hook 'kill-emacs-hook 'dump-object-to-file-save-alist)
        (add-hook 'emacs-startup-hook 'restore-objects-from-directory)
        (add-hook 'kill-emacs-hook 'tv-dump-some-buffers-to-list)
        (add-hook 'emacs-startup-hook 'tv-restore-some-buffers 'append))
      (when (or (memq 'dump-object-to-file-save-alist kill-emacs-hook)
                (memq 'tv-dump-some-buffers-to-list kill-emacs-hook)
                (memq 'restore-objects-from-directory emacs-startup-hook)
                (memq 'tv-restore-some-buffers emacs-startup-hook))
        (remove-hook 'kill-emacs-hook 'dump-object-to-file-save-alist)
        (remove-hook 'emacs-startup-hook 'restore-objects-from-directory)
        (remove-hook 'kill-emacs-hook 'tv-dump-some-buffers-to-list)
        (remove-hook 'emacs-startup-hook 'tv-restore-some-buffers))))

;; Kill-backward 
(defun tv-kill-whole-line ()
  "Similar to `kill-whole-line' but don't kill new line.
Also alow killing whole line in a shell prompt without trying
to kill prompt.
Can be used from any place in the line."
  (interactive)
  (end-of-line)
  (let ((end (point)) beg)
    (forward-line 0)
    (while (get-text-property (point) 'read-only)
      (forward-char 1))
    (setq beg (point)) (kill-region beg end))
  (when (eq (point-at-bol) (point-at-eol))
    (delete-blank-lines) (skip-chars-forward " ")))

;; Similar to what eev does
(defun tv-eval-last-sexp-at-eol ()
  (interactive)
  (end-of-line)
  (call-interactively 'eval-last-sexp))

;; Delete-char-or-region 
(defun tv-delete-char (arg)
  (interactive "p")
  (if (helm-region-active-p)
      (delete-region (region-beginning) (region-end))
      (delete-char arg)))

;; Easypg 
(defun epa-sign-to-armored ()
  "Create a .asc file."
  (interactive)
  (let ((epa-armor t))
    (call-interactively 'epa-sign-file)))

;; Same as above but usable as alias in eshell
(defun gpg-sign-to-armored (file)
  "Create a .asc file."
  (let ((epa-armor t))
    (epa-sign-file file nil nil)))

;; Usable from eshell as alias
(defun gpg-sign-to-sig (file)
  "Create a .sig file."
  (epa-sign-file file nil 'detached))

;; Insert-log-from-patch 
(defun tv-insert-log-from-patch (patch)
  (interactive (list (helm-c-read-file-name
                      "Patch: ")))
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
    (insert data)))

;; Show infos on files using an easy interface for `file-attributes'.
;; [DEPRECATED] Use `helm-ff-attributes'
;;
(defun* show-file-attributes
    (file &key type links uid gid access-time modif-time status size mode gid-change inode device-num dired)
  "Comprehensive reading of `file-attributes'."
  (let ((all (destructuring-bind
                   (type links uid gid access-time modif-time status size mode gid-change inode device-num)
                 (file-attributes file 'string)
               (list :type        type
                     :links       links
                     :uid         uid
                     :gid         gid
                     :access-time access-time
                     :modif-time  modif-time
                     :status      status
                     :size        size
                     :mode        mode
                     :gid-change  gid-change
                     :inode       inode
                     :device-num  device-num))))
    (cond (type
           (let ((result (getf all :type)))
             (cond ((stringp result)
                    "symlink")
                   (result "directory")
                   (t "file"))))
          (links (getf all :links))
          (uid   (getf all :uid))
          (gid   (getf all :gid))
          (access-time
           (format-time-string "%Y-%m-%d %R" (getf all :access-time)))
          (modif-time
           (format-time-string "%Y-%m-%d %R" (getf all :modif-time)))
          (status
           (format-time-string "%Y-%m-%d %R" (getf all :status)))
          (size (getf all :size))
          (mode (getf all :mode))
          (gid-change (getf all :gid-change))
          (inode (getf all :inode))
          (device-num (getf all :device-num))
          (dired
           (concat
            (getf all :mode) " "
            (number-to-string (getf all :links)) " "
            (getf all :uid) ":"
            (getf all :gid) " "
            (number-to-string (getf all :size)) " "
            (format-time-string "%Y-%m-%d %R" (getf all :modif-time))))
          (t all))))


(defun* serial-rename-with1 (dir &key ext suffix prefix without)
  (let* ((ls-dir     (file-expand-wildcards (if ext
                                                (format "%s*.%s" (file-name-as-directory dir) ext)
                                                (format "%s*" (file-name-as-directory dir)))
                                            t)))
    (loop for i in ls-dir
          for no-ext = (file-name-sans-extension (file-name-nondirectory i))
          for new-name = (if (and without (string-match without no-ext))
                             (replace-match "" t t no-ext)
                             no-ext)
          for suffixed = (and suffix (expand-file-name (concat new-name suffix "." ext) dir))
          for prefixed = (and prefix (expand-file-name (concat prefix new-name "." ext) dir))
          for both = (and prefix suffix (expand-file-name (concat prefix new-name suffix "." ext) dir))
          for replace = (expand-file-name (concat new-name "." ext) dir)
          do (rename-file i (or both suffixed prefixed replace)))))


(defun* rename-file-with1 (file &key suffix prefix without)
  (let* ((dir (file-name-directory file))
         (ext (file-name-extension file))
         (no-ext (file-name-sans-extension (file-name-nondirectory file)))
         (new-name (if (and without (string-match without no-ext))
                       (replace-match "" t t no-ext)
                       no-ext))
         (suffixed (and suffix (expand-file-name (concat new-name suffix "." ext) dir)))
         (prefixed (and prefix (expand-file-name (concat prefix new-name "." ext) dir)))
         (both (and prefix suffix (expand-file-name (concat prefix new-name suffix "." ext) dir)))
         (replace (expand-file-name (concat new-name "." ext) dir)))
    (rename-file file (or both suffixed prefixed replace))))


;;; eshell-pager 
;;
;;
(defun eshell-pager (command &rest args)
  "Display the output of COMMAND by chunk of lines."
  (let* ((height   (/ (frame-height) 2)) ; Assume we use 1 or 2 windows.
         (split    (with-temp-buffer
                     (apply #'call-process command nil t nil args)
                     (goto-char (point-min))
                     (loop
                           while (not (eobp))
                           for beg = (point)
                           do (forward-line (- height 3))
                           collect (buffer-substring beg (point)))))
         (it       (iter-list split)) ; Initialize with a simple iterator.
         (last-elm (iter-next it)))
    (flet ((print-result ()
             (pop-to-buffer (get-buffer-create "*Pager*"))
             (erase-buffer)
             (save-excursion
               (when last-elm
                 (insert last-elm)))))
      (save-window-excursion
        (print-result) ; Print the first chunk of lines before starting loop.
        (while (let ((char (read-key "===[ Next: SPACE, Prec: b, AnyKey:Exit ]===")))
                 (case char
                   (32 ; SPACE: Continue with a circular iterator.
                    (setq it (iter-sub-next-circular split last-elm))
                    (setq last-elm (iter-next it)))
                   (98 ; b Go back.
                    (setq it (iter-sub-prec-circular split last-elm))
                    (setq last-elm (iter-next it)))
                   (t nil))) ; Exit loop if any other key is pressed.
          (print-result))))))

;; Switch indenting lisp style.
(defun toggle-lisp-indent ()
  (interactive)
  (if (eq lisp-indent-function 'common-lisp-indent-function)
      (progn
        (setq lisp-indent-function 'lisp-indent-function)
        (message "Switching to Emacs lisp indenting style."))
      (setq lisp-indent-function 'common-lisp-indent-function)
      (message "Switching to Common lisp indenting style.")))

;; C-mode conf
(defun tv-cc-this-file ()
  (interactive)
  (when (eq major-mode 'c-mode)
    (let* ((iname (buffer-file-name (current-buffer)))
           (oname (file-name-sans-extension iname)))
      (compile (format "make -k %s" oname)))))
(add-hook 'c-mode-hook #'(lambda ()
                           (declare (special c-mode-map))
                           (define-key c-mode-map (kbd "C-c C-c") 'tv-cc-this-file)))

;; Insert line numbers in region
(defun tv-insert-lineno-in-region (beg end)
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (loop while (re-search-forward "^.*$" nil t)
          for count from 1 do
          (replace-match
           (concat (format "%d " count) (match-string 0))))))

;; switch to emacs version
(defun eselect-emacs ()
  (interactive)
  (let ((execs '("b2m" "ctags" "ebrowse" "emacs" "emacsclient" "etags" "grep-changelog" "rcs-checkin"))
        helm-async-be-async) ; Turn off async because we are removing the emacs executable 'emacs'.
    (let* ((src-bin (expand-file-name (helm-comp-read
                                       "EmacsVersion: "
                                       (directory-files "/sudo::/usr/local/sbin"
                                                        nil directory-files-no-dot-files-regexp))
                                      "/sudo::/usr/local/sbin"))
           (bin-list (loop for i in execs
                           for full = (expand-file-name i src-bin)
                           when (file-exists-p full)
                           collect full))
           (src-info (helm-comp-read
                      "EmacsInfoVersion: "
                      (loop for i in
                            (directory-files "/sudo::/usr/local/share" t directory-files-no-dot-files-regexp)
                            when (string-match "info.*[0-9]+\\'" i) collect i))))
      (when (y-or-n-p (format "Really switch from %s to another emacs? " emacs-version))
        (loop for i in execs
              for full = (expand-file-name i "/sudo::/usr/local/bin/")
              when (file-exists-p full)
              do (delete-file (expand-file-name i "/sudo::/usr/local/bin/")))
        (delete-file "/sudo::/usr/local/share/info")
        (helm-dired-action "/sudo::/usr/local/bin/"
                           :action 'symlink
                           :files bin-list)
        (helm-dired-action "/sudo::/usr/local/share/info"
                           :action 'symlink :files (list src-info))
        (message "Switched to %s version" (file-name-nondirectory src-bin))))))

;; Permutations

(defun* permutations (bag &key result-as-string print)
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
             (loop for e in bag append
                   (loop for p in (permutations (remove e bag))
                         collect (cons e p))))))
    (when (or result-as-string print)
      (setq result (loop for i in result collect (mapconcat 'identity i ""))))
    (if print
        (with-current-buffer (get-buffer-create "*permutations*")
          (erase-buffer)
          (loop for i in result
                do (insert (concat i "\n")))
          (pop-to-buffer (current-buffer)))
        result)))

;; Verlan.
(defun tv-reverse-lines-in-region (beg end)
  (interactive "r")
  "Verlan region."
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((bl (point-at-bol))
             (el (point-at-eol))
             (cur-line (buffer-substring bl el))
             (split (loop for i across cur-line collect i)))
        (delete-region bl el)
        (loop for i in (reverse split) do (insert i)))
      (forward-line 1))))

;; Interface to df command-line.
;;
(defun dfh (directory)
  "Interface to df -h command line.
If a prefix arg is given choose directory, otherwise use `default-directory'."
  (interactive (list (if current-prefix-arg
                         (helm-c-read-file-name
                          "Directory: " :test 'file-directory-p)
                         default-directory)))
  (let ((df-info (tv-get-disk-info directory t)))
    (pop-to-buffer (get-buffer-create "*df info*"))
    (erase-buffer)
    (insert (format "*Volume Info for `%s'*\n\nDevice: %s\nMaxSize: \
%s\nUsed: %s\nAvailable: %s\nCapacity in use: %s\nMount point: %s"
                    directory
                    (getf df-info :device)
                    (getf df-info :blocks)
                    (getf df-info :used)
                    (getf df-info :available)
                    (getf df-info :capacity)
                    (getf df-info :mount-point)))
    (view-mode 1)))

;; Interface to du (directory size)
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

(defun tv-toggle-resplit-window ()
  (interactive)
  (when (> (count-windows) 1)
    (let ((buf (current-buffer))
          before-height) 
      (with-current-buffer buf
        (setq before-height (window-height))
        (delete-window)
        (set-window-buffer
         (select-window (if (= (window-height) before-height)
                            (split-window-vertically)
                            (split-window-horizontally)))
         buf)))))

;; Euro million
(defun euro-million ()
  (interactive)
  (flet ((star-num (limit)
           ;; Get a random number between 1 to 12.
           (let ((n 0))
             (while (= n 0) (setq n (random limit)))
             n))
         (get-stars ()
           ;; Return a list of 2 differents numbers from 1 to 12.
           (let* ((str1 (number-to-string (star-num 12)))
                  (str2 (let ((n (number-to-string (star-num 12))))
                          (while (string= n str1)
                            (setq n (number-to-string (star-num 12))))
                          n)))
             (list str1 str2)))           
         (result ()
           ;; Collect random numbers without  dups.
           (loop with L repeat 5
                 for r = (star-num 51)
                 if (not (member r L))
                 collect r into L
                 else
                 collect (let ((n (star-num 51)))
                           (while (memq n L)
                             (setq n (star-num 51)))
                           n) into L
                 finally return L)))
    (with-current-buffer (get-buffer-create "*Euro million*")
      (erase-buffer)
      (insert "Grille aléatoire pour l'Euro Million\n\n")
      (loop with ls = (loop repeat 5 collect (result))  
            for i in ls do
            (progn
              (insert (mapconcat #'(lambda (x)
                                     (let ((elm (number-to-string x)))
                                       (if (= (length elm) 1) (concat elm " ") elm)))
                                 i " "))
              (insert " Stars: ")
              (insert (mapconcat 'identity (get-stars) " "))
              (insert "\n"))
            finally do (pop-to-buffer "*Euro million*")))))

;; Just an example to use `url-retrieve'
(defun tv-download-file-async (url &optional noheaders to)
  (lexical-let ((noheaders noheaders) (to to))
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

(defun tv-group-sexp-matching-regexp-at-point (arg regexp)
  "Take all sexps matching REGEXP and put them at point.
The sexps are searched after point, unless ARG.
In this case, sexps are searched before point."
  (interactive "P\nsRegexp: ")
  (let ((pos (point))
        (fun (if arg 're-search-backward 're-search-forward))
        (sep (and (y-or-n-p "Separate sexp with newline? ") "\n")))
    (loop while (funcall fun regexp nil t)
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
(defun tv-check-paren-error ()
  (interactive)
  (let (pos-err)
    (save-excursion
      (goto-char (point-min))
      (catch 'error
        (condition-case err
            (forward-list 9999)
          (error
           (throw 'error
             (setq pos-err (caddr err)))))))
    (if pos-err
        (message "Paren error found in sexp starting at %s"
                 (goto-char pos-err))
        (message "No paren error found")))) 

(when (require 'async)
  (defun async-byte-compile-file (file)
    (interactive "fFile: ")
    (let ((proc
           (async-start
            `(lambda ()
               (require 'bytecomp)
               ,(async-inject-variables "\\`load-path\\'")
               (let ((default-directory ,(file-name-directory file)))
                 (add-to-list 'load-path default-directory)
                 (ignore-errors
                   (load ,file))
                 ;; returns nil if there were any errors
                 (prog1
                     (byte-compile-file ,file)
                   (load ,file)))))))

      (unless (condition-case err
                  (async-get proc)
                (error
                 (ignore (message "Error: %s" (car err)))))
        (ignore (message "Recompiling %s...FAILED" file))))))

;;; Generate strong passwords.
;;
(defun* genpasswd (&optional (limit 12))
  "Generate strong password of length LIMIT.
LIMIT should be a number divisible by 2, otherwise
the password will be of length (floor LIMIT)."
  (loop with alph = ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                     "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                     "w" "x" "y" "z" "A" "B" "C" "D" "E" "F" "G"
                     "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                     "S" "T" "U" "V" "W" "X" "Y" "Z" "#" "!" "$"
                     "&" "~" ";"]
        ;; Divide by 2 because collecting 2 list.
        for i from 1 to (floor (/ limit 2))
        for rand1 = (int-to-string (random 9))
        for alphaindex = (random (length alph))
        for rand2 = (aref alph alphaindex)
        ;; Collect a random number between O-9
        collect rand1 into ls
        ;; collect a random alpha between a-zA-Z.
        collect rand2 into ls
        finally return
        ;; Now shuffle ls.
        (loop for n in ls
              for elm = (nth (random (length ls)) ls)
              concat elm)))

;;; Rotate windows
;;
;;

(defun rotate-windows ()
  (interactive)
  (require 'iterator)
  (assert (> (length (window-list)) 1)
          nil "Error: Can't rotate with a single window")
  (unless helm-alive-p
    (loop with wlist1 = (iter-circular (window-list))
          with wlist2 = (iter-circular (cdr (window-list))) 
          with len = (length (window-list))
          for count from 1
          for w1 = (iter-next wlist1)
          for b1 = (window-buffer w1)
          for s1 = (window-start w1)
          for w2 = (iter-next wlist2)
          for b2 = (window-buffer w2)
          for s2 = (window-start w2)
          while (< count len)
          do (progn (set-window-buffer w1 b2)
                    (set-window-start w1 s2)
                    (set-window-buffer w2 b1)
                    (set-window-start w2 s1)))))
(global-set-key (kbd "C-c -") 'rotate-windows)


;; Provide 
(provide 'tv-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; tv-utils.el ends here
