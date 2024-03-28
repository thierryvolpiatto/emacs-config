;;; dired-extension.el -- improvements for dired

;	$Id: dired-extension.el,v 1.36 2010/05/30 07:03:33 thierry Exp thierry $	

;;; Code:
(require 'dired)
(require 'cl-lib)

;;; Dired config

(setq dired-font-lock-keywords
  (list
   ;; Marked files.
   ;; Allow copy/rename/sym/hard files to be marked also.
   (list ;(concat "^[" (char-to-string dired-marker-char) "]")
    (concat "^\\([^ " (char-to-string dired-del-marker) "]\\)")
    '(".+" nil nil (0 dired-marked-face))) ; Don't jump to filename to mark whole line.

   ;; Flagged files.
   (list (concat "^[" (char-to-string dired-del-marker) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))

   ;; Symbolic links.
   (list dired-re-sym ;"\\([^ ]+\\) -> [^ ]+$"
	 '(".+" (dired-move-to-filename) nil (0 dired-symlink-face)))

   ;; Flagged files or not yet saved (.# or #.#)
   (list "\\(^..*-\\).*\\( [0-9:]* \\)\\(\.?#.*#?\\)$" '(3 dired-symlink-face))
   
   ;; Directory headers.
   (list dired-subdir-regexp '(1 dired-header-face))
   
   ;; Size used in dir (second line).
   (list "^..\\([a-zA-Z ]*\\)\\([0-9.,]+[kKMGTPEZY]?\\)\\( [a-zA-Z]*\\)?\\( [0-9.,]+[kKMGTPEZY]?\\)?" '(2 '((:foreground "cyan"))))

   ;; Dired marks. (C,D, etc... at beginning of line)
   (list dired-re-mark '(0 dired-mark-face))
   
   ;; Match from beginning of line to filename.
   (list "^..\\([drwxslt-]*\\) *\\([0-9]*\\) *\\([a-z ]*\\) *\\([0-9.,]*[kKMGTPEZY]?\\)\\( *[ 0-9a-zA-Z-éèû.]* [0-9:]*[ 0-9:]* \\)"
         '(1 '((:foreground "IndianRed"))))
   (list "^..\\([drwxslt-]*\\) *\\([0-9]*\\) *\\([a-z ]*\\) *\\([0-9.,]*[kKMGTPEZY]?\\)\\( *[ 0-9a-zA-Z-éèû.]* [0-9:]*[ 0-9:]* \\)"
         '(2 '((:foreground "cyan"))))
   (list "^..\\([drwxslt-]*\\) *\\([0-9]*\\) *\\([a-z ]*\\) *\\([0-9.,]*[kKMGTPEZY]?\\)\\( *[ 0-9a-zA-Z-éèû.]* [0-9:]*[ 0-9:]* \\)"
         '(3 '((:foreground "ForestGreen"))))
   (list "^..\\([drwxslt-]*\\) *\\([0-9]*\\) *\\([a-z ]*\\) *\\([0-9.,]*[kKMGTPEZY]?\\)\\( *[ 0-9a-zA-Z-éèû.]* [0-9:]*[ 0-9:]* \\)"
         '(4 '((:foreground "cyan"))))
   (list "^..\\([drwxslt-]*\\) *\\([0-9]*\\) *\\([a-z ]*\\) *\\([0-9.,]*[kKMGTPEZY]?\\)\\( *[ 0-9a-zA-Z-éèû.]* [0-9:]*[ 0-9:]* \\)"
         '(5 '((:foreground "Gold"))))
   
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; file name itself.  We search for Dired defined regexps, and then use the
   ;; Dired defined function `dired-move-to-filename' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the file name.

   ;; Subdirectories.
   (list dired-re-dir
	 '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))

   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
     (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
	   '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the filename,
		    ;; move back to the start of the filename
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (dired-move-to-filename)))
	     nil (0 dired-ignored-face))))
   
   ;; Regular file names.
   (list "\\(^..*-\\).*\\( [0-9:]* \\)\\(.*\\)$"
         '(".+" (dired-move-to-filename) nil (0 '((:foreground "Dodgerblue3")))))

   
   ;; Filenames extensions.
   ;(list "[^ .]\\.\\([a-zA-Z]*\\)[*]?$" '(1 '((:foreground "purple")) t))
   (list "[^ .]\\.\\([a-zA-Z]*\\)$" '(1 '((:foreground "purple")) t))
   ;(list "[^ .]\\.\\([^. /]+\\)$" '(1 '((:foreground "purple")) t))

   ;; Executable flags (Use C-u s)
   (list "[^ .]\\([*]?$\\)" '(1 '((:foreground "red")) t))
   
   ;; Compressed filenames extensions.
   (list "[^ .]\\.\\([tz7]?[bgi]?[pzZ]2?\\)[*]?$" '(1 '((:foreground "yellow")) t))
  
   ;; Total available size (second line), not used by tramp so put it after all.
   (list "^..\\([a-zA-Z ]*\\)\\([0-9.,]+[kKMGTPEZY]?\\)\\( [a-zA-Z]*\\)?\\( [0-9.,]+[kKMGTPEZY]?\\)?" '(4 '((:foreground "cyan")) t))
   
   ;; Files that are group or world writable.
  (list (concat dired-re-maybe-mark dired-re-inode-size
        	 "\\([-d]\\(....w....\\|.......w.\\)\\)")
         '(1 dired-warning-face)
         '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))

   ;; Explicitly put the default face on file names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat dired-re-maybe-mark dired-re-inode-size dired-re-perms ".*:$")
	 '(".+" (dired-move-to-filename) nil (0 'default)))))


;;; showup size available when -h arg of ls used.

(defun tv:-advice-get-free-disk-space (dir)
  (unless (file-remote-p (expand-file-name dir))
    (save-match-data
      ;; That is for windows.
      (if (fboundp 'file-system-info)
	  (let ((fsinfo (file-system-info dir)))
	    (if fsinfo
	        (format "%.0f" (/ (nth 2 fsinfo) 1024))))
        ;; And this is for Linux.
        (when (executable-find directory-free-space-program)
          (cl-getf (tv:get-disk-info dir 'human) :available))))))

(defun tv:get-disk-info (directory &optional human)
  (let* ((directory-free-space-args
          (if (and dired-actual-switches
                   (string-match "h" dired-actual-switches))
              (concat directory-free-space-args "h")
            directory-free-space-args))
         (default-directory (expand-file-name directory))
         (dir  (or (file-remote-p default-directory 'localname)
                   default-directory))
         (args (if human
                   (concat directory-free-space-args "h")
                 directory-free-space-args))
         (data (with-temp-buffer
                 (process-file directory-free-space-program
                               nil t nil args dir)
                 (split-string (buffer-string) "\n" t)))
         (values (split-string (cadr data))))
    (cl-loop for i in '(:device :blocks :used :available :capacity :mount-point)
             for j in values
             append (list i j))))

(when (< emacs-major-version 27)
  (advice-add 'get-free-disk-space :override 'tv:-advice-get-free-disk-space))


(provide 'dired-extension)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; dired-extension.el ends here
