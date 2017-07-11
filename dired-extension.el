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


;;; Redefine `insert-directory' to showup size available when -h arg of ls used.
;;
(defun insert-directory1 (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings
representing individual options.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'.

When SWITCHES contains the long `--dired' option, this function
treats it specially, for the sake of dired.  However, the
normally equivalent short `-D' option is just passed on to
`insert-directory-program', as any other option."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
	(let (result (beg (point)))

	  ;; Read the actual directory using `insert-directory-program'.
	  ;; RESULT gets the status code.
	  (let* (;; We at first read by no-conversion, then after
		 ;; putting text property `dired-filename, decode one
		 ;; bunch by one to preserve that property.
		 (coding-system-for-read 'no-conversion)
		 ;; This is to control encoding the arguments in call-process.
		 (coding-system-for-write
		  (and enable-multibyte-characters
		       (or file-name-coding-system
			   default-file-name-coding-system))))
	    (setq result
		  (if wildcard
		      ;; Run ls in the directory part of the file pattern
		      ;; using the last component as argument.
		      (let ((default-directory
			      (if (file-name-absolute-p file)
				  (file-name-directory file)
				(file-name-directory (expand-file-name file))))
			    (pattern (file-name-nondirectory file)))
			(call-process
			 shell-file-name nil t nil
			 "-c"
			 (concat (if (memq system-type '(ms-dos windows-nt))
				     ""
				   "\\") ; Disregard Unix shell aliases!
				 insert-directory-program
				 " -d "
				 (if (stringp switches)
				     switches
				   (mapconcat 'identity switches " "))
				 " -- "
				 ;; Quote some characters that have
				 ;; special meanings in shells; but
				 ;; don't quote the wildcards--we want
				 ;; them to be special.  We also
				 ;; currently don't quote the quoting
				 ;; characters in case people want to
				 ;; use them explicitly to quote
				 ;; wildcard characters.
				 (shell-quote-wildcard-pattern pattern))))
		    ;; SunOS 4.1.3, SVr4 and others need the "." to list the
		    ;; directory if FILE is a symbolic link.
 		    (unless full-directory-p
 		      (setq switches
 			    (if (stringp switches)
 				(concat switches " -d")
 			      (add-to-list 'switches "-d" 'append))))
		    (apply 'call-process
			   insert-directory-program nil t nil
			   (append
			    (if (listp switches) switches
			      (unless (equal switches "")
				;; Split the switches at any spaces so we can
				;; pass separate options as separate args.
				(split-string switches)))
			    ;; Avoid lossage if FILE starts with `-'.
			    '("--")
			    (progn
			      (if (string-match "\\`~" file)
				  (setq file (expand-file-name file)))
			      (list
			       (if full-directory-p
				   (concat (file-name-as-directory file) ".")
				 file))))))))

	  ;; If we got "//DIRED//" in the output, it means we got a real
	  ;; directory listing, even if `ls' returned nonzero.
	  ;; So ignore any errors.
	  (when (if (stringp switches)
		    (string-match "--dired\\>" switches)
		  (member "--dired" switches))
	    (save-excursion
	      (forward-line -2)
	      (when (looking-at "//SUBDIRED//")
		(forward-line -1))
	      (if (looking-at "//DIRED//")
		  (setq result 0))))

	  (when (and (not (eq 0 result))
		     (eq insert-directory-ls-version 'unknown))
	    ;; The first time ls returns an error,
	    ;; find the version numbers of ls,
	    ;; and set insert-directory-ls-version
	    ;; to > if it is more than 5.2.1, < if it is less, nil if it
	    ;; is equal or if the info cannot be obtained.
	    ;; (That can mean it isn't GNU ls.)
	    (let ((version-out
		   (with-temp-buffer
		     (call-process "ls" nil t nil "--version")
		     (buffer-string))))
	      (if (string-match "ls (.*utils) \\([0-9.]*\\)$" version-out)
		  (let* ((version (match-string 1 version-out))
			 (split (split-string version "[.]"))
			 (numbers (mapcar 'string-to-number split))
			 (min '(5 2 1))
			 comparison)
		    (while (and (not comparison) (or numbers min))
		      (cond ((null min)
			     (setq comparison '>))
			    ((null numbers)
			     (setq comparison '<))
			    ((> (car numbers) (car min))
			     (setq comparison '>))
			    ((< (car numbers) (car min))
			     (setq comparison '<))
			    (t
			     (setq numbers (cdr numbers)
				   min (cdr min)))))
		    (setq insert-directory-ls-version (or comparison '=)))
		(setq insert-directory-ls-version nil))))

	  ;; For GNU ls versions 5.2.2 and up, ignore minor errors.
	  (when (and (eq 1 result) (eq insert-directory-ls-version '>))
	    (setq result 0))

	  ;; If `insert-directory-program' failed, signal an error.
	  (unless (eq 0 result)
	    ;; Delete the error message it may have output.
	    (delete-region beg (point))
	    ;; On non-Posix systems, we cannot open a directory, so
	    ;; don't even try, because that will always result in
	    ;; the ubiquitous "Access denied".  Instead, show the
	    ;; command line so the user can try to guess what went wrong.
	    (if (and (file-directory-p file)
		     (memq system-type '(ms-dos windows-nt)))
		(error
		 "Reading directory: \"%s %s -- %s\" exited with status %s"
		 insert-directory-program
		 (if (listp switches) (concat switches) switches)
		 file result)
	      ;; Unix.  Access the file to get a suitable error.
	      (access-file file "Reading directory")
	      (error "Listing directory failed but `access-file' worked")))

	  (when (if (stringp switches)
		    (string-match "--dired\\>" switches)
		  (member "--dired" switches))
	    ;; The following overshoots by one line for an empty
	    ;; directory listed with "--dired", but without "-a"
	    ;; switch, where the ls output contains a
	    ;; "//DIRED-OPTIONS//" line, but no "//DIRED//" line.
	    ;; We take care of that case later.
	    (forward-line -2)
            (when (looking-at "//SUBDIRED//")
              (delete-region (point) (progn (forward-line 1) (point)))
              (forward-line -1))
	    (if (looking-at "//DIRED//")
		(let ((end (line-end-position))
		      (linebeg (point))
		      error-lines)
		  ;; Find all the lines that are error messages,
		  ;; and record the bounds of each one.
		  (goto-char beg)
		  (while (< (point) linebeg)
		    (or (eql (following-char) ?\s)
			(push (list (point) (line-end-position)) error-lines))
		    (forward-line 1))
		  (setq error-lines (nreverse error-lines))
		  ;; Now read the numeric positions of file names.
		  (goto-char linebeg)
		  (forward-word 1)
		  (forward-char 3)
		  (while (< (point) end)
		    (let ((start (insert-directory-adj-pos
				  (+ beg (read (current-buffer)))
				  error-lines))
			  (end (insert-directory-adj-pos
				(+ beg (read (current-buffer)))
				error-lines)))
		      (if (memq (char-after end) '(?\n ?\s))
			  ;; End is followed by \n or by " -> ".
			  (put-text-property start end 'dired-filename t)
			;; It seems that we can't trust ls's output as to
			;; byte positions of filenames.
			(put-text-property beg (point) 'dired-filename nil)
			(end-of-line))))
		  (goto-char end)
		  (beginning-of-line)
		  (delete-region (point) (progn (forward-line 1) (point))))
	      ;; Take care of the case where the ls output contains a
	      ;; "//DIRED-OPTIONS//"-line, but no "//DIRED//"-line
	      ;; and we went one line too far back (see above).
	      (forward-line 1))
	    (if (looking-at "//DIRED-OPTIONS//")
		(delete-region (point) (progn (forward-line 1) (point)))))

	  ;; Now decode what read if necessary.
	  (let ((coding (or coding-system-for-read
			    file-name-coding-system
			    default-file-name-coding-system
			    'undecided))
		coding-no-eol
		val pos)
	    (when (and enable-multibyte-characters
		       (not (memq (coding-system-base coding)
				  '(raw-text no-conversion))))
	      ;; If no coding system is specified or detection is
	      ;; requested, detect the coding.
	      (if (eq (coding-system-base coding) 'undecided)
		  (setq coding (detect-coding-region beg (point) t)))
	      (if (not (eq (coding-system-base coding) 'undecided))
		  (save-restriction
		    (setq coding-no-eol
			  (coding-system-change-eol-conversion coding 'unix))
		    (narrow-to-region beg (point))
		    (goto-char (point-min))
		    (while (not (eobp))
		      (setq pos (point)
			    val (get-text-property (point) 'dired-filename))
		      (goto-char (next-single-property-change
				  (point) 'dired-filename nil (point-max)))
		      ;; Force no eol conversion on a file name, so
		      ;; that CR is preserved.
		      (decode-coding-region pos (point)
					    (if val coding-no-eol coding))
		      (if val
			  (put-text-property pos (point)
					     'dired-filename t)))))))

	  (if full-directory-p
	      ;; Try to insert the amount of free space.
	      (save-excursion
		(goto-char beg)
		;; First find the line to put it on.
                ;(when (re-search-forward "^ *\\(total\\)" nil t)
                (let* ((directory-free-space-args (if (and dired-actual-switches
                                                           (string-match "h" dired-actual-switches))
                                                      (concat directory-free-space-args "h")
                                                      directory-free-space-args))
                       (available (get-free-disk-space ".")))
                  (when (re-search-forward "^ *\\(total\\)" nil t)
                    (when available
                      ;; Replace "total" with "used", to avoid confusion.
                      (replace-match "total used in directory" nil nil nil 1)
                      (end-of-line)
                      (insert " available " available))))))))))

(defalias 'insert-directory 'insert-directory1)

(defun get-free-disk-space (dir &optional human)
  (unless (file-remote-p dir)
    ;; That is for windows.
    (if (fboundp 'file-system-info)
	(let ((fsinfo (file-system-info dir)))
	  (if fsinfo
	      (format "%.0f" (/ (nth 2 fsinfo) 1024))))
        ;; And this is for Unix/GNULinux.
        (when (executable-find directory-free-space-program)
          (cl-getf (tv-get-disk-info dir human) :available)))))

(defun tv-get-disk-info (directory &optional human)
  (let* ((dir     (expand-file-name directory))
         (args    (if human
                      (concat directory-free-space-args "h")
                      directory-free-space-args))
         (data   (with-temp-buffer
                   (call-process directory-free-space-program
                                 nil t nil args dir)
                   (split-string (buffer-string) "\n" t)))
         (values (split-string (second data))))
    (cl-loop for i in '(:device :blocks :used :available :capacity :mount-point)
             for j in values
             append (list i j))))



(provide 'dired-extension)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; dired-extension.el ends here
