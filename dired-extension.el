;;; dired-extension.el -- improvements for dired

;	$Id: dired-extension.el,v 1.36 2010/05/30 07:03:33 thierry Exp thierry $	

;;; Code:
(eval-when-compile (require 'dired)
                   (require 'image-dired))
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


;; Allow for consp `dired-directory' too.
;; Took in dired+.el waiting a fix in emacs24.
;;
(defun dired-buffers-for-dir (dir &optional file)
  "Return a list of buffers that dired DIR (top level or in-situ subdir).
If FILE is non-nil, include only those whose wildcard pattern (if any)
matches FILE.
The list is in reverse order of buffer creation, most recent last.
As a side effect, killed dired buffers for DIR are removed from
`dired-buffers'."
  (setq dir (file-name-as-directory dir))
  (let ((alist  dired-buffers) result elt buf pattern)
    (while alist
      (setq elt (car alist)
            buf (cdr elt))
      (if (buffer-name buf)
          (if (dired-in-this-tree dir (car elt))
              (with-current-buffer buf
                (and (assoc dir dired-subdir-alist)
                     (or (null file)
                         (let ((wildcards
                                ;; Allow for consp `dired-directory' too.
                                (file-name-nondirectory (if (consp dired-directory)
                                                            (car dired-directory)
                                                            dired-directory))))
                           (or (= 0 (length wildcards))
                               (string-match (dired-glob-regexp wildcards) file))))
                     (setq result (cons buf result)))))
          ;; else buffer is killed - clean up:
          (setq dired-buffers (delq elt dired-buffers)))
      (setq alist (cdr alist)))
    result))

(defun dired-on-marked (buf-name)
  (interactive "sBufferName: ")
  (dired (cons buf-name (dired-get-marked-files))))
(define-key dired-mode-map (kbd "C-c D") 'dired-on-marked)


;;; Provide
(provide 'dired-extension)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:

;;; dired-extension.el ends here
