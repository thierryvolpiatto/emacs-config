;;; files-extension.el -- extension to files.el

;;; Code:

(require 'files)
(require 'dired-aux)
(eval-when-compile (require 'cl))

(defun files-equal-p (file1 file2 &optional noexist)
  "Return non-nil if FILE1 and FILE2 name the same file.
This function works even on non--existing files."
  (let ((handler (or (find-file-name-handler file1 'files-equal-p)
                     (find-file-name-handler file2 'files-equal-p))))
    (if handler
        (funcall handler 'files-equal-p file1 file2 noexist)
      (let ((f1-attr (file-attributes (file-truename file1)))
            (f2-attr (file-attributes (file-truename file2))))
        (if (and f1-attr f2-attr)
            (equal f1-attr f2-attr)
          (when noexist
            (string= (file-truename
                      (file-name-as-directory file1))
                     (file-truename
                      (file-name-as-directory file2)))))))))

;; (files-equal-p "~/tmp/Test/Test6" "/home/thierry/tmp/Test/Test6/")
;; (files-equal-p "~/.emacs.el" "/home/thierry/.emacs.d/emacs-config-laptop/.emacs.el")
;; (files-equal-p "~/tmp/Test" "~/tmp/test")

(defun file-subdir-of-p (dir1 dir2 &optional noexist)
  "Return non-nil if DIR1 is a subdirectory of DIR2.
Note that a directory is treated by this function as a subdirectory of itself.
This function only works when its two arguments already exist unless
optional argument NOEXIST is non--nil, otherwise it returns nil."
  (let ((handler (or (find-file-name-handler dir1 'file-subdir-of-p)
                     (find-file-name-handler dir2 'file-subdir-of-p))))
    (if handler
        (funcall handler 'file-subdir-of-p dir1 dir2 noexist)
        (when (or noexist
                  (and (file-directory-p dir1)
                       (file-directory-p dir2)))
          (loop with f1 = (file-truename dir1)
                with f2 = (file-truename dir2)
                with ls1 = (or (split-string f1 "/" t) (list "/"))
                with ls2 = (or (split-string f2 "/" t) (list "/"))
                for p = (string-match "^/" f1)
                for i in ls1
                for j in ls2
                when (string= i j)
                concat (if p (concat "/" i) (concat i "/"))
                into root
                finally return
                (files-equal-p (file-truename root) f2 noexist))))))


(defun copy-directory (directory newname &optional keep-time parents copy-contents)
  "Copy DIRECTORY to NEWNAME.  Both args must be strings.
This function always sets the file modes of the output files to match
the corresponding input file.

The third arg KEEP-TIME non-nil means give the output files the same
last-modified time as the old ones.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

Noninteractively, the last argument PARENTS says whether to
create parent directories if they don't exist.  Interactively,
this happens by default.

If NEWNAME names an existing directory, copy DIRECTORY as a
subdirectory there.  However, if called from Lisp with a non-nil
optional argument COPY-CONTENTS, copy the contents of DIRECTORY
directly into NEWNAME instead."
  (interactive
   (let ((dir (read-directory-name
	       "Copy directory: " default-directory default-directory t nil)))
     (list dir
	   (read-directory-name
	    (format "Copy directory %s to: " dir)
	    default-directory default-directory nil nil)
	   current-prefix-arg t nil)))
  (when (file-subdir-of-p newname directory t)
    (error "Cannot copy `%s' into its subdirectory `%s'"
           directory newname))
  ;; If default-directory is a remote directory, make sure we find its
  ;; copy-directory handler.
  (let ((handler (or (find-file-name-handler directory 'copy-directory)
		     (find-file-name-handler newname 'copy-directory))))
    (if handler
	(funcall handler 'copy-directory directory
                 newname keep-time parents copy-contents)

      ;; Compute target name.
      (setq directory (directory-file-name (expand-file-name directory))
	    newname   (directory-file-name (expand-file-name newname)))

      (cond ((not (file-directory-p newname))
	     ;; If NEWNAME is not an existing directory, create it;
	     ;; that is where we will copy the files of DIRECTORY.
	     (make-directory newname parents))
	    ;; If NEWNAME is an existing directory and COPY-CONTENTS
	    ;; is nil, copy into NEWNAME/[DIRECTORY-BASENAME].
	    ((not copy-contents)
	     (setq newname (expand-file-name
			    (file-name-nondirectory
			     (directory-file-name directory))
			    newname))
	     (and (file-exists-p newname)
		  (not (file-directory-p newname))
		  (error "Cannot overwrite non-directory %s with a directory"
			 newname))
	     (make-directory newname t)))

      ;; Copy recursively.
      (dolist (file
	       ;; We do not want to copy "." and "..".
	       (directory-files directory 'full
				directory-files-no-dot-files-regexp))
	(if (file-directory-p file)
	    (copy-directory file newname keep-time parents)
	  (let ((target (expand-file-name (file-name-nondirectory file) newname))
		(attrs (file-attributes file)))
	    (if (stringp (car attrs)) ; Symbolic link
		(make-symbolic-link (car attrs) target t)
	      (copy-file file target t keep-time)))))

      ;; Set directory attributes.
      (let ((modes (file-modes directory))
	    (times (and keep-time (nth 5 (file-attributes directory)))))
	(if modes (set-file-modes newname modes))
	(if times (set-file-times newname times))))))

(defun dired-create-files (file-creator operation fn-list name-constructor
					&optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

Optional MARKER-CHAR is a character with which to mark every
newfile's entry, or t to use the current marker character if the
old file was marked."
  (let (dired-create-files-failures failures
	skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
	     overwrite-backup-query)	; for dired-handle-overwrite
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME and FROM are the same directory or
              ;; If DESTNAME is a subdirectory of FROM, return error.
              (and (file-subdir-of-p destname from t)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            (condition-case err
                (progn
                  (funcall file-creator from to dired-overwrite-confirmed)
                  (if overwrite
                      ;; If we get here, file-creator hasn't been aborted
                      ;; and the old entry (if any) has to be deleted
                      ;; before adding the new entry.
                      (dired-remove-file to))
                  (setq success-count (1+ success-count))
                  (message "%s: %d of %d" operation success-count total)
                  (dired-add-file to actual-marker-char))
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
		operation (length failures)
		(dired-plural-s (length failures))
		total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
		operation (length failures)
		total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
		operation (length skipped) total
		(dired-plural-s total))
       skipped))
     (t
      (message "%s: %s file%s"
	       operation success-count (dired-plural-s success-count)))))
  (dired-move-to-filename))


(defun dired-copy-file-recursive (from to ok-flag &optional
				       preserve-time top recursive)
  (when (file-subdir-of-p to from)
    (error "Cannot copy `%s' into its subdirectory `%s'" from to))
  (let ((attrs (file-attributes from)))
    (if (and recursive
	     (eq t (car attrs))
	     (or (eq recursive 'always)
		 (yes-or-no-p (format "Recursive copies of %s? " from))))
	;; This is a directory.
	(copy-directory from to preserve-time)
      ;; Not a directory.
      (or top (dired-handle-overwrite to))
      (condition-case err
	  (if (stringp (car attrs))
	      ;; It is a symlink
	      (make-symbolic-link (car attrs) to ok-flag)
	    (copy-file from to ok-flag preserve-time))
	(file-date-error
	 (push (dired-make-relative from)
	       dired-create-files-failures)
	 (dired-log "Can't set date on %s:\n%s\n" from err))))))

(provide 'files-extension)

;;; files-extension.el ends here
