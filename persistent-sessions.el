;;; Persistent-objects
;;

;;; Code:

(eval-when-compile (require 'dired))

;; Main function to save objects.
(defun psession--dump-object-to-file (obj file)
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

(defvar psession--elisp-objects-default-directory "~/.emacs.d/elisp-objects/")
(defvar psession--object-to-save-alist '((ioccur-history . "ioccur-history.el")
                                         (extended-command-history . "extended-command-history.el")
                                         (helm-external-command-history . "helm-external-command-history.el")
                                         (helm-surfraw-engines-history . "helm-surfraw-engines-history.el")
                                         (psession--save-buffers-alist . "tv-save-buffers-alist.el")
                                         (helm-ff-history . "helm-ff-history.el")
                                         (helm-grep-history . "helm-grep-history.el")
                                         (kill-ring . "kill-ring.el")
                                         (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
                                         (register-alist . "register-alist.el")
                                         ))

(defun psession--dump-object-to-file-save-alist ()
  (when psession--object-to-save-alist
    (loop for (o . f) in psession--object-to-save-alist
          for abs = (expand-file-name f psession--elisp-objects-default-directory)
          ;; Registers are treated specially.
          if (and (eq o 'register-alist)
                  (eval o))
          do (psession--dump-object-save-register-alist f)
          else do
          ;; Don't dump object when it is nil
          (and (eval o) (psession--dump-object-to-file o abs)))))

(defun* psession--restore-objects-from-directory
    (&optional (dir psession--elisp-objects-default-directory))
  (let ((file-list (cddr (directory-files dir t))))
    (loop for file in file-list do (load file))))

(defun* psession--dump-object-save-register-alist (&optional (file "register-alist.el"))
  "Save `register-alist' but only supported objects."
  (let ((register-alist (loop for (char . val) in register-alist
                              unless (or (markerp val)
                                         (vectorp val)
                                         (and (consp val) (window-configuration-p (car val))))
                              collect (cons char val)))
        (def-file (expand-file-name file psession--elisp-objects-default-directory)))
    (psession--dump-object-to-file 'register-alist def-file)))

;;; Persistents-buffer 
;;
;;
(defvar psession--save-buffers-unwanted-buffers-regexp ".*[.]org\\|diary\\|[.]newsticker-cache$")
(defun psession--save-some-buffers ()
  (loop with dired-blist = (loop for (f . b) in dired-buffers
                                 when (buffer-name b)
                                 collect b)
        with blist = (append (buffer-list) dired-blist)
        for b in blist
        for buf-fname = (or (buffer-file-name b) (car (rassoc b dired-buffers)))
        for place = (with-current-buffer b (point))
        when (and buf-fname
                  (not (string-match tramp-file-name-regexp buf-fname))
                  (not (string-match  psession--save-buffers-unwanted-buffers-regexp
                                      buf-fname))
                  (file-exists-p buf-fname))
        collect (cons buf-fname place)))

(defvar psession--save-buffers-alist nil)
(defun psession--dump-some-buffers-to-list ()
  (setq psession--save-buffers-alist (psession--save-some-buffers)))

(defun psession--restore-some-buffers ()
  (let* ((max (length psession--save-buffers-alist))
         (progress-reporter (make-progress-reporter "Restoring buffers..." 0 max)))
    (loop for (f . p) in psession--save-buffers-alist
          for count from 0
          do
          (with-current-buffer (find-file-noselect f 'nowarn)
            (goto-char p)
            (push-mark p 'nomsg)
            (progress-reporter-update progress-reporter count)))
    (progress-reporter-done progress-reporter)))

(defun* psession--set-emacs-session-backup (&key enable)
  (if enable
      (unless (or (memq 'psession--dump-object-to-file-save-alist kill-emacs-hook)
                  (memq 'psession--dump-some-buffers-to-list kill-emacs-hook)
                  (memq 'psession--restore-objects-from-directory emacs-startup-hook)
                  (memq 'psession--restore-some-buffers emacs-startup-hook))
        (add-hook 'kill-emacs-hook 'psession--dump-object-to-file-save-alist)
        (add-hook 'emacs-startup-hook 'psession--restore-objects-from-directory)
        (add-hook 'kill-emacs-hook 'psession--dump-some-buffers-to-list)
        (add-hook 'emacs-startup-hook 'psession--restore-some-buffers 'append))
      (when (or (memq 'psession--dump-object-to-file-save-alist kill-emacs-hook)
                (memq 'psession--dump-some-buffers-to-list kill-emacs-hook)
                (memq 'psession--restore-objects-from-directory emacs-startup-hook)
                (memq 'psession--restore-some-buffers emacs-startup-hook))
        (remove-hook 'kill-emacs-hook 'psession--dump-object-to-file-save-alist)
        (remove-hook 'emacs-startup-hook 'psession--restore-objects-from-directory)
        (remove-hook 'kill-emacs-hook 'psession--dump-some-buffers-to-list)
        (remove-hook 'emacs-startup-hook 'psession--restore-some-buffers))))


(provide 'persistent-sessions)

;;; persistent-sessions.el ends here
