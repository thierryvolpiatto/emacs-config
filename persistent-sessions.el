;;; Persistent-objects
;;

;;; Code:

(eval-when-compile (require 'dired))

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
                               (helm-grep-history . "helm-grep-history.el")
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


(provide 'persistent-sessions)

;;; persistent-sessions.el ends here
