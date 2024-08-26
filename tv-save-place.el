;;; tv-save-place.el --- Save places. -*- lexical-binding: t -*- 
;;

;;; Commentary:

;; A simple replacement of saveplace.el

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Places are saved and restored by psession!
(defvar tv-save-place-cache (make-hash-table :test 'equal))
(defvar tv-save-place-ignore-file-regexps '("\\.git/" "-autoloads.el\\'"))
(defun tv-save-place ()
  (let ((file (buffer-file-name))
        pos)
    (when (and file
               (cl-loop for re in tv-save-place-ignore-file-regexps
                        never (string-match re file)))
      (widen)
      (setq pos (point))
      (unless (<= pos 1)
        (puthash file pos tv-save-place-cache)))))

(defun tv-save-place-refresh-cache ()
  (cl-loop for k being the hash-keys of tv-save-place-cache
           using (hash-values v)
           for buf = (get-buffer (file-name-nondirectory k))
           when (buffer-live-p buf)
           do (with-current-buffer buf
                (unless (eq (point) v)
                  (tv-save-place)))))

(defun tv-save-place-restore-pos ()
  (let* ((file (buffer-file-name))
         (pos (gethash file tv-save-place-cache)))
    (when pos (goto-char pos))))

;;;###autoload
(define-minor-mode tv-save-place-mode
    "Save position in files."
  :group 'convenience
  :global t
  (if tv-save-place-mode
      (progn
        (add-hook 'kill-buffer-hook 'tv-save-place)
        (add-hook 'before-revert-hook 'tv-save-place)
        (add-hook 'find-file-hook 'tv-save-place-restore-pos 100)
        (add-hook 'kill-emacs-hook 'tv-save-place-refresh-cache))
    (remove-hook 'kill-buffer-hook 'tv-save-place)
    (remove-hook 'before-revert-hook 'tv-save-place)
    (remove-hook 'find-file-hook 'tv-save-place-restore-pos)
    (remove-hook 'kill-emacs-hook 'tv-save-place-refresh-cache)))

(provide 'tv-save-place.el)

;;; tv-save-place.el ends here
