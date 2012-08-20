;;; dvc-init.el --- config pour dvc. 

;;; Code:

;; Basics
(require 'dvc-autoloads)
(dvc-insinuate-gnus)

;; Bof...
(setq dvc-tips-enabled nil)

;; Don't show time-stamp by default.
(setq dvc-bookmarks-show-time-stamp nil)

;; Don't show partner url's on startup (T-u)
(setq dvc-bookmarks-show-partner-url nil)

;; Thats shouldn't be set but if not...
(setq vc-delete-logbuf-window t)

;; Reload backend in dvc-status
(defadvice dvc-add-files (after reload-status-buffer)
  "reload dvc-status after adding files"
  (dvc-status))
(ad-activate 'dvc-add-files)

(defadvice dvc-revert-files (after reload-status-buffer)
  "reload dvc-status after reverting files"
  (dvc-status))
(ad-activate 'dvc-revert-files)

(defadvice dvc-remove-files (after reload-status-buffer () activate)
  "update display after removing files"
  (dvc-status))

;; Font-lock diff output
(add-hook 'xhg-log-mode-hook
          #'(lambda ()
              (font-lock-add-keywords nil '(("^\\+.*" . font-lock-variable-name-face)))
              (font-lock-add-keywords nil '(("^\\-.*" . font-lock-doc-face)))))

(add-hook 'xgit-diff-mode-hook
          #'(lambda ()
              (font-lock-add-keywords nil '(("^\\+.*" . font-lock-variable-name-face)))
              (font-lock-add-keywords nil '(("^\\-.*" . font-lock-doc-face)))))

;; Global keys for mq
(global-set-key (kbd "C-x Q N") 'xhg-qnew)
(global-set-key (kbd "C-x Q R") 'xhg-qrefresh)

;; «dvc-rename-from-dired» (to ".dvc-rename-from-dired")
(defun dvc-dired-rename (new-name)
  (interactive "sNewName: ")
  (let ((source (dired-filename-at-point)))
    (dvc-rename source new-name)))

(define-key dired-mode-map (kbd "C-c R") 'dvc-dired-rename)

;; «git-config» (to ".git-config")
(setq xgit-use-index 'never) ; xgit doesn't support index and have no command to stage.
(xgit-insinuate-gnus)
(defalias 'xgit-dvc-export-via-email 'xgit-gnus-send-commit-notification)

(defun helm-dvc-apply-patch (patch)
  (let ((default-directory helm-ff-default-directory))
    (dvc-apply-patch patch)))


;; provide
(provide 'dvc-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dvc-init.el ends here
