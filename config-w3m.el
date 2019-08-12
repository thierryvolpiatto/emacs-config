;;; .emacs-config-w3m.el -- config w3m for thievol

;; Code:

(setq w3m-bookmark-file "~/.w3m/bookmark.html")

;; Get icons from melpa directory
(setq w3m-icon-directory (expand-file-name
                          "icons"
                          (file-name-directory
                           (locate-library "w3m"))))
(setq w3m-default-save-directory "~/download/")

(setq w3m-coding-system 'utf-8
      w3m-language "french"
      w3m-output-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-default-display-inline-images nil
      ;; Set these variables manually as `w3m-display-mode' is sucking.
      w3m-use-tab t
      w3m-pop-up-frames nil
      w3m-pop-up-windows nil)

;; `w3m-bookmark-save-buffer' is backing up bookmark file by renaming,
;; so that when `w3m-bookmark-file' is a symlink the symlink is
;; replaced by the file, fix this. 
(defun tv/advice--w3m-bookmark-save-buffer ()
  (cond
   ((buffer-file-name)
    (basic-save-buffer))
   ((buffer-modified-p)
    (when (and (file-exists-p w3m-bookmark-file)
               make-backup-files
               (funcall backup-enable-predicate w3m-bookmark-file))
      (with-current-buffer (find-file-noselect w3m-bookmark-file)
        (backup-buffer)
        (kill-buffer)))
    (write-region (point-min) (point-max) w3m-bookmark-file nil t)
    (kill-buffer))))
(advice-add 'w3m-bookmark-save-buffer :override #'tv/advice--w3m-bookmark-save-buffer)

(use-package w3m-search
    :defer t
    :config
    (add-to-list 'w3m-search-engine-alist '("DuckDuckGo" "https://duckduckgo.com/lite/?q=%s&kp=1"))
    (setq w3m-search-default-engine "DuckDuckGo"))

(setq w3m-home-page "http://www.duckduckgo.com")

;; enable-cookies-in-w3m 
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)

;; netscape-vs-firefox 
(setq browse-url-netscape-program "firefox")

;; Remove-trailing-white-space-in-w3m-buffers 
(add-hook 'w3m-display-hook
          #'(lambda (url)
              (let ((buffer-read-only nil))
                (delete-trailing-whitespace))))

(defun tv/w3m-fill-region-or-paragraph ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (region-active-p)
        (call-interactively #'fill-region)
      (call-interactively #'fill-paragraph))))

(provide 'config-w3m)

;;; .emacs-config-w3m.el ends here


