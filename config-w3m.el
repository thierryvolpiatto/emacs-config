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
      w3m-default-display-inline-images t)

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

(when (require 'w3m-search)
  (add-to-list 'w3m-search-engine-alist '("DuckDuckGo" "https://duckduckgo.com/lite/?q=%s&kp=1"))
  (setq w3m-search-default-engine "DuckDuckGo"))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-region "w3m"
  "Render region in current buffer and replace with result." t)
(autoload 'w3m-toggle-inline-image "w3m"
  "Toggle the visibility of an image under point." t)

(setq w3m-home-page "http://www.duckduckgo.com")

;; enable-cookies-in-w3m 
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)

;; w3m-antenna 
(autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)

;; netscape-vs-firefox 
(setq browse-url-netscape-program "firefox")

;; Change tabs easily and helm-w3m-bookmarks.
(when (require 'w3m)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "V") 'helm-w3m-bookmarks)
  (define-key w3m-mode-map (kbd "M") 'w3m-view-url-with-browse-url))

;; Remove-trailing-white-space-in-w3m-buffers 
(add-hook 'w3m-display-hook
          #'(lambda (url)
              (let ((buffer-read-only nil))
                (delete-trailing-whitespace))))


(provide 'config-w3m)

;;; .emacs-config-w3m.el ends here


