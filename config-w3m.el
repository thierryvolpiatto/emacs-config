;;; .emacs-config-w3m.el -- config w3m for thievol

;; Code:
(require 'w3m-load)
(setq w3m-bookmark-file "~/.w3m/bookmark.html")
(setq w3m-icon-directory "~/elisp/jenkins/w3m/icons")
(setq w3m-default-save-directory "~/download/")

(setq w3m-coding-system 'utf-8
      w3m-language "french"
      w3m-output-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)
 
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(autoload 'w3m-region "w3m"
  "Render region in current buffer and replace with result." t)
(autoload 'w3m-toggle-inline-image "w3m"
  "Toggle the visibility of an image under point." t)

(setq w3m-home-page "http://www.google.fr")

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
  (define-key w3m-mode-map (kbd "V") 'helm-w3m-bookmarks))

;; Remove-trailing-white-space-in-w3m-buffers 
(add-hook 'w3m-display-hook
          #'(lambda (url)
              (let ((buffer-read-only nil))
                (delete-trailing-whitespace))))

(global-set-key (kbd "<f7> h") 'w3m)

;; Provide
(provide 'config-w3m)

;;; .emacs-config-w3m.el ends here


