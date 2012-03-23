;;; init-helm-thierry.el --- My startup file for helm. 
;;; Code:

(require 'helm-config)

;;;; Test Sources or new helm code. 
;;   !!!WARNING EXPERIMENTAL!!!
;;

;;;; Extensions

;;;; Mercurial-Qpatchs
(require 'helm-mercurial)

;;;; Delicious-bookmarks-tv
(require 'helm-delicious)

;;;; helm-ipython
;;
;;
(require 'helm-ipython)

(define-key py-mode-map (kbd "M-<tab>") 'helm-ipython-complete)
(define-key py-shell-map (kbd "M-<tab>") 'helm-ipython-complete)
(define-key py-mode-map (kbd "C-c M") 'helm-ipython-import-modules-from-buffer)


;;;; Helm-faces

(defface helm-tv-header '((t (:background "#22083397778B" :foreground "white" :underline t)))
  "Face for source header in the helm buffer." :group 'helm)

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "q")   'helm-qpatchs)
(define-key helm-command-map (kbd "v")   'helm-eev-anchors)
(define-key helm-command-map (kbd "d")   'helm-delicious)
(define-key helm-command-map (kbd "y e") 'helm-yaoddmuse-emacswiki-edit-or-view)
(define-key helm-command-map (kbd "y p") 'helm-yaoddmuse-emacswiki-post-library)
(define-key helm-command-map (kbd "g")   'helm-apt)


;;; Global-map
;;
;;
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)
(global-set-key (kbd "C-x r b") 'helm-bookmark-ext) ; Replace regular bookmark binding.
(global-set-key (kbd "C-h r")   'helm-info-emacs)
(global-set-key (kbd "C-c C-b") 'helm-browse-code)
(global-set-key (kbd "C-:")     'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")     'helm-calcul-expression)
(global-set-key (kbd "C-c h f") 'helm-info-at-point)
(global-set-key (kbd "C-c g")   'helm-google-suggest)
(global-set-key (kbd "C-c y")   'helm-yahoo-suggest)
(global-set-key (kbd "M-g s")   'helm-do-grep)
(define-key global-map [remap insert-register] 'helm-register)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

;; Lisp complete or indent.
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
(define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)

;; lisp complete.
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)

;;; Describe key-bindings
;;
;;
;(require 'helm-descbinds)
(helm-descbinds-install)            ; C-h b, C-x C-h

;; My-helm-describe-keybindings
;(require 'helm-describe-keybindings)
;(helm-describe-bindings-mode 1)

;;; Helm-variables
;;
;;
(setq helm-google-suggest-use-curl-p            t
      helm-kill-ring-threshold                  1
      helm-raise-command                        "wmctrl -xa %s"
      helm-yaoddmuse-use-cache-file             t
      helm-scroll-amount                        1
      helm-candidate-number-limit               100
      helm-quick-update                         t
      helm-candidate-separator                  (propertize (make-string 42 ?-) 'face '((:foreground "red")))
      helm-header-face                          'helm-tv-header
      helm-selection-face                       '((:background "ForestGreen"))
      helm-idle-delay                           0.1
      helm-input-idle-delay                     0.1
      helm-ff-avfs-directory                    "~/.avfs"
      helm-ff-history-max-length                100
      helm-su-or-sudo                           "sudo"
      helm-c-kill-ring-max-lines-number         5
      helm-ff-transformer-show-only-basename    t
      helm-c-default-external-file-browser      "thunar"
      ;helm-c-pdfgrep-default-read-command       "evince --page-label=%p '%f'"
      helm-c-etags-use-regexp-search            nil
      helm-c-use-adaptative-sorting             t
      ;helm-c-google-suggest-default-browser-function 'browse-url-uzbl
      )

;;; Debugging
;;
;;
(defun helm-debug-switch-to-log ()
  (interactive)
  (switch-to-buffer "*Helm Log*")
  (set-buffer "*Helm Log*")
  (goto-char (point-min))
  (re-search-forward "[\+]*$" nil t))

(defun helm-debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

;;; Enable helm-mode
;;
;;
(helm-mode 1)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
