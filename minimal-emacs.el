;;; Minimal-emacs.el - Minimal config for Emacs.

;;; Code:

(require 'cl)
(blink-cursor-mode -1)
(show-paren-mode 1)
;; ;(setenv "LC_ALL" "C")

;; ;; «load-paths» (to ".load-paths")
;; (add-to-list 'Info-default-directory-list "/home/thierry/elisp/info")

;; (dolist (i '("/usr/local/share/emacs/site-lisp"
;; 	     "/home/thierry/elisp"
;;              "/home/thierry/elisp/emacs-wget"
;; 	     "/home/thierry/elisp/flim"
;; 	     "/home/thierry/elisp/apel"
;; 	     "/home/thierry/elisp/auctex"
;; 	     "/home/thierry/elisp/autoconf-mode"
;; 	     "/home/thierry/elisp/libidn"
;; 	     "/home/thierry/elisp/librep"
;; 	     "/home/thierry/elisp/subversion"
;; 	     "/home/thierry/elisp/tex-utils"
;;              "/home/thierry/elisp/AC/"
;;              "/home/thierry/elisp/emms/lisp/"
;; 	     "/usr/local/share/emacs/site-lisp"
;; 	     "/home/thierry/elisp/ipython"
;; 	     "/home/thierry/elisp/python-mode"
;; 	     "/home/thierry/elisp/emacs-w3m/"
;; 	     "/home/thierry/elisp/ledger/"
;; 	     "/home/thierry/.emacs.d/"
;;              "/home/thierry/.emacs.d/themes/"
;; 	     "/home/thierry/.emacs.d/emacs-config-laptop/"
;; 	     "/home/thierry/elisp/anything/"
;; 	     ;"/home/thierry/elisp/eev/"
;; 	     "/home/thierry/elisp/D.A-libs/"
;; 	     "/home/thierry/elisp/pymacs"
;;              ;"/home/thierry/elisp/elscreen"
;;              "/home/thierry/elisp/google-apps"
;; 	     ;"/home/thierry/elisp/ngnus/lisp"
;;              "/home/thierry/elisp/bbdb/lisp"
;;              ;"~/elisp/g-client/"
;;              ;"~/elisp/wave-client-for-emacs/lisp"
;; 	     "/usr/share/emacs/site-lisp/libidn"))
;;   (add-to-list 'load-path i))

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/anything")
;(require 'autodoc)
;(require 'init-anything-thierry)
;; (when (require 'dired)
;;   (require 'tv-utils))

;; Anything minimal config
;(require 'anything-config)
(autoload 'anything-command-map "anything-config.el" nil t 'keymap)
;(autoload 'anything-find-files "anything-config")
;(require 'anything-match-plugin)
;(require 'anything-complete)

;(global-set-key (kbd "C-x C-f") 'anything-find-files)
;(global-set-key (kbd "M-x") 'anything-M-x)
;(setq anything-c-find-files-show-icons t)
;(global-set-key (kbd "C-x C-b") 'anything-buffers+)

;; bookmark
;(require 'org)
;(require 'bookmark-extensions)
;(require 'addressbook-bookmark)

;; Ioccur
;(require 'ioccur)
(global-set-key (kbd "C-c o") 'ioccur)
(global-set-key (kbd "C-c C-o") 'ioccur-find-buffer-matching)
(load "~/.emacs.d/elisp-objects/ioccur-history.elc")
;; ;(setq ioccur-read-char-or-event-skip-read-key t)

;; ;(setq term-file-prefix nil)

;; ;; Yank from clipboard/primary
;; (setq x-select-enable-clipboard t)

;; (defun yank-from-primary ()
;;   "X-apps ==> Emacs."
;;   (interactive)
;;   ;; Thats work with CLIPBOARD also.
;;   ;; When copying in other apps to CLIPBOARD
;;   ;; selection is added to PRIMARY also.
;;   ;; so using PRIMARY cover the both,
;;   ;; that is PRIMARY and CLIPBOARD.
;;   (let ((primary (x-selection 'PRIMARY)))
;;     (when primary (insert primary))))
;; (global-set-key (kbd "C-c v") 'yank-from-primary)

;; ;; Auto completion
;; ;; (add-hook 'minibuffer-setup-hook
;; ;;           (lambda () (add-hook 'post-command-hook
;; ;;                                'minibuffer-completion-help nil t)))

;; Eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(with-current-buffer "*scratch*" (lisp-interaction-mode)) 
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
;(with-current-buffer "*scratch*" (turn-on-eldoc-mode))
