;;; Minimal-emacs.el - Minimal config for Emacs.

;;; Code:

(require 'cl)
(blink-cursor-mode -1)
(show-paren-mode 1)

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/anything")
;(require 'autodoc)
;(require 'init-anything-thierry)
;; (when (require 'dired)
;;   (require 'tv-utils))

;; Anything minimal config
;(require 'anything-config)
(autoload 'anything-find-files "anything-config" nil t)
(autoload 'anything-M-x "anything-config" nil t)


;(require 'anything-match-plugin)
;(require 'anything-complete)

;(global-set-key (kbd "C-x C-f") 'anything-find-files)
(global-set-key (kbd "M-x") 'anything-M-x)
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
