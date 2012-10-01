;;;
;;; code:

(require 'cl)
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-message t)

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/emacs-async") ; required by tv-utils.
(add-to-list 'load-path "~/elisp/helm")
(add-to-list 'load-path "~/elisp/helm-extensions/")
(add-to-list 'load-path "~/.emacs.d/emacs-config-laptop")

(setq default-frame-alist '((foreground-color . "Wheat")
                            (background-color . "Black")
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (cursor-color . "red")
                            (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")))

;; Don't-fucking-split-this-windows-horizontally 
(setq split-width-threshold nil)

;; Enable recursive buffers
(require 'mb-depth)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Helm minimal config
(require 'helm-config) ; Contain all autoloads

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; (require 'descbinds-helm)
;; (descbinds-helm-install)            ; C-h b, C-x C-h


(fset 'yes-or-no-p 'y-or-n-p)

;; Tv-utils
(require 'tv-utils)


;; Ioccur
(require 'ioccur)
(global-set-key (kbd "C-c o") 'ioccur)
(global-set-key [remap occur] 'ioccur)
(global-set-key [remap isearch-forward] 'ioccur)
(global-set-key (kbd "C-c C-o") 'ioccur-find-buffer-matching)
;(load "~/.emacs.d/elisp-objects/ioccur-history.elc")

;; Eldoc
(require 'eldoc-eval)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(with-current-buffer "*scratch*" (lisp-interaction-mode)) 
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

;;; miniconf-2.el ends here
