;;;
;;; code:

(require 'cl)
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-message t)

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/anything")
(add-to-list 'load-path "~/.emacs.d/emacs-config-laptop")

(setq default-frame-alist '((foreground-color . "Wheat")
                            (background-color . "DarkSlateGray")
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

;;; Anything minimal config
(require 'anything-config)
;(autoload 'anything-find-files "anything-config" nil t)
;(autoload 'anything-M-x "anything-config" nil t)
;(require 'anything-match-plugin)
;(require 'anything-complete)
(require 'autodoc)
(global-set-key (kbd "C-x C-f") 'anything-find-files)
(global-set-key (kbd "M-x") 'anything-M-x)
(global-set-key (kbd "C-x C-b") 'anything-buffers-list)

;; (require 'descbinds-anything)
;; (descbinds-anything-install)            ; C-h b, C-x C-h
(fset 'yes-or-no-p 'y-or-n-p)
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
