;;; Minimal-emacs.el - Minimal config for Emacs.
;;; Code:

(require 'cl)
(blink-cursor-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-message t)

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/emacs-helm")
(add-to-list 'load-path "~/elisp/emacs-async")
(add-to-list 'load-path "~/elisp/emacs-helm-extensions")
(add-to-list 'load-path "~/.emacs.d/emacs-config-laptop")
;(add-to-list 'load-path "~/elisp/DA-libs")

;; Frames setting.
;; (setq default-frame-alist '((foreground-color . "Wheat")
;;                             (background-color . "Black")
;;                             (menu-bar-lines . 0)
;;                             (tool-bar-lines . 0)
;;                             (alpha . nil)
;;                             (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;;                             (cursor-color . "red")
;;                             ;(minibuffer . nil)
;;                             ))

(setq initial-frame-alist
       '((name . "emacs-1")
         (foreground-color . "Wheat")
         (background-color . "Black")
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
         (cursor-color . "red")
         (minibuffer . nil)
         (width . 157)
         (height . 40)
         ))

(setq default-frame-alist '((foreground-color . "Wheat")
                            (background-color . "DarkSlateGray")
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (alpha . nil)
                            (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                            (cursor-color . "red")
                            (minibuffer . nil)
                            ))

(setq minibuffer-frame-alist
      '((top . -56) (left . 1)
        (width . 157) (height . 2)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (minibuffer . only)
        (background-color . "White")
        (foreground-color . "Black")
        ))

(setq minibuffer-auto-raise nil)
(setq eldoc-in-minibuffer-own-frame-p t)

(add-hook 'window-setup-hook #'(lambda ()
                               (other-window 1 t)
                               (select-frame-set-input-focus (last-nonminibuffer-frame))))

(setq special-display-buffer-names `((,(help-buffer)
                                     (minibuffer . nil)
                                     (width . 80)
                                     (height . 24)
                                     (left-fringe . 0)
                                     (border-width . 0)
                                     (menu-bar-lines . 0)
                                     (tool-bar-lines . 0)
                                     (unsplittable . t)
                                     (top . 24)
                                     (left . 450)
                                     (background-color . "LightSteelBlue")
                                     (foreground-color . "black")
                                     (alpha . nil)
                                     (fullscreen . nil))
                                     ("*Compile-Log*"
                                     (minibuffer . nil)
                                     (width . 85)
                                     (height . 24)
                                     (left-fringe . 0)
                                     (border-width . 0)
                                     (menu-bar-lines . 0)
                                     (tool-bar-lines . 0)
                                     (unsplittable . t)
                                     (top . 24)
                                     (left . 450)
                                     (background-color . "Palevioletred1")
                                     (foreground-color . "black")
                                     (alpha . nil)
                                     (fullscreen . nil))))


;; Push the mouse out of the way.
(mouse-avoidance-mode 'banish)

;; Don't-fucking-split-this-windows-horizontally 
(setq split-width-threshold nil)

;; Enable recursive buffers
(require 'mb-depth)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq completion-cycle-threshold t)

;;; Helm minimal config
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(require 'helm-descbinds)
(helm-descbinds-install)            ; C-h b, C-x C-h
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

;;; minimal-emacs.el ends here
