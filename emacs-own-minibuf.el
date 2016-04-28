;;; Minimal-emacs.el --- Minimal config for Emacs.

;; Use own frame for minibuffer.

;;; Code:

(blink-cursor-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-message t)

(add-to-list 'load-path "~/elisp/")
(add-to-list 'load-path "~/elisp/helm")
(add-to-list 'load-path "~/elisp/emacs-async")
(add-to-list 'load-path "~/elisp/helm-extensions")
(add-to-list 'load-path "~/.emacs.d/emacs-config")

;; Require with messages to debug more easily.
(defun require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
          (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))

;;; Frames setting for own minibuffer frame.
;;
(setq initial-frame-alist
       '((name . "emacs-1")
         (foreground-color . "Wheat")
         (background-color . "Black")
         (vertical-scroll-bars . nil)
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
         (cursor-color . "red")
         (minibuffer . nil)
         ;(width . 157)
         ;(height . 40)
         (fullscreen . maximized)
         ))

(setq default-frame-alist '((foreground-color . "Wheat")
                            (background-color . "DarkSlateGray")
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (alpha . nil)
                            (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                            (cursor-color . "red")
                            (minibuffer . nil)
                            ))

(setq minibuffer-frame-alist
      '((top . -40)
        (left . 1)
        (vertical-scroll-bars . nil)
        ;(width . 157)
        (height . 1)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (minibuffer . only)
        (name . "Emacs minibuffer")
        (background-color . "DarkGoldenrod")
        (foreground-color . "Black")
        ))

(setq minibuffer-prompt-properties '(read-only t face '((:foreground "ForestGreen"))))
(setq minibuffer-auto-raise nil) ; This is evil.
(setq eldoc-in-minibuffer-own-frame-p t)

(add-hook 'window-setup-hook #'(lambda ()
                                 (other-window 1 t)
                                 (select-frame-set-input-focus (last-nonminibuffer-frame))))

(add-hook 'minibuffer-exit-hook #'(lambda ()
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
(require 'init-helm-thierry)

(helm-mode 1)
(require 'helm-descbinds)
(helm-descbinds-install)            ; C-h b, C-x C-h
(fset 'yes-or-no-p 'y-or-n-p)
(require 'tv-utils)

;; Eldoc
(require 'eldoc-eval)
(eldoc-in-minibuffer-mode 1)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(with-current-buffer "*scratch*" (lisp-interaction-mode)) 
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

;;; minimal-emacs.el ends here
