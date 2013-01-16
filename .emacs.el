;;; .emacs.el ---

;;; Code:

(require 'cl)

;;; Annoyances section
;;
(global-set-key (kbd "<f11>") nil)
;; (add-hook 'emacs-startup-hook #'(lambda ()
;;                                   (when (get-buffer "*Compile-Log*")
;;                                     (kill-buffer "*Compile-Log*")
;;                                     (delete-other-windows))))

;; Annoyance number 1 is bidi
;; Turn OFF bidi everywhere.
(setq-default bidi-display-reordering nil)

;;; Environment
;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; calendar-date-style
(setq calendar-date-style 'european)

;; No-startup-screen
(setq inhibit-startup-message t)

;; consequent-log-file
(setq message-log-max 1000)

;; Kill emacs
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
      (save-buffers-kill-terminal)))

;; Require with messages to debug more easily.
(defun tv-require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
          (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))

(defun tv-maybe-load-ngnus (&optional force)
  (when (or force (< emacs-major-version 24))
    (setq load-path (loop for i in load-path
                          unless (string-match "gnus" i)
                          collect i))
    (add-to-list 'load-path "~/elisp/ngnus/lisp")
    (tv-require 'gnus-load "~/elisp/ngnus/lisp/gnus-load.el")
    (tv-require 'info)
    (add-to-list 'Info-directory-list "~/elisp/ngnus/texi/")
    (add-to-list 'Info-default-directory-list "~/elisp/ngnus/texi/")))
;; (tv-maybe-load-ngnus t)

(defun tv-maybe-add-org-load-path (&optional force)
  (when (or (< emacs-major-version 24) force)
    (setq load-path (loop for i in load-path
                          unless (string-match "org" i)
                          collect i))
    (dolist (lib '("~/elisp/org-active"
                   "~/elisp/org-active/lisp"))
      (add-to-list 'load-path lib))))
;; (tv-maybe-add-org-load-path t)


;;; load-paths
;; For Info paths see:
;; [EVAL] (getenv "INFOPATH")
(tv-require 'info)
(add-to-list 'Info-directory-list "/usr/local/share/info")
(add-to-list 'Info-directory-list "/usr/local/share/info-auctex")
(add-to-list 'Info-directory-list "/usr/share/info")
(add-to-list 'Info-directory-list "~/elisp/info")
(add-to-list 'Info-directory-list "~/elisp/info/eshell-doc")

(when (< emacs-major-version 24)
  (add-to-list 'load-path "/home/thierry/elisp/ngnus/lisp/gnus-fallback-lib/eieio"))

(dolist (i '("/usr/local/share/emacs/site-lisp"
             "/usr/local/share/emacs/site-lisp/auctex"
	     "~/elisp/"
             ;"~/elisp/dvc/lisp/"
	     "~/elisp/magit"
             "~/elisp/Emacs-wgrep"
             "~/elisp/auctex"
             "~/elisp/auctex/preview"
	     "~/elisp/autoconf-mode"
	     "~/elisp/bzr"
	     "~/elisp/cmake"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
             "~/elisp/w3m"
	     "~/elisp/git"
	     "~/elisp/tex-utils"
	     "~/elisp/muse/lisp"
	     "~/elisp/muse/contrib"
             "~/elisp/emms/lisp/"
	     "~/elisp/ledger/"
             "~/elisp/helm"
             "~/elisp/helm-extensions"
             "~/elisp/google-maps"
             "~/elisp/org-active/contrib/lisp" ; Contain htmlize.el
             "~/elisp/slime"
             "~/elisp/slime/contrib"
             "~/.emacs.d/"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config-laptop/"
             "~/elisp/emacs-async"
	     ))
  (add-to-list 'load-path i t)) ; Add all at end of `load-path' to avoid conflicts.


;;; autoconf-mode site-lisp configuration
(autoload 'autoconf-mode "autoconf-mode"
  "Major mode for editing autoconf files." t)
(autoload 'autotest-mode "autotest-mode"
  "Major mode for editing autotest files." t)
(add-to-list 'auto-mode-alist
	     '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist
	     '("\\.at\\'" . autotest-mode))

;;; cmake site-lisp configuration
(autoload 'cmake-mode "cmake-mode" "Major mode for editing CMake files." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;; desktop-file-utils site-lisp configuration
(add-to-list 'load-path "~/elisp/desktop-file-utils/")
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(add-to-list 'auto-mode-alist
 '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))
(add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)

;;; app-office/ledger site-lisp configuration
(autoload 'ledger-mode "ledger" "A mode for editing ledger data files." t)

;;; lua-mode site-lisp configuration
(autoload 'lua-mode "lua-mode" "Mode for editing Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(setq lua-default-application "/usr/bin/lua")

;;; emacs-wget site-lisp configuration
;;
;;
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)


;;; Require's
;;
;;
(tv-require 'usage-memo)
(tv-require 'auth-source)
(tv-require 'epa-file)
(tv-require 'init-helm-thierry)
(tv-require 'bookmark-extensions)
(tv-require 'bookmark-firefox-handler)
(tv-require 'firefox-protocol)
(tv-require 'addressbook-bookmark)
(tv-require 'org-config-thierry)
(tv-require 'muse-autoloads)
(tv-require 'muse-mode)
(tv-require 'muse-wiki)
(tv-require 'muse-html)
(tv-require 'muse-latex)
(tv-require 'muse-texinfo)
(tv-require 'muse-docbook)
(tv-require 'muse-colors)
(tv-require 'htmlize-hack)
(tv-require 'magit)
(tv-require 'magit-stgit)
;(tv-require 'dvc-init)
(tv-require 'emms-mpd-config)
(tv-require 'dired-extension)
(tv-require 'htmlize)
(tv-require 'no-word)
(tv-require 'eldoc-eval)
(tv-require 'flymake)
(tv-require 'esh-toggle)
(tv-require 'tex-site)
(tv-require 'ledger-config)
(tv-require 'slime-autoloads)
(tv-require 'slime)
(tv-require 'cl-info)
(tv-require 'ioccur)
(tv-require 'mb-depth)
(tv-require 'tv-utils)
(tv-require 'rectangle-utils)
(tv-require 'smallurl)
(tv-require 'zop-to-char)
(tv-require 'iedit)
(tv-require 'csv2org)
(tv-require 'el-expectations)
(tv-require 'el-mock)
(tv-require 'simple-call-tree)
(tv-require 'google-maps)
(tv-require 'googlecl)
(tv-require 'iterator)
(tv-require 'google-weather)
(tv-require 'org-google-weather)
(tv-require 'markdown-mode)
(tv-require 'boxquote)
(tv-require 'config-w3m)
(tv-require 'wgrep-helm)
(when (tv-require 'dired-aux)
  (tv-require 'helm-async))
(tv-require 'smtpmail-async)


;;; Global keys
;;
;;
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      'revert-buffer)
(global-set-key (kbd "C-c v")                      'yank-from-X)
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
(global-set-key (kbd "<f7> m")                     'tv-gnus)
(global-set-key (kbd "<f7> j")                     'webjump)
(global-set-key (kbd "<f7> s g")                   'search-word)
(global-set-key (kbd "<f7> s u")                   'tv-search-gmane)
(global-set-key (kbd "<f7> i")                     'erc-freenode-connect)
(global-set-key (kbd "<f7> g")                     'bitlbee)
(global-set-key (kbd "<f7> n")                     'newsticker-show-news)
(global-set-key (kbd "<f11> e c")                  'eshell-toggle-cd)
(global-set-key (kbd "<f11> e t")                  'eshell-toggle)
(global-set-key (kbd "<f11> s h")                  'shell)
(global-set-key (kbd "<f11> t")                    'tv-term)
(global-set-key (kbd "<f11> i")                    'ielm)
(global-set-key (kbd "<f11> p")                    'python-shell-switch-to-shell)
(global-set-key (kbd "C-%")                        'calculator)
(global-set-key (kbd "<f2>")                       'tv-flyspell)
(global-set-key (kbd "<f5> p s b")                 'tv-ps-print-buffer)
(global-set-key (kbd "<f5> p s r")                 'tv-ps-print-region)
(global-set-key (kbd "<f5> p b")                   'print-buffer)
(global-set-key (kbd "<f5> p r")                   'print-region)
(global-set-key (kbd "<f5> p i")                   'pr-interface)
(global-set-key (kbd "<f11> l r")                  'tv-start-slime)
(global-set-key (kbd "<f11> l e")                  'slime-scratch)
(global-set-key (kbd "<f11> l l")                  'slime-list-connections)
(global-set-key [remap occur]                      'helm-occur) ; M-s o
(global-set-key (kbd "C-s")                        'ioccur)
(global-set-key (kbd "M-s s")                      'isearch-forward)
(global-set-key (kbd "C-c C-o")                    'ioccur-find-buffer-matching)
(global-set-key (kbd "<M-down>")                   'tv-scroll-down)
(global-set-key (kbd "<M-up>")                     'tv-scroll-up)
(global-set-key (kbd "<C-M-down>")                 'tv-scroll-other-down)
(global-set-key (kbd "<C-M-up>")                   'tv-scroll-other-up)
(global-set-key (kbd "<C-prior>")                  'text-scale-decrease)
(global-set-key (kbd "<C-next>")                   'text-scale-increase)
(global-set-key (kbd "C-x C-²")                    'delete-other-windows)
(global-set-key (kbd "C-x C-&")                    'delete-window)
(global-set-key (kbd "C-x C-é")                    'split-window-vertically)
(global-set-key (kbd "C-x C-\"")                   'split-window-horizontally)
(global-set-key (kbd "C-x r v")                    'string-insert-rectangle)
(global-set-key (kbd "C-x r e")                    'extend-rectangle-to-end)
(global-set-key (kbd "C-x r h")                    'rectangle-menu)
(global-set-key (kbd "C-x r <right>")              'rectangle-insert-at-right)
(global-set-key (kbd "C-x r M-w")                  'copy-rectangle)
(global-set-key (kbd "M-z")                        'zop-to-char)
(global-set-key (kbd "<f5> g m")                   'google-maps)
(global-set-key (kbd "M-\"")                       'tv-insert-double-quote)
(global-set-key (kbd "C-M-\`")                     'tv-insert-double-backquote)
(global-set-key (kbd "M-\[")                       'tv-insert-vector)
(global-set-key (kbd "C-M-(")                      'tv-move-pair-forward)
(global-set-key (kbd "C-M-\"")                     'tv-insert-double-quote-and-close-forward)
(global-set-key (kbd "C-M-)")                      'tv-insert-pair-and-close-forward)
(global-set-key [remap save-buffers-kill-terminal] 'tv-stop-emacs) ; C-x C-c
(global-set-key (kbd "<f5> r")                     'find-file-as-root)
(global-set-key (kbd "C-c Y")                      'tv-yank-from-screen)
(global-set-key (kbd "C-c C")                      'tv-copy-for-screen)
(global-set-key [C-left]                           'screen-top)
(global-set-key [C-right]                          'screen-bottom)
(global-set-key (kbd "C-<")                        'other-window-backward)
(global-set-key (kbd "C->")                        'other-window-forward)
(global-set-key (kbd "<f11> s c")                  'go-to-scratch)
(global-set-key (kbd "C-x r a")                    'tv-append-to-register)
(global-set-key (kbd "C-c t r")                    'translate-at-point)
(global-set-key (kbd "<f5> c")                     'tv-toggle-calendar)
(global-set-key (kbd "C-h C-e")                    'tv-tail-echo-area-messages)
(global-set-key [remap kill-whole-line]            'tv-kill-whole-line)
(global-set-key (kbd "M-e")                        'tv-eval-last-sexp-at-eol)
(global-set-key (kbd "C-d")                        'tv-delete-char)
(global-set-key (kbd "C-x C-'")                    'tv-toggle-resplit-window)
(global-set-key (kbd "C-§")                        'iedit-mode-on-function)
(global-set-key (kbd "C-x C-(")                    'tv-resize-window)


;;; Themes
;;
;;
(defvar tv-theme-directory "~/.emacs.d/themes/")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv-theme-directory))

(defvar tv-current-theme 'naquadah)
;; Load my favourite theme.
(add-hook 'emacs-startup-hook #'(lambda () (load-theme tv-current-theme)))


;; libidn is not in gentoo.d. load it
;(tv-require 'idna)
;(tv-require 'punycode)

;; column-number
(column-number-mode 1)

;; desktop-save
;; (desktop-save-mode 1)
;; (setq desktop-restore-eager 5)
;; (add-to-list 'desktop-globals-to-save 'ioccur-history)
;; (add-to-list 'desktop-globals-to-save 'helm-external-command-history)
;; (add-to-list 'desktop-globals-to-save 'helm-surfraw-engines-history)
;; (add-to-list 'desktop-globals-to-save 'helm-ff-history)
;; (add-to-list 'desktop-globals-to-save 'helm-external-command-history)


;;; Usage-memo
;;
;;
;; Add memo to describe-func/variable
(umemo-initialize)
(defun umemo-electric-quit ()
  (interactive)
  (if (umemo-point-is-in-memo-area-p (point))
      (call-interactively (global-key-binding "q"))
      (and (view-mode 1) (View-quit))))
(define-key usage-memo-mode-map (kbd "q") 'umemo-electric-quit)


;;; Gnus-config
;;
;;
(setq gnus-asynchronous t)
(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)
(setq send-mail-command 'gnus-msg-mail)
(setq gnus-init-file "~/.emacs.d/.gnus.el")

(defvar tv-gnus-loaded-p nil)
(defun tv-load-gnus-init-may-be ()
  (unless tv-gnus-loaded-p
    (load gnus-init-file)
    (setq tv-gnus-loaded-p t)))

(add-hook 'message-mode-hook 'tv-load-gnus-init-may-be)
(add-hook 'gnus-before-startup-hook 'tv-load-gnus-init-may-be)

(defun quickping (host)
  "Return non--nil when host is reachable."
  (= 0 (call-process "ping" nil nil nil "-c1" "-W30" "-q" host)))

(defun tv-gnus (arg)
  (interactive "P")
  (if (or arg (not (quickping "imap.gmail.com")))
      (gnus-unplugged)
      (gnus)))

;; Use now org-keywords in gnus.
(add-hook 'message-mode-hook #'(lambda ()
				 (define-key message-mode-map (kbd "<f11> k") 'helm-org-keywords)))

(autoload 'gnus-dired-attach "gnus-dired.el")
(declare-function 'gnus-dired-attach "gnus-dired.el" (files-to-attach))
(define-key dired-mode-map (kbd "C-c C-a") 'gnus-dired-attach)

(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups 'ask-server)

;;; Authinfo
;;
;;
(if (file-exists-p "~/.authinfo.gpg")
    (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
    (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))


;;; Font lock
;;
;;
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;;; Save-minibuffer-history
;;
;;
(setq savehist-file "~/.emacs.d/history"
      history-delete-duplicates t)
(setq history-length 50) ; default is 30.
(savehist-mode 1)

;;; Recentf
;;
;;
(setq recentf-save-file "~/.emacs.d/recentf")
;; `recentf-mode' will be started by helm when needed,
;; so no need to start it here


;;; Frame and window config.
;;
;;
;; My current-font: [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:   [EVAL]: (helm 'helm-c-source-xfonts)
;; Choose a color:  [EVAL]: (helm 'helm-c-source-colors)

(setq-default frame-background-mode 'dark)
(setq initial-frame-alist '((fullscreen . maximized)))
(setq frame-auto-hide-function 'delete-frame)

(if (or (daemonp)
        (not (window-system))
        (< emacs-major-version 24))
    (setq default-frame-alist '((vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                                (cursor-color . "red")))

    (setq default-frame-alist `((foreground-color . "Wheat")
                                (background-color . "DarkSlateBlue")
                                (alpha . nil)
                                ;; New frames go in right corner.
                                (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                                (vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                                (cursor-color . "red")
                                (fullscreen . nil)
                                )))

;; Speedbar
(setq speedbar-frame-parameters
      `((minibuffer . nil)
        (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
        (width . 20)
        (fullscreen . nil) ; Not needed when fullscreen isn't set in .Xressources.
        (left . ,(- (* (window-width) 8) 160)) ; Speed-bar on right of screen.
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))

;;; Emacs transparency.
;;
;;
(when (window-system)
  (defun tv-transparency-modify (arg)
    "Increase Emacs frame transparency.
With a prefix arg decrease transparency."
    (interactive "P")
    (let* ((ini-alpha (frame-parameter nil 'alpha))
           (def-alpha (or ini-alpha 80))
           (mod-alpha (if arg
                          (min (+ def-alpha 10) 100)
                          (max (- def-alpha 10)
                               frame-alpha-lower-limit)))) ; 20
      (modify-frame-parameters nil (list (cons 'alpha mod-alpha)))
      (message "Alpha[%s]" mod-alpha)))
  (global-set-key (kbd "C-8") 'tv-transparency-modify))

;;; Special buffer display.
;;
;;
(setq special-display-buffer-names `(("*Help*"
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
                                      (background-color . "Lightsteelblue1")
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
                                      (fullscreen . nil))
                                     ("*Dict*"
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
                                      (foreground-color . "DarkGoldenrod")
                                      (alpha . nil)
                                      (fullscreen . nil))))


;; Don't split this windows horizontally
(setq split-width-threshold nil)

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)

(defun tv-resize-window ()
  (interactive)
  (assert (not (one-window-p t)) nil "Can't resize main frame!")
  (while (let ((input (read-key (propertize "Resize window (Arrow keys, split h/v |_, Ow `C-<')"
                                            'face 'minibuffer-prompt))))
           (case input
             (right
              (and (or (window-in-direction 'right)
                       (window-in-direction 'left))
                   (enlarge-window-horizontally 1)))
             (left
              (and (or (window-in-direction 'right)
                       (window-in-direction 'left))
                   (shrink-window-horizontally 1)))
             (down
              (and (or (window-in-direction 'above)
                       (window-in-direction 'below))
                   (enlarge-window 1)))
             (up
              (and (or (window-in-direction 'above)
                       (window-in-direction 'below))
                   (shrink-window 1)))
             (?| (split-window-horizontally))
             (?_ (split-window-vertically))
             (?\C-< (other-window-backward 1) t)
             (t nil)))))


;;; Banish mouse on bottom right
;;
;;
(if (and (display-mouse-p)
         (tv-require 'avoid nil t)
         (not (boundp 'mouse-avoidance-banish-position)))
    (progn
      (defcustom mouse-avoidance-banish-position '((frame-or-window . frame)
                                                   (side . right)
                                                   (side-pos . -2)
                                                   (top-or-bottom . bottom)
                                                   (top-or-bottom-pos . 1))
        "Position to which Mouse Avoidance mode `banish' moves the mouse.
An alist where keywords mean:
FRAME-OR-WINDOW: banish the mouse to corner of frame or window.
SIDE: banish the mouse on right or left corner of frame or window.
SIDE-POS: Distance from right or left edge of frame or window.
TOP-OR-BOTTOM: banish the mouse to top or bottom of frame or window.
TOP-OR-BOTTOM-POS: Distance from top or bottom edge of frame or window."
        :group   'avoid
        :type    '(alist :key-type symbol :value-type symbol)
        :options '(frame-or-window side (side-pos integer)
                   top-or-bottom (top-or-bottom-pos integer)))


      (defun mouse-avoidance-banish-destination ()
        "The position to which Mouse Avoidance mode `banish' moves the mouse.

If you want the mouse banished to a different corner set
`mouse-avoidance-banish-position' as you need."
        (let* ((fra-or-win         (assoc-default
                                    'frame-or-window
                                    mouse-avoidance-banish-position 'eq))
               (list-values        (case fra-or-win
                                     (frame (list 0 0 (frame-width) (frame-height)))
                                     (window (window-edges))))
               (alist              (loop for v in list-values
                                         for k in '(left top right bottom)
                                         collect (cons k v)))
               (side               (assoc-default
                                    'side
                                    mouse-avoidance-banish-position 'eq))
               (side-dist          (assoc-default
                                    'side-pos
                                    mouse-avoidance-banish-position 'eq))
               (top-or-bottom      (assoc-default
                                    'top-or-bottom
                                    mouse-avoidance-banish-position 'eq))
               (top-or-bottom-dist (assoc-default
                                    'top-or-bottom-pos
                                    mouse-avoidance-banish-position 'eq))
               (side-fn            (case side
                                     (left '+)
                                     (right '-)))
               (top-or-bottom-fn   (case top-or-bottom
                                     (top '+)
                                     (bottom '-))))
          (cons (funcall side-fn                        ; -/+
                         (assoc-default side alist 'eq) ; right or left
                         side-dist)       ; distance from side
                (funcall top-or-bottom-fn ; -/+
                         (assoc-default top-or-bottom alist 'eq) ; top/bottom
                         top-or-bottom-dist)))) ; distance from top/bottom
      (mouse-avoidance-mode 'banish))
    ;; Emacs-24.1* already have this feature.
    (setq mouse-avoidance-banish-position '((frame-or-window . frame)
                                            (side . right)
                                            (side-pos . -2)
                                            (top-or-bottom . bottom)
                                            (top-or-bottom-pos . 1))))
(mouse-avoidance-mode 1)


;;; Bookmarks
;;
;;
(setq bookmark-bmenu-toggle-filenames nil)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(setq bookmark-automatically-show-annotations nil)
(add-to-list 'org-agenda-files bmkext-org-annotation-directory)
(setq bmkext-external-browse-url-function 'browse-url-firefox) ; 'browse-url-uzbl
(setq bmkext-jump-w3m-defaut-method 'external) ; Set to 'external to use external browser, w3m for w3m.

(defun tv-pp-bookmark-alist ()
  "Quickly print `bookmark-alist'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pp-bookmark-alist*"))
  (erase-buffer)
  (dolist (i bookmark-alist)
    (pp i (current-buffer))))

;;; Muse
;;
;;
(add-hook 'find-file-hooks 'muse-mode-maybe)
(setq muse-wiki-allow-nonexistent-wikiword t)

;;; Browse url
;;
;;
;(setq browse-url-browser-function 'browse-url-firefox)

;;; Erc config
;;
;;
(defun erc-freenode-connect ()
  (interactive)
  (let ((erc-auth
         (auth-source-user-or-password  '("login" "password")
                                        "irc.freenode.net:6667"
                                        "erc")))
    (erc :server   "irc.freenode.net"
         :port     "6667"
         :nick     (car erc-auth)
         :password (cadr erc-auth))))

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     ;"#emacs"
                                     ;"#gentoofr"
                                     ;"#gentoo-lisp")))
                                     ;"#stumpwm"
                                     "#uzbl")))

;; Bitlbee
(defun bitlbee (server)
  "Connect to a Bitlbee server.
Actually i am registered on im.uk.bitlbee for talk.google.com.
Localhost is used for yahoo messenger.
I will have to register <password> on others
and also to add an account with
account add <protocol> moi@mail.com password."
  (interactive (list (helm-comp-read "Choose a Bitlbee Server: "
                                         '(("LocalServer yahoo+gmail" . "localhost:6667")
                                           ("GoogleTalk on Pub server" . "im.uk.bitlbee.org:6667")
                                           "im.bitlbee.org:6667"
                                           "testing.bitlbee.org:6667"
                                           "im.rootdir.de:6668"
                                           "irc.net:6667"
                                           "bitlbee1.asnetinc.net:6667"
                                           "bitlbee.hensema.net:6667"
                                           "bitlbee.extreme-players.de:6667"
                                           "irc2im.picasa.hu:6667"
                                           "im.sixxs.net:6667"
                                           "im.kernel-oops.de:7777"
                                           "im.rondom.org:7070"
                                           "im.okkernoot.net:6667"
                                           "im.codemonkey.be:6667"
                                           "im.se.bitlbee.org:6667")
                                         :must-match t
                                         :name "Bitlbee Servers")))

  (let ((bitlb-auth (auth-source-user-or-password
                     '("login" "password")
                     "im.uk.bitlbee.org:6667"
                     "bitlbee"))
        server-sans-port port)
    (when (string-match "\\(.*\\)\\(:\\)\\([0-9]+\\)" server)
      (setq server-sans-port (match-string 1 server))
      (setq port (match-string 3 server)))
    (erc :server   server-sans-port
         :port     port
         :nick     (car bitlb-auth)
         :password (cadr bitlb-auth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For memo the IRC commands for BITLBEE:
;;
;; * account - IM-account list maintenance
;; * add - Add a buddy to your contact list
;; * info - Request user information
;; * remove - Remove a buddy from your contact list
;; * block - Block someone
;; * allow - Unblock someone
;; * set - Miscellaneous settings
;; * help - BitlBee help system
;; * save - Save your account data
;; * rename - Rename (renick) a buddy
;; * yes - Accept a request
;; * no - Deny a request
;; * qlist - List all the unanswered questions root asked
;; * register - Register yourself
;; * identify - Identify yourself with your password
;; * drop - Drop your account
;; * blist - List all the buddies in your contact list
;; * nick - Change friendly name, nick
;; * join_chat - Join a named groupchat/conference room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; Don't use RET to send line
;; (define-key erc-mode-map (kbd "RET") nil)
;; (define-key erc-mode-map (kbd "C-c RET") 'erc-send-current-line)

;; confirm-quit-emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; Ediff-config
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Dired
;; use the directory in the other windows as default target
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t) ; Emacs vcs only
(define-key dired-mode-map (kbd "C-k")   #'(lambda () (interactive) (dired-do-delete 1)))
(define-key dired-mode-map (kbd "b")     #'(lambda () (interactive) (dired-do-byte-compile 1)))
(define-key dired-mode-map (kbd "C-c c") 'csv2org-dired)
(define-key dired-mode-map (kbd "C-t !") #'(lambda ()
                                             (interactive)
                                             (image-dired-show-all-from-dir default-directory)))
(define-key dired-mode-map (kbd ": a") 'epa-sign-to-armored)


;; Backup when overwriting from dired (nil, always, ask).
;(setq dired-backup-overwrite 'always)
(setq dired-backup-overwrite nil)

;; Search only in filenames.
(setq dired-isearch-filenames 'dwim)

(setq dired-listing-switches (purecopy "-alh"))

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Affiche-l'heure-au-format-24h
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq display-time-use-mail-icon t)

;; Limite-max-lisp
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)


;;; emacs-backup-config
;;
(setq backup-directory-alist '(("" . "/home/thierry/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-file-name-transforms nil)


(setq case-fold-search t)

;; Mark-ring
(setq mark-ring-max 50)
(setq global-mark-ring-max 32)

;; show-paren-mode
(show-paren-mode 1)
(setq show-paren-ring-bell-on-mismatch t)

;; Start-emacs-server
(add-hook 'after-init-hook #'(lambda ()
                               (unless (daemonp)
                                 (server-start)
                                 (setq server-raise-frame t))))


;; Path-to-abbrev-file
(setq abbrev-file-name "/home/thierry/.emacs.d/.abbrev_defs")

;; Copy-and-cut-to-x-clipboard
;; Don't add to emacs kill-ring use yank-from-clipboard instead (C-c v)
(setq interprogram-paste-function nil)

;; This enable pushing x-selection to emacs kill-ring
;; X-apps ==> emacs kill-ring
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; (setq x-select-enable-primary t)
(setq select-active-regions t)

;; Emacs kill-ring ==> X-apps
(setq x-select-enable-clipboard t)
(when (boundp 'x-select-enable-clipboard-manager)
  (setq x-select-enable-clipboard-manager nil))

(defun yank-from-X ()
  "Yank from X-apps to Emacs."
  (interactive)
  (let ((primary (x-get-selection 'PRIMARY))
        (clip    (x-get-selection 'CLIPBOARD)))
    (cond (primary (insert primary))
          (clip    (insert clip))
          (t       (yank)))))


;; Whitespace-mode
(when (tv-require 'whitespace)
  (add-to-list 'whitespace-style 'lines-tail)
  (setq whitespace-line-column 80))

;; antiword
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))


;; Elisp

;; Eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)

(when (tv-require 'eldoc)
  (set-face-attribute 'eldoc-highlight-function-argument nil :underline "red"))

;; Tooltip face
(set-face-attribute 'tooltip nil
                    :foreground "black"
                    :background "NavajoWhite"
                    :family "unknown-DejaVu Sans Mono-bold-normal-normal"
		    :underline t)

(defadvice edebug-eval-expression (around with-eldoc activate)
  "This advice enable eldoc support."
  (interactive (list (with-eldoc-in-minibuffer
                       (read-from-minibuffer
                        "Eval: " nil read-expression-map t
                        'read-expression-history))))
  ad-do-it)

;; Indent-when-newline (RET) in all elisp modes
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") 'newline-and-indent)

;; eval-region
(define-key lisp-interaction-mode-map (kbd "C-M-!") 'tv-eval-region)
(define-key emacs-lisp-mode-map (kbd "C-M-!") 'tv-eval-region)

;; byte-compile-file
(define-key emacs-lisp-mode-map (kbd "C-c C-c b") 'byte-compile-file)

;; Next page
(define-key emacs-lisp-mode-map (kbd "<next>") 'forward-page)
(define-key emacs-lisp-mode-map (kbd "<prior>") 'backward-page)

;; Indent-only-with-spaces
(setq-default indent-tabs-mode nil)


;;; Python config
;;
;;
(tv-require 'python)
(tv-require 'helm-ipython)
(define-key python-mode-map (kbd "C-c C-b") 'helm-browse-code)
(define-key python-mode-map (kbd "<M-tab>") 'helm-ipython-complete)
(define-key python-mode-map (kbd "C-c C-i") 'helm-ipython-import-modules-from-buffer)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "import rlcompleter2
rlcompleter2.setup()
from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Entete-py
(defun tv-insert-python-header ()
  "insert python header at point"
  (interactive)
  (insert "#!/usr/bin/env python\n"
          "# -*- coding: utf-8 -*-\n\n"
          "## Title: \n"
          "## Description: \n"
          "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
          "## Commentary:\n\n"))


;;; Shell config
;;

;; Prompt shell read only
(setq comint-prompt-read-only t)

;; Newline and indent in `sh-mode'.
(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))


;;; Eshell-config
;;
;;
;; Eshell-prompt
(setq eshell-prompt-function
      #'(lambda nil
          (concat
           (getenv "USER")
           "@"
           (system-name)
           ":"
           (abbreviate-file-name (eshell/pwd))
           (if (= (user-uid) 0) " # " " $ "))))

;; Compatibility 24.2/24.3
(unless (fboundp 'eshell-pcomplete)
  (defun eshell-pcomplete ()
    "Eshell wrapper for `pcomplete'."
    (interactive)
    (condition-case nil
        (pcomplete)
      (text-read-only (completion-at-point))))) ; Workaround for bug#12838.


(add-hook 'eshell-mode-hook #'(lambda ()
                                ;; Eshell smart initialize
                                (require 'em-smart)
                                (setq eshell-where-to-jump 'begin)
                                (setq eshell-review-quick-commands nil)
                                (setq eshell-smart-space-goes-to-end t)
                                (eshell-smart-initialize)
                                ;; helm completion with pcomplete
                                (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                ;; helm lisp completion
                                (define-key eshell-mode-map [remap lisp-complete-symbol] 'helm-lisp-completion-at-point)
                                ;; helm completion on eshell history.
                                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                ;; Eshell prompt
                                (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")))
                                
;; Eshell history size
(setq eshell-history-size 1000) ; Same as env var HISTSIZE.

;; Eshell-banner
(setq eshell-banner-message (format "%s %s\nwith Emacs %s on %s"
                                    (propertize
                                     "Eshell session started on"
                                     'face '((:foreground "Goldenrod")))
                                    (propertize
                                     (format-time-string "%c")
                                     'face '((:foreground "magenta")))
                                    (propertize emacs-version
                                                'face '((:foreground "magenta")))
                                    (propertize
                                     (with-temp-buffer
                                       (call-process "uname" nil t nil "-r")
                                       (buffer-string))
                                     'face '((:foreground "magenta")))))

;; Eshell-et-ansi-color
(ignore-errors
  (dolist (i (list 'eshell-handle-ansi-color
                   'eshell-handle-control-codes
                   'eshell-watch-for-password-prompt))
    (add-to-list 'eshell-output-filter-functions i)))

;; Eshell-save-history-on-exit
;; Possible values: t (always save), 'never, 'ask (default)
(setq eshell-save-history-on-exit t)

;; Eshell-directory
(setq eshell-directory-name "/home/thierry/.emacs.d/eshell/")

;; Eshell-visual
(setq eshell-term-name "eterm-color")
(when (tv-require 'em-term)
  (dolist (i '("kop" "ledger" "htop" "ipython"))
    (add-to-list 'eshell-visual-commands i)))

;;; pcomplete Completion functions on specific commands (Find, hg etc...)
;;
;;
(tv-require 'pcomplete-extension)

;; Finally load eshell on startup.
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))

;; Term-et-ansi-term
(defun tv-term ()
  (interactive)
  (ansi-term "/bin/bash"))

;; Kill buffer after C-d in ansi-term.
(defadvice term-sentinel (after kill-buffer () activate)
  (kill-buffer))

(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to subprocess.
Sends an EOF only if point is at the end of the buffer and there is no input."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
	(progn (comint-send-eof) (kill-buffer))
      (delete-char arg))))

;; Entete-Bash
(defun tv-insert-bash-header ()
  "insert bash header at point"
  (interactive)
  (insert "#!/bin/bash\n"
          "## Title:\n"
          "## Description: \n"
          "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
          "## Commentary:\n\n"))

;;; flyspell-aspell
;;
;;
(setq-default ispell-program-name "aspell")
(setq ispell-local-dictionary "francais")

(defun tv-flyspell (arg)
  "Toggle `flyspell-mode'.
With prefix arg always start and let me choose dictionary."
  (interactive "P")
  (if arg
      (let ((dic (helm-comp-read
                  "Dictionnaire: "
                  '("francais" "english"))))
        (unless flyspell-mode (flyspell-mode 1))
        (ispell-change-dictionary dic)
        (flyspell-delete-all-overlays))
      (call-interactively 'flyspell-mode)))

;;; Woman/man
;;
;;
(setq woman-use-own-frame nil)
(setq Man-notify-method 'pushy)
(defface man-args-face '((t (:foreground "Magenta" :underline t)))
  "*Face used in man page to show arguments and sections."
  :group 'man)

;;; Printing config
;;
;(setq helm-ff-printer-list (helm-ff-find-printers))
(setq lpr-command "gtklp")
;(setq lpr-switches '("-P"))
(setq printer-name "EpsonStylus")
(setq-default ps-print-header nil)
(setq ps-font-size   '(10 . 11.5))
(setq ps-font-family 'Courier)

(defun tv-ps-print-buffer ()
  (interactive)
  (if current-prefix-arg
      (ps-print-buffer-with-faces)
      (ps-print-buffer)))

(defun tv-ps-print-region (beg end)
  (interactive "r")
  (if current-prefix-arg
      (ps-print-region-with-faces beg end)
      (ps-print-region beg end)))

;; (load "a2ps-print")
;; (setq a2ps-switches '("-1Rf12" "-Z"))
;; (global-set-key (kbd "<f5> p a b") 'a2ps-buffer)
;; (global-set-key (kbd "<f5> p a r") 'a2ps-region)

;; auto-compression-mode
(auto-compression-mode 1)

;;; Auctex/Latex config
;;
;;
(load "preview-latex.el" nil t t)

;; detect needed steps after rebuild
(setq TeX-parse-self t)

(tv-require 'xdvi-search)

;; (setq TeX-view-program-selection '(((output-dvi style-pstricks)
;;                                     "dvips and gv")
;;                                    (output-dvi "xdvi")
;;                                    (output-pdf "Evince")
;;                                    (output-html "xdg-open")))

;; To turn on RefTeX Minor Mode for all LaTeX files,
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;; Replace AUCTeX functions
(setq reftex-plug-into-AUCTeX t)

(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

;; Parametres latex divers
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'TeX-language-fr-hook
          #'(lambda () (ispell-change-dictionary "french")))
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq TeX-PDF-mode t)

;; Insertion-d'un-squelette-latex

(defun tv-insert-skel-latex-doc ()
  "Insert a LaTeX skeleton in an empty file."
  (interactive)
  (insert "\\documentclass[a4paper,11pt]{article}\n"
       "\\usepackage[french]{babel}\n"
       "\\usepackage[utf8]{inputenc}\n"
       "\\usepackage{textcomp}% Allow to use euro sign\n"
       "\n"
       "%\\usepackage[pdftex=true,
           %hyperindex=true,
           %colorlinks=true]{hyperref}"
       "\n"
       "%\\usepackage[hypertex=true,
           %hyperindex=true,
           %colorlinks=false]{hyperref}"
       "\n"
       "%\\usepackage{url}\n"
       "%\\usepackage{natbib}\n"
       "%\\usepackage{setspace}\n"
       "%\\usepackage{qtree}\n"
       "%\\usepackage{booktabs}\n"
       "\n"
       "\n"
       "\\begin{document}\n"
       "%\n"
       "%\\begin{titlepage}\n"
       "\\title{}\n"
       "\\date{\\today}\n"
       "\\author{}\n"
       "\\maketitle\n"
       "%\\tableofcontents\n"
       "%\\end{titlepage}\n"
       "\n"
       "\n"
       "\\end{document}\n")
  (goto-char (point-min))
  (when (re-search-forward "[\\]title")
    (beginning-of-line)
    (forward-char 7)))

;; Insertion-d'un-squelette-latex-de-lettre

(defun tv-insert-skel-latex-letter ()
  "Insert a latex skeleton letter in an empty file"
  (interactive)
  (insert "\\documentclass[a4paper,11pt]{letter}\n"
       "\\usepackage[french]{babel}\n"
       "\\usepackage[utf8]{inputenc}\n"
       "\\usepackage{textcomp}% Allow to use euro sign\n"
       "\\begin{document}\n"
       "%\\name{}% Nom de l'expéditeur\n"
       "\\address{Thierry Volpiatto \\\\ 141 Carraire des Lecques \\\\ 83270 St Cyr sur mer}% Adresse de l'expéditeur\n"
       "\\signature{Thierry Volpiatto}% Signature de l'expéditeur\n"
       "\\date{\\today}\n"
       "\n"
       "\n"
       "\\begin{letter}{}% Nom du destinataire\n"
       "\\opening{}% Formule de salutation : cher monsieur, etc.\n"
       "\n"
       "% Corps de la lettre\n"
       "\n"
       "\\closing{}% Formule de politesse : veuillez agréer, etc.\n"
       "\\ps{PS:}{}% Post-scriptum\n"
       "\\cc{}% Autres destinataires de la lettre\n"
       "\\encl{}% Pièces jointes\n"
       "\\end{letter}\n"
       "\\end{document}\n")
  (goto-char (point-min))
  (when
      (re-search-forward "[\\]begin\{letter\}")
    (beginning-of-line)
    (forward-char 15)))

;; newsticker-config
(setq newsticker-frontend 'newsticker-plainview)
(setq newsticker-retrieval-method 'extern)
(setq newsticker-show-descriptions-of-new-items nil)

(defun newsticker-quit-and-stop ()
  (interactive)
  (with-current-buffer "*newsticker*"
    (newsticker-close-buffer)
    (newsticker-stop)))

(defadvice newsticker-next-feed (around recenter activate)
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed))
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (prog1 (point) (recenter)))

(defadvice newsticker-mark-all-items-at-point-as-read-and-redraw (after recenter activate)
  (recenter))

(defun newsticker--next-line ()
  (interactive)
  (let ((line-move-visual t))
    (next-line)))

(when (tv-require 'newsticker)
  (define-key newsticker-mode-map (kbd "Q") 'newsticker-quit-and-stop)
  (define-key newsticker-mode-map (kbd "b") 'newsticker-previous-feed))

(add-hook 'newsticker-mode-hook #'(lambda () (setq bidi-display-reordering nil)))

;;; Tramp-config
;;
;;
;(tv-require 'tramp)
;(setq tramp-default-method "ssh") ; methode par defaut
;(setq tramp-verbose 6)
;(setq helm-tramp-verbose 6)


;; No messages
(setq tramp-message-show-message nil)

;; Allow connecting as root on all remote Linux machines except this one.
;; Use e.g /sudo:host:/path
(add-to-list 'tramp-default-proxies-alist
             '("\\`thievol\\'" "\\`root\\'" "/ssh:%h:"))

(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

;; Connect to my freebox as 'freebox' user.
(add-to-list 'tramp-default-user-alist
             '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))

;;; Ange-ftp
;;
;;
(setq ange-ftp-try-passive-mode t)
(setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on")))


;; Mode-lecture-photo-auto
(auto-image-file-mode 1)


;;; Slime config
;;
;;
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-backend "/home/thierry/elisp/slime/swank-loader.lisp")
(slime-setup '(slime-fancy
               slime-asdf
               slime-tramp
               slime-banner
               slime-autodoc
               slime-xref-browser))
(setq slime-net-coding-system 'utf-8-unix
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;; Fix indentation in CL loop.
(setq lisp-simple-loop-indentation 1)
(setq lisp-loop-keyword-indentation 6)
(setq lisp-loop-forms-indentation 6)

;; Fix indentation in cl-flet and cl-labels
(eval-after-load "cl-indent.el"
  (let ((l '((flet ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
             (cl-flet* . flet)
             (labels . flet)
             (cl-flet . flet)
             (cl-labels . flet)
             (cl-macrolet . flet))))
    (dolist (el l)
      (put (car el) 'common-lisp-indent-function
           (if (symbolp (cdr el))
               (get (cdr el) 'common-lisp-indent-function)
               (car (cdr el)))))))

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(cl-flet[*]?\\|cl-labels\\|cl-macrolet\\)\\>" 1 font-lock-keyword-face)
     ("(\\<\\(cl-loop\\|cl-dolist\\)\\>" 1 font-lock-keyword-face))))

(add-hook 'slime-load-hook #'(lambda () (tv-require 'slime-tramp)))

(defun tv-slime-port (process)
  (let ((slime-port (or (process-id process)
                        (process-contact process))))
    (setq slime-port (cadr slime-port))
    slime-port))

(defun tv-get-slime-buffer-list ()
  (let ((buf-list nil))
    (dolist (b (buffer-list))
      (when (string-match "*slime-repl sbcl*" (buffer-name b))
        (push (buffer-name b) buf-list)))
    buf-list))

(defun tv-start-slime ()
  (interactive)
  (if (slime-connected-p)
      (if (< (length slime-net-processes) 2)
          (slime)
          (slime-list-connections))
      (slime)))

;; common-lisp-info
(setq Info-additional-directory-list '("~/elisp/info/gcl-info/"))

;; Save-slime-scratch-buffer
(setq slime-scratch-file "~/.emacs.d/slime-scratch.lisp")



;; ioccur
(define-key org-mode-map (kbd "C-c C-o") 'ioccur-find-buffer-matching)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil)          ; C-x n n
(put 'scroll-left 'disabled nil)               ; C-x > or <
(put 'downcase-region 'disabled nil)           ; C-x C-l
(put 'upcase-region 'disabled nil)             ; C-x C-u
(put 'set-goal-column 'disabled nil)           ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired

;; setup-minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Scroll-down-Scroll-up
(defun tv-scroll-down ()
  (interactive)
  (scroll-down -1))
(define-key org-mode-map (kbd "<M-down>") 'tv-scroll-down)

(defun tv-scroll-up ()
  (interactive)
  (scroll-down 1))
(define-key org-mode-map (kbd "<M-up>") 'tv-scroll-up)

;; Enable-scroll-other-window-globally
(defun tv-scroll-other-down ()
  (interactive)
  (scroll-other-window 1))
(define-key org-mode-map (kbd "<C-M-down>") 'tv-scroll-other-down)

(defun tv-scroll-other-up ()
  (interactive)
  (scroll-other-window -1))
(define-key org-mode-map (kbd "<C-M-up>") 'tv-scroll-other-up)

;; xmodmap
(load "xmodmap")

;; sql-mode
(setq sql-sqlite-program "sqlite3")

;; sqlite-dump
(autoload 'sqlite-dump "sqlite-dump" nil t)
(modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))


;; align-let
(autoload 'align-let-keybinding "align-let" nil t)
(add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
(add-hook 'lisp-interaction-mode-hook 'align-let-keybinding)
(add-hook 'lisp-mode-hook 'align-let-keybinding)


;;; line-move-visual.
;;
;; next-line go to real next line when set to nil.
;; When nil scrolling performances are better in files with long lines.
;; When non--nil move to next visual line. (slow)
(setq line-move-visual nil)
;(add-hook 'html-mode-hook 'visual-line-mode)

;;; Rst-mode
;;
(add-hook 'rst-mode-hook 'auto-fill-mode)

;;; Undo-tree
;;
;; Set `undo-outer-limit' to high value to avoid messages on long output.
;(setq undo-outer-limit 20000000)
;; undo-limit
;(setq undo-limit 100000)
(tv-require 'undo-tree)
(global-undo-tree-mode)


;; Calendar-and-diary
(setq holiday-bahai-holidays nil)
(setq holiday-solar-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-oriental-holidays nil)
;; (setq calendar-christian-all-holidays-flag t)

(setq diary-display-function 'diary-fancy-display)
;(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'initial-calendar-window-hook 'mark-diary-entries)
(setq mark-holidays-in-calendar t)
(setq diary-number-of-entries 4)

(defface diary-special-event '((t (:foreground "green")))
  "*Face used for special event in diary."  :group 'diary)

;; calendar-date-style is set [HERE]:
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Dimanche" "Lundi" "Mardi"
       "Mercredi" "Jeudi" "Vendredi" "Samedi"]
      calendar-month-name-array
      ["Janvier" "Février" "Mars" "Avril"
       "Mai" "Juin" "Juillet" "Août" "Septembre"
       "Octobre" "Novembre" "Décembre"])

(defvar holiday-french-holidays nil
  "French holidays")

(setq holiday-french-holidays
      `((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Victoire")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 11 "Armistice")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 12 25 "Noël")
        (holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc 50 "Pentecôte")
        (holiday-float 3 0 -1 "Heure d'été")
        (holiday-float 10 0 -1 "Heure d'hiver")))

(setq calendar-holidays holiday-french-holidays)

;; Checkdoc
(autoload 'checkdoc-batch       "checkdoc-batch" nil t)
(autoload 'checkdoc-batch-files "checkdoc-batch" nil t)

;; Mode-line
(set-face-attribute 'mode-line-emphasis nil :foreground "red")

;; Google-Apps
(setq google-maps-static-default-zoom 10)

;; Toggle-show-trailing-whitespace
(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;;; World-time
;;
;;
(add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
(add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
(add-to-list 'display-time-world-list '("America/Denver" "Denver"))
(add-to-list 'display-time-world-list '("America/Los_Angeles" "Los_Angeles/Seattle"))
(add-to-list 'display-time-world-list '("America/Denver" "Moab"))
(add-to-list 'display-time-world-list '("America/Vancouver" "Vancouver"))
(add-to-list 'display-time-world-list '("America/Montreal" "Montreal"))
(add-to-list 'display-time-world-list '("America/New_York" "Ottawa"))


;;; Trash
;;
;(setq delete-by-moving-to-trash t)

;; Minibuffers completion
(setq completion-cycle-threshold t) ; always cycle, no completion buffers.

;; Remove undesired hooks.
;(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'find-file-hook 'tla-find-file-hook)

;;; vc
;;
;;
;; Possible values: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
;(setq vc-handled-backends '(RCS CVS SVN Hg Git Bzr))
(setq vc-handled-backends '(RCS CVS SVN Hg Git))

;;; Temporary Bugfixes until fixed in trunk.
;;
(when (or (version< emacs-version "24.2")
          (version= emacs-version "24.2"))
  (defun y-or-n-p (prompt)
    "Ask user a \"y or n\" question.  Return t if answer is \"y\".
PROMPT is the string to display to ask the question.  It should
end in a space; `y-or-n-p' adds \"(y or n) \" to it.

No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no.  \(Actually, it uses
the bindings in `query-replace-map'; see the documentation of that variable
for more information.  In this case, the useful bindings are `act', `skip',
`recenter', and `quit'.\)

Under a windowing system a dialog box will be used if `last-nonmenu-event'
is nil and `use-dialog-box' is non-nil."
    ;; ¡Beware! when I tried to edebug this code, Emacs got into a weird state
    ;; where all the keys were unbound (i.e. it somehow got triggered
    ;; within read-key, apparently).  I had to kill it.
    (let ((answer 'recenter))
      (cond
        (noninteractive
         (setq prompt (concat prompt
                              (if (eq ?\s (aref prompt (1- (length prompt))))
                                  "" " ")
                              "(y or n) "))
         (let ((temp-prompt prompt))
           (while (not (memq answer '(act skip)))
             (let ((str (read-string temp-prompt)))
               (cond ((member str '("y" "Y")) (setq answer 'act))
                     ((member str '("n" "N")) (setq answer 'skip))
                     (t (setq temp-prompt (concat "Please answer y or n.  "
                                                  prompt))))))))
        ((and (display-popup-menus-p)
              (listp last-nonmenu-event)
              use-dialog-box)
         (setq answer
               (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
        (t
         (setq prompt (concat prompt
                              (if (eq ?\s (aref prompt (1- (length prompt))))
                                  "" " ")
                              "(y or n) "))
         (while
             (let* ((key
                     (let ((cursor-in-echo-area t))
                       (when minibuffer-auto-raise
                         (raise-frame (window-frame (minibuffer-window))))
                       (read-key (propertize (if (or (eq answer 'recenter)
                                                     (eq answer 'scroll))
                                                 prompt
                                                 (concat "Please answer y or n.  "
                                                         prompt))
                                             'face 'minibuffer-prompt)))))
               (setq answer (lookup-key query-replace-map (vector key) t))
               (cond
                 ((memq answer '(skip act)) nil)
                 ((eq answer 'recenter) (recenter) t)
                 ((memq answer '(exit-prefix quit)) (signal 'quit nil) t)
                 ((eq key ?\C-v)
                  (setq answer 'scroll)
                  (condition-case nil (scroll-up 1) (error nil)) t)
                 ((eq key ?\M-v)
                  (setq answer 'scroll)
                  (condition-case nil (scroll-down 1) (error nil)) t)
                 (t t)))
           (ding)
           (discard-input))))
      (let ((ret (eq answer 'act)))
        (unless noninteractive
          ;; FIXME this prints one too many spaces, since prompt
          ;; already ends in a space.  Eg "... (y or n)  y".
          (message "%s %s" prompt (if ret "y" "n")))
        ret))))


;; ----Empty---

;;; winner-mode config
;;
;;
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*dvc-error*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"
                              ))

(when (tv-require 'winner)
  (defvar winner-boring-buffers-regexp
    "\*[hH]elm.*\\|\*xhg.*\\|\*xgit.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (loop for buf in (mapcar 'cdr (cdr conf))
               for pos = (winner-get-point buf nil)
               if (and pos (not (memq buf buffers)))
               do (push buf buffers)
               collect pos)))
      (winner-set-conf (car conf))
      (let (xwins)                      ; to be deleted

        ;; Restore points
        (dolist (win (winner-sorted-window-list))
          (unless (and (pop alive)
                       (setf (window-point win)
                             (winner-get-point (window-buffer win) win))
                       (not (or (member (buffer-name (window-buffer win))
                                        winner-boring-buffers)
                                (string-match winner-boring-buffers-regexp
                                              (buffer-name (window-buffer win))))))
            (push win xwins)))          ; delete this window

        ;; Restore marks
        (letf (((current-buffer)))
          (loop for buf in buffers
                for entry = (cadr (assq buf winner-point-alist))
                for win-ac-reg = (winner-active-region)
                do (progn (set-buffer buf)
                       (set-mark (car entry))
                       (setf win-ac-reg (cdr entry)))))
        ;; Delete windows, whose buffers are dead or boring.
        ;; Return t if this is still a possible configuration.
        (or (null xwins)
            (progn
              (mapc 'delete-window (cdr xwins)) ; delete all but one
              (unless (one-window-p t)
                (delete-window (car xwins))
                t))))))

  (defalias 'winner-set 'winner-set1))
(winner-mode 1)

;;; google weather
;;
;;
(setq org-google-weather-format "%L: %i %c, [%l,%h] %s")

;;; Battery
;;
;;
;; (ignore-errors
;;   (setq battery-mode-line-format "[Bat:%b%p%%,%L]")
;;   (display-battery-mode 1))

;;; Display time in mode-line
;;
;;
(setq display-time-string-forms
      '(;; date
        (if (and (not display-time-format) display-time-day-and-date)
            (format-time-string "[%a %e %b " now)
            "")
        ;; time
        (concat
         (propertize
          (format-time-string (or display-time-format
                                  (if display-time-24hr-format " %H:%M" " %-I:%M%p"))
                              now)
         'face '((:foreground "green"))
         'help-echo (format-time-string " %a %b %e, %Y" now)) "]")
        ;; cpu load average
        ;; (if (and load (not (string= load "")))
        ;;     (format "cpu:%s" load) "")
        ""
        ;; mail
        ""))

(defun cat-command ()
  "A command for cats."
  (interactive)
  (tv-require 'animate)
  (let ((mouse "
           ___(00)
        ~~/_____^'>
          /    \\")
        (h-pos (floor (/ (window-height) 2)))
        (contents (buffer-string))
        (mouse-buffer (generate-new-buffer "*mouse*")))
    (save-excursion
      (switch-to-buffer mouse-buffer)
      (insert contents)
      (setq truncate-lines t)
      (animate-string mouse h-pos 0)
      (dotimes (_ (window-width))
        (sit-for 0.01)
        (dotimes (n 3)
          (helm-goto-line (+ h-pos n 2) t)
          (move-to-column 0)
          (insert " "))))
    (kill-buffer mouse-buffer)))

;;; markdown-mode
;;
;;
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdpp$" . markdown-mode))

;;; Semantic
;;
;;
;; (tv-require 'semantic)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-show-unmatched-syntax-mode t)
;; (semantic-mode 1)

;;; opendns
;;
;;
(defun opendns-status ()
  "Check if I am using opendns or not."
  (interactive)
  (with-current-buffer (url-retrieve-synchronously "http://www.opendns.com/welcome")
    (goto-char (point-min))
    (when (or (search-forward "You aren't using OpenDNS yet" nil t)
              (search-forward "Your Internet is safer, faster, and smarter<br />because you're using OpenDNS" nil t))
      (message "%s" (replace-regexp-in-string "<br />" " " (match-string 0))))))

;;; Webjump sites
;;
;;
(setq webjump-sites
      '(("pythonlib" .  "http://docs.python.org/lib/genindex.html")
        ("pythondoc" . "http://docs.python.org/index.html")
        ("tkinter-tuto" . "http://infohost.nmt.edu/tcc/help/pubs/tkinter/")
        ("pygtk-tuto" . "http://www.pygtk.org/pygtk2tutorial/index.html")
        ("emacsmanfr" . "http://www.linux-france.org/article/appli/emacs/manuel/html/index.html")
        ("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/SiteMap")
        ("wikipedia" . "http://fr.wikipedia.org/wiki/Accueil")
        ("elispcode" . "http://www.emacswiki.org/cgi-bin/wiki/Cat%c3%a9gorieCode")
        ("elisp-reference-manual" . "http://www.gnu.org/software/emacs/elisp/html_node/index.html")
        ))

;;; Ffap
;;
;;
;; Tramp/ange behave badly in 99.9% of the time for ftp, disable.
(setq ffap-url-unwrap-remote (remove "ftp" ffap-url-unwrap-remote))

;;; Ido virtual buffers
;;
;;
(setq ido-use-virtual-buffers t)

;;; Deactivate mouse scrolling
;;
(mouse-wheel-mode -1)

;;; Printing variables
;;
;;
;; (setq print-gensym t
;;       print-length nil
;;       print-level nil
;;       print-circle t
;;       eval-expression-print-level nil)

;;; Report bug
;;
(setq report-emacs-bug-no-explanations t)

;;; Save/restore emacs-session
;;
;;
(tv-set-emacs-session-backup :enable (not (daemonp)))

;;; Link scratch buffer to file
;;
;; Need to be loaded at very end of config, use append.
(add-hook 'emacs-startup-hook 'go-to-scratch 'append)

;;; .emacs.el ends here
