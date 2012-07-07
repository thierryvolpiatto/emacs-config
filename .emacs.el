;;; .emacs.el ---
;;
;; Filename: .emacs.el
;; Description: My .emacs for thievol
;; Author: thierry
;; Created: sam aoû 16 19:06:09 2008 (+0200)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;; Code:

;;; Environment
;; See: (find-fline "~/.emacs.d/.eshell/login")
;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; calendar-date-style
(setq calendar-date-style 'european)

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

;; load-paths
;; For Info paths see:
;; [EVAL] (find-fline "~/.profile" "INFOPATH")
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
             "~/elisp/bbdb/lisp"
             "~/elisp/dvc/lisp/"
	     "~/elisp/magit"
             "~/elisp/auctex"
             "~/elisp/auctex/preview"
	     "~/elisp/autoconf-mode"
	     "~/elisp/bzr"
	     "~/elisp/cmake"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
	     "~/elisp/git"
	     "~/elisp/tex-utils"
	     "~/elisp/flim"
	     "~/elisp/apel"
	     "~/elisp/muse/lisp"
	     "~/elisp/muse/contrib"
             "~/elisp/emms/lisp/"
	     "~/elisp/ipython"
	     "~/elisp/python-mode"
	     "~/elisp/w3m/"
	     "~/elisp/ledger/"
             "~/elisp/emacs-helm"
             "~/elisp/emacs-helm-extensions"
	     "~/elisp/eev/"
             "~/elisp/google-maps"
             "~/elisp/org-active/contrib/lisp" ; Contain htmlize.el
             "~/elisp/slime"
             "~/elisp/slime/contrib"
             "~/.emacs.d/"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config-laptop/"
             "~/elisp/emacs-async"
	     ))
  (add-to-list 'load-path i))

(when (< emacs-major-version 24)
  (dolist (lib '("~/elisp/org-active"
                 "~/elisp/org-active/lisp"))
    (add-to-list 'load-path lib)))

(defun tv-maybe-load-ngnus (&optional force)
  (when (or force (< emacs-major-version 24))
    (add-to-list 'load-path "~/elisp/ngnus/lisp")
    (tv-require 'gnus-load "~/elisp/ngnus/lisp/gnus-load.el")
    (tv-require 'info)
    (add-to-list 'Info-directory-list "~/elisp/ngnus/texi/")
    (add-to-list 'Info-default-directory-list "~/elisp/ngnus/texi/")))

(tv-maybe-load-ngnus)

;; Load-all-gentoo's-files-from-site-lisp
;; Reuse gentoo's old autoload files for external packages.
;; (mapc 'load
;;       (directory-files "~/elisp/site-gentoo.d" t directory-files-no-dot-files-regexp))

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

;;; libidn site-lisp configuration
(autoload 'idna-to-ascii "idna"
  "Returns an ASCII Compatible Encoding (ACE) of STR.")
(autoload 'idna-to-unicode "idna"
  "Returns a possibly multibyte string after decoding STR.")
(autoload 'punycode-encode "punycode"
  "Returns a Punycode encoding of STR.")
(autoload 'punycode-decode "punycode"
  "Returns a possibly multibyte string which is the punycode decoding of STR.")

;;; lua-mode site-lisp configuration
(autoload 'lua-mode "lua-mode" "Mode for editing Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(setq lua-default-application "/usr/bin/lua")

;;; emacs-wget site-lisp configuration
;;
;;
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(load "w3m-wget")
(add-hook 'w3m-mode-hook '(lambda () (tv-require 'w3m-wget)))

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)


;;; Require's
;;
;;
(tv-require 'cl)
(tv-require 'eev-thierry)
(tv-require 'usage-memo)
(tv-require 'auth-source)
(tv-require 'epa-file)
(tv-require 'init-helm-thierry)
(tv-require 'bookmark-extensions)
(tv-require 'bookmark-firefox-handler)
(tv-require 'firefox-protocol)
(tv-require 'addressbook-bookmark)
(tv-require 'config-w3m)
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
(tv-require 'dvc-init)
(tv-require 'emms-mpd-config)
(tv-require 'dired-aux)
(tv-require 'dired-x)
(tv-require 'dired-extension)
(tv-require 'htmlize)
(tv-require 'regex-tool)
(tv-require 'no-word)
(tv-require 'eldoc-eval)
(tv-require 'ipython)
(tv-require 'python-mode)
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
(when (tv-require 'dired-aux)
  (tv-require 'helm-async))
(tv-require 'smtpmail-async)

;; Test if this overhide `helm-command-map-prefix-key'
;(global-set-key (kbd "C-x c") #'(lambda () (interactive) (message "Hello")))


;;; Global keys
;;
;;
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      'revert-buffer)
(global-set-key (kbd "C-c v")                      'yank-from-X)
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "M-e")                        'eek-eval-sexp-eol)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
(global-set-key (kbd "<f7> m")                     'tv-gnus)
(global-set-key (kbd "<f7> h")                     'w3m)
(global-set-key (kbd "<f7> t")                     'w3m-dtree)
(global-set-key (kbd "<f7> j")                     'webjump)
(global-set-key (kbd "<f7> s g")                   'search-word)
(global-set-key (kbd "<f7> s u")                   'tv-search-gmane)
(global-set-key (kbd "<f7> i")                     'erc-freenode-connect)
(global-set-key (kbd "<f7> g")                     'bitlbee)
(global-set-key (kbd "<f7> n")                     'newsticker-show-news)
(global-set-key (kbd "<f11> r")                    'regex-tool)
(global-set-key (kbd "<f11> e c")                  'eshell-toggle-cd)
(global-set-key (kbd "<f11> e t")                  'eshell-toggle)
(global-set-key (kbd "<f11> s h")                  'shell)
(global-set-key (kbd "<f11> t")                    'tv-term)
(global-set-key (kbd "<f11> i")                    'ielm)
(global-set-key (kbd "<f11> p")                    'py-shell)
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
;(global-set-key (kbd "C-z l")                      'helm-elscreen)
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
(global-set-key (kbd "C-x r L")                    'list-registers)
(global-set-key (kbd "C-c t r")                    'translate-at-point)
(global-set-key (kbd "<f5> c")                     'tv-toggle-calendar)
(global-set-key (kbd "C-c h e")                    'tv-tail-echo-area-messages)
(global-set-key (kbd "C-c k")                      'tv-kill-backward)
(global-set-key (kbd "C-d")                        'tv-delete-char)
(global-set-key (kbd "C-x C-'")                    'tv-toggle-resplit-window)
(global-set-key (kbd "C-§")                        'iedit-mode-on-function)


;;; Themes
;;
;;
(defvar tv-theme-directory "~/.emacs.d/themes/")
;; Fix the last stupid changes of 24.
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
(tv-require 'gnus-async)
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
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun tv-gnus (arg)
  (interactive "P")
  (if (or arg (not (quickping "imap.gmail.com")))
      (gnus-unplugged)
      (gnus)))

;; Use now org-keywords in gnus.
(add-hook 'message-mode-hook #'(lambda ()
				 (define-key message-mode-map (kbd "<f11> k") 'helm-org-keywords)))

(autoload 'gnus-dired-attach "gnus-dired.el")
(when (tv-require 'dired)
  (define-key dired-mode-map (kbd "C-c C-a") 'gnus-dired-attach))

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
(setq savehist-file "~/.emacs.d/history")
(setq history-length 1000)
(savehist-mode 1)

;;; Recentf
;;
;;
(setq recentf-save-file "~/.emacs.d/recentf")
;; `recentf-mode' will be started by helm when needed,
;; so no need to start it here

;; undo-limit
(setq undo-limit 30000)

;;; Frame-parameters
;;
;;
;; My current-font: [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:   [EVAL]: (helm 'helm-c-source-xfonts)
;; Choose a color:  [EVAL]: (helm 'helm-c-source-colors)

;; [See Initial config: EVAL]: (find-fline "~/.Xressources")

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
                                (background-color . "DarkSlateGray")
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
           (def-alpha (or ini-alpha 90))
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


;; Don't fucking split this windows horizontally
(setq split-width-threshold nil)

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)


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
(setq bmkext-jump-w3m-defaut-method 'w3m) ; Set to 'external to use external browser, w3m for w3m.

(defun tv-pp-bookmark-alist ()
  "Quickly print `bookmark-alist'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pp-bookmark-alist*"))
  (erase-buffer)
  (dolist (i bookmark-alist)
    (pp i (current-buffer))))

;;; Emacs-w3m
;;
;;
(setq w3m-icon-directory "~/elisp/emacs-w3m/icons")

(defun dired-w3m-find-file ()
  (interactive)
  (w3m-find-file (dired-get-filename)))

(when (tv-require 'dired)
  (progn
    (tv-require 'w3m-load)
    (tv-require 'mime-w3m)
    (define-key dired-mode-map (kbd "C-c F") 'dired-w3m-find-file)))

;;; Default web browser
;;
;;
;(setq browse-url-browser-function 'w3m-browse-url)
;(setq browse-url-browser-function 'browse-url-uzbl)
(setq browse-url-browser-function 'browse-url-firefox)
;(setq browse-url-browser-function 'browse-url-mozilla)

;; w3m-mode-map
(define-key w3m-mode-map (kbd "C-c v") 'helm-w3m-bookmarks)
(define-key w3m-mode-map (kbd "C-c M") 'w3m-view-this-page-in-uzbl)
(substitute-key-definition 'w3m-view-url-with-external-browser
                           'tv-w3m-view-this-page-in-firefox
                           w3m-mode-map)

;; muse-config
(add-hook 'find-file-hooks 'muse-mode-maybe)
(setq muse-wiki-allow-nonexistent-wikiword t)

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

;; consequent-log-file
(setq message-log-max 1000)

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; No-startup-screen
(setq inhibit-startup-message t)

;; Ediff-config
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Dired
;; use the directory in the other windows as default target
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t) ; Emacs vcs only
(define-key dired-mode-map (kbd "C-k")   #'(lambda () (interactive) (dired-do-delete 1)))
(define-key dired-mode-map (kbd "b")     #'(lambda () (interactive) (dired-do-byte-compile 1)))
(define-key dired-mode-map (kbd "C-t -") 'thumb-convert-current-dir)
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

;; emacs-backup-config
;; Backup
(setq backup-directory-alist '(("" . "/home/thierry/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-file-name-transforms nil)


;; Eval==> (describe-variable 'case-fold-search)
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
(define-key py-mode-map (kbd "C-c C-b") 'helm-browse-code)

;; config-python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("ipython" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Search-in-python-library
;; (find-fline "~/.emacs.d/emacs-config-laptop/.emacs-config-w3m.el" "tv-python-search")
(define-key py-mode-map (kbd "<f7> s p") 'tv-python-search)

;;pdb==> python debugger (installation de gdb neccessaire)
(setq gud-pdb-command-name "/home/thierry/bin/pdb.py")
(tv-require 'pdbtrack)

;; Pylint-via-flymake
;; fonctionne avec le script python /usr/local/bin/epylint

(defvar flymake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f4> f e") 'flymake-display-err-menu-for-current-line)
    (define-key map (kbd "<f4> f n") 'flymake-goto-next-error)
    map)
   "Keymap used for flymake commands.")

;; Flymake-pour-python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

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
;; Set `undo-outer-limit' to high value to avoid messages on long output.
(setq undo-outer-limit 6000000)

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
      (lambda nil
        (concat
         (getenv "USER")
         "@"
         (system-name)
         ":"
         (abbreviate-file-name (eshell/pwd))
         (if (= (user-uid) 0) " # " " $ "))))
(add-hook 'eshell-mode-hook #'(lambda ()
                                (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")))

;; helm completion with pcomplete
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

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

;;; pcomplete Completion functions on specific commands
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

;;; Woman
;;
;;
(setq woman-use-own-frame nil)

;;; Printing config
;;
;(setq helm-ff-printer-list (helm-ff-find-printers))
(setq lpr-command "gtklp")
(setq lpr-switches '("-P"))
(setq printer-name "Epson-Stylus-Photo-R265")
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
               (lambda () (ispell-change-dictionary "french")))
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
(setq newsticker-html-renderer 'w3m-region)

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
(tv-require 'tramp)
;(setq tramp-default-method "ssh") ; methode par defaut

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
(setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on")
                                    ("dl.free.fr" . "on")))

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

(add-hook 'slime-load-hook (lambda () (tv-require 'slime-tramp)))

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

;; mozilla-javascript
;; Javascript and mozilla (interaction with firefox)
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; (defun tv-pop-to-moz-repl ()
;;   (interactive)
;;   (if (tv-get-pid-from-process-name "firefox")
;;       (inferior-moz-switch-to-mozilla)
;;       (message "Please start first firefox and repl!")))

;; ioccur
(define-key org-mode-map (kbd "C-c C-o") 'ioccur-find-buffer-matching)
;(setq ioccur-fontify-buffer-p nil)

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
(load "xmodmap.elc")

;; sql-mode
(setq sql-sqlite-program "sqlite3")

;; sqlite-dump
(autoload 'sqlite-dump "sqlite-dump" nil t)
(modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))

;; key-for-copy-files-async
(define-key dired-mode-map (kbd "C-c C-S-c") 'tv-slime-dired-copy-files-or-dir-async)
(define-key dired-mode-map (kbd "C-c C-S-d") 'tv-slime-dired-delete-files-async)

;;; Isearch
;;
;;
(setq isearch-allow-scroll t)

;; Implement a decent "online" help like helm. (i.e show help without quitting).
(defun isearch-help-internal (bufname insert-content-fn)
  (save-window-excursion
    (switch-to-buffer (get-buffer-create bufname))
    (erase-buffer)
    (funcall insert-content-fn)
    (setq mode-line-format "%b (SPC,C-v:NextPage  b,M-v:PrevPage  other:Exit)")
    (setq cursor-type nil)
    (goto-char 1)
    (isearch-help-event-loop)))

(defun isearch-help-event-loop ()
  (ignore-errors
    (catch 'exit-isearch-help
      (loop for event = (read-event) do
           (case event
             ((?\C-v ? ) (scroll-up))
             ((?\M-v ?b) (scroll-down))
             (t (throw 'exit-isearch-help (isearch-update))))))))

(defun isearch-help ()
  (interactive)
  (isearch-help-internal
   " *Isearch Help*"
   (lambda ()
     (insert (substitute-command-keys isearch-help-message))
     (org-mode))))

(defvar isearch-help-message "\\{isearch-mode-map}")
(define-key isearch-mode-map (kbd "C-?") 'isearch-help)
(define-key isearch-mode-map [remap isearch-describe-bindings] 'isearch-help)

;; Isearch mode-line help.
(defvar isearch-old-mode-line nil)
(defun isearch-mode-line-help ()
  (let (mode-line-in-non-selected-windows)
    (setq isearch-old-mode-line mode-line-format)
    (setq mode-line-format '(" " mode-line-buffer-identification " "
                             (line-number-mode "%l") " "
                             "C-?:Help,C-s:Forward,C-b:Backward,C-w:Yank at Point,\
C-y:Yank,M-n/p:kill-ring nav,C/M-%%:Query replace/regexp,M-s r:toggle-regexp."))))
(add-hook 'isearch-mode-hook 'isearch-mode-line-help)
(defun isearch-mode-line-help-restore ()
  (setq mode-line-format isearch-old-mode-line))
(add-hook 'isearch-mode-end-hook 'isearch-mode-line-help-restore)

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
(setq vc-handled-backends '(RCS CVS SVN Hg Git Bzr))

;;; Temporary Bugfixes until fixed in trunk.
;;

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
      ret)))


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
             do (progn (set-buffer buf)
                       (set-mark (car entry))
                       (setf (winner-active-region) (cdr entry)))))
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
(ignore-errors
  (setq battery-mode-line-format "[Bat:%b%p%%,%L]")
  (display-battery-mode 1))

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

;;; Ido virtual buffers
;;
;;
(setq ido-use-virtual-buffers t)

;;; Printing variables
;;
;;
;; (setq print-gensym t
;;       print-level 12
;;       print-circle t
;;       eval-expression-print-level 12)

;;; Report bug
;;
(setq report-emacs-bug-no-explanations t)

;;; Save/restore emacs-session
;;
;;
(tv-set-emacs-session-backup :enable t)

;; Link-scratch-to-file
;;(find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "go-to-scratch")
;; [For Emacs24 Fri Jun 17 21:56:29 2011]
;; This have to be loaded at the end to overhide emacs vanilla scratch buffer
;; with splash-screen etc... This important for versions of emacs starting at
;; Fri Jun 17 21:55:55 2011
;; Turn OFF bidi everywhere.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq-default bidi-display-reordering nil))
          'append)
(add-hook 'emacs-startup-hook 'go-to-scratch 'append)
(add-hook 'emacs-startup-hook 'cat-command 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs.el ends here
