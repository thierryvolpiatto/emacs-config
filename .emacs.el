;;; .emacs.el ---
;;
;; Filename: .emacs.el
;; Description: My .emacs for thievol
;; Author: thierry
;; Maintainer:
;; Created: sam aoû 16 19:06:09 2008 (+0200)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;
;;; Code:

;; Environment 
;; See: (find-fline "~/.emacs.d/.eshell/login")
;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; calendar-date-style 
(setq calendar-date-style 'european) ; [SEE] 

;; load-paths 
;; For Info paths see:
;; [EVAL] (find-fline "~/.profile" "INFOPATH")
;; [EVAL] (getenv "INFOPATH")
(require 'info)
(add-to-list 'Info-directory-list "/usr/local/share/info")
(add-to-list 'Info-directory-list "/usr/share/info")
(add-to-list 'Info-directory-list "~/elisp/info")

(when (< emacs-major-version 24)
  (add-to-list 'load-path "/home/thierry/elisp/ngnus/lisp/gnus-fallback-lib/eieio"))

(dolist (i '("/usr/local/share/emacs/site-lisp"
	     "~/elisp/"
	     "~/elisp/auctex"
	     "~/elisp/autoconf-mode"
	     "~/elisp/bzr"
	     "~/elisp/cmake"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
	     ;"~/elisp/gentoo-syntax"
	     "~/elisp/git"
	     ;"~/elisp/lua-mode"
	     "~/elisp/mercurial"
	     ;"~/elisp/pymacs"
	     "~/elisp/subversion"
	     "~/elisp/tex-utils"
	     "~/elisp/flim"
	     "~/elisp/apel"
	     ;"~/elisp/libidn"
	     ;"~/elisp/librep"
	     "~/elisp/muse/lisp"
	     "~/elisp/muse/contrib"
             "~/elisp/AC/"
             "~/elisp/emms/lisp/"
	     "~/elisp/ipython"
	     "~/elisp/python-mode"
	     "~/elisp/emacs-w3m/"
	     "~/elisp/ledger/"
	     "~/elisp/anything/"
	     "~/elisp/eev/"
             "~/elisp/elscreen"
             "~/elisp/google-maps"
             ;"~/elisp/org-active"
             ;"~/elisp/org-active/lisp"
             "~/elisp/org-active/contrib/lisp" ; Contain htmlize.el
             "~/elisp/slime"
             "~/elisp/slime/contrib"
             "~/.emacs.d/"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config-laptop/"
	     ))
  (add-to-list 'load-path i))

(when (< emacs-major-version 24)
  (dolist (lib '("~/elisp/org-active"
                 "~/elisp/org-active/lisp"))
    (add-to-list 'load-path lib)))

;; Load-all-gentoo's-files-from-site-lisp
;; Reuse gentoo's old autoload files for external packages.
(mapc 'load
      (directory-files "~/elisp/site-gentoo.d" t directory-files-no-dot-files-regexp))

;; Emacs-customize-have-it's-own-file 
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;;; Require's
;;
;;
(require 'cl)
(require 'eev-thierry)
(require 'usage-memo)
(require 'auth-source)
(require 'epa-file)
(require 'init-anything-thierry)
(require 'bookmark-extensions)
(require 'bookmark-firefox-handler)
(require 'bookmark-uzbl-handler)
(require 'firefox-protocol)
(require 'addressbook-bookmark)
(require 'config-w3m)
(require 'org-config-thierry)
(require 'muse-autoloads)
(require 'muse-mode)
(require 'muse-wiki)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)
(require 'muse-colors)
(require 'htmlize-hack)
(require 'psvn)
(require 'dvc-init)
(require 'emms-mplayer-config)
(require 'yaoddmuse)
(require 'dired-aux)
(require 'dired-x)
(require 'dired-extension)
(require 'htmlize)
(require 'regex-tool)
(require 'no-word)
(require 'eldoc-eval)
(require 'ipython)
(require 'python-mode)
(require 'flymake)
(require 'em-xtra)
(require 'esh-toggle)
(require 'woman)
(require 'tex-site)
(require 'ledger-config)
(require 'boxquote)
(require 'slime-autoloads)
(require 'slime)
(require 'cl-info)
(require 'moz)
(require 'ioccur)
(require 'mb-depth)
(require 'autodoc)
(require 'auto-document)
(require 'tv-utils)
(require 'rectangle-utils)
(require 'xml-weather)
(require 'smallurl)
(require 'elscreen)
(require 'zop-to-char)
(require 'iedit)
(require 'csv2org)
(require 'el-expectations)
(require 'el-mock)
(require 'simple-call-tree)
(require 'google-maps)
(require 'googlecl)

;;; Global keys
;;
;;
(global-set-key (kbd "M-e")                        'eek-eval-sexp-eol)
(global-set-key (kbd "<f7> m")                     'tv-gnus)
(global-set-key (kbd "C-c R")                      'revert-buffer)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
(global-set-key (kbd "<f7> h")                     'w3m)
(global-set-key (kbd "<f7> t")                     'w3m-dtree)
(global-set-key (kbd "<f7> j")                     'webjump)
(global-set-key (kbd "<f7> s g")                   'search-word)
(global-set-key (kbd "<f7> s u")                   'tv-search-gmane)
(global-set-key (kbd "<f7> i")                     'erc-freenode-connect)
(global-set-key (kbd "<f7> g")                     'bitlbee)
(global-set-key (kbd "<f5> s")                     'svn-status)
(global-set-key (kbd "C-c v")                      'yank-from-X)
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "<f11> r")                    'regex-tool)
(global-set-key (kbd "M-<f4>")                     'flymake-mode)
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "<f11> e c")                  'eshell-toggle-cd)
(global-set-key (kbd "<f11> e t")                  'eshell-toggle)
(global-set-key (kbd "<f11> s h")                  'shell)
(global-set-key (kbd "<f11> t")                    'tv-term)
(global-set-key (kbd "<f11> i")                    'ielm)
(global-set-key (kbd "<f11> p")                    'py-shell)
(global-set-key [(control return)]                 'calculator)
(global-set-key (kbd "<f2>")                       'tv-flyspell)
(global-set-key (kbd "<f5> p s b")                 'tv-ps-print-buffer)
(global-set-key (kbd "<f5> p s r")                 'tv-ps-print-region)
(global-set-key (kbd "<f5> p b")                   'print-buffer)
(global-set-key (kbd "<f5> p r")                   'print-region)
(global-set-key (kbd "<f5> p i")                   'pr-interface)
(global-set-key (kbd "<f7> n")                     'newsticker-show-news)
(global-set-key (kbd "<f7> q f")                   'boxquote-describe-function)
(global-set-key (kbd "<f7> q v")                   'boxquote-describe-variable)
(global-set-key (kbd "<f7> q k")                   'boxquote-describe-key)
(global-set-key (kbd "<f7> q r")                   'boxquote-region)
(global-set-key (kbd "<f7> q u")                   'boxquote-unbox-region)
(global-set-key (kbd "<f7> q t")                   'boxquote-title)
(global-set-key (kbd "<f7> q c")                   'boxquote-copy-box-without-box)
(global-set-key (kbd "<f11> l r")                  'tv-start-slime)
(global-set-key (kbd "<f11> l e")                  'slime-scratch)
(global-set-key (kbd "<f11> l l")                  'slime-list-connections)
(global-set-key (kbd "<f11> j s")                  'tv-pop-to-moz-repl)
(global-set-key [remap occur]                      'ioccur)
(global-set-key [remap isearch-forward]            'ioccur)
(global-set-key (kbd "C-c o")                      'ioccur)
(global-set-key (kbd "C-c C-o")                    'ioccur-find-buffer-matching)
(global-set-key (kbd "<M-down>")                   'tv-scroll-down)
(global-set-key (kbd "<M-up>")                     'tv-scroll-up)
(global-set-key (kbd "<C-M-down>")                 'tv-scroll-other-down)
(global-set-key (kbd "<C-M-up>")                   'tv-scroll-other-up)
(global-set-key (kbd "C-h K")                      'find-function-on-key)
(global-set-key (kbd "C-h F")                      'find-function-at-point)
(global-set-key (kbd "C-h V")                      'find-variable-at-point)
(global-set-key (kbd "<C-prior>")                  'text-scale-decrease)
(global-set-key (kbd "<C-next>")                   'text-scale-increase)
(global-set-key (kbd "C-x C-²")                    'delete-other-windows)
(global-set-key (kbd "C-x C-&")                    'delete-other-windows)
(global-set-key (kbd "C-x C-à")                    'delete-window)
(global-set-key (kbd "C-x C-é")                    'split-window-vertically)
(global-set-key (kbd "C-x C-\"")                   'split-window-horizontally)
(global-set-key (kbd "C-x r v")                    'string-insert-rectangle)
(global-set-key (kbd "C-x r e")                    'extend-rectangle-to-end)
(global-set-key (kbd "C-x r h")                    'rectangle-menu)
(global-set-key (kbd "C-x r <right>")              'rectangle-insert-at-right)
(global-set-key (kbd "C-x r M-w")                  'copy-rectangle)
(global-set-key (kbd "<f5> x f")                   'xml-weather-forecast-at)
(global-set-key (kbd "<f5> x n")                   'xml-weather-today-at)
(global-set-key (kbd "<f5> x l")                   'xml-weather-today-favorite)
(global-set-key (kbd "<f5> x t")                   'xml-weather-run-ticker)
(global-set-key (kbd "C-c u")                      'smallurl-replace-at-point)
(global-set-key (kbd "C-z l")                      'anything-elscreen)
(global-set-key (kbd "M-z")                        'zop-to-char)
(global-set-key (kbd "<f5> g m")                   'google-maps)
(global-set-key (kbd "M-\"")                       'tv-insert-double-quote)
(global-set-key (kbd "C-M-\`")                     'tv-insert-double-backquote)
(global-set-key (kbd "M-\[")                       'tv-insert-vector)
(global-set-key (kbd "C-M-(")                      'tv-move-pair-forward)
(global-set-key (kbd "C-M-\"")                     'tv-insert-double-quote-and-close-forward)
(global-set-key (kbd "C-M-)")                      'tv-insert-pair-and-close-forward)
(global-set-key [remap save-buffers-kill-terminal] 'tv-stop-emacs)

;; Themes 
(defvar tv-theme-directory "~/.emacs.d/themes/")
;; Fix the last stupid changes of 24.
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv-theme-directory))

(defvar tv-current-theme 'naquadah)
;; Load my favourite theme.
(add-hook 'emacs-startup-hook #'(lambda () (load-theme tv-current-theme)))

(defun tv-change-theme (theme)
  (interactive
   (list (anything-comp-read
          "Theme: "
          (loop with themes = (directory-files
                               tv-theme-directory
                               nil directory-files-no-dot-files-regexp)
             for theme in themes
             unless (string-match "\.hg" theme)
             collect (replace-regexp-in-string
                      "\-theme" "" (file-name-sans-extension theme)))
          :must-match t
          :fc-transformer #'(lambda (candidates sources)
                              (loop for i in candidates
                                 if (string= i (symbol-name tv-current-theme))
                                 collect (propertize i 'face '((:foreground "red")))
                                 else
                                 collect i)))))
  (let ((stheme (intern theme)))
    (load-theme stheme)
    (setq tv-current-theme stheme)))

;; libidn is not in gentoo.d. load it
;(require 'idna)
;(require 'punycode)

;; column-number 
(column-number-mode 1)

;; desktop-save 
;; (desktop-save-mode 1)
;; (setq desktop-restore-eager 5)
;; (add-to-list 'desktop-globals-to-save 'ioccur-history)
;; (add-to-list 'desktop-globals-to-save 'anything-external-command-history)
;; (add-to-list 'desktop-globals-to-save 'anything-surfraw-engines-history)
;; (add-to-list 'desktop-globals-to-save 'anything-ff-history)
;; (add-to-list 'desktop-globals-to-save 'anything-external-command-history)


;; usage-memo 
;; Add memo to describe-func/variable
(umemo-initialize)
(defun umemo-electric-quit ()
  (interactive)
  (if (umemo-point-is-in-memo-area-p (point))
       (call-interactively (global-key-binding "q"))
       (and (view-mode 1) (View-quit))))
(define-key usage-memo-mode-map (kbd "q") 'umemo-electric-quit)


;; gnus-config 
(defun tv-maybe-load-ngnus (&optional force)
  (when (or force (< emacs-major-version 24))
    (add-to-list 'load-path "~/elisp/ngnus/lisp")
    (require 'gnus-load)
    (require 'info)
    (add-to-list 'Info-directory-list "~/elisp/ngnus/texi/")
    (add-to-list 'Info-default-directory-list "~/elisp/ngnus/texi/")))

;(tv-maybe-load-ngnus 'force)
(tv-maybe-load-ngnus)

;(require 'gnus-async)
;(setq gnus-asynchronous t)

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)
(setq send-mail-command 'gnus-msg-mail)
(setq gnus-init-file "~/.emacs.d/emacs-config-laptop/.gnus.el")

(defvar tv-gnus-loaded-p nil)
(defun tv-load-gnus-init-may-be ()
  (unless tv-gnus-loaded-p
    (load gnus-init-file)
    (setq tv-gnus-loaded-p t)))

(add-hook 'message-mode-hook 'tv-load-gnus-init-may-be)
(add-hook 'gnus-before-startup-hook 'tv-load-gnus-init-may-be)

(defun tv-gnus (arg)
  (interactive "P")
  (let ((gc-cons-threshold 3500000))
    (if arg (gnus-unplugged) (gnus))))

;; Use now org-keywords in gnus.
(add-hook 'message-mode-hook #'(lambda () 
				 (define-key message-mode-map (kbd "<f11> k") 'anything-org-keywords)))

(autoload 'gnus-dired-attach "gnus-dired.el")
(when (require 'dired)
  (define-key dired-mode-map (kbd "C-c C-a") 'gnus-dired-attach))

(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups 'ask-server)

;; Authinfo-settings-with-epa 
(if (file-exists-p "~/.authinfo.gpg")
    (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
    (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))

;; Don't-fucking-split-this-windows-horizontally 
(setq split-width-threshold nil)

;; basic-config 
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Pas-de-dialog-gtk 
(setq use-file-dialog nil)

;; Save-minibuffer-history 
(setq savehist-file "~/.emacs.d/history")
(setq history-length 1000)
(savehist-mode 1)

;; Recentf 
(setq recentf-save-file "~/.emacs.d/recentf")
(recentf-mode 1)

;; undo-limit 
(setq undo-limit 30000)

;;; Frame-parameters
;;
;;
;; My current-font: [EVAL] (cdr (assoc 'font (frame-parameters)))
;; Choose a font:   [EVAL] (anything 'anything-c-source-xfonts)
;; Choose a color:  [EVAL] (anything 'anything-c-source-colors)

;; Speedbar
(setq speedbar-frame-parameters
      `((minibuffer . nil)
        (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
        (width . 20)
        (fullscreen . nil)
        (left . ,(- (* (window-width) 8) 160))
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))

;;; mouse-avoidance-mode
;;
;; Push the mouse out of the way.
(mouse-avoidance-mode 'banish)

;;; Emacs transparency - only with compiz.
;;
;;
(when (window-system)
  (defun tv-transparency-modify (arg)
    "Increase Emacs frame transparency.
With a prefix arg decrease transparency."
    (interactive "P")
    (let* ((ini-alpha (frame-parameter nil 'alpha))
           (def-alpha (or ini-alpha 100))
           (mod-alpha (if arg (+ def-alpha 10) (- def-alpha 10))))
      (when (and (>= mod-alpha frame-alpha-lower-limit) (<= mod-alpha 100))
        (modify-frame-parameters nil (list (cons 'alpha mod-alpha))))))
  (global-set-key (kbd "C-8") 'tv-transparency-modify))

;;; special buffer display.
;;
;;
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
                                     (fullscreen . nil))))

;;; Bookmarks
;;
;;
(setq bookmark-bmenu-toggle-filenames nil)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(setq bookmark-automatically-show-annotations nil)
(add-to-list 'org-agenda-files bmkext-org-annotation-directory)
(setq bmkext-external-browse-url-function 'browse-url-uzbl)
(setq bmkext-jump-w3m-defaut-method 'w3m) ; Set to 'external to use external browser, w3m for w3m.

(defun tv-pp-bookmark-alist ()
  "Quickly print `bookmark-alist'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pp-bookmark-alist*"))
  (erase-buffer)
  (dolist (i bookmark-alist)
    (pp i (current-buffer))))

;;; emacs-w3m
;;
;;
(setq w3m-icon-directory "~/elisp/emacs-w3m/icons")

(defun dired-w3m-find-file ()
  (interactive)
  (w3m-find-file (dired-get-filename)))

(when (require 'dired)
  (progn
    (require 'w3m-load)
    (require 'mime-w3m)
    (define-key dired-mode-map (kbd "C-c F") 'dired-w3m-find-file)))

;(setq browse-url-browser-function 'w3m-browse-url)
(setq browse-url-browser-function 'browse-url-uzbl)

;; w3m-mode-map 
(define-key w3m-mode-map (kbd "C-c v") 'anything-w3m-bookmarks)
(define-key w3m-mode-map (kbd "C-c M") 'w3m-view-this-page-in-uzbl)
(substitute-key-definition 'w3m-view-url-with-external-browser
                           'tv-w3m-view-this-page-in-firefox
                           w3m-mode-map)

;; muse-config 
(add-hook 'find-file-hooks 'muse-mode-maybe)
(setq muse-wiki-allow-nonexistent-wikiword t)

;; erc-config 
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
  (interactive (list (anything-comp-read "Choose a Bitlbee Server: "
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

;; winner-mode-config 
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
                              ))

(when (require 'winner)
  (defvar winner-boring-buffers-regexp
    "\*[aA]nything.*\\|\*xhg.*\\|\*xgit.*")
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

;; load-emms 
;(require 'emms-mpd-config)
(define-key dired-mode-map (kbd "C-c p d") 'emms-play-dired)

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

;; yaoddmuse 
(setq yaoddmuse-username "ThierryVolpiatto")
(setq yaoddmuse-directory "/home/thierry/.emacs.d/yaoddmuse")
(setq yaoddmuse-wikis
      '(("TestWiki" "http://www.emacswiki.org/cgi-bin/test" utf-8 "uihnscuskc=1;")
        ("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8 "uihnscuskc=1;")))

;; Dired 
;; use the directory in the other windows as default target
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t) ; Emacs vcs only
(define-key dired-mode-map (kbd "C-k") #'(lambda () (interactive) (dired-do-delete 1)))
(define-key dired-mode-map (kbd "b") #'(lambda () (interactive) (dired-do-byte-compile 1)))
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

;; Battery 
;; (require 'battery)
;; (run-with-timer "2" 60 #'(lambda ()
;;                            (if (equal (cdr (assoc 76 (battery-linux-proc-acpi)))
;;                                       "on-line")
;;                                (setq battery-mode-line-format "[%b%p%%,%d°C,%L]")
;;                                (setq battery-mode-line-format "[%b%p%%,%d°C,%t]"))))
;; (display-battery-mode)


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
(when (require 'whitespace)
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

(when (require 'eldoc)
  (set-face-attribute 'eldoc-highlight-function-argument nil :underline "red"))

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

;; Indent-only-with-spaces 
(setq-default indent-tabs-mode nil)

;; Lua-mode 
;; (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-hook 'lua-mode-hook 'turn-on-font-lock)
;; (add-hook 'lua-mode-hook 'hs-minor-mode)

;; Python-config 
;; Ipython completion is provided by rlcompleter2
;; And anything-ipython.el (need ipython.el)
;; pymacs/Pycomplete can be used also but are not needed for make
;; working anything-ipython.

;; terminal-ipython 
;; (setenv "PYTHONSTARTUP" "/home/thierry/.pythonstartup")
(define-key py-shell-map (kbd "\t") 'ipython-complete)
;; ;(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")
(define-key py-mode-map (kbd "C-c C-b") 'anything-browse-code)
;; ;; Pymacs 
;; (setenv "PYMACS_PYTHON" "python2.6")
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; Pycomplete 
;(require 'pycomplete)

;; config-python-mode 
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("ipython" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Search-in-python-library 
;; (find-fline "~/.emacs.d/emacs-config-laptop/.emacs-config-w3m.el" "tv-python-search")
(define-key py-mode-map (kbd "<f7> s p") 'tv-python-search)

;;pdb==> python debugger (installation de gdb neccessaire)
(setq gud-pdb-command-name "/home/thierry/bin/pdb.py")
(load "pdbtrack.el")

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

;(global-set-key (kbd "C-c e p") 'tv-insert-python-header)

;; shell-config 

;; Set `undo-outer-limit' to hight value to avoid messages when gentoo emerge
(setq undo-outer-limit 6000000)
                       
;; prompt-shell-read-only 
(setq comint-prompt-read-only t)

;; [obsolete]
;; couleur-dans-le-shell 
;; (j'ai ajouté dumb dans /etc/LS_COLOR egalement)
;(require 'ansi-color)
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; newline-and-indent-in-sh-mode 
(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))


;; Eshell-config

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

;; anything completion with pcomplete
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'anything-esh-pcomplete)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-p") 'anything-eshell-history)))

;; Eshell-banner 
(setq eshell-banner-message (format "%s %s\n"
                                    (propertize
                                     "Eshell session started"
                                     'face '((:foreground "Goldenrod")))
                                    (propertize
                                     (format-time-string "%c")
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
(setq eshell-directory-name "/home/thierry/.emacs.d/.eshell/")


;; Eshell-command 

;; Eshell-toggle 

;; Eshell-visual 
(setq eshell-term-name "eterm-color")
(when (require 'em-term)
  (dolist (i '("kop" "ledger" "htop"))
    (add-to-list 'eshell-visual-commands i)))

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
      (let ((dic (anything-comp-read
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

;; Gtklp-par-defaut 
(setq lpr-command "gtklp")
(setq-default ps-print-header nil)
(set-variable 'lpr-switches '("-PStylus-Photo-R265"))
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

;; auctex-config 

(setq TeX-view-program-selection '(((output-dvi style-pstricks)
                                    "dvips and gv")
                                   (output-dvi "xdvi")
                                   (output-pdf "xpdf")
                                   (output-html "xdg-open")))

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
(when (require 'newsticker)
  (define-key newsticker-mode-map (kbd "Q") 'newsticker-quit-and-stop))

(defadvice newsticker-next-feed (around recenter activate)
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed))
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (prog1 (point) (recenter)))

(defadvice newsticker-mark-all-items-at-point-as-read-and-redraw (after recenter activate)
  (recenter))

;; Tramp-config 
;(require 'tramp)
;(setq tramp-default-method "ssh") ; methode par defaut

;; Mode-lecture-photo-auto 
(auto-image-file-mode 1)

;; slime-config 
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

(add-hook 'slime-load-hook (lambda () (require 'slime-tramp)))

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
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(defun tv-pop-to-moz-repl ()
  (interactive)
  (if (tv-get-pid-from-process-name "firefox")
      (inferior-moz-switch-to-mozilla)
      (message "Please start first firefox and repl!")))

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

;; isearch 
(setq isearch-allow-scroll t)

;; align-let 
(autoload 'align-let-keybinding "align-let" nil t)
(add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
(add-hook 'lisp-interaction-mode-hook 'align-let-keybinding)
(add-hook 'lisp-mode-hook 'align-let-keybinding)


;; line-move-visual 
;(setq line-move-visual nil)

;; xml-weather 
(setq xml-weather-default-icons-directory "~/xml-weather-icons/icons/31x31")
(setq xml-weather-moon-icons-directory "~/xml-weather-icons/moon-icons2/31X31/")

;; rst-mode 
(add-hook 'rst-mode-hook 'auto-fill-mode)


;; Undo-tree 
;(require 'undo-tree)
;(global-undo-tree-mode)

;; Elscreen
(defun anything-elscreen ()
  (interactive)
  (anything-other-buffer 'anything-c-source-elscreen "*Anything Elscreen*"))

;(elscreen-set-prefix-key (kbd "C-&"))
;(global-set-key (kbd "C-z") 'ignore)

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

;; iedit 
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

;; Checkdoc 
(autoload 'checkdoc-batch       "checkdoc-batch" nil t)
(autoload 'checkdoc-batch-files "checkdoc-batch" nil t)

;; Mode-line 
(set-face-attribute 'mode-line-emphasis nil :foreground "red")
(set-face-attribute 'mode-line-buffer-id nil :foreground "Green4" :reverse-video t)

;; Google-Apps 
(setq google-maps-static-default-zoom 10)

;; Toggle-show-trailing-whitespace 
(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; World-time 
(add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
(add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
(add-to-list 'display-time-world-list '("America/Denver" "Denver"))

;; Start gmail notifier
;; (find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "defun gmail-notify-start")
;(gmail-notify-start)

;; Trash
;(setq delete-by-moving-to-trash t)

;; Popwin makes you free from the hell of annoying buffers such like
;; *Help*, *Completions*, *compilation*, and etc.
;(require 'popwin)
;(setq display-buffer-function 'popwin:display-buffer)

;; Auto complete
;; (require 'auto-complete)
;; (require 'ac-dabbrev)
;; (require 'auto-complete-emacs-lisp)
;; (require 'auto-complete-latex)
;; (setq ac-l-dict-directory "~/elisp/AC/ac-l-dict")
;; (add-hook 'latex-mode 'auto-complete-mode)
;; (ac-emacs-lisp-setup)
;; (ac-emacs-lisp-init)
;; (add-hook 'lisp-interaction-mode 'auto-complete-mode)

;; Haskell-mode
;; (add-to-list 'load-path "~/elisp/haskell-mode")
;; (load "haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)

;; Kill emacs
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
      (save-buffers-kill-terminal)))

;; Minibuffers completion
(setq completion-cycle-threshold t) ; always cycle, no completion buffer.

;; Uzbl
;(setq browse-url-uzbl-program "uzbl-browser")

;; Save/restore emacs-session
(tv-set-emacs-session-backup :enable t)

;; Link-scratch-to-file 
;;(find-fline "~/.emacs.d/emacs-config-laptop/tv-utils.el" "go-to-scratch")
;; [For Emacs24 Fri Jun 17 21:56:29 2011]
;; This have to be loaded at the end to overhide emacs vanilla scratch buffer
;; with splash-screen etc... This important for versions of emacs starting at
;; Fri Jun 17 21:55:55 2011
(add-hook 'emacs-startup-hook 'go-to-scratch 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; .emacs.el ends here
