;;; .emacs.el ---

;;; Code:

(require 'cl-lib)

;; (setenv "LANG" "C")

;; Fix loosing focus on window manager
;; when quitting emacs.
;; WM should be set accordingly.
;; See WM configuration screenshot here:
;; /home/thierry/Documents/configuration_screenshots_xubuntu/Screenshot - 30062014 - 17:09:56.png

(setq focus-follows-mouse t)


;;; Compatibility
;;
;;
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in cl-case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))


;;; Temporary Bugfixes until fixed in trunk.
;;
(when (require 'net-utils)
  (defun ping (host)
    "Ping HOST.
If your system's ping continues until interrupted, you can try setting
`ping-program-options'."
    (interactive "sPing host: ")
    (let ((options
           (if ping-program-options
               (append ping-program-options (list host))
               (list host))))
      (net-utils-run-simple
       (concat "Ping" " " host)
       ping-program
       options)))

  (defun run-dig (host)
    "Run dig program."
    (interactive "sLookup host: ")
    (net-utils-run-simple
     (concat "** "
             (mapconcat 'identity
                        (list "Dig" host dig-program)
                        " ** "))
     dig-program
     (list host))))

(defadvice term-command-hook (before decode-string)
  (setq string (decode-coding-string string locale-coding-system)))

(when (version< emacs-version "24.3.50.1") (ad-activate 'term-command-hook))


;;; Annoyances section
;;
(global-set-key (kbd "<f11>") nil)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (when (get-buffer "*Compile-Log*")
                                    (kill-buffer "*Compile-Log*")
                                    (delete-other-windows))))

;; Annoyance number 1 is bidi
;; Turn OFF bidi everywhere.
(setq-default bidi-display-reordering nil)
;(setq-default cache-long-scans nil) ; Fix bug#15973 among others.

;; Disable uniquify enabled by default in 24.4.
(setq uniquify-buffer-name-style nil)

;; electric-indent-mode
(electric-indent-mode -1)

(setq register-preview-delay nil)

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

;; kill-ring
(setq kill-ring-max 30)


;;; load-paths
;; For Info paths see:
;; [EVAL] (getenv "INFOPATH")
(tv-require 'info)
(add-to-list 'Info-directory-list "/usr/local/share/info")
(add-to-list 'Info-directory-list "/usr/share/info")
(add-to-list 'Info-directory-list "~/elisp/info")
(add-to-list 'Info-directory-list "~/elisp/info/eshell-doc")

(dolist (i '("/usr/local/share/emacs/site-lisp"
             "/usr/local/share/emacs/site-lisp/auctex"
             "/usr/local/share/emacs/site-lisp/mu4e"
	     "~/elisp/"
             "~/elisp/emacs-w3m"
             "~/elisp/Emacs-wgrep"
             "~/elisp/auctex"
             "~/elisp/auctex/preview"
	     "~/elisp/autoconf-mode"
	     "~/elisp/cmake"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
             "~/elisp/undo-tree"
             "~/elisp/emacs-git-gutter"
             "~/elisp/iedit"
             "~/elisp/emacs-wgrep"
	     "~/elisp/tex-utils"
	     "~/elisp/muse/lisp"
	     "~/elisp/muse/contrib"
             "~/elisp/emms/lisp/"
	     "~/elisp/ledger/"
             "~/elisp/helm"
             "~/elisp/helm-extensions"
             "~/elisp/google-maps"
             "~/elisp/slime"
             "~/elisp/slime/contrib"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config/"
             "~/elisp/emacs-async"
             "~/elisp/elscreen"
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
(tv-require 'w3m-wget)

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;;; Melpa marmalade
;;
(package-initialize)
(setq package-archives '(
                         ;("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))


;;; Require's
;;
;;
(tv-require 'auth-source)
(tv-require 'epa-file)
(tv-require 'init-helm-thierry)
(tv-require 'bookmark-extensions)
(tv-require 'bookmark-firefox-handler)
(tv-require 'firefox-protocol)
(tv-require 'addressbook-bookmark)
(tv-require 'org-config-thierry)
(autoload 'magit-status "magit.el" nil t)
(autoload 'magit-blame-mode "magit-blame.el" nil t)
(tv-require 'emms-mpd-config)
(tv-require 'dired-extension)
(tv-require 'htmlize)
(tv-require 'no-word)
(tv-require 'flymake)
(tv-require 'esh-toggle)
(tv-require 'slime-autoloads)
(tv-require 'cl-info)
(autoload 'ioccur "ioccur.el" nil t)
(tv-require 'mb-depth)
(tv-require 'tv-utils)
(tv-require 'ledger-config)
(tv-require 'rectangle-utils)
(tv-require 'smallurl)
(autoload 'zop-to-char "zop-to-char.el" nil t)
(autoload 'zop-up-to-char "zop-to-char.el" nil t)
(tv-require 'iedit)
(tv-require 'iedit-rect)
;(tv-require 'simple-call-tree)
(tv-require 'lacarte)
(autoload 'google-maps "google-maps.el" nil t)
(tv-require 'iterator)
(autoload 'markdown-mode "markdown-mode.el")
(autoload 'gfm-mode "markdown-mode.el")
(autoload 'boxquote-region "boxquote.el" nil t)
(autoload 'psession-mode "psession.el")
(tv-require 'wgrep-helm)
(when (tv-require 'dired-aux)
  (tv-require 'dired-async))
(tv-require 'smtpmail-async)
(tv-require 'async-bytecomp)
(setq aync-debug t)
(autoload 'golden-ratio-mode "golden-ratio.el" nil t)
(autoload 'emamux:send-command "emamux.el" nil t)
(autoload 'emamux:copy-kill-ring "emamux.el" nil t)
(autoload 'emamux:yank-from-list-buffers "emamux.el" nil t)
(tv-require 'config-w3m)
(tv-require 'mu4e-config)

(setq emamux:completing-read-type 'helm)


;;; Gnus-config
;;;
;;
;;
(setq gnus-asynchronous t)
;(setq mail-user-agent 'gnus-user-agent)
;(setq read-mail-command 'gnus)
;(setq send-mail-command 'gnus-msg-mail)
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
  (= 0 (call-process "ping" nil nil nil "-c1" "-W10" "-q" host)))

(defun tv-gnus (arg)
  "Start Gnus.
If Gnus have been started and a *Group* buffer exists,
switch to it, otherwise check if a connection is available and
in this cl-case start Gnus plugged, otherwise start it unplugged."
  (interactive "P")
  (let ((buf (get-buffer "*Group*")))
    (if (buffer-live-p buf)
        (switch-to-buffer buf)
        (if (or arg (not (quickping "imap.gmail.com")))
            (gnus-unplugged)
            (gnus)))))

;; Borred C-g'ing all the time and hanging emacs
;; while in gnus (while tethering or not).
;; Kill all nnimap/nntpd processes when exiting summary.
(defun tv-gnus-kill-all-procs ()
  (cl-loop for proc in (process-list)
        when (string-match "\\*?\\(nnimap\\|nntpd\\)" (process-name proc))
        do (delete-process proc)))
(add-hook 'gnus-exit-group-hook 'tv-gnus-kill-all-procs)
(add-hook 'gnus-group-catchup-group-hook 'tv-gnus-kill-all-procs)

(autoload 'gnus-dired-attach "gnus-dired.el")
(declare-function 'gnus-dired-attach "gnus-dired.el" (files-to-attach))
(define-key dired-mode-map (kbd "C-c C-a") 'gnus-dired-attach)

;; Auth-source
(setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))


;; Use helm-occur as default but fallback to ioccur when helm is broken
(defun tv-helm-or-ioccur (force)
  (interactive "P")
  (if force
      (ioccur)
      (condition-case nil
          (helm-occur)
        (error (ioccur)))))

;;; iedit
;;
;;
(defun iedit-narrow-to-end (arg)
  (interactive "P")
  (require 'iedit)
  (save-restriction
    (narrow-to-region (point-at-bol) (point-max))
    (iedit-mode arg)))

(defun iedit-narrow-to-defun (arg)
  (interactive "P")
  (require 'iedit)
  (save-restriction
    (narrow-to-defun)
    (iedit-mode arg)))

;;; Run or hide shell
(defun tv-shell ()
  (interactive)
  (if (eq major-mode 'shell-mode)
      (bury-buffer) (shell)))


;;; Global keys
;;
;;
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      #'(lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
(global-set-key (kbd "<f7> m")                     'tv-gnus)
(global-set-key (kbd "<f8>")                       'mu4e)
(global-set-key (kbd "<f7> j")                     'webjump)
(global-set-key (kbd "<f7> i")                     'erc-freenode-connect)
(global-set-key (kbd "<f7> g")                     'bitlbee)
(global-set-key (kbd "<f7> n")                     'newsticker-show-news)
(global-set-key (kbd "<f11> e c")                  'eshell-toggle-cd)
(global-set-key (kbd "<f11> e t")                  'eshell-toggle)
(global-set-key (kbd "<f11> s h")                  'tv-shell)
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
(global-set-key [remap occur]                      'ioccur) ; M-s o
(global-set-key (kbd "C-s")                        'helm-occur);'tv-helm-or-ioccur)
(global-set-key (kbd "M-s s")                      'isearch-forward)
(global-set-key (kbd "C-c C-o")                    'ioccur-find-buffer-matching)
(global-set-key (kbd "<M-down>")                   'tv-scroll-down)
(global-set-key (kbd "<M-up>")                     'tv-scroll-up)
(global-set-key (kbd "<C-M-down>")                 'tv-scroll-other-down)
(global-set-key (kbd "<C-M-up>")                   'tv-scroll-other-up)
(global-set-key (kbd "<C-prior>")                  'text-scale-decrease) ; font size.
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
(global-set-key [remap zap-to-char]                'zop-to-char)
(global-set-key (kbd "<f5> g m")                   'google-maps)
(global-set-key (kbd "M-\"")                       'tv-insert-double-quote)
(global-set-key (kbd "C-M-\`")                     'tv-insert-double-backquote)
;(global-set-key (kbd "M-\[")                       'tv-insert-vector)
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
(global-set-key (kbd "C-x r a")                    'tv-append-to-register)
(global-set-key (kbd "C-c t r")                    'translate-at-point)
(global-set-key (kbd "<f5> c")                     'tv-toggle-calendar)
(global-set-key (kbd "C-h C-e")                    'tv-tail-echo-area-messages)
(global-set-key [remap kill-whole-line]            'tv-kill-whole-line)
(global-set-key (kbd "M-e")                        'tv-eval-last-sexp-at-eol)
(global-set-key (kbd "C-d")                        'tv-delete-char)
(global-set-key (kbd "C-x C-'")                    'tv-toggle-resplit-window)
(global-set-key (kbd "C-§")                        'iedit-narrow-to-end)
(global-set-key (kbd "C-²")                        'iedit-narrow-to-defun)
(defun goto-scratch () (interactive) (switch-to-buffer "*scratch*"))
(global-set-key (kbd "<f11> s c")                  'goto-scratch)


;;; Elscreen
;;
(when (locate-library "elscreen")
  (autoload 'elscreen-start "elscreen.el")
  (elscreen-start)
  (global-set-key (kbd "C-z l") 'helm-elscreen))


;;; Themes
;;
;;
(defvar tv-theme-directory "~/.emacs.d/themes/")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv-theme-directory))

;; Load my favourite theme.
(add-hook 'emacs-startup-hook #'(lambda () (load-theme 'naquadah)))

;; column-number
(column-number-mode 1)


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
(recentf-mode 1)


;;; Frame and window config.
;;
;;
;; My current-font: [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:   [EVAL]: (progn (when (require 'helm-font) (helm 'helm-source-xfonts)))
;; Choose a color:  [EVAL]: (progn (when (require 'helm-color) (helm 'helm-source-colors)))
;; To reload .Xresources [EVAL]: (shell-command xrdb "~/.Xresources")

(defvar tv-default-font (assoc-default 'font (frame-parameters)))
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
                                (font . ,tv-default-font)
                                (cursor-color . "red")
                                (fullscreen . nil)
                                )))

;; Speedbar
(add-hook 'speedbar-load-hook
          #'(lambda ()
              (setq speedbar-frame-parameters
                    `((minibuffer . nil)
                      (font . ,tv-default-font)
                      (width . 20)
                      (fullscreen . nil) ; Not needed when fullscreen isn't set in .Xressources.
                      (left . ,(- (* (window-width) 8)
                                  (frame-width))) ; Speed-bar on right of screen.
                      (border-width . 0)
                      (menu-bar-lines . 0)
                      (tool-bar-lines . 0)
                      (unsplittable . t)
                      (left-fringe . 0)))))

;;; Emacs transparency.
;;
;;

(defun tv-transparency-modify (arg)
  "Increase Emacs frame transparency.
With a prefix arg decrease transparency."
  (interactive "P")
  (when (window-system)
    (let* ((ini-alpha (frame-parameter nil 'alpha))
           (def-alpha (or ini-alpha 80))
           (mod-alpha (if arg
                          (min (+ def-alpha 10) 100)
                          (max (- def-alpha 10)
                               frame-alpha-lower-limit)))) ; 20
      (modify-frame-parameters nil (list (cons 'alpha mod-alpha)))
      (message "Alpha[%s]" mod-alpha))))
(global-set-key (kbd "C-8") 'tv-transparency-modify)

;;; Special buffer display.
;;
;;
(setq special-display-regexps `(("\\*Help\\*"
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
                                ("\\*Compile-Log\\*"
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
                                ("\\*Dict\\*"
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
                                 (fullscreen . nil))
                                ))
;; Don't split this windows horizontally
(setq split-width-threshold nil)

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)


;;; Bookmarks
;;
;;
(setq bookmark-bmenu-toggle-filenames nil)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
(setq bookmark-automatically-show-annotations nil)
(eval-after-load "bookmark.el"
  (and (boundp 'bookmark-bmenu-use-header-line)
       (setq bookmark-bmenu-use-header-line nil)))
(setq bmkext-external-browse-url-function 'browse-url-firefox)
;(setq bmkext-jump-w3m-defaut-method 'external) ; Set to 'external to use external browser, w3m for w3m.
(eval-after-load "addressbook-bookmark.el"
  (addressbook-turn-on-mail-completion))

(defun tv-pp-bookmark-alist ()
  "Quickly print `bookmark-alist'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pp-bookmark-alist*"))
  (erase-buffer)
  (dolist (i bookmark-alist)
    (pp i (current-buffer))))

;;; Browse url
;;
;;
(setq browse-url-browser-function 'browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

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

;;; Dired
;;
;
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
(setq dired-backup-overwrite nil) ; nil, always, ask.
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
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
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
(setq global-mark-ring-max 120)

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

;; Copy/paste
(setq select-active-regions t)

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
  (defun eldoc-highlight-function-argument (sym args index)
    "Highlight argument INDEX in ARGS list for function SYM.
In the absence of INDEX, just call `eldoc-docstring-format-sym-doc'."
    (let ((start          nil)
          (end            0)
          (argument-face  'eldoc-highlight-function-argument)
          (args-lst (mapcar (lambda (x)
                              (replace-regexp-in-string
                               "\\`[(]\\|[)]\\'" "" x))
                            (split-string args))))
      ;; Find the current argument in the argument string.  We need to
      ;; handle `&rest' and informal `...' properly.
      ;;
      ;; FIXME: What to do with optional arguments, like in
      ;;        (defun NAME ARGLIST [DOCSTRING] BODY...) case?
      ;;        The problem is there is no robust way to determine if
      ;;        the current argument is indeed a docstring.

      ;; When `&key' is used finding position based on `index'
      ;; would be wrong, so find the arg at point and determine
      ;; position in ARGS based on this current arg.
      (when (string-match "&key" args)
        (let* (case-fold-search
               key-have-value
               (sym-name (symbol-name sym))
               (cur-w (current-word))
               (args-lst-ak (cdr (member "&key" args-lst)))
               (limit (save-excursion
                        (when (re-search-backward sym-name nil t)
                          (match-end 0))))
               (cur-a (if (and cur-w (string-match ":\\([^ ()]*\\)" cur-w))
                          (substring cur-w 1)
                          (save-excursion
                            (let (split)
                              (when (re-search-backward ":\\([^()\n]*\\)" limit t)
                                (setq split (split-string (match-string 1) " " t))
                                (prog1 (car split)
                                  (when (cdr split)
                                    (setq key-have-value t))))))))
               ;; If `cur-a' is not one of `args-lst-ak'
               ;; assume user is entering an unknow key
               ;; referenced in last position in signature.
               (other-key-arg (and (stringp cur-a)
                                   args-lst-ak
                                   (not (member (upcase cur-a) args-lst-ak))
                                   (upcase (car (last args-lst-ak))))))
          (unless (string= cur-w sym-name)
            ;; The last keyword have already a value
            ;; i.e :foo a b and cursor is at b.
            ;; If signature have also `&rest'
            ;; (assume it is after the `&key' section)
            ;; go to the arg after `&rest'.
            (if (and key-have-value
                     (save-excursion
                       (not (re-search-forward ":.*" (point-at-eol) t)))
                     (string-match "&rest \\([^ ()]*\\)" args))
                (setq index nil ; Skip next block based on positional args.
                      start (match-beginning 1)
                      end   (match-end 1))
                ;; If `cur-a' is nil probably cursor is on a positional arg
                ;; before `&key', in this case, exit this block and determine
                ;; position with `index'.
                (when (and cur-a ; A keyword arg (dot removed) or nil.
                           (or (string-match
                                (concat "\\_<" (upcase cur-a) "\\_>") args)
                               (string-match
                                (concat "\\_<" other-key-arg "\\_>") args)))
                  (setq index nil ; Skip next block based on positional args.
                        start (match-beginning 0)
                        end   (match-end 0)))))))
      ;; Handle now positional arguments.
      (while (and index (>= index 1))
        (if (string-match "[^ ()]+" args end)
            (progn
              (setq start (match-beginning 0)
                    end   (match-end 0))
              (let ((argument (match-string 0 args)))
                (cond ((string= argument "&rest")
                       ;; All the rest arguments are the same.
                       (setq index 1))
                      ((string= argument "&optional"))         ; Skip.
                      ((string= argument "&allow-other-keys")) ; Skip.
                      ;; Back to index 0 in ARG1 ARG2 ARG2 ARG3 etc...
                      ;; like in `setq'.
                      ((or (and (string-match-p "\\.\\.\\.$" argument)
                                (string= argument (car (last args-lst))))
                           (and (string-match-p "\\.\\.\\.$"
                                                (substring args 1 (1- (length args))))
                                (= (length (remove "..." args-lst)) 2)
                                (> index 1) (oddp index)))
                       (setq index 0))
                      (t
                       (setq index (1- index))))))
            (setq end           (length args)
                  start         (1- end)
                  argument-face 'font-lock-warning-face
                  index         0)))
      (let ((doc args))
        (when start
          (setq doc (copy-sequence args))
          (add-text-properties start end (list 'face argument-face) doc))
        (setq doc (eldoc-docstring-format-sym-doc
                   sym doc (if (functionp sym) 'font-lock-function-name-face
                               'font-lock-keyword-face)))
        doc)))
  
  (when (fboundp 'eldoc-function-argstring-format)
    (defun eldoc-function-argstring-format (argstring)
      "Apply `eldoc-argument-case' to each word in ARGSTRING.
The words \"&rest\", \"&optional\", \"&key\" and \"&allow-other-keys\"
are returned unchanged."
      (mapconcat
       (lambda (s)
         (if (string-match-p
              "\\`(?&\\(?:optional\\|rest\\|key\\|allow-other-keys\\))?\\'" s)
             s
           (funcall eldoc-argument-case s)))
       (split-string argstring) " "))))

;; Tooltip face
(set-face-attribute 'tooltip nil
                    :foreground "black"
                    :background "NavajoWhite"
                    :family "unknown-DejaVu Sans Mono-bold-normal-normal"
		    :underline t)

(autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
(eldoc-in-minibuffer-mode 1)
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
(tv-require 'helm-ipython)
(define-key python-mode-map (kbd "<M-tab>") 'helm-ipython-complete)
(define-key python-mode-map (kbd "C-c C-i") 'helm-ipython-import-modules-from-buffer)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --autoindent"
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

(add-hook 'python-mode-hook
  #'(lambda ()
      (define-key python-mode-map (kbd "C-m") 'newline-and-indent)))

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
;;;

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
  (defalias 'eshell-pcomplete 'pcomplete))
(unless (fboundp 'eshell-complete-lisp-symbol)
  (defalias 'eshell-complete-lisp-symbol 'lisp-complete-symbol))

(add-hook 'eshell-mode-hook #'(lambda ()
                                (setq eshell-pwd-convert-function (lambda (f)
                                                                    (if (file-equal-p (file-truename f) "/")
                                                                        "/" f)))
                                ;; helm completion with pcomplete
                                (setq eshell-cmpl-ignore-case t)
                                (eshell-cmpl-initialize)
                                (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                ;; helm lisp completion
                                (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
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
  (dolist (i '("tmux" "htop" "ipython" "alsamixer" "git-log"))
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
(defadvice term-sentinel (after kill-buffer activate)
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
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(tv-require 'xdvi-search)

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
       "\\address{Thierry Volpiatto \\\\ 430 Chemin des Amandiers \\\\ 83330 Le Beausset}% Adresse de l'expéditeur\n"
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

(add-hook 'newsticker-mode-hook
          (lambda ()
            (define-key newsticker-mode-map (kbd "Q") 'newsticker-quit-and-stop)
            (define-key newsticker-mode-map (kbd "b") 'newsticker-previous-feed)))


;;; Tramp-config
;;
;;
(tv-require 'tramp)
(setq tramp-default-method "ssh") ; methode par defaut
(setq tramp-verbose 6) ; See `helm-tramp-verbose' in init-helm.

;; Android settings (Only available on trunk)
;;
(when (boundp 'tramp-connection-properties)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "192.168.0.24") "remote-shell" "sh"))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "zte") "remote-shell" "sh"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (pushnew "/system/xbin" tramp-remote-path :test 'equal)
  (add-to-list 'tramp-remote-process-environment "TMPDIR=$HOME/tmp"))

;; No messages
(setq tramp-message-show-message nil)

;; Allow connecting as root on all remote Linux machines except this one.
;; Use e.g /sudo:host:/path
(add-to-list 'tramp-default-proxies-alist
             '("\\`thievol\\'" "\\`root\\'" "/ssh:%h:"))

(add-to-list 'tramp-default-proxies-alist
             '("\\`thievolrem\\'" "\\`root\\'" "/ssh:%h:"))

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
;; Allow scrolling horizontally in large images
(add-hook 'image-mode-hook #'(lambda () (set (make-variable-buffer-local 'auto-hscroll-mode) nil)))


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
             (cl-macrolet . flet)
             )))
    (dolist (el l)
      (put (car el) 'common-lisp-indent-function
           (if (symbolp (cdr el))
               (get (cdr el) 'common-lisp-indent-function)
               (car (cdr el)))))))

;; cl- prefixed symbols are not font-locked in emacs-24.3 and also many in 24.4.
;; (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
;;   (font-lock-add-keywords
;;    mode
;;    '(("(\\<\\(cl-flet[*]?\\|cl-labels\\|cl-macrolet\\)\\>" 1 font-lock-keyword-face)
;;      ("(\\<\\(cl-loop\\|cl-dolist\\)\\>" 1 font-lock-keyword-face))))

;; Reenable font-locking for cl. (Removed in 24.3.50.1)
;; (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
;;   (font-lock-add-keywords
;;    mode
;;    '(("(\\<\\(flet[*]?\\|labels\\|symbol-macrolet\\|macrolet\\|loop\\|e?case\\|e?typecase\\)\\_>" 1 font-lock-keyword-face)
;;      ("(\\<\\(return-from\\|return\\|block\\)\\_>" 1 font-lock-keyword-face)
;;      ("(\\<\\(lexical-let[*]?\\|destructuring-bind\\)\\_>" 1 font-lock-keyword-face)
;;      ("(\\<\\(eval-when\\|declaim\\|proclaim\\)\\_>" 1 font-lock-keyword-face)
;;      ("(\\<\\(cl-assert\\)\\_>" 1 font-lock-warning-face)
;;      ("(\\<\\(defun[*]?\\|defmacro[*]?\\|defsubst[*]?\\|defstruct\\)\\_>" 1 font-lock-keyword-face)
;;      ("(\\<\\(defun[*]?\\|defmacro[*]?\\|defsubst[*]?\\)\\_>\\s-+\\<\\([^ ]*\\)\\>" 2 font-lock-function-name-face)
;;      ("(\\<\\(defstruct\\)\\_>\\s-+\\<\\([^ ]*\\)\\>" 2 font-lock-type-face))))

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
  (require 'slime)
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
(add-hook 'ioccur-save-pos-before-jump-hook 'ioccur-save-current-pos-to-mark-ring)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil)          ; C-x n n
(put 'narrow-to-page 'disabled nil)            ; C-x n p
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
(add-to-list 'display-time-world-list '("Australia/Melbourne" "Melbourne"))
(add-to-list 'display-time-world-list '("Australia/Canberra" "Canberra"))
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

;;; VC
;;
;;
;; Possible values: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
(setq vc-handled-backends '(RCS Hg Git))


;;; Redefine push-mark to update mark in global-mark-ring
;;
;;
(defun push-mark (&optional location nomsg activate)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil."
  (unless (null (mark t))
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (when (> (length mark-ring) mark-ring-max)
      (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (if (and global-mark-ring
           (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
      ;; The last global mark pushed was in this same buffer.
      ;; Don't push another one but update it.
      (setcar global-mark-ring (copy-marker (mark-marker)))
      (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
      (when (> (length global-mark-ring) global-mark-ring-max)
        (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
        (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (if (or activate (not transient-mark-mode))
      (set-mark (mark t)))
  nil)


;;; winner-mode config
;;
;;
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              "*esh command on file*"
                              ))

(when (tv-require 'winner)
  (defvar winner-boring-buffers-regexp "\\*[hH]elm.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (cl-loop for buf in (mapcar 'cdr (cdr conf))
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
              (cl-loop for buf in buffers
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


;;; markdown-mode
;;
;;
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdpp$" . markdown-mode))

;;; Semantic
;;
;;
;; (semantic-mode 1)
;; With my fixes in lisp/cedet/semantic/bovine/el.el.
;; (load-file "~/elisp/el.el")
;; (when (fboundp 'semantic-default-elisp-setup) (semantic-default-elisp-setup))


;;; Ffap
;;
;;
;; Tramp/ange behave badly in 99.9% of the time for ftp, disable.
(setq ffap-url-unwrap-remote (remove "ftp" ffap-url-unwrap-remote))

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

;;; git-gutter-mode
;;
(autoload 'git-gutter-mode "git-gutter")
(add-hook 'emacs-lisp-mode-hook 'git-gutter-mode)
(tv/define-key-with-subkeys
 global-map (kbd "C-x v n") ?n 'git-gutter:next-hunk ((?p . 'git-gutter:previous-hunk)))
(tv/define-key-with-subkeys
 global-map (kbd "C-x v p") ?p 'git-gutter:previous-hunk ((?n . 'git-gutter:next-hunk)))

(global-set-key [remap vc-dir] 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key [remap vc-create-tag] 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;;; Golden ratio
;;
(defun helm-running-p () helm-alive-p)
(setq golden-ratio-inhibit-functions '(helm-running-p))
(setq golden-ratio-exclude-modes '("ediff-mode"))
(setq golden-ratio-exclude-buffer-names '("*helm marked*"))
(setq golden-ratio-recenter t)
(add-hook 'ediff-before-setup-windows-hook #'(lambda () (golden-ratio-mode -1)))
(add-hook 'ediff-quit-hook #'(lambda () (golden-ratio-mode 1)))
(golden-ratio-mode 1)

;;; Magit
;;
(setq magit-restore-window-configuration t)

;;; Report bug
;;
(setq report-emacs-bug-no-explanations t)

(defun tv-find-or-kill-gnu-bug-number (bug-number arg)
  (interactive (list (read-number "Bug number: " (thing-at-point 'number))
                     current-prefix-arg))
  (let ((url (format "http://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s" bug-number)))
    (if arg
        (progn
          (kill-new url)
          (message "Bug `#%d' url's copied to kill-ring" bug-number))
        (browse-url url))))

;;; Info
;;
(defface tv-info-ref-item
    '((((background dark)) :background "DimGray" :foreground "Gold")
      (((background light)) :background "firebrick" :foreground "LightGray"))
  "Face for item stating with -- in info." :group 'Info :group 'faces)

(defvar tv-info-title-face 'tv-info-ref-item)
(defvar tv-info-underline 'underline)
(defvar info-unicode-quote-start (string 8216))
(defvar info-unicode-quote-end (string 8217))
(defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
                                           info-unicode-quote-start
                                           info-unicode-quote-start
                                           info-unicode-quote-end
                                           info-unicode-quote-end
                                           ))
(defun tv-font-lock-doc-rules ()
  (font-lock-add-keywords
   nil `(("[^\\s\][`]\\([^`']+\\)[`']?[^\\s\][']?" 1 font-lock-type-face)
         (,info-unicode-quoted-regexp 1 font-lock-type-face)
         ("^ --.*$" . tv-info-title-face)
         ("[_]\\([^_]+\\)[_]" 1 tv-info-underline)
         ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
         ("\\*Warning:\\*" . font-lock-warning-face)
         ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
         ("^[[:upper:]]+ ?$" . font-lock-comment-face)
         ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
         )))

(add-hook 'Info-mode-hook 'tv-font-lock-doc-rules)

;;; Outline-mode bindings
(tv/define-key-with-subkeys outline-mode-map (kbd "C-c C-p")
                            ?p 'outline-previous-visible-heading
                            ((?n . 'outline-next-visible-heading)))
(tv/define-key-with-subkeys outline-mode-map (kbd "C-c C-n")
                            ?n 'outline-next-visible-heading
                            ((?p . 'outline-previous-visible-heading)))
(tv/define-key-with-subkeys outline-mode-map (kbd "C-c C-f")
                            ?f 'outline-forward-same-level
                            ((?b . 'outline-backward-same-level)))
(tv/define-key-with-subkeys outline-mode-map (kbd "C-c C-b")
                            ?b 'outline-backward-same-level
                            ((?f . 'outline-forward-same-level)))

;;; Be sure to reenable touchpad when quitting emacs
(add-hook 'kill-emacs-hook #'(lambda ()
                               (and (executable-find "reenable_touchpad.sh")
                                    (shell-command "reenable_touchpad.sh"))))

;;; Save/restore emacs-session
;;
;;
(psession-mode 1)

;;; Link scratch buffer to file
;;
;;
(add-hook 'emacs-startup-hook 'tv-restore-scratch-buffer)

;;; .emacs.el ends here
