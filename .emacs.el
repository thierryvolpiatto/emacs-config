;;; .emacs.el --- emacs configuration

;;; Code:

(require 'cl-lib)

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;;; VC
;;
;; Possible values for vc backends: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
(setq vc-handled-backends '(RCS Hg Git))

;;; Melpa marmalade
;;
(when (and (= emacs-major-version 24)
           (not (version< emacs-version "24.4.1")))
  (add-to-list 'load-path "~/.emacs.d/emacs-config/")
  ;; Load the emacs-25 package.el version adapted to emacs-24.
  (load "package-24"))

(package-initialize)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ))

(setq package-pinned-packages '((async . "melpa")))

;;; Use-package.
;;
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

;;; Global settings
;;
;; confirm-quit-emacs
(setq confirm-kill-emacs 'y-or-n-p)

(setq case-fold-search t)

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Affiche-l'heure-au-format-24h
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)
(setq display-time-use-mail-icon t)

;; Limite-max-lisp
(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

;; Annoyances section
;;
(global-set-key (kbd "<f11>") nil)
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (when (get-buffer "*Compile-Log*")
                                    (kill-buffer "*Compile-Log*")
                                    (delete-other-windows))))

;; Disable uniquify enabled by default in 24.4.
(setq uniquify-buffer-name-style nil)

;; electric-indent-mode
(electric-indent-mode -1)

(setq register-preview-delay nil)

;; No-startup-screen
(setq inhibit-startup-message t)

;; consequent-log-file
(setq message-log-max 1000)

;; kill-ring
(setq kill-ring-max 60)

;; mark ring
(setq mark-ring-max 60)

;; Kill emacs
(defun tv-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
      (save-buffers-kill-terminal)))

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; column-number in mode-line.
(column-number-mode 1)

;; Environment variables
;;
;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")
(prefer-coding-system 'utf-8)

;; Save-minibuffer-history
(setq savehist-file "~/.emacs.d/history"
      history-delete-duplicates t)
(setq history-length 100) ; default is 30.
(savehist-mode 1)

;; Themes
(defvar tv-theme-directory "~/.emacs.d/themes/")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv-theme-directory))

;; Load my favourite theme.
(add-hook 'emacs-startup-hook #'(lambda () (load-theme 'naquadah)))

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
    (setq default-frame-alist `((vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (title . ,(format "Emacs-%s" emacs-version))
                                (font . "-unknown-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                                (cursor-color . "red")))

    (setq default-frame-alist `((foreground-color . "Wheat")
                                (background-color . "black")
                                (alpha . 90)
                                ;; New frames go in right corner.
                                (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                                (vertical-scroll-bars . nil)
                                (title . ,(format "Emacs-%s" emacs-version))
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

;;; Special buffer display.
;;
;;
(setq special-display-regexps `(("\\*Help"
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
                                ("\\*Compile-Log"
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
                                 (background-color . "Brown4")
                                 (foreground-color . "black")
                                 (alpha . nil)
                                 (fullscreen . nil))
                                ("\\*Dict"
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

;; Browse url
;;
;;
(setq browse-url-browser-function 'helm-browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

;;; Ediff
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; emacs-backup-config
;;
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; show-paren-mode
;;
(show-paren-mode 1)
(setq show-paren-ring-bell-on-mismatch t)

;; Start-emacs-server
;;
(add-hook 'after-init-hook #'(lambda ()
                               (unless (daemonp)
                                 (server-start)
                                 (setq server-raise-frame t))))


;; Path-to-abbrev-file
(setq abbrev-file-name "/home/thierry/.emacs.d/.abbrev_defs")

;; Copy/paste
(setq select-active-regions t)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'narrow-to-page 'disabled nil)   ; C-x n p
(put 'scroll-left 'disabled nil)     ; C-x > or <
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'upcase-region 'disabled nil)   ; C-x C-u
(put 'set-goal-column 'disabled nil) ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired

;; setup-minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Woman/man
(setq woman-use-own-frame nil)
(setq Man-notify-method 'pushy)
(defface man-args-face '((t (:foreground "Magenta" :underline t)))
  "*Face used in man page to show arguments and sections."
  :group 'man)

;; Printing
(setq lpr-command "gtklp")
(setq printer-name "EpsonStylus")
(setq-default ps-print-header nil)
(setq ps-font-size   '(10 . 11.5))
(setq ps-font-family 'Courier)

;; auto-compression-mode
(auto-compression-mode 1)

;; Mode-lecture-photo-auto
(auto-image-file-mode 1)

;; Allow scrolling horizontally in large images
(add-hook 'image-mode-hook #'(lambda () (set (make-variable-buffer-local 'auto-hscroll-mode) nil)))

;; line-move-visual.
(setq line-move-visual nil)

;; Rst-mode
(add-hook 'rst-mode-hook 'auto-fill-mode)

;; Trash
;; (setq delete-by-moving-to-trash t)

;; Minibuffers completion
(setq completion-cycle-threshold t) ; always cycle, no completion buffers.

;; Diff
(customize-set-variable 'diff-switches "-w")

;; Report bug
(setq report-emacs-bug-no-explanations t)

;; Indent-only-with-spaces
(setq-default indent-tabs-mode nil)

;; Prompt shell read only
(setq comint-prompt-read-only t)

;; Newline and indent in `sh-mode'.
(add-hook 'sh-mode-hook #'(lambda ()
                            (define-key sh-mode-map (kbd "RET") 'newline-and-indent)))

;; winner-mode config
(setq winner-boring-buffers '("*Completions*"
                              "*Compile-Log*"
                              "*inferior-lisp*"
                              "*Fuzzy Completions*"
                              "*Apropos*"
                              "*Help*"
                              "*cvs*"
                              "*Buffer List*"
                              "*Ibuffer*"
                              ))

(winner-mode 1)

;; Display time in mode-line
(setq display-time-string-forms
      '( ;; date
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
          'help-echo (format-time-string " %a %b %e, %Y" now))
         (and time-zone " (") time-zone (and time-zone ")")
         "]")
        ;; cpu load average
        ;; (if (and load (not (string= load "")))
        ;;     (format "cpu:%s" load) "")
        ""
        ;; mail
        ""))

;; Mode-line
(set-face-attribute 'mode-line-emphasis nil :foreground "red")

;; World-time
(add-to-list 'display-time-world-list '("Greenwich" "Greenwich"))
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
(add-to-list 'display-time-world-list '("Europe/Moscow" "Moscow"))
(add-to-list 'display-time-world-list '("Europe/Berlin" "Berlin"))
(add-to-list 'display-time-world-list '("Europe/Oslo" "Oslo"))
(add-to-list 'display-time-world-list '("Europe/Lisbon" "Lisbon"))
(add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
(add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
(add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
(add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
(add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))

;; flyspell-aspell
(setq-default ispell-program-name "aspell")
(setq ispell-local-dictionary "francais")

;; Kill buffer after C-d in ansi-term.
(defadvice term-sentinel (after kill-buffer activate)
  (kill-buffer))

;; Require with messages to debug more easily.
(defun tv-require (feature &optional filename noerror)
  (message "Loading %s..." (symbol-name feature))
  (condition-case err
      (if (require feature filename noerror)
          (message "Loading %s done" (symbol-name feature))
          (message "Loading %s Failed" (symbol-name feature)))
    (error
     (signal 'error (list feature (car err) (cadr err))))))


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

;; Fix compatibility with emacs 24.3.
;; Avoid rebuilding all the autoloads just for this when switching to 24.3.
(unless (fboundp 'function-put)
  (defalias 'function-put
      ;; We don't want people to just use `put' because we can't conveniently
      ;; hook into `put' to remap old properties to new ones.  But for now, there's
      ;; no such remapping, so we just call `put'.
      #'(lambda (f prop value) (put f prop value))
    "Set function F's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, F can only be a symbol, not a lambda expression."))


;;; Temporary Bugfixes until fixed upstream.
;;

(defadvice term-command-hook (before decode-string)
  (setq string (decode-coding-string string locale-coding-system)))

(when (version< emacs-version "24.3.50.1") (ad-activate 'term-command-hook))

;; Speedup `describe-variable' for variables with huge value description.
(defun tv/describe-variable (old-fn &rest args)
  ;; `cl-flet' can't be used here because `pp' should
  ;; appear lexically in its body, which is not the case.
  ;; Using `flet' is an option, but even better is binding
  ;; (symbol-function 'pp) with `cl-letf'.
  (cl-letf (((symbol-function 'pp)
             (lambda (object &optional stream)
               (let ((fn (lambda (ob &optional stream)
                           (princ (pp-to-string ob)
                                  (or stream standard-output))
                           (terpri)))
                     (print-circle t))
                 (if (listp object)
                     (progn
                       (insert "\n(")
                       (mapc fn object)
                       (cl-letf (((point) (1- (point))))
                         (insert ")")))
                     (funcall fn object stream))))))
    (apply old-fn args)))
(advice-add 'describe-variable :around #'tv/describe-variable)


;;; load-path
;;
(dolist (i '("/usr/local/share/emacs/site-lisp"
             "/usr/local/share/emacs/site-lisp/mu4e"
	     "~/elisp/"
             "~/elisp/google-maps.el"
             "~/elisp/Emacs-wgrep"
             "~/elisp/auctex"
             "~/elisp/auctex/preview"
	     "~/elisp/autoconf-mode"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
	     "~/elisp/tex-utils"
	     "~/elisp/ledger/"
             "~/elisp/helm"
             "~/elisp/helm-extensions"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config/"
	     ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;;; Info
;;
(use-package info
    :init
  (progn
    ;; Additional info directories
    (add-to-list 'Info-directory-list "/usr/local/share/info")
    (add-to-list 'Info-directory-list "/usr/share/info")
    (add-to-list 'Info-directory-list "~/elisp/info")
    (add-to-list 'Info-directory-list "~/elisp/info/eshell-doc")
    ;; Fancy faces in info.
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
             ("^[[:upper:],]\\{2,\\}.*$" . font-lock-comment-face)
             ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
             )))

    (add-hook 'Info-mode-hook 'tv-font-lock-doc-rules)))


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
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;;; desktop-file-utils site-lisp configuration
(add-to-list 'load-path "~/elisp/desktop-file-utils/")
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(add-to-list 'auto-mode-alist
             '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))
(add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)

;;; lua-mode site-lisp configuration
(autoload 'lua-mode "lua-mode" "Mode for editing Lua scripts" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(setq lua-default-application "/usr/bin/lua")

;;; emacs-wget site-lisp configuration
;;
;;
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(use-package w3m-wget)
;; Use wget in eshell.
(defun eshell/wget (url)
  (wget url))


;;; Use `net-utils-run-simple' in net-utils fns.
;;
(use-package net-utils
    :config
  (progn
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
       (list host)))))

;;; Async
;;
(use-package async
    :config
  (progn
    (defun tv/async-byte-compile-file (file)
      (interactive "fFile: ")
      (let ((proc
             (async-start
              `(lambda ()
                 (require 'bytecomp)
                 ,(async-inject-variables "\\`load-path\\'")
                 (let ((default-directory ,(file-name-directory file)))
                   (add-to-list 'load-path default-directory)
                   (byte-compile-file ,file))))))

        (unless (condition-case err
                    (async-get proc)
                  (error (ignore (message "Error: %s" (car err)))))
          (message "Recompiling %s...FAILED" file))))))

(use-package dired-async :config (dired-async-mode 1))
(use-package smtpmail-async
    :commands 'async-smtpmail-send-it)
(use-package async-bytecomp
    :config (setq async-bytecomp-allowed-packages '(all)))

;;; Helm
;;
(use-package helm
    :init (load "init-helm-thierry.el"))

;;; Firefox protocol
;;
(autoload 'firefox-protocol-installer-install "firefox-protocol" nil t)

;;; Org
;;
(use-package org :config (use-package org-config-thierry))

;;; Emms
;;
(use-package emms :config (use-package emms-vlc-config) :defer t)

;;; Dired
;;
(use-package dired
    :init (progn
            (setq dired-dwim-target t)
            (setq dired-auto-revert-buffer t)
            (setq dired-backup-overwrite nil) ; nil, always, ask.
            (setq dired-isearch-filenames 'dwim)
            (setq dired-listing-switches (purecopy "-alh")))
    :config (use-package dired-extension)
    :defer t)

;;; htmlize
;;
(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'htmlize-file "htmlize" nil t)
(autoload 'htmlize-many-files "htmlize" nil t)
(autoload 'htmlize-many-files-dired "htmlize" nil t)

;;; Cl-info
;;
(autoload 'cl-info "cl-info" nil t)

;;; Ioccur
;;
(use-package ioccur
    :commands (ioccur)
    :init
  (add-hook 'ioccur-save-pos-before-jump-hook 'ioccur-save-current-pos-to-mark-ring)
  :bind ([remap occur] . ioccur)) ; M-s o

;;; google-maps
;;
(use-package google-maps
    :init (setq google-maps-static-default-zoom 10)
    :bind ("<f5> g m" . google-maps))

;;; tv-utils fns
;;
(use-package tv-utils
    :commands (tv-eval-region tv/async-byte-compile-file)
    :init (progn
            (bind-key "C-M-!" 'tv-eval-region lisp-interaction-mode-map) 
            (bind-key "C-M-!" 'tv-eval-region emacs-lisp-mode-map))
    :bind (("M-\"" . tv-insert-double-quote)
           ("C-M-\`" . tv-insert-double-backquote)
           ("C-M-(" . tv-move-pair-forward)
           ("C-M-\"" . tv-insert-double-quote-and-close-forward)
           ("C-M-)" . tv-insert-pair-and-close-forward)
           ("<f5> r" . find-file-as-root)
           ("C-x r a" . tv-append-to-register)
           ("C-c t r" . translate-at-point)
           ("<f5> c" . tv-toggle-calendar)
           ("C-h C-e" . tv-tail-echo-area-messages)
           ([remap kill-whole-line] . tv-kill-whole-line)
           ("M-e" . tv-eval-last-sexp-at-eol)
           ([remap delete-char] . tv-delete-char)
           ("C-x C-'" . tv/split-windows)
           ("C-<" . other-window-backward)
           ("C->" . other-window-forward)
           ([C-left] . screen-top)
           ([C-right] . screen-bottom)))

;;; Ledger
;;
(use-package ledger
    :init (setenv "LEDGER_PAGER" "cat")
    :commands 'ledger-mode
    :config (use-package ledger-config))

;;; Rectangle
;;
(use-package rectangle-utils
    :commands (rectangle-menu
               copy-rectangle
               rectangle-insert-at-right
               extend-rectangle-to-end)
    :bind (("C-x r e" . extend-rectangle-to-end)
           ("C-x r h" . rectangle-menu)
           ("C-x r <right>" . rectangle-insert-at-right)))

;;; Smallurl
;;
(use-package smallurl
    :commands (smallurl smallurl-replace-at-point))

;;; Zop-to-char
;;
(use-package zop-to-char
    :commands (zop-to-char zop-up-to-char)
    :init
    (progn
      (setq zop-to-char-prec-keys '(left ?\C-b ?\M-a)
            zop-to-char-next-keys '(right ?\C-f ?\M-e)))
    :bind ([remap zap-to-char] . zop-to-char))

;;; Iedit
;;
(use-package iedit
    :init
  (progn
    (bind-key "C-;" 'iedit-mode)
    (bind-key "C-²" 'iedit-mode-toggle-on-function)))

(use-package iedit-rect
    :init (bind-key [C-return] 'iedit-rectangle-mode)
    :commands (iedit-rectangle-mode))

;;; Lacarte
;;
(autoload 'lacarte-get-overall-menu-item-alist "lacarte")

;;; Iterator
;;
(use-package iterator)

;;; psession
;;
(use-package psession
    :config (psession-mode 1))

;;; Golden-ratio
;;
(use-package golden-ratio
    :init
  (progn
    (add-hook 'ediff-before-setup-windows-hook #'(lambda () (golden-ratio-mode -1)))
    (add-hook 'ediff-quit-hook #'(lambda () (golden-ratio-mode 1))))
  :config
  (progn
    (defun helm-running-p () helm-alive-p)
    (setq golden-ratio-inhibit-functions '(helm-running-p))
    (setq golden-ratio-exclude-buffer-regexp '("\\`\\*[Hh]elm.*\\*\\'"))
    (setq golden-ratio-exclude-modes '(ediff-mode calendar-mode wget-mode
                                       gnus-summary-mode gnus-article-mode))
    (setq golden-ratio-recenter t)
    (golden-ratio-mode 1)))

;;; pcomplete
;;
(use-package pcomplete-extension)

;;; Xmodmap
;;
(use-package xmodmap)

;;; Migemo
;;
(use-package migemo
    :init
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "-e"))
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-isearch-enable-p nil)))

;;; Magit
;;
(use-package magit
    :init
  (progn
    (setq magit-status-buffer-name-format "*magit status: %a*")
    (setq magit-restore-window-configuration t)
    (setq git-commit-fill-column 120)
    (setq git-commit-summary-max-length 80)
    (setq auto-revert-verbose nil)
    (setq magit-revision-show-gravatars nil)
    (setq magit-uniquify-buffer-names nil))
  :config
  (bind-key "C" 'magit-commit-add-log magit-diff-mode-map)
  (bind-key "C-]" 'magit-toggle-margin magit-log-mode-map)
  :defer t)

;;; Emamux
;;
(use-package emamux
    :init (setq emamux:completing-read-type 'helm)
    :bind ("C-c y" . emamux:yank-from-list-buffers))

;;; Undo-tree
;;
(use-package undo-tree
    :init
  (progn
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))
    (setq undo-tree-auto-save-history t))
  :config
  (global-undo-tree-mode 1))

;;; Zoom-window
;;
(use-package zoom-window
    :init (setq zoom-window-mode-line-color "DarkGreen")
    :bind ("C-x C-z" . zoom-window-zoom))

;;; Recentf
;;
(use-package recentf
    :init
  (progn
    (setq recentf-save-file "~/.emacs.d/recentf")
    ;; `recentf-mode' will be started by helm when needed,
    ;; so no need to start it here
    (setq recentf-max-saved-items 100))
  :config
  (recentf-mode 1)
  :defer t)

;;; Eldoc
;;
(use-package eldoc
    :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode))
  :config
  ;; Don't load this on emacs-25
  (when (fboundp 'eldoc-highlight-function-argument)
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
                        ((string= argument "&optional")) ; Skip.
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
         (split-string argstring) " ")))))

(use-package eldoc-eval
    :config
  (progn
    (eldoc-in-minibuffer-mode 1)
    (defadvice edebug-eval-expression (around with-eldoc activate)
      "This advice enable eldoc support."
      (interactive (list (with-eldoc-in-minibuffer
                           (read-from-minibuffer
                            "Eval: " nil read-expression-map t
                            'read-expression-history))))
      ad-do-it)))

;;; Python config
;;
;;
;; (tv-require 'helm-ipython)
;; (define-key python-mode-map (kbd "<M-tab>") 'helm-ipython-complete)
;; (define-key python-mode-map (kbd "C-c C-i") 'helm-ipython-import-modules-from-buffer)

(use-package python
    :no-require t
    :init
  (progn
    (setq
     gud-pdb-command-name "ipdb"
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

    (when (fboundp 'jedi:setup)
      (add-hook 'python-mode-hook 'jedi:setup)))
  :config
  (progn
    (defun tv-insert-python-header ()
      "insert python header at point"
      (interactive)
      (insert "#!/usr/bin/env python\n"
              "# -*- coding: utf-8 -*-\n\n"
              "## Title: \n"
              "## Description: \n"
              "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
              "## Commentary:\n\n")))
  :bind ("<f11> p" . python-shell-switch-to-shell))

;;; xdvi (Needed in auctex)
;;
(use-package xdvi-search)

;;; Tramp-config
;;
(use-package tramp
    :no-require t
    :config
  (progn
    ;; scp is better for copying large files.
    (setq tramp-default-method "scp")
    ;; (setq tramp-verbose 6) ; See `helm-tramp-verbose' in init-helm.

    ;; Android settings (Only available on trunk)
    ;;
    (when (boundp 'tramp-connection-properties)
      (add-to-list 'tramp-connection-properties
                   (list (regexp-quote "192.168.0.24") "remote-shell" "sh"))
      (add-to-list 'tramp-connection-properties
                   (list (regexp-quote "zte") "remote-shell" "sh"))
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
      (cl-pushnew "/system/xbin" tramp-remote-path :test 'equal)
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
                 '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))))

;;; Ange-ftp
;;
(use-package ange-ftp
    :init
  (progn
    (setq ange-ftp-try-passive-mode t)
    (setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on"))))
  :no-require t)

;;; Calendar and diary
;;
(use-package calendar
    :config
  (progn
    (setq diary-file "~/.emacs.d/diary")
    (unless (fboundp 'fancy-diary-display) ; Fix emacs-25.
      (defalias 'fancy-diary-display 'diary-fancy-display))
    (setq calendar-date-style 'european)
    (setq calendar-mark-diary-entries-flag t)
    (setq calendar-mark-holidays-flag t)
    (setq holiday-bahai-holidays nil)
    (setq holiday-hebrew-holidays nil)
    (setq holiday-islamic-holidays nil)
    (setq holiday-oriental-holidays nil)

    (setq diary-display-function 'diary-fancy-display)
    (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    (add-hook 'initial-calendar-window-hook 'mark-diary-entries)
    (setq mark-holidays-in-calendar t)
    (setq diary-number-of-entries 4)

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
            (holiday-fixed 2 14 "Fête des amoureux")
            (holiday-fixed 5 1 "Fête du travail")
            (holiday-fixed 5 8 "Victoire")
            (holiday-float 5 0 -1 "Fête des Mères")
            (holiday-float 6 0 3 "Fête des Pères")
            (holiday-fixed 7 14 "Fête nationale")
            (holiday-fixed 8 15 "Assomption")
            (holiday-fixed 10 31 "Halloween")
            (holiday-easter-etc -47 "Mardi Gras")
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

    (setq calendar-holidays `(,@holiday-solar-holidays
                              ,@holiday-french-holidays))

    (defun tv/calendar-diary-or-holiday (arg)
      "A single command for diary and holiday entries."
      ;; Assume diary and holidays are shown in calendar.
      (interactive "p")
      (let* ((ovs (overlays-at (point)))
             (props (cl-loop for ov in ovs
                             for prop = (cadr (overlay-properties ov))
                             when (or (and (eq prop 'diary)
                                           'diary)
                                      (and (eq prop 'holiday)
                                           'holiday))
                             collect prop)))
        (cond ((and (memq 'diary props) (memq 'holiday props))
               (diary-view-entries arg)
               (calendar-cursor-holidays))
              ((memq 'diary props)
               (diary-view-entries arg))
              ((memq 'holiday props)
               (calendar-cursor-holidays))
              (t (message "Nothing special on this date")))))

    (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-month)
    (define-key calendar-mode-map (kbd "C-<left>")  'calendar-backward-month)
    (define-key calendar-mode-map (kbd "RET") 'tv/calendar-diary-or-holiday))
  :defer t)

;;; Bookmarks
;;
(use-package bookmark
    :no-require t
    :init
  (progn
    (add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
    (setq bookmark-bmenu-toggle-filenames nil)
    (setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
    (setq bookmark-automatically-show-annotations nil))
  :config
  (progn
    (and (boundp 'bookmark-bmenu-use-header-line)
         (setq bookmark-bmenu-use-header-line nil))

    (defun tv-pp-bookmark-alist ()
      "Quickly print `bookmark-alist'."
      (interactive)
      (switch-to-buffer (get-buffer-create "*pp-bookmark-alist*"))
      (erase-buffer)
      (dolist (i bookmark-alist)
        (pp i (current-buffer))))))

;;; git-gutter-mode
;;
(use-package git-gutter
    :init
  (progn
    (customize-set-variable 'git-gutter:update-interval 2) ; Activate live update timer.
    (setq git-gutter:hide-gutter t) ; Always a 0 width margin when no changes.
    (bind-key [remap vc-dir] 'git-gutter:popup-hunk)
    ;; Stage current hunk
    (bind-key [remap vc-create-tag] 'git-gutter:stage-hunk)
    ;; Revert current hunk
    (bind-key (kbd "C-x v r") 'git-gutter:revert-hunk))
  :config
  (progn
    (global-git-gutter-mode) ; Enable live update.
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v n") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v p") ?p 'git-gutter:previous-hunk '((?n . git-gutter:next-hunk)))))

;;; Slime
;;
;;
(use-package slime
    :init
  (progn
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (slime-setup '(slime-fancy
                   slime-asdf
                   slime-tramp
                   slime-banner
                   slime-autodoc
                   slime-xref-browser))
    (setq slime-net-coding-system 'utf-8-unix
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
    ;; Save-slime-scratch-buffer
    (setq slime-scratch-file "~/.emacs.d/slime-scratch.lisp")
    ;; common-lisp-info
    (add-to-list 'Info-additional-directory-list "~/elisp/info/gcl-info/")
    (add-hook 'slime-load-hook #'(lambda () (tv-require 'slime-tramp)))
    (bind-key "<f11> l r" 'tv-start-slime)
    (bind-key "<f11> l e" 'slime-scratch)
    (bind-key "<f11> l l" 'slime-list-connections)
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
          (slime))))
  :no-require t)

;;; Gnus-config
;;
(use-package gnus
    :init
  (progn
    (setq gnus-asynchronous t)
    ;; (setq mail-user-agent 'gnus-user-agent)
    ;; (setq read-mail-command 'gnus)
    ;; (setq send-mail-command 'gnus-msg-mail)
    (setq gnus-init-file "~/.emacs.d/emacs-config/.gnus.el")

    (defvar tv-gnus-loaded-p nil)
    (defun tv-load-gnus-init-may-be ()
      (unless (or tv-gnus-loaded-p
                  (eq major-mode 'mu4e-compose-mode))
        (load gnus-init-file)
        (setq tv-gnus-loaded-p t)))

    (add-hook 'message-mode-hook 'tv-load-gnus-init-may-be)
    (add-hook 'gnus-before-startup-hook 'tv-load-gnus-init-may-be)

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
    (global-set-key (kbd "<f7> m") 'tv-gnus)
    
    ;; Borred C-g'ing all the time and hanging emacs
    ;; while in gnus (while tethering or not).
    ;; Kill all nnimap/nntpd processes when exiting summary.
    (defun tv-gnus-kill-all-procs ()
      (cl-loop for proc in (process-list)
               when (string-match "\\*?\\(nnimap\\|nntpd\\)" (process-name proc))
               do (delete-process proc)))
    (add-hook 'gnus-exit-group-hook 'tv-gnus-kill-all-procs)
    (add-hook 'gnus-group-catchup-group-hook 'tv-gnus-kill-all-procs))
  :defer t
  :disabled t)

;;; Addressbook
;;
(use-package addressbook-bookmark
    :commands (addressbook-turn-on-mail-completion
               addressbook-bookmark-set
               addressbook-gnus-sum-bookmark
               addressbook-mu4e-bookmark
               addressbook-bmenu-edit
               addressbook-bookmark-jump))

;;; W3m
;;
(use-package w3m
    :config (use-package config-w3m)
    :bind ("<f7> h" . w3m)
    :defer t)

;;; Mu4e
;;
(use-package mu4e
    :config (progn (use-package mu4e-config)
                   (addressbook-turn-on-mail-completion))
    :commands 'mu4e
    :bind ("<f8>" . mu4e))

;;; Message
;;
(use-package message
    :config (progn (use-package mu4e-config)
                   (addressbook-turn-on-mail-completion)))

;;; Auth-source
;;
(with-eval-after-load "auth-source"
  (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t))))

;;; esh-toggle
;;
(use-package esh-toggle
    :commands (eshell-toggle-cd eshell-toggle)
    :bind (("<f11> e c" . eshell-toggle-cd)
           ("<f11> e t" . eshell-toggle)))

;;; Auctex/Latex config
;;
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; To turn on RefTeX Minor Mode for all LaTeX files,
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode

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

;;; Elscreen
;;
(when (locate-library "elscreen")
  (autoload 'elscreen-start "elscreen.el")
  (elscreen-start)
  (global-set-key (kbd "C-z l") 'helm-elscreen))

;;; Whitespace-mode
;;
(with-eval-after-load "whitespace"
  (add-to-list 'whitespace-style 'lines-tail)
  (setq whitespace-line-column 80))

;;; antiword
;;
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;;; align-let
;;
(autoload 'align-let-keybinding "align-let" nil t)
(add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
(add-hook 'lisp-interaction-mode-hook 'align-let-keybinding)
(add-hook 'lisp-mode-hook 'align-let-keybinding)

;;; sqlite-dump
;;
(autoload 'sqlite-dump "sqlite-dump" nil t)
(modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))
(setq sql-sqlite-program "sqlite3")

;;; Checkdoc
;;
(autoload 'checkdoc-batch       "checkdoc-batch" nil t)
(autoload 'checkdoc-batch-files "checkdoc-batch" nil t)

;;; markdown-mode
;;
(use-package markdown-mode
    :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.mdpp$" . markdown-mode))))

(use-package ffap
  :init 
  ;; Tramp/ange behave badly in 99.9% of the time for ftp, disable.
  (setq ffap-url-unwrap-remote (remove "ftp" ffap-url-unwrap-remote)))

;;; Eshell-config
;;
(use-package eshell
    :init
  (progn
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
                                    ;; Helm completion with pcomplete
                                    (setq eshell-cmpl-ignore-case t)
                                    (eshell-cmpl-initialize)
                                    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                    ;; Helm lisp completion
                                    (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
                                    ;; Helm completion on eshell history.
                                    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                    ;; Eshell prompt
                                    (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")
                                    ;; Allow yanking right now instead of returning "Mark set"
                                    (push-mark)))

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
    (with-eval-after-load "em-term"
      (dolist (i '("tmux" "htop" "ipython" "alsamixer" "git-log"))
        (add-to-list 'eshell-visual-commands i))))
  :config
  ;; Finally load eshell on startup.
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (let ((default-directory (getenv "HOME")))
                                      (command-execute 'eshell)
                                      (bury-buffer)))))

;;; linum-relative
;;
(use-package linum-relative
    :commands (linum-relative-mode
               helm-linum-relative-mode))

;;; Outline-mode
;;
(use-package outline
    :requires helm
    :config
  (progn
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-p")
                                  ?p 'outline-previous-visible-heading
                                  '((?n . outline-next-visible-heading)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-n")
                                  ?n 'outline-next-visible-heading
                                  '((?p . outline-previous-visible-heading)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-f")
                                  ?f 'outline-forward-same-level
                                  '((?b . outline-backward-same-level)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-b")
                                  ?b 'outline-backward-same-level
                                  '((?f . outline-forward-same-level)))))

;;; Charmap
;;
(use-package charmap :ensure t)


;;; Various fns
;;

(defun tv/update-helm-only-symbol (dir)
  (cl-loop for f in (directory-files dir t "\\.el\\'")
           do (with-current-buffer (find-file-noselect f)
                (save-excursion
                  (goto-char (point-min))
                  (let (fun)
                    (while (re-search-forward "(with-helm-alive-p" nil t)
                      (when (setq fun (which-function))
                        (end-of-defun)
                        (unless (looking-at "(put")
                          (insert (format "(put '%s 'helm-only t)\n" fun))))))))))

(defun quickping (host)
  "Return non--nil when host is reachable."
  (= 0 (call-process "ping" nil nil nil "-c1" "-W10" "-q" host)))

(defun goto-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

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

(defun tv-find-or-kill-gnu-bug-number (bug-number arg)
  "Browse url corresponding to emacs gnu bug number or kill it."
  (interactive (list (read-number "Bug number: " (thing-at-point 'number))
                     current-prefix-arg))
  (let ((url (format "http://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s" bug-number)))
    (if arg
        (progn
          (kill-new url)
          (message "Bug `#%d' url's copied to kill-ring" bug-number))
        (browse-url url))))

(defun tv-find-or-kill-helm-bug-number (bug-number arg)
  "Browse url corresponding to helm bug number or kill it."
  (interactive (list (read-number "Bug number: " (thing-at-point 'number))
                     current-prefix-arg))
  (let ((url (format "https://github.com/emacs-helm/helm/issues/%s" bug-number)))
    (if arg
        (progn
          (kill-new url)
          (message "Bug `#%d' url's copied to kill-ring" bug-number))
        (browse-url url))))

;; Entete-Bash
(defun tv-insert-bash-header ()
  "insert bash header at point"
  (interactive)
  (insert "#!/bin/bash\n"
          "## Title:\n"
          "## Description: \n"
          "## Author:Thierry Volpiatto<thierry dot volpiatto FROM gmail DOT com>\n"
          "## Commentary:\n\n"))

(defun tv-shell ()
  (interactive)
  (if (eq major-mode 'shell-mode)
      (bury-buffer) (shell)))

;; Toggle-show-trailing-whitespace
(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun tv-restore-scratch-buffer ()
  (unless (buffer-file-name (get-buffer "*scratch*"))
    (and (get-buffer "*scratch*") (kill-buffer "*scratch*")))
  (with-current-buffer (find-file-noselect "~/.emacs.d/save-scratch.el")
    (rename-buffer "*scratch*")
    (lisp-interaction-mode)
    (setq lexical-binding t)
    (use-local-map lisp-interaction-mode-map))
  (when (or (eq (point-min) (point-max))
            ;; For some reason the scratch buffer have not a zero size.
            (<= (buffer-size) 2))
    (insert ";;; -*- coding: utf-8; mode: lisp-interaction; lexical-binding: t -*-\n;;\n;; SCRATCH BUFFER\n;; ==============\n\n")))

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

;; This is bounded to C-d in shell.
(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward or send an EOF to subprocess.
Sends an EOF only if point is at the end of the buffer and there is no input."
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
        (progn (comint-send-eof) (kill-buffer))
        (delete-char arg))))

(defun tv-term ()
  (interactive)
  (ansi-term "/bin/bash"))


;;; Bindings
;;
;;
(global-set-key (kbd "C-z")                        nil) ; Disable `suspend-frame'.
(global-set-key (kbd "C-!")                        'eshell-command)
(global-set-key (kbd "C-c R")                      #'(lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c W")                      'whitespace-mode)
(global-set-key (kbd "C-M-j")                      #'(lambda () (interactive) (kill-sexp -1)))
(global-set-key (kbd "<f7> j")                     'webjump)
(global-set-key (kbd "<f11> s h")                  'tv-shell)
(global-set-key (kbd "<f11> t")                    'tv-term)
(global-set-key (kbd "<f11> i")                    'ielm)
(global-set-key (kbd "C-c @")                      'tv-flyspell)
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
(global-set-key (kbd "C-x r M-w")                  'copy-rectangle)
(global-set-key [remap save-buffers-kill-terminal] 'tv-stop-emacs) ; C-x C-c
(global-set-key (kbd "<f11> s c")                  'goto-scratch)
(global-set-key (kbd "C-8")                        'tv-transparency-modify)


;;; Elisp
;;
;; Fix indentation in CL loop.
(setq lisp-indent-function 'common-lisp-indent-function
      lisp-simple-loop-indentation 1
      lisp-loop-keyword-indentation 6
      lisp-loop-forms-indentation 6)

;; Fix indentation in cl-flet and cl-labels
(with-eval-after-load "cl-indent.el"
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

;; Tooltip face
(set-face-attribute 'tooltip nil
                    :foreground "black"
                    :background "NavajoWhite"
                    :family "unknown-DejaVu Sans Mono-bold-normal-normal"
                    :underline t)

;; Indent-when-newline (RET) in all elisp modes
(define-key lisp-interaction-mode-map (kbd "RET") 'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)
(define-key lisp-mode-map (kbd "RET") 'newline-and-indent)

;; byte-compile-file
(define-key emacs-lisp-mode-map (kbd "C-c C-c b") 'byte-compile-file)

;; Next page
(define-key emacs-lisp-mode-map (kbd "<next>") 'forward-page)
(define-key emacs-lisp-mode-map (kbd "<prior>") 'backward-page)

;; Which function
(autoload 'which-function "which-func.el")
(define-key emacs-lisp-mode-map (kbd "C-c ?")
  (lambda () (interactive) (message "[%s]" (which-function))))


;;; Semantic
;;
;;
;; (add-hook 'semantic-mode-hook
;;           ;; With my fixes in lisp/cedet/semantic/bovine/el.el.
;;           (lambda ()
;;             (load-file "~/elisp/el.el")
;;             (when (fboundp 'semantic-default-elisp-setup)
;;               (semantic-default-elisp-setup))))
;; (semantic-mode 1)

;;; Be sure to reenable touchpad when quitting emacs
(add-hook 'kill-emacs-hook #'(lambda ()
                               (and (executable-find "reenable_touchpad.sh")
                                    (shell-command "reenable_touchpad.sh"))))

;; Link now scratch buffer to file
(tv-restore-scratch-buffer)

;;; .emacs.el ends here
