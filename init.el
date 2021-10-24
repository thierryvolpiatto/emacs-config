;;; init.el --- emacs configuration. -*- lexical-binding: t -*-

;;; Code:

(defvar tv/startup-time (current-time))
(defun tv/emacs-load-time ()
  (let ((time (float-time (time-subtract (current-time) tv/startup-time))))
    (message "Emacs config loaded in %s seconds"
             (format "%.2f" time))))
(add-hook 'emacs-startup-hook #'tv/emacs-load-time t)

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation t
        native-comp-async-query-on-exit t
        native-comp-async-jobs-number 4
        native-comp-async-report-warnings-errors 'silent))

(require 'cl-lib)

(setq inhibit-startup-echo-area-message "thierry")

;;; package.el
;;
;;; Melpa/Elpa
;;
;; Emacs-26
(unless (boundp 'package-quickstart)
  (load-file (expand-file-name "early-init.el" user-emacs-directory))
  (package-initialize))

;; Need to update manually package-quickstart.el with
;; `package-quickstart-refresh' after each update.
(when (boundp 'package-quickstart) (setq package-quickstart t))

(defun tv/fix-selected-packages ()
  (interactive)
  (package-initialize)
  (package--save-selected-packages (package--find-non-dependencies)))

;;; Use-package
;;
(eval-when-compile (require 'use-package))


;;; load-path
;;
(dolist (i '("~/elisp/"
             "~/elisp/autoconf-mode"
             "~/elisp/desktop-file-utils"
             "~/elisp/tex-utils"
             "~/elisp/helm-extensions"
             "~/.emacs.d/themes/"
             "~/.emacs.d/emacs-config/"
             ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;;; VC
;;
;; Possible values for vc backends: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
(setq vc-handled-backends '(RCS Git)
      vc-follow-symlinks t
      vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; Global settings
;;
;;  Global bindings
(global-set-key (kbd "C-z")   nil) ; Disable `suspend-frame'.
(global-set-key (kbd "<f11>") nil)
(global-set-key (kbd "C-c R") (lambda () (interactive) (revert-buffer t t)))
(global-set-key [remap save-buffers-kill-terminal] 'tv/stop-emacs) ; C-x C-c

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop/restart emacs
(defun tv/stop-emacs-1 ()
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun tv/stop-emacs (arg)
  "Close emacs, with a prefix arg restart it.
Restart works only on graphic display."
  (interactive "P")
  (let ((confirm-kill-emacs (unless (and arg (display-graphic-p))
                              'y-or-n-p))
        (kill-emacs-query-functions
         (if (and arg (display-graphic-p))
             (append (list
                      (lambda ()
                        (when (y-or-n-p (format "Really restart %s? "
                                                (capitalize (invocation-name))))
                          (add-hook 'kill-emacs-hook
                                    (lambda ()
                                      (call-process-shell-command
                                       (format "(%s &)"
                                               ;; eselect-emacs.sh
                                               ;; should have kept
                                               ;; only one of
                                               ;; emacs/remacs.
                                               (or (executable-find "emacs")
                                                   (executable-find "remacs")))))
                                    t))))
                     kill-emacs-query-functions)
           kill-emacs-query-functions)))
    (tv/stop-emacs-1)))

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; Limite-max-lisp
(setq max-lisp-eval-depth 40000
      max-specpdl-size    100000)

;; Increase GC
(setq gc-cons-threshold 20000000)

;; Disable bidi
;; (setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)

;; column-number in mode-line.
(column-number-mode 1)

;; Environment variables
;;
;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")

;; Coding system.
(prefer-coding-system 'utf-8)

;; Themes
(defvar tv/theme-directory "~/.emacs.d/themes/")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv/theme-directory))

;; Load my favourite theme.
(add-hook 'emacs-startup-hook (lambda () (load-theme 'naquadah)))

;;; emacs-backup-config
;;
(defun tv/backup-file-p (file)
  (or (normal-backup-enable-predicate file)
      (and (locate-dominating-file file ".git") t)))

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-enable-predicate #'tv/backup-file-p
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Trash
;; `move-file-to-trash' doesn't use `substitute-in-file-name' to extract
;; value of XDG_DATA_HOME, so ensure this is unset in emacs.
(setenv "XDG_DATA_HOME")
;; (setq delete-by-moving-to-trash t)

;; Start-emacs-server
;;
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (server-start)
                               (setq server-raise-frame t))))

;; Copy/paste
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil
      select-enable-clipboard t
      select-enable-primary nil)

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
;; Remove message and error popping up at end of active minibuffer in
;; emacs-27.
(when (boundp 'minibuffer-message-clear-timeout) ; Emacs-27+
  ;; (setq minibuffer-message-clear-timeout 1.5)
  (setq set-message-function nil
        clear-message-function nil)
  (remove-hook 'minibuffer-setup-hook 'minibuffer-error-initialize))

;; History variables
(setq history-delete-duplicates t)
(setq history-length            100) ; default is 30.

(setq report-emacs-bug-no-explanations t
      comint-prompt-read-only          t
      uniquify-buffer-name-style       nil
      register-preview-delay           nil
      inhibit-startup-message          t
      message-log-max                  1000
      kill-ring-max                    80
      mark-ring-max                    60
      global-mark-ring-max             200)

;; Disable indent-tabs-mode
(setq-default indent-tabs-mode nil)

;; Disable all isearch bindings
(global-set-key [remap isearch-forward] 'undefined)
(global-set-key [remap isearch-backward] 'undefined)
(global-set-key [remap isearch-forward-regexp] 'undefined)
(global-set-key [remap isearch-backward-regexp] 'undefined)
(global-set-key (kbd "C-r") nil)
(global-set-key (kbd "C-s") nil)
(global-set-key (kbd "C-M-s") nil)
(global-set-key (kbd "C-M-r") nil)

;; `while-no-input-ignore-events' is not set in emacs-27+ (bug#46940).
(unless (and (boundp 'while-no-input-ignore-events)
             while-no-input-ignore-events)
  (setq while-no-input-ignore-events
        '(focus-in
          focus-out
          help-echo
          iconify-frame
          make-frame-visible
          selection-request)))

;; New events are available in emacs-28
(when (>= emacs-major-version)
  (setq while-no-input-ignore-events
        (append '(file-notify dbus-event) while-no-input-ignore-events)))


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
    (lambda (f prop value) (put f prop value))
    "Set function F's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, F can only be a symbol, not a lambda expression."))

;; Fix slow helm frame popup in emacs-26 helm issue #1976
(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

;; Don't beep even with visible-bell (debian)
(setq ring-bell-function 'ignore)

(setq line-move-visual nil)


;;; Use package declarations

;;; Diminish
;;
(use-package diminish :ensure t)

;;; Popup
;;
(use-package popup :ensure t)

;;; Info
;;
(use-package info
  :config
  (progn
    ;; Additional info directories
    (add-to-list 'Info-directory-list "/usr/local/share/info")
    (add-to-list 'Info-directory-list "/usr/share/info")
    (add-to-list 'Info-directory-list "~/elisp/info")
    ;; Fancy faces in info.
    (defface tv/info-ref-item
      '((((background dark)) :background "DimGray" :foreground "Gold")
        (((background light)) :background "firebrick" :foreground "LightGray"))
      "Face for item stating with -- in info." :group 'Info :group 'faces)

    (defvar tv/info-title-face 'tv/info-ref-item)
    (defvar tv/info-underline 'underline)
    (defvar info-unicode-quote-start (string 8216))
    (defvar info-unicode-quote-end (string 8217))
    (defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
                                               info-unicode-quote-start
                                               info-unicode-quote-start
                                               info-unicode-quote-end
                                               info-unicode-quote-end
                                               ))
    (defun tv/font-lock-doc-rules ()
      (font-lock-add-keywords
       nil `(("[^][\\s`]\\([^[](`'+\\)`']?[^][\\s']?" 1 font-lock-type-face)
             (,info-unicode-quoted-regexp 1 font-lock-type-face)
             ("^ --.*$" . tv/info-title-face)
             ("[_]\\([^_]+\\)[_]" 1 tv/info-underline)
             ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
             ("\\*Warning:\\*" . font-lock-warning-face)
             ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
             ("^[[:upper:],]\\{2,\\}$" . font-lock-comment-face)
             ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
             )))

    (add-hook 'Info-mode-hook 'tv/font-lock-doc-rules)
    (define-key Info-mode-map [remap Info-index] 'helm-info-at-point)))

;;; Emms
;;
(use-package emms
  :ensure t
  :commands 'helm-emms
  :config (use-package emms-vlc-config))

;;; Async
;;
;; Need to be called before helm config.
(use-package async
  :config
  (progn
    ;; Dired async.
    (use-package dired-async :config (dired-async-mode 1))
    ;; Smtp async.
    (use-package smtpmail-async
      :commands 'async-smtpmail-send-it)))

;;; Helm
;;
(require 'init-helm)

;;; Term - ansi-term
;;
(use-package term
  :config
  (progn
    ;; Kill buffer after C-d in ansi-term.
    (defadvice term-sentinel (after kill-buffer activate)
      (kill-buffer))
    (defun tv/term ()
      (interactive)
      (ansi-term "/bin/bash"))
    (defadvice term-command-hook (before decode-string)
      (setq string (decode-coding-string string locale-coding-system)))
    (when (version< emacs-version "24.3.50.1") (ad-activate 'term-command-hook)))
  :bind ("<f11> t" . tv/term))

;; Browse url
;;
;;
(use-package browse-url
  :config
  ;; See avail browser at ~/labo/github/helm/helm-net.el:253
  (setq browse-url-firefox-program "firefox"
        browse-url-browser-function 'helm-browse-url-firefox))

;;; Ediff
;;
(use-package ediff
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally
          ediff-show-ancestor         nil)))

;;; Help
;;
(use-package help
  :after tv-utils
  :config
  (progn
    ;; Fix curly quotes in emacs-25
    (and (boundp 'text-quoting-style)
         (setq text-quoting-style 'grave))
    ;; Since they use pp-buffer, it is not possible to override pp so
    ;; we need to duplicate the whole function with modifications and
    ;; override the original by advice.
    ;; Test: (describe-variable 'load-history)
    (advice-add 'describe-variable :override #'tv/describe-variable)))

;;; comment
;;
(use-package newcomment
  :config
  (progn
    ;; Change the behavior of `M-;' by commenting line.
    ;; Much simpler than emacs-25 `comment-line'.
    (defun comment--advice-dwim (old--fn &rest args)
      (if (region-active-p)
          (apply old--fn args)
        (save-excursion
          (goto-char (point-at-bol))
          (push-mark (point-at-eol) t t)
          (apply old--fn args))
        (indent-region (point-at-bol) (point-at-eol))
        (forward-line 1)
        (back-to-indentation)))
    (advice-add 'comment-dwim :around 'comment--advice-dwim)))

;;; Woman/man
;;
(use-package woman
  :config
  (setq woman-use-own-frame nil))

(use-package man
    :config
  (setq Man-notify-method 'pushy))

;; show-paren-mode
;;
(use-package paren
  :config
  (progn
    (show-paren-mode 1)
    (setq show-paren-ring-bell-on-mismatch t)))

(use-package electric
  :config (electric-indent-mode -1))

;;; auto-compression-mode
;;
(use-package jka-cmpr-hook
  :config (auto-compression-mode 1))

;;; Shell script
;;
(use-package sh-script
  :bind (:map sh-mode-map
              ("RET" . newline-and-indent)
              ("C-h f" . helm-info-bash)))

;;; Auto-conf
;;
(use-package autoconf-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode)))

(use-package autotest-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.at\\'" . autotest-mode)))

;;; Desktop-entry-mode
;;
(use-package desktop-entry-mode
  :load-path "~/elisp/desktop-file-utils/"
  :commands 'desktop-entry-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode)))

;;; Winner
;;
(use-package winner
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*mu4e-loading*"
                                ))
  (winner-mode 1)
  (helm-define-key-with-subkeys
      winner-mode-map (kbd "C-c <left>")
      'left 'winner-undo '((right . winner-redo))
      nil nil 3))

;;; All-the-icons and mode-line
;;
(use-package all-the-icons
  :ensure t
  :config
  (defun tv/git-branch-in-mode-line ()
    (require 'helm-ls-git)
    (when (and (buffer-file-name (current-buffer))
               (fboundp 'helm-ls-git--branch)
               (helm-ls-git-root-dir))
      (format " (%s %s)"
              (char-to-string #x29a9) ; (⦩) Needs a one line height char.
              (propertize (helm-ls-git--branch) 'face '(:foreground "yellow")))))

  (defun tv/select-git-branches-menu ()
    (let ((branchs (split-string (shell-command-to-string "git branch") "\n" t)))
      (cl-loop with current
               for b in branchs
               for branch = (replace-regexp-in-string "[ ]" "" b)
               when (string-match "\\`\\*" branch) do (setq current branch)
               collect (vector branch
                               `(lambda ()
                                  (interactive)
                                  (let ((real (replace-regexp-in-string "\\`\\*" "" ,branch)))
                                    (if (string= ,branch ,current)
                                        (message "Already on %s branch" real)
                                      (shell-command (format "git checkout -q '%s'" real))
                                      (message "Switched to %s branch" real)))))
               into lst
               finally return
               (append '("Git branches")
                       lst
                       '("--" ["Git status" vc-dir])))))

  (defun tv/custom-modeline-github-vc ()
    (require 'helm-ls-git)
    (let ((branch     
           (when (and (buffer-file-name (current-buffer))
                      (fboundp 'helm-ls-git--branch)
                      (helm-ls-git-root-dir))
             (helm-ls-git--branch)))
          (status-color (if (string= (helm-ls-git-status) "")
                            "SkyBlue" "yellow")))
      (when branch
        (concat
         (propertize (format " %s" (all-the-icons-faicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
         " · "
         (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                     'face `(:height 1.3 :family ,(all-the-icons-octicon-family) :foreground "Deepskyblue3")
                     'display '(raise -0.1))
         (propertize (format " %s" branch)
                     'face `(:height 0.9 :foreground ,status-color)
                     'mouse-face 'highlight
                     'help-echo "Mouse-1: Switch to branch"
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda ()
                                            (interactive)
                                            (popup-menu (tv/select-git-branches-menu)))))))))
  
  (setq-default mode-line-format '("%e"
                                   mode-line-front-space
                                   mode-line-mule-info
                                   mode-line-client
                                   mode-line-modified
                                   mode-line-remote
                                   mode-line-frame-identification
                                   mode-line-buffer-identification
                                   " "
                                   mode-line-modes
                                   " "
                                   "%p %l/%c"
                                   " "
                                   (:eval (tv/custom-modeline-github-vc))
                                   " "
                                   mode-line-misc-info
                                   mode-line-end-spaces)))

;;; Time
;;
(use-package time
  :config
  (defun tv/round-time-to-nearest-hour ()
    (let* ((time-string (format-time-string " %I:%M "))
           (split (split-string time-string ":"))
           (hour (string-to-number (car split)))
           (min (string-to-number (cadr split))))
      (cond ((and (> min 45) (<= min 59))
             (format " %02d:%02d" (1+ hour) 0))
            ((or (= min 0) (<= min 15))
             (format " %02d:%02d" hour 0))
            ((and (> min 15) (<= min 45))
             (format " %02d:%s" hour 30)))))
  
  (defvar tv/time-icons
    '((" 00:00" . "🕛")
      (" 01:00" . "🕐")
      (" 02:00" . "🕑")
      (" 03:00" . "🕒")
      (" 04:00" . "🕓")
      (" 05:00" . "🕔")
      (" 06:00" . "🕕")
      (" 07:00" . "🕖")
      (" 08:00" . "🕗")
      (" 09:00" . "🕘")
      (" 10:00" . "🕙")
      (" 11:00" . "🕚")
      (" 12:00" . "🕛")
      (" 00:30" . "🕧")
      (" 01:30" . "🕜")
      (" 02:30" . "🕝")
      (" 03:30" . "🕞")
      (" 04:30" . "🕟")
      (" 05:30" . "🕠")
      (" 06:30" . "🕡")
      (" 07:30" . "🕢")
      (" 08:30" . "🕣")
      (" 09:30" . "🕤")
      (" 10:30" . "🕥")
      (" 11:30" . "🕦")
      (" 12:30" . "🕧")))

  (defun tv/custom-modeline-time ()
    (let* ((hour (tv/round-time-to-nearest-hour))
           (icon (assoc-default (tv/round-time-to-nearest-hour) tv/time-icons)))
      (concat
       (propertize (format-time-string " %H:%M ")
                   'face `(:height 0.9 :foreground "green")
                   'help-echo (format "%s\n Mouse-1: display calendar"
                                      (format-time-string " %A %e %b, %Y" now))
                   'mouse-face 'highlight
                   'local-map (make-mode-line-mouse-map 'mouse-1 'tv/toggle-calendar))
       icon)))

  ;; World-time
  (when (eq display-time-world-list t) ; emacs-26+
    (setq display-time-world-list
          (let ((nyt (format-time-string "%z" nil "America/New_York"))
                (gmt (format-time-string "%z" nil "Europe/London")))
            (if (string-equal nyt gmt)
                legacy-style-world-list
              zoneinfo-style-world-list))))
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
  (add-to-list 'display-time-world-list '("Europe/Athens" "Athens"))
  (add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
  (add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
  (add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
  (add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
  (add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))
  
  (setq display-time-24hr-format   t
        display-time-day-and-date  (null (display-graphic-p))
        display-time-string-forms
        '( ;; date
          (if (and (not display-time-format) display-time-day-and-date)
              (format-time-string " %a%e %b " now)
            "")
          ;; time
          (concat
           (tv/custom-modeline-time)
           ;; `time-zone' is a let-bounded var in `display-time-update'.
           (and time-zone (format "(%s)" time-zone)))))
  (display-time))

;;; Frame and window config.
;;
;;
;; My current-font:      [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:        [EVAL]: (progn (when (require 'helm-font) (helm 'helm-source-xfonts)))
;; Choose a color:       [EVAL]: (progn (when (require 'helm-color) (helm 'helm-source-colors)))
;; To reload .Xresources [EVAL]: (shell-command "xrdb ~/.Xdefaults")
;; For ligatures use either
;; "-SAJA-Cascadia Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
;; or
;; "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1".
;; Cascadia is available at
;; https://github.com/microsoft/cascadia-code/releases and Fira is
;; available in linuxmint.
(use-package frame
  :config
  (progn
    (defvar tv/default-font (cond ((string= (invocation-name) "remacs")
                                   "-*-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
                                  ((and (>= emacs-major-version 27)
                                        (condition-case nil
                                            (font-info "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
                                          (error nil)))
                                   "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
                                  (t
                                   ;; Use .Xdefaults config (Dejavu).
                                   (assoc-default 'font (frame-parameters)))))
    ;; Need fonts-emojione package (apt)
    ;; See (info "(elisp) Fontsets")
    (when (member "Emoji One" (font-family-list))
      (set-fontset-font
       t 'symbol (font-spec :family "Emoji One") nil 'prepend))
    (setq-default frame-background-mode 'dark)
    (setq initial-frame-alist '((fullscreen . maximized)))
    (setq frame-auto-hide-function 'delete-frame)
    (defun tv/transparency-modify (arg)
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
    
    (if (or (daemonp)
            (not (window-system))
            (< emacs-major-version 24))
        (setq default-frame-alist `((vertical-scroll-bars . nil)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (title . ,(format "%s-%s"
                                                      (capitalize (invocation-name))
                                                      emacs-version))
                                    (cursor-color . "red")))

      (setq default-frame-alist `((foreground-color . "Wheat")
                                  (background-color . "Gray20")
                                  (alpha . 90) ;; Needs compositing manager.
                                  ;; New frames go in right corner.
                                  (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                                  (vertical-scroll-bars . nil)
                                  (title . ,(format "%s-%s"
                                                    (capitalize (invocation-name))
                                                    emacs-version))
                                  (tool-bar-lines . 0)
                                  (menu-bar-lines . 0)
                                  (font . ,tv/default-font)
                                  (cursor-color . "red")
                                  (fullscreen . nil)
                                  )))

    ;; Special buffer display.
    (add-hook 'window-setup-hook
              (lambda ()
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
                                                ("\\*helm apt show\\*"
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
                                                 (background-color . "Lightsteelblue4")
                                                 (foreground-color . "black")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                )))))
  :bind ("C-8" . tv/transparency-modify))

(use-package window
  :no-require t
  ;; Don't split windows horizontally.
  :init (setq split-width-threshold nil)
  (use-package helm
    :config
    (setq fit-window-to-buffer-horizontally 1)
    (helm-define-key-with-subkeys global-map (kbd "C-x ^") ?^ 'enlarge-window
                                  '((?ç . shrink-window)
                                    (?} . enlarge-window-horizontally)
                                    (?{ . shrink-window-horizontally))
                                  (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor" 'face 'minibuffer-prompt))
    (helm-define-key-with-subkeys global-map (kbd "C-x }") ?} 'enlarge-window-horizontally
                                  '((?^ . enlarge-window)
                                    (?ç . shrink-window)
                                    (?{ . shrink-window-horizontally))
                                  (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor" 'face 'minibuffer-prompt)))
    :bind (("C-x C-²" . delete-window)
           ("C-x C-&" . delete-other-windows)
           ("C-x C-é" . split-window-vertically)
           ("C-x C-\"" . split-window-horizontally)))

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

;;; Org
;;
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c C-k" . org-capture))
  :config (use-package org-config))

;;; Dired
;;
(use-package dired
  :init (progn
          (setq dired-dwim-target t)
          (setq dired-auto-revert-buffer t)
          (setq dired-backup-overwrite nil) ; nil, always, ask.
          (setq dired-isearch-filenames 'dwim)
          (setq dired-listing-switches (purecopy "-alh")))
  :config
  (use-package dired-extension)
  (use-package wdired
    :config (setq wdired-use-dired-vertical-movement 'sometimes))
  :defer t)

;;; htmlize
;;
(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'htmlize-file "htmlize" nil t)
(autoload 'htmlize-many-files "htmlize" nil t)
(autoload 'htmlize-many-files-dired "htmlize" nil t)

;;; tv-utils fns
;;
(use-package tv-utils
  :commands (tv/eval-region tv/restore-scratch-buffer)
  :init (progn
          (bind-key "C-M-!" 'tv/eval-region lisp-interaction-mode-map) 
          (bind-key "C-M-!" 'tv/eval-region emacs-lisp-mode-map))
  :config (advice-add 'view-echo-area-messages :around 'tv/view-echo-area-messages)
  :bind (("M-\""                  . tv/insert-double-quote)
         ("C-M-`"                 . tv/insert-double-backquote)
         ("C-M-("                 . tv/move-pair-forward)
         ("C-M-\""                . tv/insert-double-quote-and-close-forward)
         ("C-M-)"                 . tv/insert-pair-and-close-forward)
         ("<f5> c"                . tv/toggle-calendar)
         ([remap kill-whole-line] . tv/kill-whole-line)
         ([remap kill-line]       . tv/kill-line)
         ([remap delete-char]     . tv/delete-char)
         ([remap c-electric-delete-forward] . tv/delete-char)
         ("C-<"                   . other-window-backward)
         ("C->"                   . other-window-forward)
         ([C-left]                . screen-top)
         ([C-right]               . screen-bottom)
         ("<M-down>"              . tv/scroll-down)
         ("<M-up>"                . tv/scroll-up)
         ("<C-M-down>"            . tv/scroll-other-down)
         ("<C-M-up>"              . tv/scroll-other-up)
         ("C-c k"                 . tv/insert-kbd-at-point)))

;;; Ledger
;;
(use-package ledger-mode
  :ensure t
  :init
  (setenv "LEDGER_PAGER" "cat")
  (add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))
  :commands (ledger-mode csv2ledger ledger-position)
  :config (use-package ledger-config
            :init
            (require 'helm-lib)
            (require 'helm-mode)))

;;; Rectangle
;;
(use-package rectangle-utils
  :bind (("C-x r e"       . rectangle-utils-extend-rectangle-to-end)
         ("C-x r h"       . rectangle-utils-menu)
         ("C-x r <right>" . rectangle-utils-insert-at-right)))

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
  :ensure t
  :config
  (defun iedit-narrow-to-defun (arg)
    (interactive "P")
    (require 'iedit)
    (save-window-excursion
      (save-restriction
        (narrow-to-defun)
        (iedit-mode arg))))
  (setq iedit-increment-format-string "%03d")
  :bind (("C-²" . iedit-narrow-to-defun)
         ("C-;" . iedit-mode)
         :map isearch-mode-map
         ("C-;" . iedit-mode-from-isearch)))

(use-package iedit-rect
  :config
  (setq iedit-increment-format-string "%03d")
  :bind (([C-return] . iedit-rectangle-mode)
         :map ctl-x-r-map
         ("RET" . iedit-rectangle-mode)))

;;; Lacarte
;;
(autoload 'lacarte-get-overall-menu-item-alist "lacarte")

;;; Iterator
;;
(use-package iterator)

;;; pcomplete
;;
(use-package pcomplete-extension)

;;; Migemo
;;
(use-package migemo
  :init
  (setq migemo-command          "cmigemo"
        migemo-options          '("-q" "-e")
        migemo-dictionary       "/usr/share/cmigemo/utf-8/migemo-dict"
        migemo-user-dictionary  nil
        migemo-regex-dictionary nil
        migemo-coding-system    'utf-8-unix
        migemo-isearch-enable-p nil)
  :disabled t)

;;; Emamux
;;
(use-package emamux
  :ensure t
  :init (setq emamux:completing-read-type 'helm)
  :config (setq emamux:get-buffers-regexp
                "^\\(buffer[0-9]+\\): +\\([0-9]+\\) +\\(bytes\\): +[\"]\\(.*\\)[\"]"
                emamux:show-buffers-with-index nil)
  :bind (("C-c y" . emamux:yank-from-list-buffers)
         ("C-c s" . emamux:send-command)))

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
  (when (fboundp 'elisp--highlight-function-argument)
    (if (>= emacs-major-version 27)
        ;; Eldoc is broken in Emacs-28, this works more or less but it
        ;; is not working as expected.
        (progn
          (defun tv/before-elisp--highlight-function-argument (old--fn &rest args)
            (let ((sym    (nth 0 args))
                  (argstr (substitute-command-keys (nth 1 args)))
                  (index  (nth 2 args))
                  (prefix (nth 3 args)))
              (apply old--fn args)))
          (advice-add 'elisp--highlight-function-argument
                      :around #'tv/before-elisp--highlight-function-argument))

      (defun tv/advice-elisp--highlight-function-argument (sym args index prefix)
        "Highlight argument INDEX in ARGS list for function SYM.
In the absence of INDEX, just call `eldoc-docstring-format-sym-doc'."
        ;; FIXME: This should probably work on the list representation of `args'
        ;; rather than its string representation.
        ;; FIXME: This function is much too long, we need to split it up!
        (let* ((start          nil)
               (end            0)
               (argument-face  'eldoc-highlight-function-argument)
               (args-lst (mapcar (lambda (x)
                                   (replace-regexp-in-string
                                    "\\`[(]\\|[)]\\'" "" x))
                                 (split-string args)))
               (args-lst-ak (cdr (member "&key" args-lst))))
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
          (when (and args-lst-ak
                     (>= index (- (length args-lst) (length args-lst-ak))))
            (let* (case-fold-search
                   key-have-value
                   (sym-name (symbol-name sym))
                   (cur-w (current-word))
                   (limit (save-excursion
                            (when (re-search-backward sym-name nil t)
                              (match-end 0))))
                   (cur-a (if (and cur-w (string-match ":\\([^ ()]*\\)" cur-w))
                              (substring cur-w 1)
                            (save-excursion
                              (let (split)
                                (when (re-search-backward ":\\([^ ()\n]*\\)" limit t)
                                  (setq split (split-string (match-string 1) " " t))
                                  (prog1 (car split)
                                    (when (cdr split)
                                      (setq key-have-value t))))))))
                   ;; If `cur-a' is not one of `args-lst-ak'
                   ;; assume user is entering an unknown key
                   ;; referenced in last position in signature.
                   (other-key-arg (and (stringp cur-a)
                                       args-lst-ak
                                       (not (member (upcase cur-a) args-lst-ak))
                                       (upcase (car (last args-lst-ak))))))
              (unless (or (null cur-w) (string= cur-w sym-name))
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
                          ((or (and (string-match-p "\\.\\.\\.\\'" argument)
                                    (string= argument (car (last args-lst))))
                               (and (string-match-p "\\.\\.\\.\\'"
                                                    (substring args 1 (1- (length args))))
                                    (= (length (remove "..." args-lst)) 2)
                                    (> index 1) (eq (logand index 1) 1)))
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
            (setq doc (eldoc-docstring-format-sym-doc prefix doc))
            doc)))
      (advice-add 'elisp--highlight-function-argument
                  :override #'tv/advice-elisp--highlight-function-argument)))
  :diminish eldoc-mode)

;;; Python config
;;
(use-package python
  :no-require t
  :init
  (progn
    (use-package gud
      :config
      (and (boundp 'gud-pdb-command-name)
           (setq gud-pdb-command-name "ipdb")))
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "-i --autoindent --simple-prompt --InteractiveShell.display_page=True"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
    (add-hook 'python-mode-hook 'flymake-mode) ;; Needs pyflakes
    (add-hook 'python-mode-hook
              (lambda ()
                (setq-local mode-name "py")
                (define-key python-mode-map (kbd "C-c C-i") 'helm-semantic-or-imenu)
                (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
                (define-key python-mode-map (kbd "C-c '") 'flymake-goto-next-error))))
  :config
  (defun tv/run-or-switch-to-python-shell ()
    (interactive)
    (let* ((buf (ignore-errors (python-shell-get-process-or-error t)))
           (proc-buf (and buf (process-buffer buf)))
           (win (and proc-buf (get-buffer-window proc-buf 'visible))))
      (cond ((and proc-buf win)
             (quit-window nil win))
            (proc-buf (pop-to-buffer proc-buf nil t))
            (t (call-interactively #'run-python)))))
  :bind ("<f11> p" . tv/run-or-switch-to-python-shell))

;;; Tramp-config
;;
(use-package tramp
  :defer t
  :no-require t
  :config
  (progn
    ;; scp is better for copying large files but not working with many
    ;; files.
    (setq tramp-default-method "ssh")
    ;; (setq tramp-verbose 6) ; See `helm-tramp-verbose' in init-helm.

    ;; No messages
    (setq tramp-message-show-message nil)

    (setq tramp-use-ssh-controlmaster-options nil)

    ;; Allow connecting as root on all remote Linux machines except this one.
    ;; Use e.g /sudo:host:/path
    (add-to-list 'tramp-default-proxies-alist
                 '("\\`thievol\\'" "\\`root\\'" "/ssh:%h:"))

    (add-to-list 'tramp-default-proxies-alist
                 '("\\`thievolrem\\'" "\\`root\\'" "/ssh:%h:"))

    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))

    ;; (when (boundp 'tramp-save-ad-hoc-proxies)
    ;;   (setq tramp-save-ad-hoc-proxies t))

    ;; Connect to my freebox as 'freebox' user.
    (add-to-list 'tramp-default-user-alist
                 '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))))

;;; Ange-ftp
;;
(use-package ange-ftp
  :init
  (progn
    ;; Following used to work with previous emacs version but is now broken.
    ;; (setq ange-ftp-try-passive-mode t)
    ;; (setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on")))
    ;; So use now directly pftp.
    (setq ange-ftp-ftp-program-name "pftp"))
  :no-require t)

;;; Calendar and diary
;;
(use-package calendar
  :config
  (progn
    ;; Disable the fucking org bindings in emacs-28
    (when (fboundp 'org--setup-calendar-bindings)
      (fset 'org--setup-calendar-bindings 'ignore))
    (setq diary-file "~/.emacs.d/diary")
    (unless (fboundp 'fancy-diary-display) ; Fix emacs-25.
      (defalias 'fancy-diary-display 'diary-fancy-display))
    (defface tv/calendar-blocks
      '((t (:background "ForestGreen")))
      "Face used to highlight diary blocks in calendar."
      :group 'calendar)
    ;; Add a different face in diary entry like this:
    ;; %%(diary-block 8 2 2021 13 2 2021 'tv/calendar-blocks-1)
    (defface tv/calendar-blocks-1
      '((t (:background "DarkOliveGreen")))
      "Face used to highlight diary blocks in calendar."
      :group 'calendar)
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
          ;; FIXME there is a bug in calendar that break diary when
          ;; abbreviated names are different than US.
          ;; calendar-day-abbrev-array
          ;; ["Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"]
          ;; calendar-day-header-array
          ;; ["Di" "Lu" "Ma" "Me" "Je" "Ve" "Sa"]
          ;; calendar-month-abbrev-array
          ;; ["Jan" "Fév" "Mar" "Avr" "Mai" "Juin" "Juil" "Aou" "Sep" "Oct" "Nov" "Déc"]
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
                             when (memq prop '(diary holiday diary-anniversary
                                                     tv/calendar-blocks tv/calendar-blocks-1))
                             collect prop)))
        (cond ((and (or (memq 'diary props)
                        (memq 'tv/calendar-blocks props)
                        (memq 'tv/calendar-blocks-1 props)
                        (memq 'diary-anniversary props))
                    (memq 'holiday props))
               (cl-letf (((symbol-function 'message) #'ignore))
                 (diary-view-entries arg))
               (calendar-cursor-holidays))
              ((or (memq 'diary props)
                   (memq 'tv/calendar-blocks props)
                   (memq 'tv/calendar-blocks-1 props)
                   (memq 'diary-anniversary props))
               (cl-letf (((symbol-function 'message) #'ignore))
                 (diary-view-entries arg)))
              ((memq 'holiday props)
               (calendar-cursor-holidays))
              (t (message "Nothing special on this date")))))

    (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-month)
    (define-key calendar-mode-map (kbd "C-<left>")  'calendar-backward-month)
    (define-key calendar-mode-map (kbd "RET")       'tv/calendar-diary-or-holiday)
    (use-package appt
      :config
      (progn
        (setq appt-display-format 'echo ; Values: 'echo, 'window or nil.
              appt-warning-time-regexp "warn ?\\([0-9]+\\)") 
        (add-hook 'emacs-startup-hook 'appt-activate))))
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
    ;; Write directly to bmk file instead of writing to a "
    ;; *bookmarks*" buffer and then writing to bmk file.
    (defun tv/advice--bookmark-write-file (file)
      "Write `bookmark-alist' to FILE."
      (let ((reporter (make-progress-reporter
                       (format "Saving bookmarks to file %s..." file))))
        (with-current-buffer (find-file-noselect file)
          (let ((vc (cond
                     ((null bookmark-version-control) nil)
                     ((eq 'never bookmark-version-control) 'never)
                     ((eq 'nospecial bookmark-version-control) version-control)
                     (t t))))
            (when (version-control-safe-local-p vc)
              (setq-local version-control vc)))
          (goto-char (point-min))
          (condition-case err
              (progn
                (delete-region (point-min) (point-max))
                (let ((coding-system-for-write
                       (or coding-system-for-write
                           bookmark-file-coding-system
                           'utf-8-emacs))
                      (print-length nil)
                      (print-level nil)
                      ;; See bug #12503 for why we bind `print-circle'.  Users
                      ;; can define their own bookmark types, which can result in
                      ;; arbitrary Lisp objects being stored in bookmark records,
                      ;; and some users create objects containing circularities.
                      (print-circle t))
                  (insert "(")
                  ;; Rather than a single call to `pp' we make one per bookmark.
                  ;; Apparently `pp' has a poor algorithmic complexity, so this
                  ;; scales a lot better.  bug#4485.
                  (dolist (i bookmark-alist) (pp i (current-buffer)))
                  (insert ")\n")
                  ;; Make sure the specified encoding can safely encode the
                  ;; bookmarks.  If it cannot, suggest utf-8-emacs as default.
                  (with-coding-priority '(utf-8-emacs)
                    (setq coding-system-for-write
                          (select-safe-coding-system (point-min) (point-max)
                                                     (list t coding-system-for-write))))
                  (goto-char (point-min))
                  (bookmark-insert-file-format-version-stamp coding-system-for-write)
                  (setq bookmark-file-coding-system coding-system-for-write)
                  (save-buffer)))
            (file-error (message "Can't write %s" file)))
          (kill-buffer (current-buffer)))
        (progress-reporter-done reporter)))
    (advice-add 'bookmark-write-file :override #'tv/advice--bookmark-write-file)
    (and (boundp 'bookmark-bmenu-use-header-line)
         (setq bookmark-bmenu-use-header-line nil))))

;;; git-gutter-mode
;;
(use-package git-gutter
  :ensure t
  :init
  (progn
    ;; Activate live update timer.
    (customize-set-variable 'git-gutter:update-interval 2)
    ;; Always a 0 width margin when no changes.
    (setq git-gutter:hide-gutter t)
    ;; Not sure why I would like to have git-gutter runs when listing
    ;; buffers or such, did I miss something?
    (setq git-gutter:update-commands nil)
    (bind-key [remap vc-dir] 'git-gutter:popup-hunk)
    ;; Stage current hunk
    (bind-key [remap vc-create-tag] 'git-gutter:stage-hunk)
    ;; Revert current hunk
    (bind-key (kbd "C-x v r") 'git-gutter:revert-hunk))
  :diminish (git-gutter-mode " 🐱")
  :config
  (progn
    (defun tv/git-gutter:popup-diff-quit ()
      (interactive)
      (with-selected-window (get-buffer-window git-gutter:popup-buffer)
        (View-quit)))
    ;; (setq git-gutter:diff-option "-b")
    (global-git-gutter-mode) ; Enable live update.
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v n") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v p") ?p 'git-gutter:previous-hunk '((?n . git-gutter:next-hunk)))
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v d") nil 'git-gutter:popup-hunk '((?n . git-gutter:next-hunk)
                                                                (?d . git-gutter:next-hunk)
                                                                (?p . git-gutter:previous-hunk)
                                                                (?q . tv/git-gutter:popup-diff-quit)))))

;;; Addressbook
;;
(use-package addressbook-bookmark
  :commands (addressbook-turn-on-mail-completion
             addressbook-bookmark-set
             addressbook-mu4e-bookmark
             addressbook-bmenu-edit
             addressbook-bookmark-jump))

;;; W3m
;;
(use-package w3m
  :ensure t
  :commands (w3m-toggle-inline-image w3m-region w3m-browse-url)
  :init (require 'config-w3m)
  :bind
  (("<f7> h" . w3m)
   :map w3m-mode-map
   ("F" . w3m-view-url-with-browse-url)
   ("M-<right>" . w3m-next-buffer)
   ("M-<left>" . w3m-previous-buffer)
   ("V" . helm-w3m-bookmarks)
   ("M" . w3m-view-url-with-browse-url)
   ("M-q" . tv/w3m-fill-region-or-paragraph)
   ("<down>" . next-line)
   ("<up>" . previous-line)
   ("RET" . tv/scroll-down)
   ("<backspace>" . tv/scroll-up)
   :map w3m-lynx-like-map
   ("S-<right>" . w3m-view-this-url-new-session)))

;;; Mu4e
;;
(use-package mu4e
  :config
  (progn (require 'mu4e-config)
         (addressbook-turn-on-mail-completion))
  :commands (mu4e)
  :bind ("<f8>" . mu4e))

;;; Autocrypt
;;
(use-package autocrypt
  :ensure t
  :config
  (use-package autocrypt-mu4e)
  (use-package autocrypt-gnus)
  (use-package autocrypt-message)
  (add-hook 'mu4e-main-mode-hook 'autocrypt-mode)
  (add-hook 'gnus-mode-hook 'autocrypt-mode)
  (add-hook 'message-mode-hook 'autocrypt-mode)
  (setq autocrypt-accounts
        '(("thievol@posteo.net"
           "E6F697C8ED3C46FC"
           mutual))))

;;; Auth-source
;;
(use-package auth-source
  :no-require t
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

;;; esh-toggle
;;
(use-package esh-toggle
  :commands (eshell-toggle-cd eshell-toggle)
  :bind (("<f2>" . eshell-toggle)
         ("<S-f2>" . eshell-toggle-cd)))

;;; Whitespace-mode
;;
(use-package whitespace
  :commands 'whitespace-mode
  :config (progn
            (add-to-list 'whitespace-style 'lines-tail)
            (setq whitespace-line-column 80))
  :bind ("C-c W" . whitespace-mode))

;;; markdown-mode
;;
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdpp$" . markdown-mode)))

(use-package ffap
  :config
  ;; See issue #2003 in helm
  (setq ffap-url-unwrap-remote '("ftp" "file"))
  (when (> emacs-major-version 24)
    ;; See issue #1716 in helm.
    (setcdr (assq 'file ffap-string-at-point-mode-alist)
            '("--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))))

;;; Eshell-config
;;
(use-package eshell
  :commands (eshell eshell-command)  
  :init
  (progn
    ;; Eshell-prompt
    (setq eshell-prompt-function
          (lambda nil
            (let ((pwd (eshell/pwd)))
              (with-temp-buffer
                (let* ((default-directory (file-name-as-directory pwd))
                       (proc (process-file
                              "git" nil t nil
                              "symbolic-ref" "HEAD" "--short"))
                       (id (propertize (if (= (user-uid) 0) " # " " $ ")
                                       'face 'default))
                       detached branch status)
                  (unless (= proc 0)
                    (erase-buffer)
                    (setq detached t)
                    (setq proc (process-file
                                "git" nil t nil
                                "rev-parse" "--short" "HEAD")))
                  (if (= proc 0)
                      (progn
                        (setq branch (replace-regexp-in-string
                                      "\n" "" (buffer-string)))
                        (erase-buffer)
                        (setq proc (process-file
                                    "git" nil t nil "status" "--porcelain"))
                        (setq status (pcase (buffer-string)
                                       ((and str (guard (and (not (string= str ""))
                                                             (= proc 0))))
                                        (if (string-match "\\`[?]" str) "?" "*"))
                                       (_ "")))
                        (format "%s@%s:%s(%s%s)%s"
                                (getenv "USER") (system-name)
                                (propertize (abbreviate-file-name pwd) 'face 'italic)
                                (propertize (format
                                             "%s%s"
                                             (if detached "detached@" "")
                                             branch)
                                            'face '((:foreground "red")))
                                (propertize status
                                            'face `((:foreground
                                                     ,(if (string= "?" status)
                                                          "OrangeRed" "gold1"))))
                                id))
                    (format "%s@%s:%s%s"
                            (getenv "USER") (system-name)
                            (propertize (abbreviate-file-name pwd) 'face 'italic)
                            id)))))))
    (setq eshell-password-prompt-regexp
      "\\(\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|[Mm]\\(?:ot de passe\\|ật khẩu\\)\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\).*:.*\\'")

    ;; Compatibility 24.2/24.3
    (unless (or (fboundp 'eshell-pcomplete)
                (>= emacs-major-version 27))
      (defalias 'eshell-pcomplete 'pcomplete))
    (unless (or (fboundp 'eshell-complete-lisp-symbol)
                (>= emacs-major-version 27))
      (defalias 'eshell-complete-lisp-symbol 'lisp-complete-symbol))

    (add-hook 'eshell-mode-hook (lambda ()
                                  (setq eshell-pwd-convert-function (lambda (f)
                                                                      (if (file-equal-p (file-truename f) "/")
                                                                          "/" f)))
                                  ;; This is needed for eshell-command (otherwise initial history is empty).
                                  (eshell-read-history eshell-history-file-name)
                                  ;; Helm completion with pcomplete
                                  (setq eshell-cmpl-ignore-case t
                                        eshell-hist-ignoredups t)
                                  (eshell-cmpl-initialize)
                                  ;; emacs-27+ use completion-at-point
                                  ;; which sucks.NOTE: eshell*map are
                                  ;; moving targets, watch out.
                                  (cond ((= emacs-major-version 27)
                                         (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete))
                                        ((= emacs-major-version 28)
                                         (define-key eshell-hist-mode-map (kbd "TAB") 'helm-esh-pcomplete))
                                        (t (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))
                                  ;; Helm completion on eshell
                                  ;; history.
                                  (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
                                  (when (boundp 'eshell-hist-mode-map)
                                    (define-key eshell-hist-mode-map (kbd "M-p") 'helm-eshell-history))
                                  ;; Eshell prompt
                                  (set-face-attribute 'eshell-prompt nil :foreground "Gold1")))

    ;; Eshell history size
    (setq eshell-history-size 1000) ; Same as env var HISTSIZE.

    ;; Eshell-banner
    (setq eshell-banner-message "")

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
      (dolist (i '("tmux" "htop" "ipython" "alsamixer" "git-log" "tig" "w3mman" "mutt"))
        (add-to-list 'eshell-visual-commands i)))
    ;; Eshell modifiers
    (with-eval-after-load "em-pred"
      (defun tv/advice--eshell-pred-substitute (&optional repeat)
        "Return a modifier function that will substitute matches."
        (let ((delim (char-after))
              match replace end)
          (forward-char)
          (setq end (eshell-find-delimiter delim delim nil nil t)
                match (buffer-substring-no-properties (point) end))
          (goto-char (1+ end))
          (setq end (or (eshell-find-delimiter delim delim nil nil t) (point))
                replace (buffer-substring-no-properties (point) end))
          (goto-char (1+ end))
          (if repeat
              (lambda (lst)
                (mapcar
                 (lambda (str)
                   (let ((i 0))
                     (while (setq i (string-match match str i))
                       (setq str (replace-match replace t nil str))))
                   str)
                 lst))
            (lambda (lst)
              (mapcar
               (lambda (str)
                 (if (string-match match str)
                     (setq str (replace-match replace t nil str)))
                 str)
               lst)))))
      ;; Allow empty string in substitution e.g. echo foo.el(:gs/.el//)
      (advice-add 'eshell-pred-substitute :override #'tv/advice--eshell-pred-substitute)
      ;; Fix echo, perhaps using as alias *echo is even better.
      (setq eshell-plain-echo-behavior t)))
  :bind ("C-!" . eshell-command))

(use-package display-line-numbers
    :commands (display-line-numbers-mode
               global-display-line-numbers-mode)
    :config
    (setq display-line-numbers-type 'relative))

;;; Outline-mode
;;
(use-package outline
  :defer t
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

;;; Flyspell
;;
(use-package ispell
  :init
  (progn
    (setq-default ispell-program-name "aspell")
    (setq ispell-local-dictionary "francais"))
  :config
  (progn
    (defun tv/toggle-flyspell (arg)
      "Toggle `flyspell-mode'." 
      (interactive "P")
      (if (and flyspell-mode (null arg))
          (progn
            (flyspell-mode -1)
            (message "Flyspell Mode disabled"))
        (flyspell-mode 1)
        (unwind-protect
            (progn
              (when (fboundp 'helm-autoresize-mode)
                (helm-autoresize-mode 1))
              (let ((dic (completing-read "Dictionary: " '("english" "francais"))))
                (ispell-change-dictionary dic)
                (flyspell-delete-all-overlays)
                (message "Starting new Ispell process aspell with %s dictionary..." dic)))
          (when (fboundp 'helm-autoresize-mode)
            (helm-autoresize-mode -1))))))
  :bind ("C-c @" . tv/toggle-flyspell))

;;; Semantic
;;
(use-package semantic
  :config
  (progn
    ;; Don't use this with elisp, prefer native imenu.
    (add-hook 'semantic-mode-hook
              ;; My patched lisp/cedet/semantic/bovine/el.el.
              (lambda ()
                (load-file "~/elisp/el.el")
                (when (fboundp 'semantic-default-elisp-setup)
                  (semantic-default-elisp-setup))))
    (semantic-mode 1))
  :disabled t)

;;; Which function
;;
(use-package which-func
  :commands 'which-function
  :config
  (progn
    (defun tv/which-func ()
      (interactive)
      (message "[%s]" (which-function))))
  :bind (:map emacs-lisp-mode-map
              ("C-c ?" . tv/which-func)))

;;; Shell
;;
(use-package shell
  :requires helm
  :config
  (progn
    (defun comint--advice-send-eof (&rest _args)
      (let ((win (selected-window)))
        (kill-buffer) (delete-window win)))
    (advice-add 'comint-send-eof :after 'comint--advice-send-eof))
  :bind (("<f11> s h" . shell)
         :map shell-mode-map
         ("M-p" . helm-comint-input-ring)))

;;; Elisp/lisp
;;
(use-package lisp-mode
  :config
  (progn
    (defun tv/set-mode-name (name)
      (setq-local mode-name name))
    (defun tv/set-lisp-interaction-name ()
      (tv/set-mode-name "Ilisp"))
    (defun tv/set-emacs-lisp-name ()
      (tv/set-mode-name "Elisp"))
    (add-hook 'lisp-interaction-mode-hook #'tv/set-lisp-interaction-name)
    (add-hook 'emacs-lisp-mode-hook #'tv/set-emacs-lisp-name)

    (use-package cl-indent
        :config
      ;; Fix indentation in CL functions (cl-flet/loop etc...).
      (setq lisp-indent-function #'common-lisp-indent-function
            lisp-simple-loop-indentation 1
            lisp-loop-keyword-indentation 9 ;; Align cl-loop clauses.
            lisp-loop-forms-indentation 9)) ;; Align cl-loop next clauses.

    (defun goto-scratch ()
      (interactive)
      (switch-to-buffer "*scratch*"))

    ;; Add fontification to some functions
    (cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (font-lock-add-keywords
       mode
       '(("(\\<\\(cl-dolist\\)\\>" 1 font-lock-keyword-face))))

    (defvar tv/autofill-modes '(emacs-lisp-mode
                                lisp-interaction-mode
                                sh-mode))
    (defun tv/point-in-comment-p (pos)
      "Returns non-nil if POS is in a comment."
      (eq 'comment (syntax-ppss-context (syntax-ppss pos))))

    (defun tv/point-in-docstring-p (pos)
      "Returns non-nil if POS is in a docstring."
      (and (eq 'string (syntax-ppss-context (syntax-ppss pos)))
           (eq (get-text-property (point) 'face) 'font-lock-doc-face)))

    (defun tv/turn-on-auto-fill-mode-maybe ()
      "Enable auto-fill-mode only in comments or docstrings.
Variable adaptive-fill-mode is disabled when a docstring field is detected."
      (when (memq major-mode tv/autofill-modes)
        (let ((in-docstring (tv/point-in-docstring-p (point))))
          (setq adaptive-fill-mode (not in-docstring))
          (auto-fill-mode
           (if (or (tv/point-in-comment-p (point))
                   in-docstring)
               1 -1)))))
    ;; Maybe turn on auto-fill-mode when a comment or docstring field
    ;; is detected. Ensure the hook is appended otherwise things like
    ;; eldoc-eval will not work.
    (add-hook 'post-command-hook #'tv/turn-on-auto-fill-mode-maybe t)

    (defun tv/pp-eval-or-expand-last-sexp (&optional arg)
      "Eval sexp at point, with ARG macroexpand it."
      (interactive "P")
      ;; Be sure to have helm loaded for helm-aif.
      (require 'helm)
      (helm-aif (or (get-buffer-window "*Pp Eval Output*" 'visible)
                    (get-buffer-window "*Pp Macroexpand Output*" 'visible))
          (progn
            (kill-buffer (window-buffer it))
            (delete-window it))
        (if arg
            (pp-macroexpand-last-sexp nil)
          (pp-eval-last-sexp nil)))))

  :bind (("<f11> s c" . goto-scratch)
         ("<S-f12>" . cancel-debug-on-entry)
         ("M-:" . pp-eval-expression)
         :map
         emacs-lisp-mode-map
         ("RET" . newline-and-indent)
         ("C-c C-c b" . byte-compile-file)
         ("<next>" . forward-page)
         ("<prior>" . backward-page)
         ("C-M-j" . backward-kill-sexp)
         ("M-e" . tv/pp-eval-or-expand-last-sexp)
         ("C-c C-a" . tv/align-let)
         :map
         lisp-interaction-mode-map
         ("RET" . newline-and-indent)
         ("C-M-j" . backward-kill-sexp)
         ("M-e" . tv/pp-eval-or-expand-last-sexp)
         ("C-c C-a" . tv/align-let)
         :map
         lisp-mode-map
         ("RET" . newline-and-indent)))

;;; face-remap - font size <C-fn-up/down>.
;;
(use-package face-remap
  :bind (("C--" . text-scale-decrease)
         ("C-+" . text-scale-increase)))

;;; Org toc for github
;;
(use-package toc-org
  :commands (toc-org-insert-toc)  
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-enable))

;;; Rectangle edit
;;
(use-package rectangle-edit :commands 'rectangle-edit)

;;; Bash-completion
;;
;; (use-package bash-completion
;;   :ensure t
;;   :commands 'bash-completion-dynamic-complete
;;   :init
;;   (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))

;;; Pcmpl-git (For Eshell)
;;
;; Seems that bash-completion and pcmpl can cohabit.
;; No subcommands completion with pcmpl in eshell though.
(use-package pcmpl-git :ensure t)

;;; Log-view
;;
(use-package log-view
  :config
  (defun tv/log-view-fontify ()
    (font-lock-add-keywords nil '(("^revision [0-9.]*" . font-lock-comment-face)
                                  ("[a-zA-Z ]*:" . font-lock-type-face))))
  (add-hook 'log-view-mode-hook 'tv/log-view-fontify))

;;; Wgrep
;;
(use-package wgrep-helm
  :ensure t
  :config
  (setq wgrep-enable-key "\C-x\C-q"))

;;; psession
;;
(use-package psession
  :config
  (psession-savehist-mode 1)
  (psession-mode 1))

;;; Rainbow-mode
;;
(use-package rainbow-mode :ensure t)

;;; Imenu
;;
(use-package imenu
  :defer t
  :config
  ;; Allow browsing use-package definitions in init files.
  (defun imenu-add-use-package-generic-expr ()
    (add-to-list
     'imenu-generic-expression
     '("Use package" "^\\s-*(\\(?:straight-\\)?use-package\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]*[^)]*" 1)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-add-use-package-generic-expr))

;;; Undo-tree
;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (defun git-gutter:undo-tree-undo (&rest _args)
    (when git-gutter-mode
      (run-with-idle-timer 0.1 nil 'git-gutter)))
  (advice-add 'undo-tree-undo :after 'git-gutter:undo-tree-undo)
  (advice-add 'undo-tree-redo :after 'git-gutter:undo-tree-undo)

  (global-undo-tree-mode 1))

;;; Isearch-light
;;
(use-package isearch-light
  :config
  (setq isl-before-position-string "≤"
        isl-after-position-string "≥")
  :bind (("C-s" . isl-search)
         ("C-z" . isl-narrow-to-defun)))

;;; Yaml-mode
;;
(use-package yaml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


;; Kill buffer and windows
(defun tv/kill-buffer-and-windows (arg)
  "Kill current-buffer and delete its window.
With a prefix arg ask with completion which buffer to kill."
  (interactive "P")
  (let* ((buffer (if arg
                     (read-buffer "Kill buffer: " (current-buffer) t)
                   (current-buffer)))
         (windows (get-buffer-window-list buffer nil t)))
    (when (kill-buffer buffer)
      (dolist (win windows)
        (when (window-live-p win)
          (ignore-errors (delete-window win)))))))
(helm-define-key-with-subkeys global-map (kbd "C-x k") ?k 'tv/kill-buffer-and-windows)

;; Fix issue with the new :extend face attribute in emacs-27
;; Prefer to extend to EOL as in previous emacs.
(defun tv/extend-faces-matching (regexp)
  (cl-loop for f in (face-list)
           for face = (symbol-name f)
           when (and (string-match regexp face)
                     (eq (face-attribute f :extend t 'default)
                         'unspecified))
           do (set-face-attribute f nil :extend t)))

(when (fboundp 'set-face-extend)
  (with-eval-after-load "magit"
    (tv/extend-faces-matching "\\`magit"))
  ;; (with-eval-after-load "helm"
  ;;   (tv/extend-faces-matching "\\`helm"))
  )

;; Fix unreadable diff/ediff in emacs-27
(when (>= emacs-major-version 27)
  (with-eval-after-load 'diff-mode
    (set-face-attribute 'diff-refine-added nil :background 'unspecified)
    (set-face-attribute 'diff-refine-removed nil :background 'unspecified)
    (set-face-attribute 'diff-refine-changed nil :background 'unspecified))
  (with-eval-after-load 'ediff-init
    (set-face-attribute 'ediff-fine-diff-A nil :background 'unspecified)
    (set-face-attribute 'ediff-fine-diff-B nil :background 'unspecified))
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil :extend t))
  (set-face-attribute 'region nil :extend t))

;; diff buffers read-only
(setq diff-default-read-only t)

;; Link now scratch buffer to file
(tv/restore-scratch-buffer)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
