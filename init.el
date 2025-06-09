;;; init.el --- emacs configuration. -*- lexical-binding: t -*-

;;; Code:

(defvar tv:startup-time (current-time))
(defun tv:emacs-load-time ()
  (let ((time (float-time (time-subtract (current-time) tv:startup-time))))
    (message "Emacs config loaded in %s seconds"
             (format "%.2f" time))))


;;; Packages.el config.
;;
(defun tv:fix-selected-packages ()
  (interactive)
  (package-initialize)
  (package--save-selected-packages (package--find-non-dependencies)))


;;; load-path
;;
(defun tv:add-subdirs-to-load-path (dir)
  (let ((default-directory (file-name-as-directory
                            (expand-file-name dir))))
    (normal-top-level-add-subdirs-to-load-path)
    (add-to-list 'load-path (expand-file-name dir))))

(tv:add-subdirs-to-load-path "~/elisp")

;;; gcmh-mode disable GC and increase gc-cons-threshold while not idle,
;; when idle, restore gc-cons-threshold and run GC after
;; gcmh-idle-delay seconds or (* gcmh-auto-idle-delay-factor
;; gcmh-last-gc-time) if set to auto.
(autoload 'gcmh-mode "gcmh.el") ; Installed in ~/elisp.
(with-eval-after-load 'gcmh
  (setq gcmh-idle-delay 'auto))
(gcmh-mode 1)

(with-eval-after-load 'find-func
  (setq find-library-include-other-files nil))

;;; Emacs customize own file
;;
(setq custom-file (if (<= emacs-major-version 30)
                      "~/.emacs.d/.emacs-custom.el"
                    "~/.emacs.d/.emacs-custom-31.el"))
(load custom-file)

;;; VC
;;
;; Possible values for vc backends: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
(setq vc-handled-backends nil)
;; Let's psession loading buffers before setting this (much faster).
;; When it is needed to run e.g. vc-annotate revert buffer before proceding.
(add-hook 'emacs-startup-hook (lambda ()
                                (setq vc-handled-backends '(RCS Git)
                                      vc-follow-symlinks t
                                      vc-ignore-dir-regexp
                                      (format "\\(%s\\)\\|\\(%s\\)"
                                              vc-ignore-dir-regexp
                                              tramp-file-name-regexp)))
          100)

;;; Global settings
;;

;; Disable annoying bindings
(global-set-key (kbd "C-z")   nil) ; Disable `suspend-frame'.
(global-set-key (kbd "<f11>") nil) ; Disable `toggle-frame-fullscreen'

;; Prevent emacs warning when hitting <XF86TouchpadOff> and
;; <XF86TouchpadOn> to disable/enabling TouchPad.
(define-key global-map (kbd "<XF86TouchpadOff>") (lambda () (interactive) (message "TouchPad disabled")))
(define-key global-map (kbd "<XF86TouchpadOn>")  (lambda () (interactive) (message "TouchPad reenabled")))

;; Revert-buffer
(defun tv:revert-buffer-no-query ()
  (interactive)
  ;; Try to save excursion as Emacs-29 moves point nowhere after
  ;; reverting.
  (save-excursion
    (revert-buffer t t)))
(global-set-key (kbd "C-c R") #'tv:revert-buffer-no-query)

;; y-or-n-p everywhere
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop/restart emacs
(defun tv:stop-emacs-1 ()
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun tv:stop-emacs (arg)
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
    (tv:stop-emacs-1)))
(global-set-key [remap save-buffers-kill-terminal] 'tv:stop-emacs) ; C-x C-c

;; Add newline at end of files
(setq require-final-newline t)

;; Limit max lisp
(setq max-lisp-eval-depth 40000
      max-specpdl-size    100000)

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
(setq custom-theme-directory "~/.emacs.d/themes/")

(defvar tv:use-random-themes nil)
(defvar tv:current-theme nil)
(defvar tv:favorite-themes '(naquadah-tv naquadah
                             syhoex github-dark-vscode
                             adwaita-dark wombat
                             leuven-dark vs-dark
                             manoj-dark wheatgrass))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if tv:use-random-themes
                (let ((theme (nth (random (length tv:favorite-themes))
                                  tv:favorite-themes)))
                  (setq tv:current-theme theme)
                  (load-theme theme t)
                  (message "Theme %s loaded" theme))
              (setq tv:current-theme (car tv:favorite-themes))
              (load-theme tv:current-theme t))))

;; Helm affixation for theme will be available after loading helm.
(defun tv:load-theme (theme)
  (interactive (list (let ((completion-extra-properties '(:category theme)))
                            (completing-read "Load custom theme: "
                                             (custom-available-themes)))))
  (setq theme (intern-soft theme))
  (when tv:current-theme
    (disable-theme tv:current-theme))
  (load-theme theme t)
  (setq tv:current-theme theme))

;;; emacs-backup-config
;;
(defun tv:backup-file-p (file)
  (and (normal-backup-enable-predicate file)
       (null (locate-dominating-file file ".git"))))

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-enable-predicate #'tv:backup-file-p
      backup-by-copying t
      version-control t
      vc-make-backup-files nil
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Start-emacs-server
;;
(autoload 'server-running-p "server")
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (server-start)
                               (setq server-raise-frame t))))

;; Copy/paste
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil
      ;; save-interprogram-paste-before-kill 72
      select-enable-clipboard t
      select-enable-primary nil)

(defun tv:mark-symbol-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cl-assert bounds nil "No symbol at point")
    (goto-char (car bounds))
    (push-mark (cdr bounds) nil t)))
;; I don't use cycle-spacing (M-SPC)
(global-set-key (kbd "M-SPC") #'tv:mark-symbol-at-point)

;; Enable some commands disabled by default
(put 'narrow-to-region 'disabled nil)          ; C-x n n
(put 'narrow-to-page 'disabled nil)            ; C-x n p
(put 'scroll-left 'disabled nil)               ; C-x <
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

(setq report-emacs-bug-no-explanations t
      comint-prompt-read-only          t
      uniquify-buffer-name-style       nil
      inhibit-startup-message          t
      message-log-max                  1000
      kill-ring-max                    120
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

;; Don't beep even with visible-bell (debian)
(setq ring-bell-function 'ignore)

;; moves point by logical lines.
(setq line-move-visual nil)

;; Align-regexp
(global-set-key (kbd "C-}") #'align-regexp)

(defun tv:view-echo-area-messages ()
  (interactive)
  (let ((win (get-buffer-window "*Messages*" 'visible)))
    (cond ((and win (one-window-p t))
           (quit-window  nil win))
          (win (delete-other-windows win))
          (t (view-echo-area-messages)))))

(defun tv:quit-echo-area ()
  (interactive)
  (with-selected-window (get-buffer-window "*Messages*" 'visible)
    (quit-window)))

(with-eval-after-load 'helm-core
  (define-key global-map (kbd "C-h e") nil)
  (helm-define-key-with-subkeys global-map (kbd "C-h e")
                                ?e 'tv:view-echo-area-messages
                                '((?q . tv:quit-echo-area))))

;;; Compatibility
;;
;;
;; For `osm' in skitour (open street map)
(unless (fboundp 'json-available-p)
  (defun json-available-p ()
    (fboundp 'json-parse-string)))


;;;; Package configurations.

;;; History
;;
(setq history-delete-duplicates t)
(setq history-length            100)
(put 'file-name-history 'history-length 1000)
;; Limit M-x history to 50.
(put 'extended-command-history 'history-length 50)

;;; Isearch-light (Installed in site-lisp with make)
;;
(load "isl-autoloads")
(global-set-key (kbd "C-s") 'isl-search)
(global-set-key (kbd "C-z") 'isl-narrow-to-defun)
(global-set-key (kbd "C-M-s") 'isl-resume)
(with-eval-after-load 'isl
  (setq isl-before-position-string "≤"
        isl-after-position-string "≥"
        isl-requires-pattern 0))

;;; Pulse (used by Isl)
;;
(with-eval-after-load 'pulse
  (setq pulse-delay 0.06))

;;; Info
;;
;; Cleanup `Info-directory-list'.
;; When an empty string is in `Info-directory-list' info search by
;; default in .emacs.d, and if it finds a file with same name as
;; the info file it uses it even if it is not an info file, e.g. tramp.
(with-eval-after-load 'info
  (setq Info-directory-list (delete "" Info-directory-list))
  ;; Additional info directories
  (add-to-list 'Info-directory-list "/usr/local/share/info")
  (add-to-list 'Info-directory-list "/usr/share/info")
  (add-to-list 'Info-directory-list "~/elisp/info")
  ;; Fancy faces in info.
  (defface tv:info-ref-item
    '((((background dark)) :background "DimGray" :foreground "Gold")
      (((background light)) :background "firebrick" :foreground "LightGray"))
    "Face for item stating with -- in info." :group 'Info :group 'faces)

  (defvar tv:info-title-face 'tv:info-ref-item)
  (defvar tv:info-underline 'underline)
  (defvar info-unicode-quote-start (string 8216))
  (defvar info-unicode-quote-end (string 8217))
  (defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
                                             info-unicode-quote-start
                                             info-unicode-quote-start
                                             info-unicode-quote-end
                                             info-unicode-quote-end
                                             ))
  (defun tv:font-lock-doc-rules ()
    (font-lock-add-keywords
     nil `(("[^][\\s`]\\([^[](`'+\\)`']?[^][\\s']?" 1 font-lock-type-face)
           (,info-unicode-quoted-regexp 1 font-lock-type-face)
           ("^ --.*$" . tv:info-title-face)
           (" [_]\\([^_]+\\)[_] " 1 tv:info-underline)
           ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
           ("\\*Warning:\\*" . font-lock-warning-face)
           ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
           ("^[[:upper:],]\\{2,\\}$" . font-lock-comment-face)
           ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
           )))

  (add-hook 'Info-mode-hook 'tv:font-lock-doc-rules)
  (define-key Info-mode-map [remap Info-index] 'helm-info-at-point))

;;; Async
;;
;; Need to be called before helm config.
;; Temporary fix for emacs bug 58919.
(when (< emacs-major-version 29)
  (with-eval-after-load 'async
    (setq async-child-init "~/.emacs.d/fix-copy-directory.el")))
;; Dired async.
(autoload 'dired-async-mode "dired-async" nil t)
(dired-async-mode 1)
;; async-bytecomp
(autoload 'async-byte-recompile-directory "async-bytecomp")
(autoload 'async-byte-compile-file "async-bytecomp")

;;; Helm
;;
(autoload 'helm-define-key-with-subkeys "helm-core")
(require 'init-helm)

;;; Term - ansi-term
;;
;; Kill buffer after C-d in ansi-term.
(defun tv:advice-term-sentinel (&rest _args) (kill-buffer))
(advice-add 'term-sentinel :after #'tv:advice-term-sentinel)

(defun tv:term ()
  (interactive)
  (ansi-term "/bin/bash"))

(global-set-key (kbd "<f11> t") 'tv:term)

;; Browse url
;;
;;
(with-eval-after-load 'browse-url
  ;; See avail browser at ~/work/github/helm/helm-net.el:253
  (setq browse-url-firefox-program "firefox"
        browse-url-browser-function 'helm-browse-url-firefox))

;;; Diff/Ediff
;;
(with-eval-after-load 'ediff
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ediff-show-ancestor         nil))

;; Fix unreadable diff/ediff in emacs-27+
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

(with-eval-after-load 'diff
  (setq diff-default-read-only t
        diff-switches '("-u")))

;;; Save place
;;
(autoload 'tv-save-place-mode "tv-save-place" nil t)
(tv-save-place-mode 1)

;;; Byzanz - screencast ffrom Emacs
;;
(autoload 'byzanz-record "tv-byzanz" nil t)

;;; wttr weather
;;
(autoload 'wttr-weather "wttr-weather" nil t)

;;; tv-utils functions.
;;
;; TODO: Make an autoload file for emacs-config dir.
(autoload 'tv:restore-scratch-buffer "tv-utils" nil t)
(autoload 'tv:insert-double-quote "tv-utils" nil t)
(autoload 'tv:insert-vector "tv-utils" nil t)
(autoload 'tv:insert-double-backquote "tv-utils" nil t)
(autoload 'tv:move-pair-forward "tv-utils" nil t)
(autoload 'tv:insert-double-quote-and-close-forward "tv-utils" nil t)
(autoload 'tv:insert-pair-and-close-forward "tv-utils" nil t)
(autoload 'tv:toggle-calendar "tv-utils" nil t)
(autoload 'tv:kill-whole-line "tv-utils" nil t)
(autoload 'tv:kill-line "tv-utils" nil t)
(autoload 'tv:delete-char "tv-utils" nil t)
(autoload 'tv:delete-char "tv-utils" nil t)
(autoload 'other-window-backward "tv-utils" nil t)
(autoload 'other-window-forward "tv-utils" nil t)
(autoload 'tv:scroll-down "tv-utils" nil t)
(autoload 'tv:scroll-up "tv-utils" nil t)
(autoload 'tv:scroll-other-down "tv-utils" nil t)
(autoload 'tv:scroll-other-up "tv-utils" nil t)
(autoload 'tv:insert-kbd-at-point "tv-utils" nil t)
(autoload 'tv:eval-region "tv-utils" nil t)
(autoload 'tv:mount-sshfs "tv-utils" nil t)

(define-key lisp-interaction-mode-map (kbd "C-M-!") 'tv:eval-region) 
(define-key emacs-lisp-mode-map (kbd "C-M-!")       'tv:eval-region)

(global-set-key (kbd "M-\"")                      'tv:insert-double-quote)
(global-set-key (kbd "M-]")                       'tv:insert-vector)
(global-set-key (kbd "C-M-`")                     'tv:insert-double-backquote)
(global-set-key (kbd "C-M-(")                     'tv:move-pair-forward)
(global-set-key (kbd "C-M-\"")                    'tv:insert-double-quote-and-close-forward)
(global-set-key (kbd "C-M-)")                     'tv:insert-pair-and-close-forward)
(global-set-key (kbd "<f5> c")                    'tv:toggle-calendar)
(global-set-key [remap kill-whole-line]           'tv:kill-whole-line)
(global-set-key [remap kill-line]                 'tv:kill-line)
(global-set-key [remap delete-char]               'tv:delete-char)
(global-set-key [remap c-electric-delete-forward] 'tv:delete-char)
(global-set-key (kbd "C-<")                       'other-window-backward)
(global-set-key (kbd "C->")                       'other-window-forward)
(global-set-key (kbd "<M-down>")                  'tv:scroll-down)
(global-set-key (kbd "<M-up>")                    'tv:scroll-up)
(global-set-key (kbd "<C-M-down>")                'tv:scroll-other-down)
(global-set-key (kbd "<C-M-up>")                  'tv:scroll-other-up)
(global-set-key (kbd "C-c k")                     'tv:insert-kbd-at-point)

;;; Help
;;
(with-eval-after-load 'help-fns
  (setq help-enable-completion-autoload nil))
;; Fix curly quotes in emacs-25
(when (boundp 'text-quoting-style)
  (setq text-quoting-style 'grave))
;; Advice describe-variable.
(require 'describe-variable)
(define-key help-mode-map (kbd "C-c e") 'tv:pp-value-in-help)

;;; comment
;;
(define-key global-map (kbd "C-M-;") 'next-line)
(with-eval-after-load 'newcomment
  (setq comment-style 'plain)
  ;; Change the behavior of `M-;' by commenting line.
  ;; Much simpler than emacs-25 `comment-line'.
  (defun comment--advice-dwim (old--fn &rest args)
    (if (region-active-p)
        (apply old--fn args)
      (let ((bol (point-at-bol))
            (eol (point-at-eol)))
        (save-excursion
          (goto-char bol)
          (push-mark  eol t t)
          (unless (eql bol eol)
            (apply old--fn args)))
        (indent-region bol eol)
        (forward-line 1)
        (back-to-indentation))))
  (advice-add 'comment-dwim :around 'comment--advice-dwim))

;;; Woman/man
;;
(with-eval-after-load 'woman
  (setq woman-use-own-frame nil))

(with-eval-after-load 'man
  (setq Man-notify-method 'pushy)
  ;; Get rid of the most of the time unuseful default.
  (advice-add 'Man-default-man-entry :override #'ignore))

;;; show-paren-mode
;;
(with-eval-after-load 'paren
  (show-paren-mode 1)
  (setq show-paren-ring-bell-on-mismatch t))

;;; Electric-mode (disable)
;;
(with-eval-after-load 'electric
  (electric-indent-mode -1))

;;; auto-compression-mode
;;
(with-eval-after-load 'jka-cmpr-hook
  (auto-compression-mode 1))

;;; Shell script
;;
(with-eval-after-load 'sh-script
  (defun tv:set-sh-script-mode-name ()
    (setq-local mode-name (if (fboundp 'all-the-icons-alltheicon)
                              (all-the-icons-alltheicon "script" :height 1.0 :v-adjust 0.0)
                            "Sh "))
    (setq mode-line-process nil))
  (add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
  (add-hook 'sh-mode-hook #'flycheck-mode)
  (add-hook 'sh-mode-hook #'tv:set-sh-script-mode-name)
  (define-key sh-mode-map (kbd "RET") 'newline-and-indent)
  (define-key sh-mode-map (kbd "C-h f") 'helm-info-bash))

;;; Winner
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
                              ))
(winner-mode 1)
(helm-define-key-with-subkeys
    winner-mode-map (kbd "C-c <left>")
    'left 'winner-undo '((right . winner-redo))
    nil nil 3)

;;; All-the-icons and mode-line
;;
;; Don't forget to install necessary fonts with M-x
;; all-the-icons-install-fonts.
;; Align at right (only emacs-30).
(unless (boundp 'mode-line-format-right-align)
  (defvar mode-line-format-right-align "")
  (defvar mode-line-right-align-edge nil))

(defun tv:select-git-branches-menu ()
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
                     '("--" ["Git status" helm-browse-project])))))

(defun tv:custom-modeline-github-vc ()
  (require 'helm-ls-git)
  (let* ((fname (buffer-file-name (current-buffer)))
         (branch
          (when (and fname
                     ;; Don't do fancy things on remote files, tramp
                     ;; is enough slow.
                     (not (file-remote-p fname))
                     (fboundp 'helm-ls-git--branch)
                     (helm-ls-git-root-dir))
            (helm-ls-git--branch)))
         (status-color    "SkyBlue")
         (git-icon        (all-the-icons-faicon "git"))
         (git-branch-icon (all-the-icons-octicon "git-branch")))
    (when branch
      (concat
       (propertize (format " %s" git-icon) 'face '(:height 1.2) 'display '(raise -0.1))
       " · "
       (propertize (format "%s" git-branch-icon)
                     'face `(:height 1.3 :family ,(all-the-icons-octicon-family) :foreground "Deepskyblue3")
                     'display '(raise -0.1))
       (propertize (format " %s" branch)
                   'face `(:height 0.9 :foreground ,status-color)
                   'mouse-face 'highlight
                   'help-echo "Mouse-1: Switch to branch"
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda ()
                                          (interactive)
                                          (popup-menu (tv:select-git-branches-menu)))))))))

(with-eval-after-load 'all-the-icons
  ;; Will have no effect in emacs-30<.
  (setq mode-line-right-align-edge 'right-fringe)
  (setq-default mode-line-format `("%e"
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
                                   (:eval (tv:custom-modeline-github-vc))
                                   " "
                                   ;; Align at right (emacs-30 only).
                                   mode-line-format-right-align
                                   (:eval (progn
                                            ;; Keep time always at right.
                                            (setq global-mode-string
                                                  (append (delete 'display-time-string
                                                                  global-mode-string)
                                                          '(display-time-string)))
                                            ""))
                                   mode-line-misc-info
                                   mode-line-end-spaces))

    ;; Icons for file extensions.
    (setf (alist-get "dat" all-the-icons-extension-icon-alist nil nil 'equal)
          '(all-the-icons-faicon "bar-chart" :face all-the-icons-cyan :height 0.9 :v-adjust 0.0))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("avi" all-the-icons-faicon "film" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("3gp" all-the-icons-faicon "film" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("m4v" all-the-icons-faicon "film" :face all-the-icons-blue))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("xz" all-the-icons-octicon "file-binary"
                   :v-adjust 0.0 :face all-the-icons-lmaroon))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("eln" all-the-icons-octicon "file-binary"
                   :v-adjust 0.0 :face all-the-icons-dsilver))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("epub" all-the-icons-octicon "book"
                   :v-adjust 0.0 :face all-the-icons-red-alt))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("torrent" all-the-icons-material "cloud_upload"
                   :v-adjust 0.0 :face all-the-icons-lgreen))
    (add-to-list 'all-the-icons-extension-icon-alist
                 '("gitignore" all-the-icons-alltheicon "git" :height 1.0  :face all-the-icons-lred))
    ;; Icons for directories
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("Vidéos" all-the-icons-faicon "film" :height 0.9 :v-adjust -0.1))
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("Musique" all-the-icons-faicon "music" :height 1.0 :v-adjust -0.1))
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("Images" all-the-icons-faicon "picture-o" :height 0.9 :v-adjust -0.2))
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("Téléchargements" all-the-icons-faicon "cloud-download" :height 0.9 :v-adjust -0.1))
    (add-to-list 'all-the-icons-dir-icon-alist
                 '("Bureau" all-the-icons-octicon "device-desktop" :height 1.0 :v-adjust -0.1))
    ;; Icons for modes.
    (setf (alist-get 'sh-mode all-the-icons-mode-icon-alist)
          '(all-the-icons-alltheicon "terminal" :face all-the-icons-purple :v-adjust 0.0))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(diary-mode all-the-icons-faicon "calendar" :height 1.0
                   :v-adjust -0.1 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(diary-fancy-display-mode all-the-icons-faicon "calendar" :height 1.0
                   :v-adjust -0.1 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(calendar-mode all-the-icons-faicon "calendar" :height 1.0
                   :v-adjust -0.1 :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(Info-mode all-the-icons-faicon "info"
                   :v-adjust -0.1 :face all-the-icons-purple))
    ;; Regexp icons.
    (setq all-the-icons-regexp-icon-alist
          (append '(("^bookmark" all-the-icons-octicon "bookmark"
                     :height 1.1 :v-adjust 0.0 :face all-the-icons-lpink))
                  (delete (assoc "bookmark" all-the-icons-regexp-icon-alist)
                          all-the-icons-regexp-icon-alist))))

;;; Nerd-icons
;;
(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-extension-icon-alist
               '("avi" nerd-icons-faicon "nf-fa-film" :face nerd-icons-blue))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("3gp" nerd-icons-faicon "nf-fa-film" :face nerd-icons-blue))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("m4v" nerd-icons-faicon "nf-fa-film" :face nerd-icons-blue))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("xz" nerd-icons-octicon "nf-oct-file_binary" :face nerd-icons-lmaroon)))

;;; Time
;;
(with-eval-after-load 'time
  (defun tv:round-time-to-nearest-hour ()
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
  
  (defvar tv:time-icons
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

  (defun tv:custom-modeline-time ()
    (let* ((hour (tv:round-time-to-nearest-hour))
           (icon (assoc-default (tv:round-time-to-nearest-hour) tv:time-icons)))
      (concat
       (propertize (format-time-string " %H:%M ")
                   'face `(:height 0.9 :foreground "green")
                   'help-echo (format "%s\n Mouse-1: display calendar"
                                      (format-time-string " %A %e %b, %Y" now))
                   'mouse-face 'highlight
                   'local-map (make-mode-line-mouse-map 'mouse-1 'tv:toggle-calendar))
       icon)))

  ;; World-time
  (when (eq display-time-world-list t)  ; emacs-26+
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
        '(;; date
          (if (and (not display-time-format) display-time-day-and-date)
              (format-time-string " %a%e %b " now)
            "")
          ;; time
          (concat
           (tv:custom-modeline-time)
           ;; `time-zone' is a let-bounded var in `display-time-update'.
           (and time-zone (format "(%s)" time-zone))))))
(display-time)

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
;; Need fonts-emojione package (apt)
;; See (info "(elisp) Fontsets")
(when (member "Emoji One" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Emoji One") nil 'prepend))

(setq-default frame-background-mode 'dark)

(setq initial-frame-alist '((fullscreen . maximized)))
(setq frame-auto-hide-function 'delete-frame)

(when (boundp 'other-window-scroll-default)
  (setq other-window-scroll-default (lambda () (get-mru-window 'visible nil t))))

(defun tv:transparency-modify-1 (arg)
  "Increase Emacs frame transparency.
If ARG is non nil decrease transparency."
  (when (window-system)
    (let* ((ini-val   (frame-parameter nil 'alpha))
           (ini-alpha (if (floatp ini-val) (round (* ini-val 100)) ini-val))
           (def-alpha (or ini-alpha 80))
           (mod-alpha (if arg
                          (min (+ def-alpha 10) 100)
                        (max (- def-alpha 10)
                             frame-alpha-lower-limit)))) ; 20
      (modify-frame-parameters nil (list (cons 'alpha mod-alpha)))
      (message "Alpha[%s]" mod-alpha))))

(defun tv:transparency-modify-increase ()
  (interactive)
  (tv:transparency-modify-1 nil))

(defun tv:transparency-modify-decrease ()
  (interactive)
  (tv:transparency-modify-1 'decrease))

(defun tv:emacs-x-toolkit ()
  (if (string-match
       "--with-x-toolkit=\\([^ ]*?\\) "
       system-configuration-options)
      (match-string 1 system-configuration-options)
    "gtk"))

(if (or (daemonp)
        (not (window-system))
        (< emacs-major-version 24))
    (setq default-frame-alist `((vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (title . ,(format "%s-%s-%s"
                                                  (capitalize (invocation-name))
                                                  emacs-version
                                                  (tv:emacs-x-toolkit)))
                                (cursor-color . "red")))

  (setq default-frame-alist `((foreground-color . "Wheat")
                              (background-color . "Gray20")
                              (alpha . 100) ;; Needs compositing manager.
                              ;; New frames go in right corner.
                              (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                              (vertical-scroll-bars . nil)
                              (title . ,(format "%s-%s-%s"
                                                (capitalize (invocation-name))
                                                emacs-version
                                                (tv:emacs-x-toolkit)))
                              (tool-bar-lines . 0)
                              (menu-bar-lines . 0)
                              (cursor-color . "red")
                              (fullscreen . nil)
                              )))

;; Special buffer display.
;; Use `display-buffer-alist' instead of deprecated
;; `special-display-regexps'. All entries must be dedicated to
;; replicate `special-display-regexps' behavior.
(customize-set-variable 'display-buffer-alist
                        (append '(("\\*Help"
                                   ;; Avoid creating new frames
                                   ;; when pressing buttons
                                   ;; in help buffer.
                                   (display-buffer-reuse-window
                                    display-buffer-pop-up-frame)
                                   (reusable-frames . 0)
                                   (dedicated . t)
                                   (pop-up-frame-parameters .
                                    ((minibuffer . nil)
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
                                     (fullscreen . nil))))
                                  ("\\*Compile-Log"
                                   ;; Without these settings
                                   ;; compile creates a new frame
                                   ;; for each block compiled in file!
                                   (display-buffer-reuse-window
                                    display-buffer-pop-up-frame)
                                   (reusable-frames . 0)
                                   (dedicated . t)
                                   (pop-up-frame-parameters .
                                    ((minibuffer . nil)
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
                                     (fullscreen . nil))))
                                  ("\\*helm apt show\\*"
                                   (display-buffer-pop-up-frame)
                                   (dedicated . t)
                                   (pop-up-frame-parameters .
                                    ((minibuffer . nil)
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
                                     (fullscreen . nil))))
                                  ("^\\*osm"
                                   (display-buffer-same-window)
                                   (dedicated . t)))
                                display-buffer-alist))

;; Don't split windows horizontally.
(setq split-width-threshold nil)
(setq fit-window-to-buffer-horizontally 1)
(helm-define-key-with-subkeys
    global-map (kbd "C-x ^") ?^ 'enlarge-window
    '((?ç . shrink-window)
      (?} . enlarge-window-horizontally)
      (?{ . shrink-window-horizontally))
    (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor"
                'face 'minibuffer-prompt))
(helm-define-key-with-subkeys
    global-map (kbd "C-x }") ?} 'enlarge-window-horizontally
    '((?^ . enlarge-window)
      (?ç . shrink-window)
      (?{ . shrink-window-horizontally))
    (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor"
                'face 'minibuffer-prompt))
(global-set-key (kbd "C-x C-²") 'delete-window)
(global-set-key (kbd "C-x C-&") 'delete-other-windows)
(global-set-key (kbd "C-x C-é") 'split-window-vertically)
(global-set-key (kbd "C-x C-\"") 'split-window-horizontally)

(defun tv:kill-buffer-and-windows (arg)
  "Kill current-buffer and delete its window.
With a prefix arg ask with completion which buffer to kill."
  (interactive "P")
  (let* ((buffer (if arg
                     (read-buffer "Kill buffer: " (current-buffer) t)
                   (current-buffer)))
         (windows (get-buffer-window-list buffer nil t)))
    (with-current-buffer buffer
      (set-buffer-modified-p nil)) ; Prevent asking in paranoiac 29+.
    (when (kill-buffer buffer)
      (dolist (win windows)
        (when (window-live-p win)
          (ignore-errors (delete-window win)))))))
(helm-define-key-with-subkeys global-map (kbd "C-x k") ?k 'tv:kill-buffer-and-windows)

;; A simple zoom window that DTRT.
;; Always overwrite the previous winconf when modifying it and zooming
;; again from the same buffer.  Each buffer keeps its own winconf.
(defvar zoom-window-config nil)
(defun zoom-window ()
  "Zoom window."
  (interactive)
  (let ((winconf (buffer-local-value 'zoom-window-config (current-buffer))))
    (cond ((and (one-window-p) (null winconf))
           (message "Window is already full screen"))
          ((one-window-p)
           (set-window-configuration winconf))
          (t
           (set (make-local-variable 'zoom-window-config)
                (current-window-configuration))
           (delete-other-windows)))))
(global-set-key (kbd "C-&") #'zoom-window)

;;; Org
;;
(with-eval-after-load 'org
  (require 'org-config))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-k") 'org-capture)

;;; Dired
;;
(with-eval-after-load 'dired
  (setq dired-dwim-target t
        dired-auto-revert-buffer t
        dired-backup-overwrite nil ; nil, always, ask.
        dired-listing-switches (purecopy "-alh")
        dired-create-destination-dirs 'ask
        dired-mouse-drag-files 'copy)
  (when (boundp 'dired-vc-rename-file)
    (setq dired-vc-rename-file t))
  (require 'dired-extension))

;;; Image dired
;;
(with-eval-after-load 'image-dired
  (setq image-dired-thumbnail-storage 'standard
        ;; Be consistent with emacs-29.
        image-dired-cmd-pngnq-program "pngquant"
        image-dired-cmd-pngnq-options '("--ext" "-nq8.png" "%t")))

;;; Mouse avoidance mode
;;
(setq mouse-avoidance-banish-position
      '((frame-or-window . frame)
        (side . right)
        (side-pos . -2)
        (top-or-bottom . top)
        (top-or-bottom-pos . 1)))
(mouse-avoidance-mode 'banish)

;;; Ledger
;;
(autoload 'ledger-mode "ledger-mode" nil t)
(autoload 'csv2ledger "ledger-config" nil t)
(autoload 'ledger-position "ledger-config" nil t)
(setenv "LEDGER_PAGER" "cat")
(add-to-list 'auto-mode-alist '("\\.dat\\'" . ledger-mode))

;;; Rectangle
;;
(autoload 'rectangle-utils-insert-at-right         "rectangle-utils" nil t)
(autoload 'rectangle-utils-menu                    "rectangle-utils" nil t)
(autoload 'rectangle-utils-extend-rectangle-to-end "rectangle-utils" nil t)
(global-set-key (kbd "C-x r e") 'rectangle-utils-extend-rectangle-to-end)
(global-set-key (kbd "C-x r h") 'rectangle-utils-menu)
(global-set-key (kbd "C-x r <right>") 'rectangle-utils-insert-at-right)

;;; Zop-to-char
;;
(autoload 'zop-to-char "zop-to-char" nil t)
(autoload 'zop-up-to-char "zop-to-char" nil t)
(with-eval-after-load 'zop-to-char
  (setq zop-to-char-prec-keys '(left ?\C-b ?\M-a)
        zop-to-char-next-keys '(right ?\C-f ?\M-e)))
(global-set-key [remap zap-to-char] 'zop-to-char)

;;; Iedit
;;
;; Installed from source in site-lisp.
;; It is patched to allow sexp replacements in
;;`iedit-replace-occurrences', see:
;; ~/work/github/iedit/iedit-lib.el:908 in iedit_read_string branch.
(autoload 'iedit-mode "iedit" nil t)
(autoload 'iedit-rectangle-mode "iedit-rect" nil t)
(defun iedit-narrow-to-defun (arg)
  (interactive "P")
  (require 'iedit)
  (save-window-excursion
    (save-restriction
      (narrow-to-defun)
      (iedit-mode arg))))
(with-eval-after-load 'iedit
  (setq iedit-increment-format-string "%03d"))
(global-set-key (kbd "C-²") 'iedit-narrow-to-defun)
(global-set-key (kbd "C-;") 'iedit-mode)

(with-eval-after-load 'iedit-rect
  (setq iedit-increment-format-string "%03d"))
(global-set-key [C-return] 'iedit-rectangle-mode)

;;; Eldoc
;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-minor-mode-string nil)

;;; Python config
;;
(when (boundp 'gud-pdb-command-name)
  (setq gud-pdb-command-name "ipdb3"))
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --autoindent --simple-prompt --InteractiveShell.display_page=True"
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'semantic-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local mode-name "py")
            (define-key python-mode-map (kbd "C-c i") 'helm-semantic-or-imenu)
            (define-key python-mode-map (kbd "C-m")   'newline-and-indent)))
(defun tv:run-or-switch-to-python-shell ()
  (interactive)
  (let* ((buf      (ignore-errors (python-shell-get-process-or-error t)))
         (proc-buf (and buf (process-buffer buf)))
         (win      (and proc-buf (get-buffer-window proc-buf 'visible))))
    (cond ((and proc-buf win)
           (quit-window nil win))
          (proc-buf (pop-to-buffer proc-buf nil t))
          (t (call-interactively #'run-python)))))
(global-set-key (kbd "<f11> p") 'tv:run-or-switch-to-python-shell)

;;; Tramp-config
;;
(with-eval-after-load 'tramp
  ;; scp is better for copying large files but not working with many
  ;; files.
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 0)
  ;; (setq tramp-verbose 10 tramp-debug-to-file t helm-tramp-verbose 10)
  ;; No messages
  (when (boundp 'tramp-message-show-message)
    (setq tramp-message-show-message nil))

  (when (< emacs-major-version 30)
    (setq tramp-use-ssh-controlmaster-options nil))

  (when (boundp 'tramp-allow-unsafe-temporary-files)
    (setq tramp-allow-unsafe-temporary-files t))
  
  (when (boundp 'tramp-use-scp-direct-remote-copying)
    (setq tramp-use-scp-direct-remote-copying t))

  ;;   Allow connecting as root on all remote Linux machines except this one.
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
               '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))
  (setq ange-ftp-ftp-program-name "pftp"))

;;; Calendar and diary
;;
(with-eval-after-load 'calendar
  ;; Disable the fucking org bindings in emacs-28
  (when (fboundp 'org--setup-calendar-bindings)
    (fset 'org--setup-calendar-bindings 'ignore))
  (setq diary-file "~/.emacs.d/diary")
  (unless (fboundp 'fancy-diary-display) ; Fix emacs-25.
    (defalias 'fancy-diary-display 'diary-fancy-display))
  (defface tv:calendar-blocks
      '((t (:background "ForestGreen")))
    "Face used to highlight diary blocks in calendar."
    :group 'calendar)
  (defface tv:diary-cyclic
    '((t (:inherit italic :foreground "Cyan")))
  "Face for diary cyclic entries."
  :group 'calendar)
  ;; Add a different face in diary entry like this:
  ;; %%(diary-block 8 2 2021 13 2 2021 'tv:calendar-blocks-1)
  (defface tv:calendar-blocks-1
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

  ;; Sunrise/sunset (S)
  ;; (info "(emacs) Sunrise/Sunset")
  (setq calendar-latitude 44.6
        calendar-longitude -6.6)

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

  (defun tv:calendar-diary-or-holiday (arg)
    "A single command for diary and holiday entries."
    ;; Assume diary and holidays are shown in calendar.
    (interactive "p")
    (let* ((ovs (overlays-at (point)))
           (props (cl-loop for ov in ovs
                           for prop = (cadr (overlay-properties ov))
                           when (memq prop '(diary tv:diary-cyclic holiday diary-anniversary
                                             tv:calendar-blocks tv:calendar-blocks-1))
                           collect prop)))
      (cond ((and (or (memq 'diary props)
                      (memq 'tv:diary-cyclic props)
                      (memq 'tv:calendar-blocks props)
                      (memq 'tv:calendar-blocks-1 props)
                      (memq 'diary-anniversary props))
                  (memq 'holiday props))
             (cl-letf (((symbol-function 'message) #'ignore))
               (diary-view-entries arg))
             (calendar-cursor-holidays))
            ((or (memq 'diary props)
                 (memq 'tv:diary-cyclic props)
                 (memq 'tv:calendar-blocks props)
                 (memq 'tv:calendar-blocks-1 props)
                 (memq 'diary-anniversary props))
             (cl-letf (((symbol-function 'message) #'ignore))
               (diary-view-entries arg)))
            ((memq 'holiday props)
             (calendar-cursor-holidays))
            (t (message "Nothing special on this date")))))
  
  (unless (fboundp 'calendar-recenter)
    (defun calendar-recenter ()
      (interactive nil 'calendar-mode)
      (let ((month-at-pt (car (calendar-cursor-to-date)))
            (iter        (iterator:list '(center left right) 'cycle))
            pos)
        (cl-labels ((cal-recenter ()
                      (setq pos (iterator:next iter))
                      (pcase pos
                        ('center (pcase month-at-pt
                                   ((pred (< _ displayed-month))
                                    (calendar-scroll-right))
                                   ((pred (> _ displayed-month))
                                    (calendar-scroll-left))))
                        ('left  (calendar-scroll-left))
                        ('right (calendar-scroll-right 2)))
                      t))
          (cal-recenter)
          (while (let ((event (read-event)))
                   (pcase event
                     ('?\C-l (cal-recenter))
                     (_ (setq unread-command-events
                              (nconc (list event) unread-command-events))
                        nil))))))))

  (define-key calendar-mode-map (kbd "C-l")       'calendar-recenter)
  (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "C-<left>")  'calendar-backward-month)
  (define-key calendar-mode-map (kbd "RET")       'tv:calendar-diary-or-holiday))

;;; Appointements (appt)
;;
(with-eval-after-load 'appt
  (setq appt-display-format 'echo     ; Values: 'echo, 'window or nil.
        ;; Allow inserting at end of diary entry e.g. "##warntime 30".
        appt-warning-time-regexp "warntime \\([0-9]+\\)"
        appt-display-mode-line t))
(add-hook 'emacs-startup-hook 'appt-activate)

;;; Bookmarks
;;
(with-eval-after-load 'bookmark
  (add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
  (setq bookmark-bmenu-toggle-filenames nil)
  (setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
  (setq bookmark-automatically-show-annotations nil)
  (and (boundp 'bookmark-bmenu-use-header-line)
       (setq bookmark-bmenu-use-header-line nil))
  (setq bookmark-watch-bookmark-file nil)
  ;; This for unknown reasons adds a orange point in fringe when
  ;; switching to HFF from a bookmark and then quitting, not sure
  ;; what this feature is for and what the benefit is, so disable it.
  (and (boundp 'bookmark-fringe-mark)
       (setq bookmark-fringe-mark nil))

  ;; Fix switching to other window or frame actions
  ;; (fix emacs bug #75354, merged in master branch).
  (when (< emacs-major-version 31)
    (defun tv:bookmark--jump-via (bookmark-name-or-record display-function)
      (let (buf point)
        (save-window-excursion
          (bookmark-handle-bookmark bookmark-name-or-record)
          (setq buf (current-buffer)
                point (point)))
        (funcall display-function buf)
        (when-let* ((win (get-buffer-window buf 0)))
          (set-window-point win point))
        (when bookmark-fringe-mark
          (let ((overlays (overlays-in (pos-bol) (1+ (pos-bol))))
                temp found)
            (while (and (not found) (setq temp (pop overlays)))
              (when (eq 'bookmark (overlay-get temp 'category))
                (setq found t)))
            (unless found
              (bookmark--set-fringe-mark))))
        ;; FIXME: we used to only run bookmark-after-jump-hook in
        ;; `bookmark-jump' itself, but in none of the other commands.
        (run-hooks 'bookmark-after-jump-hook)
        (when bookmark-automatically-show-annotations
          ;; if there is an annotation for this bookmark,
          ;; show it in a buffer.
          (bookmark-show-annotation bookmark-name-or-record))))
    (advice-add 'bookmark--jump-via :override #'tv:bookmark--jump-via))

  ;; Don't use `bookmark-current-bookmark' which is always wrong.
  (advice-add 'bookmark-make-record :before
              (lambda (&rest _ignore)
                "Disable the evil `bookmark-current-bookmark' mechanism."
                (kill-local-variable 'bookmark-current-bookmark))))

;;; git-gutter-mode
;;
(autoload 'global-git-gutter-mode "git-gutter" nil t)
(global-git-gutter-mode) ; Enable live update.
(setq git-gutter:lighter " 🐱")
;; Activate live update timer.
(customize-set-variable 'git-gutter:update-interval 1)
;; Always a 0 width margin when no changes.
(setq git-gutter:hide-gutter t)
(global-set-key [remap vc-dir] 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key [remap vc-create-tag] 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;; Toggle whitespace changes view
(global-set-key (kbd "C-c _") 'git-gutter:toggle-space-view)
;; Run git-gutter after specified commands
(add-to-list 'git-gutter:update-commands 'helm-ls-git-run-stage-marked-and-extend-commit)
(add-to-list 'git-gutter:update-commands 'undo)

(defun tv:git-gutter:popup-diff-quit ()
  (interactive)
  (with-selected-window (get-buffer-window git-gutter:popup-buffer)
    (View-quit)))
;; (setq git-gutter:diff-option "-b")

(helm-define-key-with-subkeys
    global-map (kbd "C-x v n") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))
(helm-define-key-with-subkeys
    global-map (kbd "C-x v p") ?p 'git-gutter:previous-hunk '((?n . git-gutter:next-hunk)))
(helm-define-key-with-subkeys
    global-map (kbd "C-x v d") nil 'git-gutter:popup-hunk '((?n . git-gutter:next-hunk)
                                                            (?d . git-gutter:next-hunk)
                                                            (?p . git-gutter:previous-hunk)
                                                            (?q . tv:git-gutter:popup-diff-quit)))

;;; Addressbook
;;
(autoload 'addressbook-turn-on-mail-completion "addressbook-bookmark" nil t)
(autoload 'addressbook-bookmark-set            "addressbook-bookmark" nil t)
(autoload 'addressbook-gnus-sum-bookmark       "addressbook-bookmark" nil t)
(autoload 'addressbook-bmenu-edit              "addressbook-bookmark" nil t)
(autoload 'addressbook-bookmark-jump           "addressbook-bookmark" nil t)
(addressbook-turn-on-mail-completion)

;;; Auth-source
;;
(with-eval-after-load 'auth-sources
  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")
        ;; Don't ask to save password in auth-source file when
        ;; entering a password from tramp.
        auth-source-save-behavior nil))

;;; Whitespace-mode
;;
(with-eval-after-load 'whitespace-mode
  (add-to-list 'whitespace-style 'lines-tail)
  (setq whitespace-line-column 80))
(global-set-key (kbd "C-c W") 'whitespace-mode)

;;; markdown-mode (autoloaded by package)
;;
(with-eval-after-load 'markdown-mode
  (setq auto-mode-alist
        (append '(("\\.markdown$" . markdown-mode)
                  ("\\.md$" . markdown-mode)
                  ("\\.mdpp$" . markdown-mode))
                auto-mode-alist)))

;;; Markdown/Org-toc
;;
(autoload 'toc-insert-headers-at-point "toc" nil t)
(autoload 'toc-toc "toc" nil t)

;;; ffap
;;
(with-eval-after-load 'ffap
  (setq ffap-url-unwrap-remote '("ftp" "file"))
  (dolist (var '(ffap-machine-p-known
                 ffap-machine-p-local
                 ffap-machine-p-unknown))
    (set var 'reject))
  (when (> emacs-major-version 24)
    ;; See issue #1716 in helm.
    (setcdr (assq 'file ffap-string-at-point-mode-alist)
            '("--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))))

;;; esh-toggle
;;
(autoload 'eshell-toggle-cd "esh-toggle" nil t)
(autoload 'eshell-toggle "esh-toggle" nil t)
(global-set-key (kbd "<f2>") 'eshell-toggle)
(global-set-key (kbd "<S-f2>") 'eshell-toggle-cd)

;;; Eshell-config
;;
(setq eshell-prompt-function
      (lambda ()
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

(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'tv-utils)
            (setq eshell-pwd-convert-function
                  (lambda (f)
                    (if (file-equal-p (file-truename f) "/")
                        "/" f)))
            ;; This is needed for eshell-command (otherwise initial history is empty).
            (eshell-read-history eshell-history-file-name)
            ;; Helm completion with pcomplete
            (setq eshell-cmpl-ignore-case t
                  eshell-hist-ignoredups t)
            (eshell-cmpl-initialize)
            ;; Make `completion-at-point' use
            ;; bash-completion which works
            ;; with mostly all (no eshell aliases).
            (add-hook 'completion-at-point-functions 'bash-completion-eshell-capf -99 t)
            ;; Completion on eshell aliases among other things. It's
            ;; pretty unclear which map to use, at least it
            ;; changes nearly at each emacs version :-(.
            (define-key eshell-hist-mode-map (kbd "C-c TAB") 'helm-esh-pcomplete)
            ;; Helm completion on eshell history.
            (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
            (when (boundp 'eshell-hist-mode-map)
              (define-key eshell-hist-mode-map (kbd "M-p") 'helm-eshell-history))
            ;; Eshell prompt
            (set-face-attribute 'eshell-prompt nil :foreground "Gold1")
            (advice-add 'eshell-send-invisible :override #'tv:advice--eshell-send-invisible)))

;; Eshell history size
(setq eshell-history-size 1000)       ; Same as env var HISTSIZE.

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

(with-eval-after-load 'em-term
  (dolist (i '("tmux" "htop" "ipython" "alsamixer" "git-log" "tig" "mutt"))
    (add-to-list 'eshell-visual-commands i)))

;; Eshell modifiers
(with-eval-after-load 'em-basic
  ;; Fix echo, perhaps using as alias *echo is even better.
  (setq eshell-plain-echo-behavior t))

(setq async-shell-command-buffer 'new-buffer)

(when (>= emacs-major-version 31)
  (setq eshell-command-async-buffer 'new-buffer))

;;; display-line-numbers
;;
(with-eval-after-load 'display-line-numbers
  (setq display-line-numbers-type 'relative))

;;; Outline/org navigation
;;
(defun tv:outline-menu ()
  (cl-loop with h = '("next visible heading"
                      "previous visible heading"
                      "forward same level"
                      "backward same level")
           for k in '("n: " "p: " "f: " "b: ")
           concat (concat (propertize k 'face 'minibuffer-prompt) (pop h) "\n")))

(with-eval-after-load 'outline
  (helm-define-key-with-subkeys outline-minor-mode-map (kbd "C-c C-n")
                                ?n 'outline-next-visible-heading
                                '((?p . outline-previous-visible-heading)
                                  (?f . outline-forward-same-level)
                                  (?b . outline-backward-same-level)
                                  (?u . outline-up-heading))
                                (tv:outline-menu))
  (helm-define-key-with-subkeys outline-minor-mode-map (kbd "C-c C-p")
                                ?p 'outline-previous-visible-heading
                                '((?n . outline-next-visible-heading)
                                  (?f . outline-forward-same-level)
                                  (?b . outline-backward-same-level)
                                  (?u . outline-up-heading))
                                (tv:outline-menu)))

(with-eval-after-load 'org
  (helm-define-key-with-subkeys org-mode-map (kbd "C-c C-n")
                                ?n 'org-next-visible-heading
                                '((?p . org-previous-visible-heading)
                                  (?f . org-forward-heading-same-level)
                                  (?b . org-backward-heading-same-level)
                                  (?u . outline-up-heading))
                                (tv:outline-menu))
  (helm-define-key-with-subkeys org-mode-map (kbd "C-c C-p")
                                ?p 'org-previous-visible-heading
                                '((?n . org-next-visible-heading)
                                  (?f . org-forward-heading-same-level)
                                  (?b . org-backward-heading-same-level)
                                  (?u . outline-up-heading))
                                (tv:outline-menu)))

;;; Flyspell
;;
(with-eval-after-load 'ispell
  (setq-default ispell-program-name "aspell")
  (setq ispell-local-dictionary "francais"))

(defun tv:toggle-flyspell (arg)
  "Toggle `flyspell-mode'." 
  (interactive "P")
  (require 'flyspell)
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
        (helm-autoresize-mode -1)))))
(global-set-key (kbd "C-c @") 'tv:toggle-flyspell)

;;; Elisp/lisp
;;
(defun tv:set-mode-name (name)
  (setq-local mode-name name))
(defun tv:set-lisp-interaction-name ()
  (if (fboundp 'all-the-icons-fileicon)
      (tv:set-mode-name (all-the-icons-fileicon "lisp"))
    "Lisp"))
(defun tv:set-emacs-lisp-name ()
  (if (fboundp 'all-the-icons-fileicon)
      (tv:set-mode-name (all-the-icons-fileicon "elisp"))
    "Elisp"))
(add-hook 'lisp-interaction-mode-hook #'tv:set-lisp-interaction-name)
(add-hook 'emacs-lisp-mode-hook #'tv:set-emacs-lisp-name)

;; Fix indentation in CL functions (cl-flet/loop etc...).
(setq lisp-indent-function #'common-lisp-indent-function
      lisp-simple-loop-indentation 1
      lisp-loop-keyword-indentation 9 ;; Align cl-loop clauses.
      lisp-loop-forms-indentation 9) ;; Align cl-loop next clauses.
(let ((l '((cl-flet ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
           (cl-flet* . cl-flet)
           (cl-labels . cl-flet)
           (cl-macrolet . cl-flet))))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
           (car (cdr el))))))

(defun tv:goto-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;;; Next/previous buffer
;;
;; Affect `switch-to-prev/next-buffer' and `next/previous-buffer'.
(setq switch-to-prev-buffer-skip (lambda (_window buffer _bury-or-kill)
                                   "Prevent switching to unwanted buffers."
                                   ;; If that function returns
                                   ;; non-nil, `switch-to-prev/next-buffer'
                                   ;; will not switch to that buffer. 
                                   (not (buffer-file-name buffer))))

(helm-define-key-with-subkeys
    global-map (kbd "C-x <right>")
    'right #'next-buffer
    '((right . next-buffer) (left . previous-buffer))
    nil nil 2)
(helm-define-key-with-subkeys
    global-map (kbd "C-x <left>")
    'left #'previous-buffer
    '((right . next-buffer) (left . previous-buffer))
    nil nil 2)

(with-eval-after-load 'thingatpt
  (define-thing-chars bug "#[[:alnum:]]+"))
(defun tv:find-emacs-bug (bug)
  (interactive (list (or (thing-at-point 'bug)
                         (read-string "Bug number: "))))
  (setq bug (replace-regexp-in-string "#" "" (format "%s" bug)))
  (helm-find-files-1 (format "https://debbugs.gnu.org/%s" bug)))

;; Add fontification to some functions
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(cl-dolist\\)\\>" 1 font-lock-keyword-face))))

(defvar tv:autofill-modes '(emacs-lisp-mode
                            lisp-interaction-mode
                            sh-mode))
(defun tv:point-in-comment-p (pos)
  "Returns non-nil if POS is in a comment."
  (eq 'comment (syntax-ppss-context (syntax-ppss pos))))

(defun tv:point-in-docstring-p (pos)
  "Returns non-nil if POS is in a docstring."
  (and (eq 'string (syntax-ppss-context (syntax-ppss pos)))
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)))

(defun tv:turn-on-auto-fill-mode-maybe ()
  "Enable auto-fill-mode only in comments or docstrings.
Variable adaptive-fill-mode is disabled when a docstring field is
detected."
  (when (memq major-mode tv:autofill-modes)
    (let ((in-docstring (tv:point-in-docstring-p (point))))
      (setq adaptive-fill-mode (not in-docstring))
      (auto-fill-mode
       (if (or (tv:point-in-comment-p (point)) in-docstring)
           1 -1)))))
;; Maybe turn on auto-fill-mode when a comment or docstring field
;; is detected. Ensure the hook is appended otherwise things like
;; eldoc-eval will not work.
(add-hook 'post-command-hook #'tv:turn-on-auto-fill-mode-maybe t)

(global-set-key (kbd "<f11> s c")                     'tv:goto-scratch)
(global-set-key (kbd "<S-f12>")                       'cancel-debug-on-entry)
(global-set-key (kbd "M-:")                           'pp-eval-expression)
(define-key emacs-lisp-mode-map (kbd "RET")           'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "<next>")        'forward-page)
(define-key emacs-lisp-mode-map (kbd "<prior>")       'backward-page)
(define-key emacs-lisp-mode-map (kbd "C-M-j")         'backward-kill-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-a")       'tv:align-let)
(define-key emacs-lisp-mode-map (kbd "C-M-r")         'raise-sexp)
(define-key lisp-interaction-mode-map (kbd "RET")     'newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "C-M-j")   'backward-kill-sexp)
(define-key lisp-interaction-mode-map (kbd "C-c C-a") 'tv:align-let)
(define-key lisp-interaction-mode-map (kbd "C-M-r")   'raise-sexp)
(define-key lisp-mode-map (kbd "RET")                 'newline-and-indent)

;;; Macro expand
;;
;; Installed in ~/elisp. Keep this version as latest needs compat package.
(autoload 'macrostep-expand "macrostep.el" nil t)
(define-key lisp-interaction-mode-map (kbd "M-e") 'macrostep-expand)
(define-key emacs-lisp-mode-map (kbd "M-e") 'macrostep-expand)
(with-eval-after-load 'macrostep
  (define-key macrostep-mode-map (kbd "M-e") 'macrostep-collapse-all))

;;; Bash completion
;;
;; Used as CAPF in eshell.
(defun bash-completion-eshell-capf ()
  (require 'bash-completion)
  (append
   (bash-completion-dynamic-complete-nocomint
    (save-excursion (eshell-bol) (point))
    (point) t)
   '(:exclusive no)))

;;; Log-view (only used with RCS)
;;
(defun tv:log-view-fontify ()
  (font-lock-add-keywords nil '(("^revision [0-9.]*" . font-lock-comment-face)
                                ("[a-zA-Z ]*:" . font-lock-type-face))))
(add-hook 'log-view-mode-hook 'tv:log-view-fontify)

;;; Wgrep
;;
;; Needs only two files, wgrep.el and wgrep-helm.el, they are
;; installed in ~/elisp, it is loaded by helm.
(with-eval-after-load 'wgrep
  (setq wgrep-enable-key "\C-x\C-q"))

;;; Imenu
;;
(defun tv:imenu-add-extras-generic-expr ()
  "Make Imenu aware of helm-make-* macros."
  (require 'imenu)
  (add-to-list
   'imenu-generic-expression
   '("Functions"
     "^\\s-*(\\(?:helm-make-\\)?\\(?:persistent-\\)?command-from-action\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]*[^)]*" 1))
  (when (< emacs-major-version 31)
    (add-to-list
     'imenu-generic-expression
     '("Types" "^\\s-*(\\(?:oclosure-define\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]*[^)]*" 1))))
(add-hook 'emacs-lisp-mode-hook #'tv:imenu-add-extras-generic-expr)

;;; Yaml-mode
;;
;; Installed in ~/elisp
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; Wfnames
;;
(autoload 'wfnames-setup-buffer "wfnames" nil t)
(with-eval-after-load 'wfnames
  (setq wfnames-create-parent-directories t
        wfnames-interactive-rename nil))

;;; Emms (install it from source for Emacs < 28 to avoid seq dependency)
;;
(with-eval-after-load 'emms
  ;; When helm-emms will load and require emms, config will be
  ;; loaded.
  (require 'emms-config))

;;; Kmacros
;;
(with-eval-after-load 'kmacro
  (global-set-key (kbd "S-<f3>") 'kmacro-set-counter))

;;; modify `exchange-point-and-mark' so that it doesn't activate mark
;;  when it is not already active.
(if (boundp 'exchange-point-and-mark-highlight-region) ; Emacs-31+
    (setq exchange-point-and-mark-highlight-region nil)
  (defun tv:exchange-point-and-mark (&optional arg)
    "Put the mark where point is now, and point where the mark is now.

If Transient Mark mode is on, a prefix ARG deactivates the mark
if it is active and activates it if it is inactive, without prefix ARG
this command doesn't reactivate the mark if it was inactive and
doesn't deactivate it if it was active.

If Transient Mark mode is off, a prefix ARG enables Transient Mark
mode temporarily."
    (interactive "P")
    (let ((omark (mark t))
          (region-active (region-active-p))
	  (temp-highlight (eq (car-safe transient-mark-mode) 'only)))
      (when (null omark)
        (user-error "No mark set in this buffer"))
      (set-mark (point))
      (goto-char omark)
      (cond (temp-highlight)
            ((xor arg (not region-active))
	     (deactivate-mark))
	    (t (activate-mark)))
      nil))
  (advice-add 'exchange-point-and-mark :override #'tv:exchange-point-and-mark))

;;; registers
;;
(with-eval-after-load 'register
  (when (< emacs-major-version 30)
    (autoload 'register-preview-mode "register-preview" nil t)
    (register-preview-mode 1)
    (setq register-preview-use-preview 'insist))
  (defun register-delete (register)
    (interactive (list (register-read-with-preview "Delete register: ")))
    (setq register-alist (delete (assoc register register-alist)
                                 register-alist)))
  
  (if (< emacs-major-version 30)
      (cl-defmethod register-preview-command-info ((_command (eql register-delete)))
        (make-register-preview-info
         :types '(all)
         :msg "Delete register `%s'"
         :act 'modify
         :smatch t))
    (customize-set-variable 'register-use-preview 'insist)
    (unless (> emacs-major-version 30)
      ;; `register-command-info' is not used anymore in 31.
      (cl-defmethod register-command-info ((_command (eql register-delete)))
        (make-register-preview-info
         :types '(all)
         :msg "Delete register `%s'"
         :act 'modify
         :smatch t))))

  ;; Override original `register-val-describe' string method. Replace
  ;; unuseful text at beginning of string by a shorter one.
  (cl-defmethod register-val-describe
      :around ((val string) verbose)
      (setq val (copy-sequence val))
      (if (eq yank-excluded-properties t)
          (set-text-properties 0 (length val) nil val)
        (remove-list-of-text-properties 0 (length val)
				        yank-excluded-properties val))
      (if verbose
          (progn
	    (princ "the text:\n")
	    (princ val))
        (cond
          ;; Extract first N characters starting with first non-whitespace.
          ((string-match (format "[^ \t\n].\\{,%d\\}"
			         (min 72 (max 0 (window-width))))
		         val)
           (princ "text:\n")
           (princ (match-string 0 val)))
          ((string-match "^[ \t\n]+$" val)
           (princ "whitespace"))
          (t
           (princ "the empty string")))))

  (define-key global-map (kbd "C-x r C-d") #'register-delete))

;;; Irregular verbs (english)
;;
(autoload 'irregular-verbs-in-english "irregular-verbs-in-English" nil t)

;;; Boxquote
;;
(autoload 'boxquote-region "boxquote.el" nil t)

;;; Find Emacs bug at point
;;
(with-eval-after-load 'thingatpt
  (define-thing-chars bug "#[:alnum:]"))

;; Test bug#1234, Issue#2345.
(defun tv:find-bug-at-point (bug-number)
  (interactive
   (list (let* ((bug (thing-at-point 'bug 'noprops))
                (reg "\\(?:[Bb]ug\\|[Ii]ssue\\)#\\([[:alnum:]]+\\)")
                (num (and bug (string-match reg bug) (match-string 1 bug))))
           (read-string "Bug number: " num))))
  (let ((url (buffer-local-value 'bug-reference-url-format (current-buffer))))
    (when (or current-prefix-arg (null url))
      (setq url (read-string (format-prompt "Url" "debbugs.gnu.org")
                             nil nil "https://debbugs.gnu.org/%s")))
    (w3m-browse-url (format url bug-number) t t)))

;;; W3m
;;
;;
(with-eval-after-load 'w3m
  (require 'config-w3m)
  (define-key w3m-mode-map (kbd "M-<right>")      'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-<left>")       'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "V")              'helm-w3m-bookmarks)
  (define-key w3m-mode-map (kbd "M")              'w3m-view-url-with-browse-url)
  (define-key w3m-mode-map (kbd "M-q")            'tv:w3m-fill-region-or-paragraph)
  (define-key w3m-mode-map (kbd "<down>")         'next-line)
  (define-key w3m-mode-map (kbd "<up>")           'previous-line)
  (define-key w3m-mode-map (kbd "RET")            'tv:w3m-RET)
  (define-key w3m-mode-map (kbd "<backspace>")    'tv:scroll-up)
  (define-key w3m-lynx-like-map (kbd "S-<right>") 'w3m-view-this-url-new-session))
(global-set-key (kbd "<f7> h") 'w3m)

;;; Gnus
;;
;; See (info "(gnus) Gnus Unplugged")
(defun tv:start-gnus (&optional arg)
  (interactive "P")
  (if arg (gnus-unplugged) (gnus-plugged)))

(global-set-key (kbd "<f9>") #'tv:start-gnus)
(setq mail-user-agent 'gnus-user-agent)
(require 'mail-config) ; Needed for C-x m when gnus has not started yet.
(setq gnus-init-file "~/.emacs.d/.gnus.el")

;;; Sensors
;;
(autoload 'sensors "sensors" nil t)

;;; Colorcomp (Mix blue, red and green)
;;
(autoload 'colorcomp "colorcomp" nil t)

;;; iterator
;;
(autoload 'iterator:list "iterator")
(autoload 'iterator:circular "iterator")


;;; Load time
;;
(tv:emacs-load-time)

;;; psession
;;
(autoload 'psession-mode "psession" nil t)
(autoload 'psession-savehist-mode "psession" nil t)
(psession-mode 1)
(psession-savehist-mode 1)
(setq psession-save-buffers-unwanted-buffers-regexp
      "\\(diary\\|\\.jpg\\|\\.png\\|\\*image-native-display\\*\\)$")

;; Link now scratch buffer to file
(tv:restore-scratch-buffer)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: "^;;;[[:blank:]]+"
;; End:

;;; init.el ends here
