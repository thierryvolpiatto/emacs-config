;;; init-helm.el --- My startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Load all autoloads for helm and its extensions
;;
(require 'helm-config)
(load "/home/thierry/elisp/helm-extensions/helm-extensions-autoloads.el")


;;; Enable Modes (helm-mode is loading nearly everything).
;;
(use-package helm-mode
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex))
                          ((assq 'flex completion-styles-alist)
                           '(flex))))))
  :config
  (helm-mode 1))

(use-package helm-adaptive
  :config
  (setq helm-adaptive-history-file nil)
  (helm-adaptive-mode 1))

;; (use-package helm-utils
;;   ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
;;   :config (helm-popup-tip-mode 1))

(use-package helm-sys
  :commands (helm-top)
  :config (helm-top-poll-mode 1))

(use-package helm-info
  :bind ("C-h r" . helm-info-emacs))

(use-package helm-ipython
  :disabled t
  :config
  (define-key python-mode-map (kbd "<M-tab>") 'helm-ipython-complete)
  (define-key inferior-python-mode-map (kbd "C-i") 'helm-ipython-complete)
  (define-key python-mode-map (kbd "C-c C-i") 'helm-ipython-import-modules-from-buffer))

(use-package helm-ring
  :config
  ;; Action for helm kill-ring
  (defun helm/emamux:copy-from-kill-ring (candidate)
    (require 'emamux)
    (emamux:check-tmux-running)
    (when (null kill-ring)
      (error "kill-ring is nil!!"))
    (emamux:set-buffer candidate 0))
  (add-to-list 'helm-kill-ring-actions '("Emamux copy" . helm/emamux:copy-from-kill-ring) t)
  :bind (:map helm-kill-ring-map
              ("C-d" . helm-kill-ring-run-persistent-delete)))

(use-package helm-recoll
  :disabled t
  :commands helm-recoll
  :init (customize-set-variable 'helm-recoll-directories
                                  '(("confdir" . "~/.recoll-config")
                                    ("lisp sources" . "~/.recoll-sources")
                                    ("work" . "~/.recoll-work"))))

(use-package helm-ls-git
  :config
  (setq helm-ls-git-status-command 'magit-status-internal)
  (defmethod helm-setup-user-source ((source helm-ls-git-source))
    (helm-source-add-action-to-source-if
     "Magit find file"
     (lambda (candidate)
       (magit-find-file (magit-branch-or-commit-at-point) candidate))
     source
     (lambda (_candidate)
       ;; For `magit-branch-or-commit-at-point'.
       (require 'magit-git)
       (with-helm-current-buffer (magit-branch-or-commit-at-point)))
     1)))

(use-package helm-buffers
  :config
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching       t
        helm-buffer-skip-remote-checking  t
        helm-buffer-max-length            22
        helm-buffers-end-truncated-string "…"
        helm-buffers-maybe-switch-to-tab  t)

  (defmethod helm-setup-user-source ((source helm-source-buffers))
  "Adds additional actions to `helm-source-buffers-list'.
- Magit status."
  (setf (slot-value source 'candidate-number-limit) 300)
  (helm-aif (slot-value source 'action)
      (setf (slot-value source 'action)
        (helm-append-at-nth  (if (symbolp it)
                                 (symbol-value it)
                               it)
                             '(("Diff buffers" . helm-buffers-diff-buffers)) 4)))
  (helm-source-add-action-to-source-if
   "Magit status"
   (lambda (candidate)
     (funcall helm-ls-git-status-command
              (with-current-buffer candidate default-directory)))
   source
   (lambda (candidate)
     (locate-dominating-file (with-current-buffer candidate default-directory)
                             ".git"))
   1)))

(use-package helm-files
  :config
  (setq helm-ff-auto-update-initial-value        t
        helm-ff-allow-non-existing-file-at-point t)
  (customize-set-variable 'helm-ff-lynx-style-map t)

  (define-key helm-read-file-map (kbd "RET") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "C-i") nil)
  (define-key helm-find-files-map (kbd "C-/") 'helm-ff-run-find-sh-command)
  (define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)

  (defun helm/ff-candidates-lisp-p (candidate)
    (cl-loop for cand in (helm-marked-candidates)
             always (string-match "\\.el$" cand)))

  (defun helm-ff-recoll-index-directory (directory)
    "Create a recoll index directory from DIRECTORY.
Add the new created directory to `helm-recoll-directories' using the
basename of DIRECTORY as name.
By using `customize-set-variable', a new source is created for this
new directory."
    (cl-assert (boundp 'helm-recoll-directories) nil
               "Package helm-recoll not installed or configured")
    (let* ((bn (helm-basename (expand-file-name directory)))
           (index-dir (format "~/.recoll-%s" bn))
           (conf-file (expand-file-name "recoll.conf" index-dir)))
      (mkdir index-dir)
      (with-current-buffer (find-file-noselect conf-file)
        (insert (format "topdirs = %s" (expand-file-name directory)))
        (save-buffer)
        (kill-buffer))
      (customize-set-variable 'helm-recoll-directories
                              (append `((,bn . ,index-dir)) helm-recoll-directories))
      (message "Don't forget to index config directory with 'recollindex -c %s'" index-dir)))

  (defun helm-ff-recoll-index-directories (_candidate)
    (let ((dirs (helm-marked-candidates)))
      (cl-loop for dir in dirs
               when (file-directory-p dir)
               do (helm-ff-recoll-index-directory dir))))

  ;; Add actions to `helm-source-find-files' IF:
  (defmethod helm-setup-user-source ((source helm-source-ffiles))
    "Adds additional actions to `helm-find-files'.
    - Byte compile file(s) async
    - Byte recompile directory
    - Magit status
    - Github issues
    - Patch region on directory
    - Open in emms
    - Update directory autoloads"
    (helm-source-add-action-to-source-if
     "Byte compile file(s) async"
     (lambda (_candidate)
       (cl-loop for file in (helm-marked-candidates)
                do (async-byte-compile-file file)))
     source
     'helm/ff-candidates-lisp-p)
    (helm-source-add-action-to-source-if
     "Recover file"
     (lambda (candidate)
       (recover-file candidate))
     source
     (lambda (candidate)
       (file-exists-p (expand-file-name
                       (format "#%s#" (helm-basename candidate))
                       (helm-basedir candidate)))))
    (helm-source-add-action-to-source-if
     "Byte recompile directory (async)"
     'async-byte-recompile-directory
     source
     'file-directory-p)
    (helm-source-add-action-to-source-if
     "Magit status"
     (lambda (_candidate)
       (funcall helm-ls-git-status-command
                helm-ff-default-directory))
     source
     (lambda (candidate)
       (and (not (string-match-p ffap-url-regexp candidate))
            helm-ff-default-directory
            (locate-dominating-file helm-ff-default-directory ".git")))
     1)
    (helm-source-add-action-to-source-if
     "Patch region on directory"
     (lambda (_candidate)
       (with-helm-current-buffer
         (shell-command-on-region (region-beginning) (region-end)
                                  (format "patch -d %s -p1"
                                          helm-ff-default-directory))))
     source
     (lambda (_candidate)
       (with-helm-current-buffer
         (and (or (eq major-mode 'mu4e-view-mode)
                  (eq major-mode 'diff-mode))
              (region-active-p))))
     1)
    (helm-source-add-action-to-source-if
     "Open in emms"
     (lambda (candidate)
       (if (file-directory-p candidate)
           (emms-play-directory candidate)
         (emms-play-file candidate)))
     source
     (lambda (candidate)
       (or (and (file-directory-p candidate)
                (directory-files
                 candidate
                 nil ".*\\.\\(mp3\\|ogg\\|flac\\)$" t))
           (string-match-p ".*\\.\\(mp3\\|ogg\\|flac\\)$" candidate)))
     1)
    (helm-source-add-action-to-source-if
     "Update directory autoloads"
     (lambda (candidate)
       (require 'autoload)
       (let ((default-directory helm-ff-default-directory)
             (generated-autoload-file
              (read-file-name "Write autoload definitions to file: "
                              helm-ff-default-directory
                              nil nil nil
                              (lambda (f)
                                (string-match "autoloads\\|loaddefs" f)))))
         (cl-letf (((symbol-function 'autoload-generated-file)
                    (lambda ()
                      (expand-file-name generated-autoload-file default-directory))))
           (update-directory-autoloads (expand-file-name candidate)))))
     source
     (lambda (candidate)
       (and (file-directory-p candidate)
            (string= (helm-basename candidate) ".")))
     1)
    (helm-source-add-action-to-source-if
     "Recoll index directory"
     'helm-ff-recoll-index-directories
     source
     'file-directory-p
     3)))

(use-package helm-dictionary ; Its autoloads are already loaded.
  :commands helm-dictionary
  :config
  (setq helm-dictionary-database
        '(("en-fr" . "~/helm-dictionary/dic-en-fr.iso")
          ("fr-en" . "~/helm-dictionary/dic-fr-en.iso"))
        helm-dictionary-online-dicts
        '(("translate.reference.com en->fr" .
           "http://translate.reference.com/translate?query=%s&src=en&dst=fr")
          ("translate.reference.com fr->en" .
           "http://translate.reference.com/translate?query=%s&src=fr&dst=en"))))

;;;; Test Sources or new helm code.
;;   !!!WARNING EXPERIMENTAL!!!

(defun helm/turn-on-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode -1)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/turn-off-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line nil)
  ;;(helm-autoresize-mode 1)
  (setq helm-split-window-in-side-p nil)
  (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/occur-which-func ()
  (interactive)
  (with-current-buffer
      (or (helm-aif (with-helm-buffer
                      (window-buffer helm-persistent-action-display-window))
              (and (null (minibufferp it)) it))
          helm-current-buffer)
    (when (eq major-mode 'emacs-lisp-mode)
      (message "[%s]" (which-function)))))

(define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func)
(define-key helm-grep-map   (kbd "C-M-a") 'helm/occur-which-func)

;; Show the visibles buffers on top of list (issue #1301)

(defun helm/modify-ido-temp-list ()
  (let ((bl (mapcar #'buffer-name (buffer-list (selected-frame)))))
    (setq ido-temp-list (nconc (cdr bl) (list (car bl))))))
;;(add-hook 'ido-make-buffer-list-hook 'helm/modify-ido-temp-list)

(defun helm-find-files-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-find-files)))

(defun helm-M-x-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-M-x)))

(defun helm-occur-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-occur)))

(defun helm-mini-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-mini)))

(defun helm-do-grep-ag-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-do-grep-ag)))

(defun helm-do-git-grep-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-grep-do-git-grep)))

(defun helm-imenu-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-imenu)))

(defun helm-top-in-frame ()
  (interactive)
  (with-helm-in-frame
    (call-interactively #'helm-top)))

(define-key ctl-x-5-map (kbd "C-x c t") 'helm-top-in-frame)
(define-key ctl-x-5-map (kbd "C-x c i") 'helm-imenu-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-f") 'helm-find-files-in-frame)
(define-key ctl-x-5-map (kbd "M-x")     'helm-M-x-in-frame)
(define-key ctl-x-5-map (kbd "C-s")     'helm-occur-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-b") 'helm-mini-in-frame)
(define-key ctl-x-5-map (kbd "M-g a")   'helm-do-grep-ag-in-frame)
(define-key ctl-x-5-map (kbd "M-g g")   'helm-do-git-grep-in-frame)

(defun helm/insert-date-in-minibuffer ()
  (interactive)
  (with-selected-window (or (active-minibuffer-window)
                            (minibuffer-window))
    (unless (or (helm-follow-mode-p)
                helm--temp-follow-flag)
      (goto-char (point-max))
      (insert (format-time-string "%Y-%m-%d-%H:%M")))))
(define-key helm-find-files-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)
(define-key helm-read-file-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)


;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g") 'helm-apt)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "w") 'helm-w3m-bookmarks)
(define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c C-i")                      'helm-imenu)
(global-set-key (kbd "<f11>")                        nil)
(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "C-s")                          'helm-occur)
(global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g i")                 'helm-gid)
(define-key global-map (kbd "C-x r p")               'helm-projects-history)
(define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r")               'helm-dictionary)

(helm-multi-key-defun helm-multi-lisp-complete-at-point
    "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
  '(helm-lisp-indent
    helm-lisp-completion-at-point
    helm-complete-file-name-at-point)
  0.3)
(define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
(define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)

;; Indent or complete with completion-at-point
;; (setq tab-always-indent 'complete)

;; (define-key global-map (kbd "<backtab>") 'completion-at-point)

;; Cycle resume
(helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume)

;; sh-mode
(add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)

;; Minibuffer history
(customize-set-variable 'helm-minibuffer-history-key [remap next-matching-history-element])


;;; Describe key-bindings
;;
;;
(helm-descbinds-mode 1)            ; C-h b, C-x C-h


;;; Helm-variables
;;
;;
(setq helm-net-prefer-curl                            nil
      helm-kill-ring-threshold                        1
      helm-raise-command                              "wmctrl -xa %s"
      helm-scroll-amount                              4
      helm-input-idle-delay                           0.01
      helm-default-external-file-browser              "thunar"
      helm-pdfgrep-default-read-command               "evince --page-label=%p '%f'"
      helm-grep-default-command                       "ack-grep -Hn --color --smart-case --no-group %e %p %f"
      helm-grep-default-recurse-command               "ack-grep -H --color --smart-case --no-group %e %p %f"
      helm-grep-ag-command                            "rg --color=always --smart-case --no-heading --line-number %s %s %s"
      helm-reuse-last-window-split-state              t
      helm-always-two-windows                         t
      helm-split-window-inside-p                      nil
      helm-show-completion-display-function           #'helm-display-buffer-in-own-frame
      helm-commands-using-frame                       '(completion-at-point helm-apropos
                                                        helm-eshell-prompts helm-imenu
                                                        helm-imenu-in-all-buffers)
      helm-actions-inherit-frame-settings             t
      helm-use-frame-when-more-than-two-windows       t
      helm-use-frame-when-dedicated-window            t
      helm-frame-background-color                     "DarkSlateGray"
      helm-show-action-window-other-window            'left
      helm-surfraw-duckduckgo-url                     "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
      helm-google-suggest-search-url                  helm-surfraw-duckduckgo-url
      helm-allow-mouse                                t
      helm-apropos-fuzzy-match                        t
      helm-lisp-fuzzy-completion                      t
      helm-locate-fuzzy-match                         t
      helm-move-to-line-cycle-in-source               t
      ;; helm-tramp-verbose                              6
      helm-org-headings-fontify                       t
      helm-autoresize-max-height                      80 ; it is %.
      helm-autoresize-min-height                      20 ; it is %.
      fit-window-to-buffer-horizontally               1
      helm-highlight-matches-around-point-max-lines   30
      helm-turn-on-recentf nil
      helm-mini-default-sources '(helm-source-buffers-list helm-source-buffer-not-found)
      helm-debug-root-directory "/home/thierry/tmp/helm-debug"
      helm-follow-mode-persistent t
      helm-emms-use-track-description-function        nil
      helm-trash-remote-files           t
      helm-grep-git-grep-command
      "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f"
      helm-dwim-target 'next-window
      helm-candidate-number-limit 500
      helm-find-noerrors t
      helm-window-show-buffers-function #'helm-window-mosaic-fn
      helm-completing-read-handlers-alist
      '((xref-find-references . helm-completing-read-default-find-tag)
        (write-file . helm-read-file-name-handler-1)
        (basic-save-buffer . helm-read-file-name-handler-1)
        (find-tag . helm-completing-read-default-find-tag)
        (xref-find-definitions . helm-completing-read-default-find-tag)
        (xref-find-references . helm-completing-read-default-find-tag)
        (tmm-menubar)
        (mu4e-view-save-attachment-multi . helm-read-file-name-handler-1)
        (mu4e-view-save-attachment-single . helm-read-file-name-handler-1)
        (cancel-debug-on-entry)
        (org-capture . helm-org-completing-read-tags)
        (org-set-tags . helm-org-completing-read-tags)
        (dired-do-rename . helm-read-file-name-handler-1)
        (dired-do-copy . helm-read-file-name-handler-1)
        (dired-do-symlink . helm-read-file-name-handler-1)
        (dired-do-relsymlink . helm-read-file-name-handler-1)
        (dired-do-hardlink . helm-read-file-name-handler-1)
        (basic-save-buffer . helm-read-file-name-handler-1)
        (write-file . helm-read-file-name-handler-1)
        (write-region . helm-read-file-name-handler-1))
      helm-el-package-autoremove-on-start t
      helm-el-package-upgrade-on-start t)

(customize-set-variable 'helm-imenu-lynx-style-map t)

;; find-file-hook
(add-hook 'find-file-hook 'helm-save-current-pos-to-mark-ring)

;; Avoid hitting forbidden directory .gvfs when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")
(add-to-list 'completion-ignored-extensions ".dbus/")
(add-to-list 'completion-ignored-extensions "dconf/")


;;; Toggle grep program
;;
;;
(defun helm/eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=always -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=always -d recurse %e -n%cH -e %p %f")
        (setq helm-grep-default-command
              "ack-grep -Hn --color --smart-case --no-group %e %p %f"
              helm-grep-default-recurse-command
              "ack-grep -H --color --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

;;; Debugging
;;
;;
(defun helm/debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))



;;; helm-occur/grep-mode
;;
(add-hook 'helm-occur-mode-hook 'hl-line-mode)
(add-hook 'helm-grep-mode-hook 'hl-line-mode)

(provide 'init-helm)

;;; init-helm.el ends here
