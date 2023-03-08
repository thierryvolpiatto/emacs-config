;;; init-helm.el --- My startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Set up helm first (will load helm-autoloads.el)

(require 'helm)
;; Only needed when installed from source.
(require 'helm-autoloads)
(setq helm-input-idle-delay                     0.01
      helm-reuse-last-window-split-state        t
      helm-always-two-windows                   t
      helm-split-window-inside-p                nil
      helm-commands-using-frame                 '(completion-at-point
                                                  ;; helm-apropos
                                                  helm-eshell-prompts helm-imenu
                                                  helm-imenu-in-all-buffers)
      helm-actions-inherit-frame-settings       t
      helm-use-frame-when-more-than-two-windows t
      helm-use-frame-when-dedicated-window      t
      helm-frame-background-color               "DarkSlateGray"
      helm-show-action-window-other-window      'left
      helm-allow-mouse                          t
      helm-move-to-line-cycle-in-source         t
      helm-autoresize-max-height                80   ; it is %.
      helm-autoresize-min-height                20   ; it is %.
      helm-debug-root-directory                 "/home/thierry/tmp/helm-debug"
      helm-follow-mode-persistent               t
      helm-candidate-number-limit               50
      helm-visible-mark-prefix                  "✓")
(set-face-foreground 'helm-mark-prefix "Gold1")
(add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
(helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume)

;;; Load all autoloads for helm extensions
;;
;;
(load "/home/thierry/elisp/helm-extensions/helm-extensions-autoloads.el")


(defun helm/debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm/occur-which-func ()
  (interactive)
  (with-current-buffer
      (or (helm-aif (with-helm-buffer
                      (window-buffer helm-persistent-action-display-window))
              (and (null (minibufferp it)) it))
          helm-current-buffer)
    (when (eq major-mode 'emacs-lisp-mode)
      (message "[%s]" (which-function)))))

(defun helm/bash-history ()
  (interactive)
  (helm :sources (helm-build-in-file-source "Bash history" "~/.bash_history"
                   :action '(("Kill new" . kill-new)
                             ("Send command to Tmux" . emamux:send-command)))
        :buffer "*helm bash history*"))

;;; Package declarations.
;;

;;; Helm-mode (it is loading nearly everything)
;; 
(add-hook 'helm-mode-hook
          (lambda ()
            (setq completion-styles
                  (cond ((assq 'helm-flex completion-styles-alist)
                         '(helm-flex)) ;; emacs-26.
                        ((assq 'flex completion-styles-alist)
                         '(flex)))))) ;; emacs-27+.

(setq helm-completion-mode-string " ⎈")

(helm-mode 1)

(setq helm-completing-read-handlers-alist
      '((find-tag . helm-completing-read-default-find-tag)
        (xref-find-definitions . helm-completing-read-default-find-tag)
        (xref-find-references . helm-completing-read-default-find-tag)
        (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
        (tmm-menubar)
        (find-file)
        (execute-extended-command)
        (cancel-debug-on-entry)
        (org-capture . helm-org-completing-read-tags)
        (org-set-tags . helm-org-completing-read-tags)
        (dired-do-rename . helm-read-file-name-handler-1)
        (dired-do-copy . helm-read-file-name-handler-1)
        (dired-do-symlink . helm-read-file-name-handler-1)
        (dired-do-relsymlink . helm-read-file-name-handler-1)
        (dired-do-hardlink . helm-read-file-name-handler-1)
        (basic-save-buffer . helm-read-file-name-handler-1)
        (write-file . (default helm-read-file-name-handler-1))
        (write-region . (default helm-read-file-name-handler-1))))

;; Fix CAP with LSP in python.
(add-to-list 'helm-completion-styles-alist '(python-mode . (emacs helm flex)))
(add-to-list 'helm-completion-styles-alist '(wfnames-mode . (emacs helm flex)))

;; Allow using `completions-detailed' in these commands. Just a proof
;; of concept as helm-apropos is displaying all these infos in a much
;; better way and faster.
(dolist (f '(describe-symbol describe-variable describe-function))
  (add-to-list 'helm-completion-styles-alist `(,f . (emacs helm flex))))

(when (boundp 'completions-detailed)
  (setq completions-detailed t))

;;; Helm-adaptive
;;
(require 'helm-adaptive)
(setq helm-adaptive-history-file nil)
(helm-adaptive-mode 1)

;;; Helm-bookmark
;;
(with-eval-after-load 'helm-bookmark
  (setq helm-bookmark-use-icon t))

;;; Helm-utils
;;
(with-eval-after-load 'helm-utils
  ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
  (helm-popup-tip-mode 1)
  (setq helm-highlight-matches-around-point-max-lines   '(30 . 30)
        helm-window-show-buffers-function #'helm-window-mosaic-fn)
  (add-hook 'find-file-hook 'helm-save-current-pos-to-mark-ring))

;;; Helm-sys
;;
(helm-top-poll-mode 1)

;;; Helm-ring
;;
(with-eval-after-load 'helm-ring
  (setq helm-kill-ring-threshold 1)
  ;; Action for helm kill-ring
  (defun helm/emamux:copy-from-kill-ring (candidate)
    (require 'emamux)
    (emamux:check-tmux-running)
    (when (null kill-ring)
      (error "kill-ring is nil!!"))
    (emamux:set-buffer candidate 0))
  (add-to-list 'helm-kill-ring-actions '("Emamux copy" . helm/emamux:copy-from-kill-ring) t)
  (add-to-list 'helm-kill-ring-actions '("Emamux send command" . emamux:send-command) t)
  (define-key helm-kill-ring-map (kbd "C-d") 'helm-kill-ring-run-persistent-delete))

;;; Helm-buffers
;;
(with-eval-after-load 'helm-buffers
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching       t
        helm-buffer-skip-remote-checking  t
        helm-buffer-max-length            22
        helm-buffers-end-truncated-string "…"
        helm-buffers-maybe-switch-to-tab  t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-buffer-not-found)
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help" "\\`\\*skitour")
        helm-buffers-show-icons t)

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent)
  
  (defun helm-buffers-clean-backslashes-in-docstrings (_candidate)
    (let ((bufs (helm-marked-candidates)))
      (cl-loop for buf in bufs
               do (with-current-buffer buf
                    ;; Predicates to determine if point is in
                    ;; docstring are based on text-props, so all text
                    ;; in buffer need to be fontified [1].
                    (jit-lock-fontify-now)
                    (tv/clean-backslashes-in-docstrings)
                    (when (buffer-modified-p)
                      (save-buffer))))))
  
  (add-to-list 'helm-type-buffer-actions
               '("Clean backslashes in docstrings"
                 . helm-buffers-clean-backslashes-in-docstrings)
               t)

  (defun tv/clean-single-quotes-in-docstring ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \n]'" nil t)
        (forward-char -1)
        (if (tv/point-in-docstring-p (point))
            (progn (insert "\\\\=") (forward-char 1))
          (forward-char 1)))))
  
  (defun helm-buffers-clean-single-quotes-in-docstrings (_candidate)
    (let ((bufs (helm-marked-candidates)))
      (cl-loop for buf in bufs
               do (with-current-buffer buf
                    (jit-lock-fontify-now) ; Same comment as [1] above.
                    (tv/clean-single-quotes-in-docstring)
                    (when (buffer-modified-p)
                      (save-buffer))))))
  (add-to-list 'helm-type-buffer-actions
               '("Clean single quotes  in docstrings" .
                 helm-buffers-clean-single-quotes-in-docstrings)
               t)
  
  (cl-defmethod helm-setup-user-source ((source helm-source-buffers))
    "Adds additional actions to `helm-source-buffers-list'.
- Git status."
    (setf (slot-value source 'candidate-number-limit) 300)
    (helm-aif (slot-value source 'action)
        (setf (slot-value source 'action)
              (helm-append-at-nth
               (if (symbolp it)
                   (symbol-value it)
                 it)
               '(("Diff buffers" . helm-buffers-diff-buffers)) 4)))
    (helm-source-add-action-to-source-if
     "Git status"
     (lambda (candidate)
       (funcall helm-ls-git-status-command
                (with-current-buffer candidate default-directory)))
     source
     (lambda (candidate)
       (locate-dominating-file (with-current-buffer candidate default-directory)
                               ".git"))
     1)))

;;; Helm-files
;;
(with-eval-after-load 'helm-files
  (setq helm-ff-auto-update-initial-value        t
        helm-ff-allow-non-existing-file-at-point t
        helm-trash-remote-files                  t
        helm-dwim-target                         'next-window
        helm-locate-recursive-dirs-command "fdfind --hidden --type d --glob '*%s*' %s"
        helm-ff-eshell-unwanted-aliases '("sudo" "cdu" "man"
                                          "gpg-pubkey-export-armor" "gpg-secretkey-export-armor")
        helm-ff-drag-and-drop-default-directory "/home/thierry/Bureau/"
        helm-file-name-history-hide-deleted t)
  
  (customize-set-variable 'helm-ff-nohighlight-matches nil)
  
  (require 'image-dired)
  (setq image-dired-thumbnail-storage 'standard
        ;; Be consistent with emacs-29.
        image-dired-cmd-pngnq-program "pngquant"
        image-dired-cmd-pngnq-options '("--ext" "-nq8.png" "%t"))
  
  ;; (defun helm-ff-wfnames (_candidate)
  ;; (let ((marked (helm-marked-candidates :with-wildcard t)))
  ;; (wfnames-setup-buffer marked)))
  
  ;; (helm-make-command-from-action helm-run-wfnames
  ;; "Run wfnames from HFF."
  ;; 'helm-ff-wfnames)
  ;; (define-key helm-find-files-map (kbd "C-x C-q") 'helm-run-wfnames)
  
  ;; (setq helm-find-files-actions
  ;; (helm-append-at-nth
  ;; helm-find-files-actions
  ;; '(("Edit filename(s)" . helm-ff-wfnames)) 2))
  (setq helm-ff-edit-marked-files-fn #'helm-ff-wfnames)
  
  (defun helm-ff-dragon (files)
    "Create a small window with FILES ready to drag and drop.
Use this to drop files on externals applications or desktop.
Dropping on emacs buffers with this is not supported.

Needs `dragon' executable: https://github.com/mwh/dragon."
    (interactive (list (helm-marked-candidates)))
    (cl-assert (executable-find "dragon") nil "Dragon executable not found")
    (apply #'call-process "dragon" nil nil nil "--all" "--and-exit" files))
  (define-key helm-find-files-map (kbd "C-c m") 'helm-ff-dragon)

  (customize-set-variable 'helm-ff-lynx-style-map t)
  (define-key helm-read-file-map (kbd "RET") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "C-i") nil)
  (define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
  
  (defun helm/insert-date-in-minibuffer ()
    (interactive)
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (unless (or (helm-follow-mode-p)
                  helm--temp-follow-flag)
        (insert (format-time-string "%Y-%m-%d-%H:%M")))))
  (define-key helm-find-files-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)
  (define-key helm-read-file-map (kbd "C-c y") 'helm/insert-date-in-minibuffer)
  
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
  
  (defun tv/change-xfce-background (file)
    (let* ((screen  (getenv "DISPLAY"))
           (monitor (shell-command-to-string
                     "echo -n $(xrandr | awk '/\\w* connected/ {print $1}')"))
           (desktop (and (display-graphic-p)
                         (x-window-property "_NET_CURRENT_DESKTOP" nil "CARDINAL" 0 nil t)))
           (prop    (format "/backdrop/screen%s/monitor%s/workspace%s/last-image"
                            (substring screen (1- (length screen)))
                            monitor
                            (or desktop 0)))
           (proc    (apply #'start-process "set background" nil "xfconf-query"
                           `("-c" "xfce4-desktop" "-p" ,prop "-s" ,file))))
      (set-process-sentinel
       proc (lambda (_proc event)
              (if (string= event "finished\n")
                  (message "Background changed successfully to %s" (helm-basename file))
                (message "Failed to change background"))))))

  (defun helm-ff-csv2ledger (candidate)
    (csv2ledger "Socgen" candidate "/home/thierry/finance/ledger.dat"))
  
  ;; Add actions to `helm-source-find-files' IF:
  (cl-defmethod helm-setup-user-source ((source helm-source-ffiles))
    "Adds additional actions and settings to `helm-find-files'.
    - Byte compile file(s) async
    - Byte recompile directory async
    - Open info file
    - Patch region on directory
    - Open in emms
    - Update directory autoloads
    - Recoll directory creation
    - Epa encrypt file
    - Change background
    - Csv2ledger"
    (helm-aif (slot-value source 'match)
        (setf (slot-value source 'match)
              (append it
                      '((lambda (candidate)
                          (string-match (concat (helm-basedir helm-input)
                                                (char-fold-to-regexp
                                                 (helm-basename helm-input)))
                                        candidate))))))
    ;; Byte compile file async
    (helm-source-add-action-to-source-if
     "Byte compile file(s) async"
     (lambda (_candidate)
       (cl-loop for file in (helm-marked-candidates)
                do (async-byte-compile-file file)))
     source
     'helm/ff-candidates-lisp-p)
    ;; Recover file from its autoload file
    (helm-source-add-action-to-source-if
     "Recover file"
     (lambda (candidate)
       (recover-file candidate))
     source
     (lambda (candidate)
       (file-exists-p (expand-file-name
                       (format "#%s#" (helm-basename candidate))
                       (helm-basedir candidate)))))
    ;; Byte recompile dir async
    (helm-source-add-action-to-source-if
     "Byte recompile directory (async)"
     'async-byte-recompile-directory
     source
     'file-directory-p)
    ;; Info on .info files
    (helm-source-add-action-to-source-if
     "Open info file"
     (lambda (candidate) (info candidate))
     source
     (lambda (candidate) (helm-aif (file-name-extension candidate)
                             (string= it "info")))
     1)
    ;; Patch region on dir
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
    ;; Emms
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
    ;; update-directory-autoloads
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
         (update-directory-autoloads default-directory)))
     source
     (lambda (candidate)
       (and (file-directory-p candidate)
            (string= (helm-basename candidate) ".")))
     1)
    ;; Setup recoll dirs
    (helm-source-add-action-to-source-if
     "Recoll index directory"
     'helm-ff-recoll-index-directories
     source
     'file-directory-p
     3)
    ;; Encrypt file
    (helm-source-add-action-to-source-if
     "Epa encrypt file"
     (lambda (candidate)
       (require 'epg) (require 'epa)
       (epa-encrypt-file candidate
                         (helm :sources (helm-build-sync-source
                                            "Select recipient for encryption: "
                                          :persistent-action 'ignore
                                          :candidates 'helm-epa-get-key-list))))
     source
     'file-exists-p
     3)
    ;; Background
    (helm-source-add-action-to-source-if
     "Change background"
     'tv/change-xfce-background
     source
     (lambda (candidate)
       (member (file-name-extension candidate) '("jpg" "jpeg" "png")))
     3)
    (helm-source-add-action-to-source-if
     "Csv2Ledger"
     'helm-ff-csv2ledger
     source
     (lambda (candidate)
       (member (file-name-extension candidate) '("csv")))
     3))
  
  (helm-ff-icon-mode 1))

;;; Helm-dictionary
;;
(with-eval-after-load 'helm-dictionary ; Its autoloads are already loaded.
  (setq helm-dictionary-database
        '(("en-fr" . "~/helm-dictionary/dic-en-fr.iso")
          ("fr-en" . "~/helm-dictionary/dic-fr-en.iso"))
        helm-dictionary-online-dicts
        '(("translate.reference.com en->fr" .
           "http://translate.reference.com/translate?query=%s&src=en&dst=fr")
          ("translate.reference.com fr->en" .
           "http://translate.reference.com/translate?query=%s&src=fr&dst=en")
          ("en.wiktionary.org" . "http://en.wiktionary.org/wiki/%s")
          ("fr.wiktionary.org" . "http://fr.wiktionary.org/wiki/%s"))
        helm-dictionary-ignore-diacritics t))

;;; Helm-wikipedia
;;
(with-eval-after-load 'helm-wikipedia
  (setq helm-wikipedia-summary-url
        "https://fr.wikipedia.org/w/api.php?action=query&format=json&prop=extracts&titles=%s&exintro=1&explaintext=1&redirects=1"
        helm-wikipedia-suggest-url
        "https://fr.wikipedia.org/w/api.php?action=opensearch&search=%s"))

;;; Helm-descbinds
;;
(helm-descbinds-mode 1)

;;; Helm-lib
;;
(with-eval-after-load 'helm-lib
  (require 'isl)
  (setq helm-scroll-amount 4)
  (setq helm-find-function-default-project
        '("~/labo/emacs/lisp/" "~/labo/github/"))
  (helm-help-define-key "C-x" 'exchange-point-and-mark)
  (helm-help-define-key "C-l" 'recenter-top-bottom)
  (helm-help-define-key "C-s" nil)
  (helm-help-define-key "C-r" nil)
  (helm-help-define-key "C-s" 'isl-search))

;;; Helm-net
;;
(with-eval-after-load 'helm-net
  (setq helm-net-prefer-curl           nil
        helm-surfraw-duckduckgo-url    "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
        helm-google-suggest-search-url helm-surfraw-duckduckgo-url))

;;; Helm-external
;;
(with-eval-after-load 'helm-external
  (setq helm-raise-command                 "wmctrl -xa %s"
        helm-default-external-file-browser "thunar")
  (require 'emms-config)
  (add-hook 'helm-open-file-externally-after-hook #'tv/emms-player-start-hook)
  (add-hook 'helm-open-file-externally-after-finish-hook #'tv/emms-player-stop-hook))

;;; Helm-grep
;;
(with-eval-after-load 'helm-grep
  (setq helm-pdfgrep-default-read-command
        "xreader --page-label=%p '%f'"
        helm-grep-default-command
        "ack-grep -Hn --color --smart-case --no-group %e -- %p %f"
        helm-grep-default-recurse-command
        "ack-grep -H --color --smart-case --no-group %e -- %p %f"
        helm-grep-ag-command
        "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --search-zip --no-heading --line-number %s -- %s %s"
        helm-grep-ag-pipe-cmd-switches
        '("--colors 'match:bg:yellow' --colors 'match:fg:black'")
        helm-grep-git-grep-command
        "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")
  (add-hook 'helm-grep-mode-hook 'hl-line-mode)
  (define-key helm-grep-map   (kbd "C-M-a") 'helm/occur-which-func))

;;; Helm-occur
;;
(with-eval-after-load 'helm-occur
  (setq helm-occur-keep-closest-position t)
  (setq helm-occur-match-shorthands t)
  (add-hook 'helm-occur-mode-hook 'hl-line-mode)
  (define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func))

;;; Helm-elisp
;;
(helm-multi-key-defun helm-multi-lisp-complete-at-point
      "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
    '(helm-lisp-indent
      helm-lisp-completion-at-point
      helm-complete-file-name-at-point)
    0.3)
(define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
(define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)

(with-eval-after-load 'helm-elisp
  (setq helm-show-completion-display-function #'helm-display-buffer-in-own-frame
        helm-apropos-fuzzy-match    t
        helm-lisp-fuzzy-completion  t
        helm-apropos-show-short-doc t))

;;; Helm-locate
;;
(with-eval-after-load 'helm-locate
  (setq helm-locate-fuzzy-match t))

;;; Helm-org
;;
(with-eval-after-load 'helm-org
  (setq helm-org-headings-fontify t))

;;; Helm-emms
;;
(with-eval-after-load 'helm-emms
  (setq helm-emms-use-track-description-function nil))

;;; Helm-find
;;
(with-eval-after-load 'helm-find
  (setq helm-find-noerrors t))

;;; Helm-elisp-package
;;
(with-eval-after-load 'helm-elisp-package
  (setq helm-el-package-initial-filter 'installed
        helm-el-package-autoremove-on-start t
        helm-el-package-upgrade-on-start t))

;;; Helm-imenu
;;
(with-eval-after-load 'helm-imenu
  (add-to-list 'helm-imenu-type-faces
               '("^Use package$" . font-lock-keyword-face))
  (add-to-list 'helm-imenu-icon-type-alist
               '("Use package" . (all-the-icons-octicon
                                  "package" :face font-lock-keyword-face)))
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (setq helm-imenu-use-icon t
        helm-imenu-hide-item-type-name t))

;;; Helm-misc
;;
(with-eval-after-load 'helm-misc
  ;; Minibuffer history (Rebind to M-s).
  (customize-set-variable 'helm-minibuffer-history-key [remap next-matching-history-element]))

;;; Helm-epa
;;
(helm-epa-mode 1)

;;; Helm-fd
;;
(with-eval-after-load 'helm-fd
  (setq helm-fd-executable "fdfind")
  (defun helm-fd-pa (candidate)
    (with-helm-buffer
      (helm-ff-kill-or-find-buffer-fname
       (expand-file-name candidate))))
  (cl-defmethod helm-setup-user-source ((source helm-fd-class))
    (setf (slot-value source 'persistent-action) 'helm-fd-pa)))

;;; Helm-ls-git
;;
(with-eval-after-load 'helm-ls-git
  (setq helm-ls-git-delete-branch-on-remote t
        helm-ls-git-auto-refresh-at-eob t))


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
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-mark-ring)
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
(global-set-key (kbd "S-<f3>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c C-i")                      'helm-imenu)
(global-set-key (kbd "<f11>")                        nil)
(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "M-s")                          nil)
(global-set-key (kbd "M-s")                          'helm-occur-visible-buffers)
(global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g l")                 'goto-line)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g M-g")               'helm-revert-next-error-last-buffer)
(define-key global-map (kbd "M-g i")                 'helm-gid)
(define-key global-map (kbd "C-x r p")               'helm-projects-history)
(define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r")               'helm-dictionary)

;; Indent or complete with completion-at-point
;; (setq tab-always-indent 'complete)

;; (define-key global-map (kbd "<backtab>") 'completion-at-point)

;; Avoid hitting forbidden directories when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")
(add-to-list 'completion-ignored-extensions ".dbus/")
(add-to-list 'completion-ignored-extensions "dconf/")


(provide 'init-helm)

;;; init-helm.el ends here
