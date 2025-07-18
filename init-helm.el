;;; init-helm.el --- My startup file for helm. -*- lexical-binding: t -*-
;;; Code:

;;; Set up helm first (will load helm-autoloads.el)

(require 'helm)
;; Only needed when installed from source.
;; NOTE: package.el creates an autoload file without a provide whereas
;; make creates it with a provide, so require helm-autoloads is
;; supported only when building with make from source.
(require 'helm-autoloads)
(setq helm-input-idle-delay                     0.01
      helm-reuse-last-window-split-state        t
      helm-always-two-windows                   t
      helm-split-window-inside-p                nil
      helm-commands-using-frame                 '(completion-at-point helm-imenu
                                                  helm-imenu-in-all-buffers)
      helm-actions-inherit-frame-settings       t
      helm-use-frame-when-more-than-two-windows t
      helm-use-frame-when-no-suitable-window    t
      helm-frame-background-color               "DarkSlateGray"
      helm-show-action-window-other-window      'left
      helm-allow-mouse                          t
      helm-move-to-line-cycle-in-source         t
      helm-autoresize-max-height                80   ; it is %.
      helm-autoresize-min-height                20   ; it is %.
      helm-debug-root-directory                 "/home/thierry/tmp/helm-debug"
      helm-follow-mode-persistent               t
      helm-candidate-number-limit               500
      helm-visible-mark-prefix                  "✓"
      helm-kill-real-or-display-selection       'real)
(set-face-foreground 'helm-mark-prefix "Gold1")
(add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
(helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume)
(define-key helm-map (kbd "C-%") #'helm-exchange-minibuffer-and-header-line)
(define-key helm-map (kbd "C--") #'helm-swap-windows)

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
        (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
        (tmm-menubar)
        (find-file)
        (execute-extended-command)
        (shell) ; Fixed by c04b867a but completion is useless here.
        (cancel-debug-on-entry)
        (dired-do-rename . helm-read-file-name-handler-1)
        (dired-do-copy . helm-read-file-name-handler-1)
        (dired-do-symlink . helm-read-file-name-handler-1)
        (dired-do-relsymlink . helm-read-file-name-handler-1)
        (dired-do-hardlink . helm-read-file-name-handler-1)
        (dired-do-touch . nil)
        (read-multiple-choice--long-answers . nil)
        (basic-save-buffer . helm-read-file-name-handler-1)
        (write-file . (default helm-read-file-name-handler-1))
        (write-region . (default helm-read-file-name-handler-1))
        (all-the-icons-insert . helm-mode-all-the-icons-handler)))

;; Fix CAP with LSP in python.
(add-to-list 'helm-completion-styles-alist '(python-mode . (emacs helm helm-flex)))

;; Custom completion matching
;; (add-to-list 'helm-completion-styles-alist '(wfnames-mode . (emacs helm flex)))
(add-to-list 'helm-completion-styles-alist '(switch-to-buffer . helm-fuzzy))

;; `completions-detailed' works now with both
;; `helm-completing-read-default-1' and
;; `helm-completing-read-default-2'. To test it with *default-2 add
;; the describe-* fns to helm-completion-styles-alist
;; i.e. (fun . (emacs helm flex)).

(if (boundp 'completions-detailed)
    (setq completions-detailed t)
  ;; Emacs-27<
  (setq helm-completions-detailed t))

;;; Helm-adaptive
;;
(require 'helm-adaptive)
(setq helm-adaptive-history-file nil)
(helm-adaptive-mode 1)

;;; Helm-bookmark
;;
(with-eval-after-load 'helm-bookmark
  (customize-set-variable 'helm-bookmark-use-icon t)
  (customize-set-variable 'helm-bookmark-annotation-sign "✫"))

;;; Helm-utils
;;
(with-eval-after-load 'helm-utils
  ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
  (helm-popup-tip-mode 1)
  (setq helm-highlight-matches-around-point-max-lines   '(30 . 30)
        helm-window-prefer-horizontal-split             t)
  (add-hook 'find-file-hook 'helm-save-current-pos-to-mark-ring))

;;; Helm-sys
;;
(helm-top-poll-mode 1)
(setq helm-top-command "env COLUMNS=%s top -b -c -n 1")

;;; Helm-ring
;;
(with-eval-after-load 'helm-ring
  (setq helm-kill-ring-threshold 1)
  
  ;; Actions for helm kill-ring
  (defun helm-ring-split-block (string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (helm-awhile (read (current-buffer))
        (kill-new (prin1-to-string it)))))

  (defun helm-kill-ring-insert-hunk (hunk)
    "Yank string HUNK copied from a diff buffer."
    (helm-kill-ring-action-yank-1
     (with-temp-buffer
       (insert hunk)
       (goto-char (point-min))
       (while (re-search-forward "^[+-]" nil t)
         (replace-match ""))
       (buffer-string))))

  (add-to-list 'helm-kill-ring-actions '("Split block" . helm-ring-split-block) t)
  (add-to-list 'helm-kill-ring-actions '("Insert hunk" . helm-kill-ring-insert-hunk) t)
  (define-key helm-kill-ring-map (kbd "C-d") 'helm-kill-ring-run-persistent-delete))

;;; Helm-buffers
;;
(with-eval-after-load 'helm-buffers
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffer-skip-remote-checking  t
        helm-buffer-max-length            36
        helm-buffers-fuzzy-matching t
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help" "\\`\\*skitour"))
  (customize-set-variable 'helm-buffers-maybe-switch-to-tab  t)
  (customize-set-variable 'helm-buffers-show-icons t)

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent))

;;; Helm-files
;;
(with-eval-after-load 'helm-files
  (setq helm-ff-auto-update-initial-value        t
        helm-ff-allow-non-existing-file-at-point t
        helm-trash-remote-files                  t
        helm-dwim-target                         'next-window
        helm-locate-recursive-dirs-command "locate -i -e --regex '^%s' '%s.*$'"
        helm-ff-eshell-unwanted-aliases '("sudo" "cdu" "man"
                                          "gpg-pubkey-export-armor" "gpg-secretkey-export-armor")
        helm-file-name-history-hide-deleted t
        helm-ff-ignore-following-on-directory t
        helm-rsync-progress-bar-function #'helm-rsync-svg-progress-bar)
  (customize-set-variable 'helm-find-files-ignore-diacritics t)

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

  (defun helm-ff-csv2ledger (candidate)
    (csv2ledger "Socgen" candidate "/home/thierry/finance/ledger.dat"))
  
  ;; Add actions to `helm-source-find-files' IF:
  (cl-defmethod helm-setup-user-source ((source helm-source-ffiles))
    "Adds additional actions and settings to `helm-find-files'.
    - Open info file
    - Patch region on directory
    - Open in emms
    - Recoll directory creation
    - Csv2ledger."
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
         (and (or (eq major-mode 'gnus-article-mode)
                  (eq major-mode 'diff-mode)
                  (eq major-mode 'mu4e-view-mode))
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
    ;; Setup recoll dirs
    (when (executable-find "recoll")
      (helm-source-add-action-to-source-if
       "Recoll index directory"
       'helm-ff-recoll-index-directories
       source
       'file-directory-p
       3))
    ;; Csv to ledger
    (helm-source-add-action-to-source-if
     "Csv2Ledger"
     'helm-ff-csv2ledger
     source
     (lambda (candidate)
       (member (file-name-extension candidate) '("csv")))
     3))

  ;; Finally enable Icons in HFF.
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
        helm-dictionary-ignore-diacritics t)
  (helm-add-to-list 'helm-dictionary-actions '("Sdcv French dictionaries" . helm-dictionary-sdcv) 2)
  (helm-add-to-list 'helm-dictionary-actions '("English dictionary" . helm-dictionary-search) 3))

(defun helm-dictionary-search (entry)
  (let* ((src (helm-get-current-source))
         (name (if (string= (helm-get-attr 'name src) "en-fr")
                   (helm-dictionary-get-candidate entry 1)
                 (helm-dictionary-get-candidate entry 2))))
    (setq name (car (split-string name)))
    (dictionary-search name)))

(defvar helm-dictionary-sdcv-directory "/home/thierry/.stardict/")
(defun helm-dictionary-sdcv (entry)
  "Search ENTRY in french dictionary.
Need sdcv and stardict-xmlittre packages as dependencies."
  (let* ((src (helm-get-current-source))
         (name (if (string= (helm-get-attr 'name src) "en-fr")
                   (helm-dictionary-get-candidate entry 2)
                 (helm-dictionary-get-candidate entry 1)))
         ;; All dictionaries are copied in separate directories dir1,
         ;; dir2 etc... to avoid duplicates in sdcv output.
         (dir helm-dictionary-sdcv-directory)
         (args '("--non-interactive" "--color" "--data-dir")))
    (when (string-match ", " name)
      (setq name (completing-read "Name: " (split-string name ", " t)))
      (when (string-match "\\(.*\\) +[[({]" name)
        (setq name (match-string 1 name))))
    (with-current-buffer (get-buffer-create "*sdcv*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (apply #'call-process "sdcv" nil t nil (append args (list dir name)))
          (ansi-color-apply-on-region (point-min) (point-max)))
        (while (re-search-forward (regexp-quote name) nil t)
          (add-face-text-property
           (match-beginning 0) (match-end 0) 'font-lock-constant-face))
        (goto-char (point-min))
        (fill-region (point-min) (point-max)))
      (special-mode))
    (pop-to-buffer "*sdcv*")))

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
  (autoload 'isl-search "isl" nil t)
  (defun tv:advice-print-table (old-fn &rest args)
  (cl-letf (((symbol-function 'cl--print-table)
             #'helm-source--cl--print-table))
    (apply old-fn args)))
  (advice-add 'push-button :around #'tv:advice-print-table)
  (setq helm-scroll-amount 4)
  (setq helm-find-function-default-project
        '("~/work/emacs/lisp/" "~/work/github/"))
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
  (add-hook 'helm-open-file-externally-after-hook #'tv:emms-player-start-hook)
  (add-hook 'helm-open-file-externally-after-finish-hook #'tv:emms-player-stop-hook))

;;; Helm-grep
;;
(with-eval-after-load 'helm-grep
  (setq helm-pdfgrep-default-read-command
        "xreader --page-label=%p '%f'"
        helm-grep-default-command
        "ack -Hn --color --smart-case --no-group %e -- %p %f"
        helm-grep-default-recurse-command
        "ack -H --color --smart-case --no-group %e -- %p %f"
        helm-grep-ag-command
        "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --search-zip --no-heading --line-number %s -- %s %s"
        helm-grep-ag-pipe-cmd-switches
        '("--colors 'match:bg:yellow' --colors 'match:fg:black'")
        helm-grep-git-grep-command
        "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")
  (set-face-attribute 'helm-grep-match nil :background "yellow" :foreground "black")
  (add-hook 'helm-grep-mode-hook 'hl-line-mode)
  (define-key helm-grep-map   (kbd "C-M-a") 'helm/occur-which-func))

;;; Helm-bm
;;
(with-eval-after-load 'helm-bm
  (setq helm-bm-sort-from-pos nil)
  (define-key helm-bm-map (kbd "<f12>") 'helm-next-line)
  (define-key helm-bm-map (kbd "S-<f12>") 'helm-previous-line))

;;; Helm-occur
;;
(with-eval-after-load 'helm-occur
  (setq helm-occur-keep-closest-position t)
  (setq helm-occur-match-shorthands t)
  (setq helm-occur-candidate-number-limit 500)
  (add-hook 'helm-occur-mode-hook 'hl-line-mode)
  (define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func))

;;; Helm-elisp
;;
(helm-multi-key-defun helm-multi-lisp-complete-at-point
      "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
    '(helm-lisp-indent
      helm-lisp-completion-at-point)
    0.3)
(define-key emacs-lisp-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)
(define-key lisp-interaction-mode-map (kbd "TAB") 'helm-multi-lisp-complete-at-point)

(with-eval-after-load 'helm-elisp
  (setq helm-show-completion-display-function #'helm-display-buffer-in-own-frame
        helm-apropos-show-short-doc t))

;;; Helm-locate
;;
(with-eval-after-load 'helm-locate
  (setq helm-locate-fuzzy-match nil))

;;; Helm-org
;;
(with-eval-after-load 'helm-org
  (setq helm-org-headings-fontify t))

;;; Helm-emms
;;
(with-eval-after-load 'helm-emms
  (setq helm-emms-use-track-description-function t)
  (helm-set-attr 'candidate-number-limit 500 helm-source-emms-dired)
  (add-to-list 'helm-emms-music-extensions "mp4"))

;;; Helm-find
;;
(global-set-key (kbd "C-/") 'helm-find)
(with-eval-after-load 'helm-find
  (setq helm-find-noerrors t
        helm-find-show-full-path-fn #'file-relative-name))

;;; Helm-imenu
;;
(with-eval-after-load 'helm-imenu
  (setq helm-imenu-extra-modes '(org-mode markdown-mode))
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-imenu-use-icon t)
  (customize-set-variable 'helm-imenu-hide-item-type-name t))

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
  (setq helm-fd-executable "fdfind"
        helm-fd-switches '("--hidden" "--type" "f" "--type" "d" "--color" "always"))
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

;;; Helm-M-x
;;
(setq helm-M-x-exclude-unusable-commands-in-mode t)

;;; helm-packages
;;
(with-eval-after-load 'helm-packages
  (setq helm-packages-isolate-fn #'helm-packages-isolate-1
        helm-packages-default-clone-directory "~/work/")
  (defun helm-packages-find-project-after-clone (package directory)
    (helm-find-files-1 (expand-file-name package directory)))
  (add-hook 'helm-packages-clone-after-hook #'helm-packages-find-project-after-clone))

;;; Helm-x-icons
;;
;; (with-eval-after-load 'helm-x-icons
;;   (customize-set-variable 'helm-x-icons-provider 'nerd-icons))

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g") 'helm-apt-search)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "w") 'helm-wikipedia-suggest)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)

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
(global-set-key (kbd "C-c i")                        'helm-imenu)
(global-set-key (kbd "C-c I")                        'helm-imenu-in-all-buffers)
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
(global-set-key (kbd "S-<f4>")                       'helm-execute-kmacro)
(global-set-key (kbd "<f11>")                        nil)
(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "M-s")                          nil)
(global-set-key (kbd "M-s")                          'helm-occur-visible-buffers)
(global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap bookmark-bmenu-list]   'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
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

(provide 'init-helm)

;;; init-helm.el ends here
