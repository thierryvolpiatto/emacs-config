;;; init-helm-thierry.el --- My startup file for helm. -*- lexical-binding: t -*- 
;;; Code:

(tv-require 'helm-config)


;;; Enable Modes (This is loading nearly everything).
;;
(helm-mode 1)
(helm-adaptative-mode 1)
(helm-autoresize-mode 1)


;;;; Extensions
;;
(load "/home/thierry/elisp/helm-extensions/helm-extensions-autoloads.el")


;;;; Test Sources or new helm code. 
;;   !!!WARNING EXPERIMENTAL!!!

(defun helm-version ()
  (with-current-buffer (find-file-noselect (find-library-name "helm-pkg"))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             "\\([0-9]+?\\)\\.\\([0-9]+?\\)\\.\\([0-9]+?\\)\\.?[0-9]*" nil t)
      (prog1 (match-string-no-properties 0) (kill-buffer))))))

(defun helm-git-version ()
  (shell-command-to-string
   "git log --pretty='format:%H' -1"))


;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g")   'helm-apt)
(define-key helm-command-map (kbd "w")   'helm-psession)
(define-key helm-command-map (kbd "z")   'helm-complex-command-history)

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                        'helm-recentf)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-c g")                        'helm-google-suggest)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key shell-mode-map (kbd "M-p")               'helm-comint-input-ring) ; shell history.

;;; Lisp complete or indent. (Rebind <tab>)
;;
;; Use `completion-at-point' with `helm-mode' if available
;; otherwise fallback to helm implementation.

;; Set to `complete' will use `completion-at-point'.
;; (and (boundp 'tab-always-indent)
;;      (setq tab-always-indent 'complete))

(helm-multi-key-defun helm-multi-lisp-complete-at-point
    "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
  '(helm-lisp-indent
    helm-lisp-completion-at-point
    helm-complete-file-name-at-point)
  0.3)

(if (and (boundp 'tab-always-indent)
         (eq tab-always-indent 'complete)
         (boundp 'completion-in-region-function))
    (progn
      (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
      (define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
      
      ;; lisp complete. (Rebind M-<tab>)
      (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
      (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
    
    (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    (define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    
    ;; lisp complete. (Rebind M-<tab>)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(unless (boundp 'completion-in-region-function)
  (add-hook 'ielm-mode-hook
            #'(lambda ()
                (define-key ielm-map    [remap completion-at-point] 'helm-lisp-completion-at-point))))

;;; helm completion in minibuffer
;;
;; (define-key minibuffer-local-map [remap completion-at-point] 'helm-lisp-completion-at-point)  ; >24.3
;; (define-key minibuffer-local-map [remap lisp-complete-symbol] 'helm-lisp-completion-at-point) ; <=24.3

;;; helm find files
;;
(define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
(define-key helm-buffer-map (kbd "C-d")     'helm-buffer-run-kill-persistent)


;;; Describe key-bindings
;;
;;
(helm-descbinds-install)            ; C-h b, C-x C-h


;;; Helm-variables
;;
;;
(setq helm-google-suggest-use-curl-p             t
      ;helm-kill-ring-threshold                   1
      helm-raise-command                         "wmctrl -xa %s"
      helm-scroll-amount                         4
      helm-quick-update                          t
      helm-idle-delay                            0.01
      helm-input-idle-delay                      0.01
      ;helm-completion-window-scroll-margin       0
      ;helm-display-source-at-screen-top          nil
      helm-ff-search-library-in-sexp             t
      ;helm-kill-ring-max-lines-number            5
      helm-default-external-file-browser         "thunar"
      helm-pdfgrep-default-read-command          "evince --page-label=%p '%f'"
      ;helm-ff-transformer-show-only-basename     t
      helm-ff-auto-update-initial-value          t
      helm-grep-default-command                  "ack-grep -Hn --smart-case --no-group %e %p %f"
      helm-grep-default-recurse-command          "ack-grep -H --smart-case --no-group %e %p %f"
      helm-reuse-last-window-split-state         t
      ;helm-split-window-default-side             'other
      ;helm-split-window-in-side-p                nil
      helm-always-two-windows                    t
      ;helm-persistent-action-use-special-display t
      helm-buffers-favorite-modes                (append helm-buffers-favorite-modes
                                                         '(picture-mode artist-mode))
      helm-ls-git-status-command                 'magit-status
      ;helm-never-delay-on-input                  nil
      ;helm-candidate-number-limit                200
      helm-M-x-requires-pattern                  0
      helm-dabbrev-cycle-threshold                3
      helm-surfraw-duckduckgo-url                "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
      ;helm-surfraw-default-browser-function      'w3m-browse-url
      helm-boring-file-regexp-list               '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
      ;helm-mode-handle-completion-in-region      t
      ;helm-moccur-always-search-in-current        t
      ;helm-tramp-verbose                         6
      ;helm-ff-file-name-history-use-recentf      t
      ;helm-follow-mode-persistent                t
      helm-apropos-fuzzy-match                    t
      helm-M-x-fuzzy-match                        t
      helm-lisp-fuzzy-completion                  t
      ;helm-locate-fuzzy-match                     t
      helm-completion-in-region-fuzzy-match       t
      helm-move-to-line-cycle-in-source           t
      ido-use-virtual-buffers                     t             ; Needed in helm-buffers-list
      helm-tramp-verbose                          6
      helm-buffers-fuzzy-matching                 t
      helm-locate-command                         "locate %s -e -A --regex %s"
      helm-org-headings-fontify                   t
      helm-autoresize-max-height                  80 ; it is %.
      helm-autoresize-min-height                  20 ; it is %.
      fit-window-to-buffer-horizontally           1
      )

(custom-set-variables '(helm-recentf-fuzzy-match t)
                      '(helm-imenu-fuzzy-match t))

;; Avoid hitting forbidden directory .gvfs when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")


;;; Toggle grep program
;;
;;
(defun eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=never -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=never -d recurse %e -n%cH -e %p %f")
        (setq helm-grep-default-command
              "ack-grep -Hn --smart-case --no-group %e %p %f"
              helm-grep-default-recurse-command
              "ack-grep -H --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

;;; Debugging
;;
;;
(defun helm-debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm-ff-candidates-lisp-p (candidate)
  (cl-loop for cand in (helm-marked-candidates)
           always (string-match "\.el$" cand)))


;;; Modify source attributes
;;
;; Add actions to `helm-source-find-files' IF:

(defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Byte compile file(s) async"
   'async-byte-compile-file
   source
   'helm-ff-candidates-lisp-p))

;;; Add magit to `helm-source-ls-git'
;;
(defmethod helm-setup-user-source ((source helm-ls-git-source))
  (let ((actions (oref source :action)))
    (oset source
          :action
          (helm-append-at-nth
           actions
           (helm-make-actions
            "Magit status"
            (lambda (_candidate)
              (with-helm-buffer
                (magit-status helm-default-directory))))
           1))))

(defmethod helm-setup-user-source ((source helm-source-buffers))
  (oset source :candidate-number-limit 200))


;;; Psession windows
;;
(defun helm-psession-windows ()
  (interactive)
  (helm :sources (helm-build-sync-source "Psession windows"
                   :candidates (lambda ()
                                 (sort (mapcar 'car psession--winconf-alist) #'string-lessp))
                   :action (helm-make-actions
                            "Restore" 'psession-restore-winconf
                            "Delete" 'psession-delete-winconf))
        :buffer "*helm psession*"))


;;; Byte recompile helm extensions directory
;;  and regenerate autoload file.
(defvar tv/helm-extensions-directory "~/elisp/helm-extensions/")
(defun tv/helm-update-extensions (dir)
  (interactive (list (read-directory-name
                      "Helm extensions directory: "
                      tv/helm-extensions-directory)))
  (let ((generated-autoload-file
         (expand-file-name "helm-extensions-autoloads.el" dir))
        (backup-inhibited t))
    (update-directory-autoloads dir)
    (async-byte-recompile-directory dir)))

;;; helm dictionary
;;
(setq helm-dictionary-database "~/helm-dictionary/dic-en-fr.iso")
(setq helm-dictionary-online-dicts '(("translate.reference.com en->fr" .
                                      "http://translate.reference.com/translate?query=%s&src=en&dst=fr")
                                     ("translate.reference.com fr->en" .
                                      "http://translate.reference.com/translate?query=%s&src=fr&dst=en")))


(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
