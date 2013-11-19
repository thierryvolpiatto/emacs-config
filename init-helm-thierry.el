;;; init-helm-thierry.el --- My startup file for helm. 
;;; Code:


(tv-require 'helm-config)


;;;; Extensions
;;
(tv-require 'helm-ls-hg)
(tv-require 'helm-descbinds)
(tv-require 'helm-ls-git)

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
  (replace-regexp-in-string
   "\n" ""
   (shell-command-to-string "git-log -n1 | head -n1 | awk '{print $2}'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g")   'helm-apt)
(define-key helm-command-map (kbd "w")   'psession-restore-winconf)

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                     'helm-M-x)
(global-set-key (kbd "M-y")                     'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                   'helm-recentf)
(global-set-key (kbd "C-x C-f")                 'helm-find-files)
(global-set-key (kbd "C-c <SPC>")               'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                 'helm-bookmark-ext)
(global-set-key (kbd "C-h r")                   'helm-info-emacs)
(global-set-key (kbd "C-:")                     'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                     'helm-calcul-expression)
(global-set-key (kbd "C-h d")                   'helm-info-at-point)
(global-set-key (kbd "C-c g")                   'helm-google-suggest)
(global-set-key (kbd "M-g s")                   'helm-do-grep)
(global-set-key (kbd "C-x C-d")                 'helm-browse-project)
(global-set-key (kbd "<f1>")                    'helm-resume)
(global-set-key (kbd "C-h C-f")                 'helm-apropos)
(global-set-key (kbd "<f5> s")                  'helm-find)
(define-key global-map [remap jump-to-register] 'helm-register)
(define-key global-map [remap list-buffers]     'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]   'helm-dabbrev)
(define-key global-map [remap find-tag]         'helm-etags-select)

;;; Lisp complete or indent. (Rebind <tab>)
;;
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
(define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)

;;; lisp complete. (Rebind M-<tab>)
;;
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)
(add-hook 'ielm-mode-hook
	  #'(lambda ()
	      (define-key ielm-map    [remap completion-at-point] 'helm-lisp-completion-at-point)))

;;; helm completion in minibuffer
;;
(define-key minibuffer-local-map [remap completion-at-point] 'helm-lisp-completion-at-point)  ; >24.3
(define-key minibuffer-local-map [remap lisp-complete-symbol] 'helm-lisp-completion-at-point) ; <=24.3

;;; helm find files
;;
(define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)

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
      helm-m-occur-idle-delay                    0.01
      ;helm-completion-window-scroll-margin       0
      ;helm-display-source-at-screen-top          nil
      helm-ff-search-library-in-sexp             t
      ;helm-kill-ring-max-lines-number            5
      helm-default-external-file-browser         "thunar"
      helm-pdfgrep-default-read-command          "evince --page-label=%p '%f'"
      ;helm-ff-transformer-show-only-basename     t
      helm-grep-default-command                  "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
      helm-grep-default-recurse-command          "ack-grep -H --smart-case --no-group --no-color %e %p %f"
      helm-reuse-last-window-split-state         t
      ;helm-split-window-default-side             'same
      ;helm-split-window-in-side-p                t
      helm-always-two-windows                    t
      helm-persistent-action-use-special-display t
      helm-buffers-favorite-modes                (append helm-buffers-favorite-modes
                                                         '(picture-mode artist-mode))
      helm-ls-git-status-command                 'magit-status
      helm-never-delay-on-input                  nil
      helm-candidate-number-limit                200
      helm-M-x-requires-pattern                  0
      helm-dabbrev-cycle-thresold                5
      helm-surfraw-duckduckgo-url                "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
      helm-boring-file-regexp-list               '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$")
      helm-mode-handle-completion-in-region      t
      ;helm-moccur-always-search-in-current        t
      ;helm-tramp-verbose                         6
      ;helm-ff-file-name-history-use-recentf      t
      ;helm-follow-mode-persistent                t
      )

;;; Toggle grep program
;;
;;
(defun eselect-toggle-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep -d recurse %e -n%cH -e %p %f")
        (setq helm-grep-default-command
              "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
              helm-grep-default-recurse-command
              "ack-grep -H --smart-case --no-group --no-color %e %p %f"))
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
  (loop for cand in (helm-marked-candidates)
        always (string-match "\.el$" cand)))

;;; Modify source attributes
;;
;; Add actions to `helm-source-find-files' IF:
(eval-after-load "helm-files.el"
  (progn
    ;; List Hg files in project.
    (helm-add-action-to-source-if
     "Hg list files"
     'helm-ff-hg-find-files
     helm-source-find-files
     'helm-hg-root-p)
    ;; Byte compile files async
    (helm-add-action-to-source-if
     "Byte compile file(s) async"
     'async-byte-compile-file
     helm-source-find-files
     'helm-ff-candidates-lisp-p)))

;; Add magit to `helm-source-ls-git'
(helm-add-action-to-source
 "Magit status"
 #'(lambda (_candidate)
     (with-helm-buffer (magit-status helm-default-directory)))
 helm-source-ls-git 1)

;;; Save current position to mark ring
;;
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;;; Fit window to buffer
;;
;(add-hook 'helm-after-update-hook #'(lambda () (fit-window-to-buffer (helm-window))))

;;; Psession source
;;
(defvar helm-psession-windows
  '((name . "Psession windows")
    (candidates . (lambda ()
                    (sort (mapcar 'car psession--winconf-alist) #'string-lessp)))
    (action . (("Restore" . psession-restore-winconf)
               ("Delete" . psession-delete-winconf)))))

(defun helm-psession ()
  (interactive)
  (helm :sources 'helm-psession-windows :buffer "*helm psession*"))

;;; enable Modes
;;
(helm-mode 1)
(helm-adaptative-mode 1)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
