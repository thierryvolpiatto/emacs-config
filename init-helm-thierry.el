;;; init-helm-thierry.el --- My startup file for helm. 
;;; Code:

(tv-require 'helm-config)
(tv-require 'helm-locate) ; Needed actually for `helm-generic-files-map'.

;;;; Extensions
;;
(tv-require 'helm-mercurial)
(tv-require 'helm-delicious)
(tv-require 'helm-descbinds)
(tv-require 'helm-ls-git)
(setq helm-ls-git-status-command 'magit-status)

;;;; Test Sources or new helm code. 
;;   !!!WARNING EXPERIMENTAL!!!
(defun helm-browse-project ()
  (interactive)
  (cond ((helm-ls-git-root-dir)
         (helm-ls-git-ls))
        ((helm-hg-root)
         (helm-hg-find-files-in-project))
        (t (helm-find-files nil))))

(defmacro with-helm-default-directory (directory &rest body)
  (declare (indent 2) (debug t))
  `(let ((default-directory (file-name-as-directory ,directory)))
     ,@body))

(defun helm-ff-browse-project (candidate)
  (with-helm-default-directory helm-ff-default-directory
      (helm-browse-project)))

(defun helm-ff-run-browse-project ()
  (interactive)
  (when helm-alive-p
    (helm-c-quit-and-execute-action 'helm-ff-browse-project)))
(define-key helm-find-files-map (kbd "C-x C-d") 'helm-ff-run-browse-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "q")   'helm-qpatchs)
(define-key helm-command-map (kbd "d")   'helm-delicious)
(define-key helm-command-map (kbd "g")   'helm-apt)

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                    'helm-M-x)
(global-set-key (kbd "M-y")                    'helm-show-kill-ring)
(global-set-key (kbd "C-c f")                  'helm-recentf)
(global-set-key (kbd "C-x C-f")                'helm-find-files)
(global-set-key (kbd "C-c <SPC>")              'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                'helm-bookmark-ext)
(global-set-key (kbd "C-h r")                  'helm-info-emacs)
(global-set-key (kbd "C-c C-b")                'helm-browse-code)
(global-set-key (kbd "C-:")                    'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                    'helm-calcul-expression)
(global-set-key (kbd "C-c h f")                'helm-info-at-point)
(global-set-key (kbd "C-c g")                  'helm-google-suggest)
(global-set-key (kbd "M-g s")                  'helm-do-grep)
(global-set-key (kbd "C-x C-d")                'helm-browse-project)
(define-key global-map [remap insert-register] 'helm-register)
(define-key global-map [remap list-buffers]    'helm-buffers-list)

;; Lisp complete or indent.
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
(define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)

;; lisp complete.
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)

;;; Describe key-bindings
;;
;;
(helm-descbinds-install)            ; C-h b, C-x C-h

;;; Helm-variables
;;
;;
(setq helm-google-suggest-use-curl-p         t
      helm-kill-ring-threshold               1
      helm-raise-command                     "wmctrl -xa %s"
      helm-scroll-amount                     1
      helm-quick-update                      t
      helm-idle-delay                        0.1
      helm-input-idle-delay                  0.1
      helm-completion-window-scroll-margin   0
      helm-c-kill-ring-max-lines-number      5
      helm-c-default-external-file-browser   "thunar"
      helm-c-use-adaptative-sorting          t
      helm-c-pdfgrep-default-read-command    "evince --page-label=%p '%f'"
      helm-ff-transformer-show-only-basename t
      helm-c-grep-default-command            "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
      helm-c-grep-default-recurse-command    "ack-grep -H --smart-case --no-group --no-color %e %p %f"
      )

;;; Toggle grep program
;;
;;
(defun eselect-toggle-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-c-grep-default-command
              "grep -d skip %e -niH -e %p %f"
              helm-c-grep-default-recurse-command
              "grep -d recurse %e -niH -e %p %f")
        (setq helm-c-grep-default-command
              "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
              helm-c-grep-default-recurse-command
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

;;; Enable pushing album to google from `helm-find-files'.
;;
;;
(defun helm-push-image-to-google (file)
  (when helm-current-prefix-arg (google-update-album-list-db))
  (let ((comp-file (concat google-db-file "c"))
        (album (helm-comp-read
                "Album: "
                (loop for i in gpicasa-album-list collect (car i)))))
    (while (not (file-exists-p comp-file)) (sit-for 0.1))
    (unless gpicasa-album-list (load-file comp-file))
    (google-post-image-to-album-1 file album)))

(defun helm-ff-candidates-lisp-p (candidate)
  (loop for cand in (helm-marked-candidates)
        always (string-match "\.el$" cand)))

;;; Add new actions to sources
;;
;; From `helm-c-source-find-files' do:

(when (require 'helm-files)
  ;; Push album to google.
  (helm-add-action-to-source
   "Push album to google"
   'google-create-album-1
   helm-c-source-find-files 1)
  ;; Push single file to google.
  (helm-add-action-to-source
   "Push file to google album"
   'helm-push-image-to-google
   helm-c-source-find-files 2)
  ;; List Hg files in project.
  (helm-add-action-to-source-if
   "Hg list files"
   'helm-ff-hg-find-files
   helm-c-source-find-files
   'helm-hg-root-p)
  ;; Byte compile files async
  (helm-add-action-to-source-if
   "Byte compile file(s) async"
   'async-byte-compile-file
   helm-c-source-find-files
   'helm-ff-candidates-lisp-p))


;;; Enable helm-mode
;;
;;
(helm-mode 1)

;;; Enable match-plugin mode
;;
;;
(helm-match-plugin-mode 1)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
