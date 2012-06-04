;;; init-helm-thierry.el --- My startup file for helm. 
;;; Code:

(require 'helm-config)
(require 'helm-locate)

;;;; Extensions
;;
(require 'helm-mercurial)
(require 'helm-delicious)
(require 'helm-descbinds)
(require 'helm-git)

;;;; Test Sources or new helm code. 
;;   !!!WARNING EXPERIMENTAL!!!
(defvar helm-hg-files-cache (make-hash-table :test 'equal))
(defun helm-hg-list-files ()
  (let ((dir (with-temp-buffer
               (call-process "hg" nil t nil "root")
               (file-name-as-directory
                (replace-regexp-in-string "\n" "" (buffer-string))))))
    (if (file-directory-p dir)
        (helm-aif (gethash dir helm-hg-files-cache)
            it
          (with-temp-buffer
            (apply #'call-process "hg" nil t nil
                   (list "manifest" "--all"))
            (loop with ls = (split-string (buffer-string) "\n" t)
                  for f in ls
                  collect (concat dir f) into tmpls
                  finally return (puthash dir tmpls helm-hg-files-cache))))
        (error "Error: Not an hg repo (no .hg found)"))))

(defvar helm-c-source-hg-list-files
  `((name . "Hg files list")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               "*helm hg*" (helm-hg-list-files))))
    (keymap . ,helm-generic-files-map)
    (candidates-in-buffer)
    (type . file)))

(defun helm-hg-find-files-in-project ()
  (interactive)
  (helm :sources 'helm-c-source-hg-list-files
        :buffer "*hg files*"))

(defun helm-ff-hg-find-files (candidate)
  (let ((default-directory (file-name-as-directory
                            (if (file-directory-p candidate)
                                (expand-file-name candidate)
                                (file-name-directory candidate)))))
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-hg-find-files-in-project)))
     default-directory)))

(defun helm-ff-git-find-files (candidate)
  (let ((default-directory (file-name-as-directory
                            (if (file-directory-p candidate)
                                (expand-file-name candidate)
                                (file-name-directory candidate))))) 
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-git-find-files)))
     default-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "q")   'helm-qpatchs)
(define-key helm-command-map (kbd "v")   'helm-eev-anchors)
(define-key helm-command-map (kbd "d")   'helm-delicious)
(define-key helm-command-map (kbd "y e") 'helm-yaoddmuse-emacswiki-edit-or-view)
(define-key helm-command-map (kbd "y p") 'helm-yaoddmuse-emacswiki-post-library)
(define-key helm-command-map (kbd "g")   'helm-apt)
(define-key helm-command-map (kbd "DEL") 'helm-resume)

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
(global-set-key (kbd "C-c y")                  'helm-yahoo-suggest)
(global-set-key (kbd "M-g s")                  'helm-do-grep)
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
(setq helm-google-suggest-use-curl-p            t
      helm-kill-ring-threshold                  1
      helm-raise-command                        "wmctrl -xa %s"
      helm-scroll-amount                        1
      helm-quick-update                         t
      helm-idle-delay                           0.1
      helm-input-idle-delay                     0.1
      helm-c-kill-ring-max-lines-number         5
      helm-c-default-external-file-browser      "thunar"
      helm-c-use-adaptative-sorting             t
      helm-c-pdfgrep-default-read-command       "evince --page-label=%p '%f'"
      )

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

(when (require 'helm-files)
  ;; Push album to google.
  (helm-add-action-to-source
   "Push album to google"
   'google-create-album-1 helm-c-source-find-files)
  ;; Push single file to google.
  (helm-add-action-to-source
   "Push file to google album"
   'helm-push-image-to-google helm-c-source-find-files)
  ;; List Hg files in project.
  (helm-add-action-to-source
   "List hg files"
   'helm-ff-hg-find-files helm-c-source-find-files)
  ;; List Git files in project.
  (helm-add-action-to-source
   "List git files"
   'helm-ff-git-find-files helm-c-source-find-files)
  )

;;; Enable helm-mode
;;
;;
(helm-mode 1)

;;; Enable match-plugin mode
;;
;;
;(helm-match-plugin-mode 1)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
