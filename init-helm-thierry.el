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

;;; Dabbrev expand
;;
(defun helm-collect-dabbrev (str limit ignore-case all)
  (let ((case-fold-search ignore-case)
        (search #'(lambda (buf pattern direction)
                    (while (case direction
                             (1  (re-search-forward pattern nil t))
                             (-1 (re-search-backward pattern nil t))) 
                      (let ((match (substring-no-properties
                                    (thing-at-point 'symbol)))) 
                        (unless (string= str match)
                          (push match result)))))))
    (loop with result
          for buf in (if all (buffer-list) (list (current-buffer)))
          do (with-current-buffer buf
               (save-excursion
                 (funcall search buf str -1))
               (save-excursion
                 (funcall search buf str 1)))
          when (> (length result) limit) return (nreverse result)
          finally return (nreverse result))))


(defun helm-dabbrev-get-candidates (abbrev)
  (with-helm-current-buffer
    (let* ((dabbrev-get #'(lambda (str all-bufs)
                             (helm-collect-dabbrev
                              str helm-candidate-number-limit
                              nil all-bufs)))
           (lst (funcall dabbrev-get abbrev nil)))
      (if (<= (length lst) 5)
          (funcall dabbrev-get abbrev 'all-bufs)
          lst))))

(defvar helm-source-dabbrev
  '((name . "Dabbrev Expand")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global
               (helm-dabbrev-get-candidates dabbrev))))
    (candidates-in-buffer)
    (action . (lambda (candidate)
                (let ((limits (with-helm-current-buffer
                                (bounds-of-thing-at-point 'symbol))))
                  (delete-region (car limits) (cdr limits))
                  (insert candidate))))))

(defun helm-dabbrev ()
  (interactive)
  (declare (special dabbrev))
  (let ((dabbrev (thing-at-point 'symbol))
        (limits (bounds-of-thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate t))
    (with-helm-show-completion (car limits) (cdr limits)
      (helm :sources 'helm-source-dabbrev
            :buffer "*helm dabbrev*"
            :input (concat "^" dabbrev " ")
            :resume 'noresume))))

(global-set-key [remap dabbrev-expand] 'helm-dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g")   'helm-apt)

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
(global-set-key (kbd "C-c C-b")                 'helm-browse-code)
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

;;; Lisp complete or indent. (Rebind <tab>)
;;
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
(define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)

;;; lisp complete. (Rebind M-<tab>)
;;
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)

;;; Describe key-bindings
;;
;;
(helm-descbinds-install)            ; C-h b, C-x C-h

;;; Helm-variables
;;
;;
(setq helm-google-suggest-use-curl-p             t
      helm-kill-ring-threshold                   1
      helm-raise-command                         "wmctrl -xa %s"
      helm-scroll-amount                         1
      helm-quick-update                          t
      helm-idle-delay                            0.1
      helm-input-idle-delay                      0.1
      helm-m-occur-idle-delay                    0.1
      ;helm-completion-window-scroll-margin       0
      ;helm-display-source-at-screen-top          nil
      helm-ff-search-library-in-sexp             t
      helm-kill-ring-max-lines-number            5
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
 helm-source-ls-git
 1)

;;; Save current position to mark ring
;;
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;;; Fit window to buffer
;;
;(add-hook 'helm-after-update-hook #'(lambda () (fit-window-to-buffer (helm-window))))

;;; enable Modes
;;
(helm-mode 1)
(helm-match-plugin-mode 1)
(helm-adaptative-mode 1)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
