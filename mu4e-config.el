;;; mu4e-config.el --- Config for mu4e.

;;; Code:

(require 'mu4e)
(setq gnus-init-file "~/.emacs.d/.gnus.el")
(load-file gnus-init-file)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-compose-complete-addresses nil)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-view-show-addresses t)

;;; Html rendering
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command (cond ((fboundp 'w3m)
                                    (lambda ()          ; Use emacs-w3m
                                      (w3m-region (point-min) (point-max))))
                                   ((executable-find "w3m")
                                    "w3m -T text/html") ; Use w3m shell-command
                                   (t (lambda ()        ; Use shr (slow)
                                        (let ((shr-color-visible-luminance-min 75)
                                              shr-width)
                                          (shr-render-region (point-min) (point-max)))))))

;(setq mail-user-agent 'mu4e-user-agent)
;(setq read-mail-command 'mu4e)

(define-key mu4e-main-mode-map "q" 'quit-window)
(define-key mu4e-main-mode-map "Q" 'mu4e-quit)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-headers-skip-duplicates t)

;; signature
(setq mu4e-compose-signature t)

;; encryption
(define-key mu4e-view-mode-map [remap mu4e-view-verify-msg-popup] 'epa-mail-verify)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/Gmail/INBOX"               . ?i)
        ("/Gmail/github-emacs-helm"   . ?h)
        ("/Gmail/emacs-helm"          . ?e)
        ("/Gmail/Friends"             . ?f)
        ("/Gmail/[Gmail].Sent Mail"   . ?s)
        ("/Gmail/[Gmail].Trash"       . ?t)
        ("/Gmail/[Gmail].Spam"        . ?!)
        ("/Gmail/[Gmail].All Mail"    . ?a)))

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Unread messages"               ?u)
        ("date:today..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Today's messages"          ?t)
        ("date:1d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Yesterday and today messages" ?y)
        ("date:7d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Last 7 days"                  ?w)
        ("mime:image/* AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Messages with images"         ?p)))

(add-hook 'mu4e-compose-mode-hook 'tv/message-mode-setup) ; loaded from .gnus.el

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;;; Updating
;;
;;
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap -q -u Basic")

;; Automatic updates.
;(setq mu4e-update-interval 600)

;; Make a full update all the
;; `tv/mu4e-max-number-update-before-toggling' mail retrievals.
(defvar tv/mu4e-counter 10) ; Ensure a full update on startup.
(defvar tv/mu4e-max-number-update-before-toggling 10)
(defvar tv/mu4e-get-mail-command-full "offlineimap -u Basic")
(defvar tv/mu4e-get-mail-command-quick "offlineimap -q -u Basic")
(defun tv/mu4e-update-mail-quick-or-full ()
  (if (>= tv/mu4e-counter
          tv/mu4e-max-number-update-before-toggling)
      (progn
        (setq mu4e-get-mail-command tv/mu4e-get-mail-command-full)
        (setq tv/mu4e-counter 0))
      (setq mu4e-get-mail-command tv/mu4e-get-mail-command-quick)
      (incf tv/mu4e-counter)))
(add-hook 'mu4e-update-pre-hook #'tv/mu4e-update-mail-quick-or-full)

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;;; bookmark handler
;;
;;
(add-hook 'mu4e-view-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'mu4e-view-bookmark-make-record)))
(add-hook 'mu4e-headers-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'mu4e-view-bookmark-make-record)))

(defun mu4e-view-bookmark-make-record ()
  (let* ((msg     (mu4e-message-at-point))
         (query   (mu4e-last-query))
         (docid   (plist-get msg :docid))
         (mode    (symbol-name major-mode))
         (subject (or (plist-get msg :subject) "No subject")))
    `(,subject
      ,@(bookmark-make-record-default 'no-file 'no-context)
        (location . (,query . ,docid))
        (mode . ,mode)
        (handler . mu4e-bookmark-jump))))

(defun mu4e-bookmark-jump (bookmark)
  (let* ((path  (bookmark-prop-get bookmark 'location))
         (mode  (bookmark-prop-get bookmark 'mode))
         (docid (cdr path))
         (query (car path)))
    (call-interactively 'mu4e)
    (mu4e-headers-search query)
    (sit-for 0.5)
    (mu4e~headers-goto-docid docid)
    (mu4e~headers-highlight docid)
    (unless (string= mode "mu4e-headers-mode")
      (call-interactively 'mu4e-headers-view-message)
      (run-with-timer 0.1 nil
                      (lambda (bmk)
                        (bookmark-default-handler
                         `("" (buffer . ,(current-buffer)) . ,(bookmark-get-bookmark-record bmk))))
                      bookmark))))

;; Allow queuing mails
(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/cur")

;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
            '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
