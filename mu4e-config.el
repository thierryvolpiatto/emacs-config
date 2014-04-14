;;; mu4e-config.el --- Config for mu4e.

;;; Code:

(setq gnus-init-file "~/.emacs.d/.gnus.el")
(load-file gnus-init-file)
(require 'mu4e)
(require 'mu4e-contrib)
(require 'helm-mu)
(require 'org-mu4e)
(require 'config-w3m)

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

(setq mail-user-agent 'mu4e-user-agent)
(setq read-mail-command 'mu4e)

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
      '(("date:1w..now helm AND NOT flag:trashed" "Last 7 days helm messages"                                                                           ?h)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Unread messages"               ?u)
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

;; Allow queuing mails
(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/")

;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
            '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Decorate mu main view
(defun mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1) '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'mu4e-main-mode-font-lock-rules)

;; Handle quoted text added with `message-mark-inserted-region' (`C-c M-m')
(add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)

(defun tv/mu4e-browse-url ()
  (interactive)
  (browse-url (w3m-active-region-or-url-at-point)))
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'tv/mu4e-browse-url)
(define-key mu4e-view-mode-map (kbd "C-i") 'w3m-next-anchor)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
