;;; mu4e-config.el --- Config for mu4e.

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-compose-complete-addresses nil)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "w3m -T text/html")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-headers-skip-duplicates t)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/INBOX"               . ?i)
        ("/github-emacs-helm"   . ?h)
        ("/emacs-helm"          . ?e)
        ("/[Gmail].Sent Mail"   . ?s)
        ("/[Gmail].Trash"       . ?t)
        ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap -u Basic")
(define-key mu4e-headers-mode-map (kbd "C-c C-c") 'mu4e~interrupt-update-mail)
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'mu4e~interrupt-update-mail)

;; something about ourselves
(setq
 user-mail-address "thierry.volpiatto@gmail.com"
 user-full-name  "thierry"
 message-signature (with-temp-buffer
                     (insert-file-contents "~/.signature")
                     (buffer-string)))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       '(("smtp.gmail.com" 587 "thierry.volpiatto@gmail.com" nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;; attempt to show images when viewing messages
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
