;;; mu4e-config.el --- Config for mu4e.

;;; Code:

(require 'mu4e)
(require 'helm-mu)

;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-compose-complete-addresses nil)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command "w3m -T text/html")
(setq mail-user-agent 'mu4e-user-agent)
(setq read-mail-command 'mu4e)

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
        ("/Friends"             . ?f)
        ("/[Gmail].Sent Mail"   . ?s)
        ("/[Gmail].Trash"       . ?t)
        ("/[Gmail].Spam"        . ?!)
        ("/[Gmail].All Mail"    . ?a)))

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed AND NOT maildir:/[Gmail].Spam" "Unread messages"               ?u)
        ("date:today..now AND NOT flag:trashed AND NOT maildir:/[Gmail].Spam" "Today's messages"          ?t)
        ("date:1d..now AND NOT flag:trashed AND NOT maildir:/[Gmail].Spam" "Yesterday and today messages" ?y)
        ("date:7d..now AND NOT flag:trashed AND NOT maildir:/[Gmail].Spam" "Last 7 days"                  ?w)
        ("mime:image/* AND NOT flag:trashed AND NOT maildir:/[Gmail].Spam" "Messages with images"         ?p)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap -q -u Basic")
(setq mu4e-update-interval 600)
(define-key mu4e-headers-mode-map (kbd "C-c C-c") 'mu4e~interrupt-update-mail)
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'mu4e~interrupt-update-mail)

;; something about ourselves
(setq user-mail-address "thierry.volpiatto@gmail.com"
      user-full-name  "thierry"
      message-signature (with-temp-buffer
                          (insert-file-contents "~/.signature")
                          (buffer-string)))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)

(setq message-send-mail-function 'async-smtpmail-send-it ;'smtpmail-send-it
      message-send-mail-partially-limit nil
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

(defvar tv/smtp-accounts
  '(("thierry.volpiatto@gmail.com"
     (:server "smtp.gmail.com"
      :port 587
      :name "Thierry Volpiatto"))
    ("tvolpiatto@yahoo.fr"
     (:server "smtp.mail.yahoo.com"
      :port 587
      :name "Thierry Volpiatto"))))

(defun tv/change-smtp-server ()
  "Use account found in `tv/smtp-accounts' according to from header.
`from' is set in `gnus-posting-styles' according to `to' header.
or manually with `tv-toggle-from-header'.
This will run in `message-send-hook'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((from         (message-fetch-field "from"))
             (user-account (loop for account in tv/smtp-accounts
                                 when (string-match (car account) from)
                                 return account))
             (server (getf (cadr user-account) :server))
             (port (getf (cadr user-account) :port))
             (user (car user-account)))
        (setq smtpmail-smtp-user            user
              smtpmail-default-smtp-server  server
              smtpmail-smtp-server          server
              smtpmail-smtp-service         port)))))

(add-hook 'message-send-hook 'tv/change-smtp-server)

(defun tv/send-mail-with-account ()
  "Change mail account to send this mail."
  (interactive)
  (save-excursion
    (let* ((from (save-restriction
                   (message-narrow-to-headers)
                   (message-fetch-field "from")))
           (mail (completing-read
                  "Use account: "
                  (mapcar 'car tv/smtp-accounts)))
           (name (getf (cadr (assoc mail tv/smtp-accounts)) :name))
           (new-from (message-make-from name mail)))
        (message-goto-from)
        (forward-line 0)
        (re-search-forward ": " (point-at-eol))
        (delete-region (point) (point-at-eol))
        (insert new-from))))
(define-key mu4e-compose-mode-map (kbd "C-c p") 'tv/send-mail-with-account)

;; Don't send to these address in wide reply.
(setq message-dont-reply-to-names '("notifications@github.com"
                                    "helm@noreply.github.com"
                                    "thierry.volpiatto@gmail.com"))

(setq user-mail-address "thierry.volpiatto@gmail.com")
(setq user-full-name "Thierry Volpiatto")

(defun tv/message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill)
  (epa-mail-mode 1))
(add-hook 'mu4e-compose-mode-hook 'tv/message-mode-setup)

;;; Mail encryption.
;;
;;
(setq mml2015-use 'epg)
(setq mml2015-encrypt-to-self t)

;; Verify/Decrypt automatically
;; only if mml knows about the protocol used.
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;; Use now org-keywords in gnus.
(add-hook 'message-mode-hook #'(lambda ()
				 (define-key message-mode-map (kbd "<f11> k") 'helm-org-keywords)))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;; Make a full update all the 5 mail retrieval
(defvar tv/mu4e~update-mail-number-of-update-flag 0)
(defvar tv/mu4e~update-mail-number-of-update-toggle 5)
(defvar tv/mu4e-get-mail-command-full "offlineimap -u Basic")
(defvar tv/mu4e-get-mail-command-quick "offlineimap -q -u Basic")
(defun tv/mu4e-update-mail-quick-or-full ()
  (if (>= tv/mu4e~update-mail-number-of-update-flag
          tv/mu4e~update-mail-number-of-update-toggle)
      (progn
        (setq mu4e-get-mail-command tv/mu4e-get-mail-command-full)
        (setq tv/mu4e~update-mail-number-of-update-flag 0))
      (setq mu4e-get-mail-command tv/mu4e-get-mail-command-quick)
      (incf tv/mu4e~update-mail-number-of-update-flag)))
(add-hook 'mu4e-update-pre-hook #'tv/mu4e-update-mail-quick-or-full)
;; Always perform a full update on startup.
(add-hook 'mu4e-main-mode-hook #'(lambda ()
                                   (setq tv/mu4e~update-mail-number-of-update-flag
                                         tv/mu4e~update-mail-number-of-update-toggle)))
;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; bookmark handler
(add-hook 'mu4e-view-mode-hook
          #'(lambda ()
              (set (make-local-variable 'bookmark-make-record-function)
                   'mu4e-view-bookmark-make-record)))

(defun mu4e-view-bookmark-make-record ()
  (let* ((msg (mu4e-message-at-point))
         (query (mu4e-last-query))
         (docid (plist-get msg :docid))
         (subject (or (plist-get msg :subject) "No subject")))
    `(,subject
      ,@(bookmark-make-record-default 'no-file 'no-context)
        (location . (,query . ,docid))
        (handler . mu4e-bookmark-jump))))

(defun mu4e-bookmark-jump (bookmark)
  (let* ((path  (bookmark-prop-get bookmark 'location))
         (docid (cdr path))
         (query (car path)))
    (call-interactively 'mu4e)
    (mu4e-headers-search query)
    (sit-for 1)
    (mu4e~headers-goto-docid docid)
    (mu4e~headers-highlight docid)
    (call-interactively 'mu4e-headers-view-message)
    (run-with-timer 0.5 nil
                    (lambda (bmk)
                      (bookmark-default-handler
                       `("" (buffer . ,(current-buffer)) . ,(bookmark-get-bookmark-record bmk))))
                    bookmark)))

(provide 'mu4e-config)

;;; mu4e-config.el ends here
