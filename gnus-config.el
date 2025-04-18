;;; gnus-config.el -- Laptop

;; Set `gnus-init-file' to the path of this file in init.el before
;; calling `gnus'.

;;; Code:

;; Don't read/write to the .newrc file, go straight to the *.eld.
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups 'ask-server)

;;; Gnus methods
;;
;;
;; Three methods described here to use Emails with Gnus:

;; 1) This is the configuration using dovecot server.
;; Need dovecot imap package installed, add this line to
;; /etc/dovecot/conf.d/10-mail.conf:
;; mail_location = maildir:~/Maildir:LAYOUT=fs
;; Need also offlineimap to feed ~/Maildir.

;; (setq gnus-select-method
;;       '(nnimap "Posteo"
;;         (nnimap-address "localhost")
;;         (nnimap-stream network)
;;         (nnimap-authenticator login)))

(setq gnus-select-method '(nntp "news.gmane.io"))

;; 2) The nnmaildir config: Use offlineimap to feed ~/.nnmaildir
;; The ~/.offlineimaprc used by offlineimap command should point to
;; ~/.nnmaildir. Looks nice but too slow to be used.
;; (setq gnus-select-method '(nnmaildir "Posteo" (directory "~/.nnmaildir")))

;; 3) the online method with nnimap (no mails offline):

;; Secondary methods are mails and possibly other nntp servers.
(setq gnus-secondary-select-methods '(;; Add as many mail account as needed with a label.
                                      ;; Add then an entry in .authinfo:
                                      ;; machine label port xxx login xxx password xxx
                                      (nnimap "posteo" ; Label for reference in .authinfo for machine name.
                                       (nnimap-address "posteo.de")
                                       ;; Don't download mime parts when receiving mail, only text part, use
                                       ;; instead `A-C' to see entire mail.
                                       (nnimap-fetch-partial-articles "text/"))))


(setq gnus-ignored-from-addresses "thievol@posteo\\.net")
(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))

;; Change "From" field according to "To" field on reply.
(setq gnus-posting-styles
      '(("thievol@posteo\\.net"
         (name "Thierry Volpiatto")
         (gcc "thievol@posteo.net")
         (address "thievol@posteo.net")
         (signature-file "~/.signature"))))

;; To add a mail account:
;;
;;  1) Add an nnimap entry in `gnus-secondary-select-methods'.
;;  2) Add an entry in `gnus-posting-styles'
;;  3) Add an entry in `tv-smtp-accounts'
;;  4) Add entries in authinfo for imap and smtp refering to labels. (See below)

(defvar tv-smtp-accounts
  '(("thievol@posteo.net"
     (:server "posteo.de"
      :port 587
      :name "Thierry Volpiatto"))))

(defun tv-change-smtp-server ()
  "Use account found in `tv-smtp-accounts' according to from header.
`from' is set in `gnus-posting-styles' according to `to' header.
or manually with `tv-send-mail-with-account'.
This will run in `message-send-hook'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((from         (message-fetch-field "from"))
             (user-account (cl-loop for account in tv-smtp-accounts thereis
                                    (and (string-match (car account) from)
                                         account)))
             (server (cl-getf (cadr user-account) :server))
             (port (cl-getf (cadr user-account) :port))
             (user (car user-account)))
        (setq smtpmail-smtp-user            user
              smtpmail-default-smtp-server  server
              smtpmail-smtp-server          server
              smtpmail-smtp-service         port)))))

(add-hook 'message-send-hook 'tv-change-smtp-server)

(defun tv-send-mail-with-account ()
  "Change mail account manually to send this mail."
  (interactive)
  (save-excursion
    (let* ((from (save-restriction
                   (message-narrow-to-headers)
                   (message-fetch-field "from")))
           (mail (completing-read
                  "Use account: "
                  (mapcar 'car tv-smtp-accounts)))
           (name (cl-getf (cadr (assoc mail tv-smtp-accounts)) :name))
           (new-from (message-make-from name mail)))
      (message-goto-from)
      (forward-line 0)
      (re-search-forward ": " (point-at-eol))
      (delete-region (point) (point-at-eol))
      (insert new-from))))
(define-key message-mode-map (kbd "C-c p") 'tv-send-mail-with-account)

;; Nnml mail directory 
(setq nnml-directory "~/Mail")

;;; Archivage des mails envoyés
;;
(setq gnus-message-archive-group "sent")

;;; Show all these headers
;;
;;
(setq gnus-visible-headers
      '("^From:"
	"^Newsgroups:"
	"^Subject:"
	"^Date:"
	"^Followup-To:"
	"^Reply-To:"
	"^Organization:"
	"^Summary:"
	"^Keywords:"
	"^To:"
	"^[BGF]?Cc:"
	"^Posted-To:"
	"^Mail-Copies-To:"
	"^Apparently-To:"
	"^X-Gnus-Warning:"
	"^Resent-From:"
	"^X-Sent:"
	"^X-Mailer:"
	"^X-Newsreader:"
	"^X-User-Agent:"
	"^User-Agent:"))

;;; Order of headers
;;
;;
(setq gnus-sorted-header-list '("^From:"
                                "^Subject:"
                                "^Summary:"
                                "^Keywords:"
                                "^Newsgroups:"
                                "^Followup-To:"
                                "^To:"
                                "^Cc:"
                                "^Date:"
                                "^User-Agent:"
                                "^X-Mailer:"
                                "^X-Newsreader:"))

(setq gnus-buttonized-mime-types
      '("multipart/alternative"
        ".*/signed"
        "multipart/encrypted"))

;; timestamp 
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format "%M%S%p%P%5y: %(%-40,40g%) %ud\n")

(defun tv:gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if time (format-time-string "%b %d  %H:%M" time) "")))
(advice-add 'gnus-user-format-function-d :override #'tv:gnus-user-format-function-d)

(setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f %* %B%s%)\n"
      gnus-user-date-format-alist '((t . "%d.%m.%Y %H:%M"))
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

;; Integration dans dired
(require 'gnus-dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Scoring 
;; The scoring system sorts articles and authors you read often to the
;; beginning of the available mails.
;; Less interesting stuff is located at the end.
(setq gnus-use-adaptive-scoring t)
(setq gnus-score-expiry-days 14)
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 4))
        (gnus-dormant-mark (from 5))
        (gnus-saved-mark (from 20) (subject 5))
        (gnus-del-mark (from -2) (subject -5))
        (gnus-read-mark (from 2) (subject 1))
        (gnus-killed-mark (from 0) (subject -3))))

(setq gnus-score-decay-constant 1)      ;default = 3
(setq gnus-score-decay-scale 0.03)      ;default = 0.05

(setq gnus-decay-scores t)              ;(gnus-decay-score 1000)

;; Use a global score file to filter gmane spam articles.
(setq gnus-global-score-files
      '("~/News/scores/all.SCORE"))

;; all.SCORE contains:
;; (("xref"
;;  ("gmane.spam.detected" -1000 nil s)))
(setq gnus-summary-expunge-below -999)


;;; Message and smtp settings
;;
;;
;;; mm-* settings
;;
;; Junk mail

;; (when (require 'mm-decode)
;;   (setq mm-discouraged-alternatives
;;         '("text/html"
;;           "text/richtext"
;;           "text/enriched"
;;           "multipart/related"
;;           "image/.*")
;;         mm-automatic-display
;;         (remove "text/html" mm-automatic-display)))

;;; Remove white space in filenames
;;
;;

;; Try to inline images
;; (setq mm-inline-text-html-with-images t)


(setq gnus-inhibit-mime-unbuttonizing nil)
(setq gnus-buttonized-mime-types '("multipart/signed"
                                   "multipart/alternative"))

;; Automatically sign/encrypt replies to signed/encrypted mails. 
(setq gnus-message-replysign nil)
(setq gnus-message-replyencrypt nil)

;; Suppression de la signature quand on quote. 
(setq message-cite-function 'message-cite-original-without-signature)

(define-key gnus-article-mode-map (kbd "C-c C-c") 'tv:browse-url-or-show-patch)

;;; gnus-config.el ends here

