;;; .gnus.el -- Laptop

;; Set `gnus-init-file' to the path of this file in init.el before
;; calling `gnus'.

;;; Code:

;;; Search engine for imap and gmane (hit `G G' in group buffer)
;;
(require 'nnir)

;; Don't read/write to the .newrc file, go straight to the *.eld.
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups 'ask-server)

;;; Gnus methods
;;
;;
;; Default method
(setq gnus-select-method '(nntp "news.gmane.io"
                           (nnir-search-engine gmane)))

(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-number)))

;;; To add a mail account:
;;
;;  1) Add an nnimap entry in `gnus-secondary-select-methods'.
;;  2) Add an entry in `gnus-posting-styles'
;;  3) Add an entry in `tv-smtp-accounts'
;;  4) Add an entries in authinfo for imap and smtp refering to labels. (See below)

;; Secondary methods are mails and possibly other nntp servers.
(setq gnus-secondary-select-methods '((nnml "")
                                      ;; Add as many gmail account as needed with a label.
                                      ;; Add then an entry in .authinfo:
                                      ;; machine label port xxx login xxx password xxx
                                      (nnimap "posteo" ; Label for reference in .authinfo for machine name.
                                       (nnimap-address "posteo.de")
                                       (nnimap-fetch-partial-articles "text/"))
                                      ;; (nntp "news.gwene.org")
                                      ))

;; [1] Don't load mime parts when receiving mail, only text part, use
;; instead `A-C' to see entire mail.

;; Change "From" field according to "To" field on reply.
(setq gnus-posting-styles
      '(("thievol@posteo\\.net"
         (name "Thierry Volpiatto")
         (address "thievol@posteo.net")
         (signature-file "~/.signature"))))

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
(setq gnus-message-archive-group '((when (message-news-p) "sent-news")))

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

(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if time (format-time-string "%b %d  %H:%M" time) "")))

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
;; Don't send to these address in wide reply.
(setq message-dont-reply-to-names '("notifications@github\\.com"
                                    ".*@noreply\\.github\\.com"
                                    "thievol@posteo\\.net"))

(setq user-mail-address "thievol@posteo.net")
(setq user-full-name "Thierry Volpiatto")

;; [smtpmail-async] Experimental, use `smtpmail-send-it' otherwise. 
(setq message-send-mail-function 'smtpmail-send-it
      ;smtpmail-debug-info t        ; Uncomment to debug
      ;smtpmail-debug-verb t        ; Uncomment to debug on server
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

;; Default settings.
;; This are default setting, they could be modified
;; by `tv-change-smtp-server' according to `tv-smtp-accounts'
;; and `gnus-posting-styles'.
(setq smtpmail-default-smtp-server "posteo.de"
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "posteo.de"
      smtpmail-smtp-service 587)

;; Passage à la ligne automatique
;;
(defun tv/message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill)
  (epa-mail-mode 1))
(add-hook 'message-mode-hook 'tv/message-mode-setup)

;; Ne pas demander si on splitte les pa 
(setq message-send-mail-partially-limit nil)

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
(setq mm-file-name-rewrite-functions
      '(mm-file-name-delete-control
        mm-file-name-delete-gotchas
        mm-file-name-trim-whitespace
        mm-file-name-collapse-whitespace
        mm-file-name-replace-whitespace))

;; Html renderer
(cond ((fboundp 'w3m)
       ;; Emacs-w3m
       (setq mm-text-html-renderer 'w3m))
      ((executable-find "w3m")
       ;; W3m (Don't need emacs-w3m)
       (setq mm-text-html-renderer 'w3m-standalone))
      (t ; Fall back to shr.
       (setq shr-color-visible-luminance-min 75)
       (setq shr-width nil) ; Use all window width.
       (setq mm-text-html-renderer 'shr)))

;; Try to inline images
;; (setq mm-inline-text-html-with-images t)

;;; Mail encryption.
;;
;;
(setq mml2015-use 'epg)
(setq mml2015-encrypt-to-self t)

;; Verify/Decrypt automatically
;; only if mml knows about the protocol used.
(setq mm-verify-option 'never)
(setq mm-decrypt-option 'known)

(setq gnus-inhibit-mime-unbuttonizing nil)
(setq gnus-buttonized-mime-types '("multipart/signed"
                                   "multipart/alternative"))

;; Automatically sign/encrypt replies to signed/encrypted mails. 
;; (setq gnus-message-replysign t)
;; (setq gnus-message-replyencrypt t)

;; Suppression de la signature quand on quote. 
(setq message-cite-function 'message-cite-original-without-signature)

;; Default directory to save attached files 
(setq mm-default-directory "~/download/")

;;; .gnus.el ends here

