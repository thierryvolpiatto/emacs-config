;;; .gnus.el -- Laptop


;;; Code:

;; Search engine for imap and gmane (hit `G G' in group buffer)
(require 'nnir)

;; methode-par-defaut 
(setq gnus-select-method '(nntp "news.gmane.org"
                           (nnir-search-engine gmane)))

;; Secondary methods
(setq gnus-secondary-select-methods '((nnml "")
                                      (nnimap "gmail"
                                       (nnimap-address "imap.gmail.com"))
                                      (nnimap "yahoo"
                                       (nnimap-address "imap.mail.yahoo.com"))
                                      ))
;(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;; See also:
;; `nnimap-fetch-partial-articles' and `A C' to see entire mail.
;(setq nnimap-fetch-partial-articles "text")

;; Mail-directory-for-gnus 
(setq nnml-directory "~/Mail")

;;; Archivage-des-mails-envoyés
;;
(setq gnus-message-archive-group '((when (message-news-p) "sent-news")))

;;; Smtp settings - Sending mail
;;
;;
;; [README] (find-fline "/usr/local/share/emacs/24.0.92/lisp/mail/smtpmail.el.gz" "Please")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Please add these lines in your .emacs(_emacs) or use customize.
;;
;;(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
;;(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
;;(setq smtpmail-default-smtp-server "YOUR SMTP HOST")
;;(setq smtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-sendto-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-debug-info t) ; only to debug problems
;;(setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;      '(("YOUR SMTP HOST" 25 "username" "password")))
;;(setq smtpmail-starttls-credentials
;;      '(("YOUR SMTP HOST" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
;; Where the 25 equals the value of `smtpmail-smtp-service', it can be an
;; integer or a string, just as long as they match (eq).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To queue mail, set `smtpmail-queue-mail' to t and use
;; `smtpmail-send-queued-mail' to send.
;(setq smtpmail-queue-mail t) ; Use M-x smtpmail-send-queued-mail when online.

(setq user-mail-address "thierry.volpiatto@gmail.com")
(setq user-full-name "Thierry Volpiatto")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      ;smtpmail-debug-verb t        ; Uncomment to debug
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

;; Default settings.
;; `smtpmail-starttls-credentials' have been removed since 24.0.92.1
;; smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


;; Posting-styles
;;
;;
;; [EVAL] (info "(gnus) Posting Styles")
;; [EVAL] (info "(gnus) X-Face")
;; [EVAL] (info "(gnus) Face")
(setq gnus-posting-styles
      '((".*"
         (name "Thierry Volpiatto")
         (address "thierry.volpiatto@gmail.com")
         (signature-file "~/.signature"))
        ((header "to" "thierry.volpiatto@gmail.com")
         (from "Thierry Volpiatto <thierry.volpiatto@gmail.com>")
         (signature-file "~/.signature"))
        ((header "to" "tvolpiatto@yahoo.fr")
         (from "Thierry Volpiatto <tvolpiatto@yahoo.fr>")
         (signature-file "~/.signature"))))

(defvar tv-smtp-accounts
  '(("thierry.volpiatto@gmail.com"
     "smtp.gmail.com"
     587)
    ("tvolpiatto@yahoo.fr"
     "smtp.mail.yahoo.com"
     587)))

(defun tv-change-smtp-server ()
  "Use account found in `tv-smtp-accounts' according to from header.
`from' is set in `gnus-posting-styles' according to `to' header.
or manually with `tv-toggle-from-header'.
This will run in `message-send-hook'."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((from (message-fetch-field "from"))
             (account (loop for account in tv-smtp-accounts thereis
                            (and (string-match (car account) from) account))))
        ;; `smtpmail-starttls-credentials' have been removed since 24.0.92.1
        ;; (setq smtpmail-starttls-credentials (list (list (nth 1 account)
        ;;                                                 (nth 2 account)
        ;;                                                 nil nil)))
        (setq smtpmail-default-smtp-server  (nth 1 account)
              smtpmail-smtp-server          (nth 1 account)
              smtpmail-smtp-service         (nth 2 account))))))

(add-hook 'message-send-hook 'tv-change-smtp-server)

(defun tv-toggle-from-header ()
  "Toggle from header manually between yahoo and gmail."
  (interactive)
  (save-excursion
    (let* ((from (save-restriction
                   (message-narrow-to-headers)
                   (message-fetch-field "from"))))
      (message-goto-from)
      (forward-line 0)
      (re-search-forward ": " (point-at-eol))
      (delete-region (point) (point-at-eol))
      (if (string-match "yahoo" from)
          (insert (message-make-from user-full-name
                                     "thierry.volpiatto@gmail.com"))
          (insert (message-make-from user-full-name
                                     "tvolpiatto@yahoo.fr"))))))
(define-key message-mode-map (kbd "C-c p") 'tv-toggle-from-header)

(defun tv-send-mail-with-account ()
  "Change mail account to send this mail."
  (interactive)
  (save-excursion
    (let* ((from (save-restriction
                   (message-narrow-to-headers)
                   (message-fetch-field "from")))
           (mail (anything-comp-read
                  "Use account: " (mapcar 'car tv-smtp-accounts)))
           (new-from (message-make-from user-full-name mail)))
        (message-goto-from)
        (forward-line 0)
        (re-search-forward ": " (point-at-eol))
        (delete-region (point) (point-at-eol))
        (insert new-from))))

;;; Registry
;;
;;
(when (eq emacs-major-version 24)
  (setq gnus-registry-max-entries 2500)
  (gnus-registry-initialize)

  (setq gnus-registry-split-strategy 'majority
        gnus-registry-ignored-groups '(("nntp" t)
                                       ("nnrss" t))
        gnus-registry-max-entries 500000
        ;; this is the default
        gnus-registry-track-extra '(sender subject)))

;;; spam-for-news
;;
;;
(require 'spam)
(spam-initialize)
(setq spam-log-to-registry t)
(setq spam-directory "~/Mail/probably-spam/")
(setq gnus-spam-process-newsgroups
      '(("^gmane\\." ((spam spam-use-gmane)))
        ("^nnimap\\." ((spam spam-use-bogofilter)
                       (spam spam-use-blacklist)
                       (spam spam-use-whitelist)))))

;; junk-mail 
(when (require 'mm-decode)
  (setq mm-discouraged-alternatives
        '("text/html"
          "text/richtext"
          "text/enriched"
          "multipart/related"
          "image/.*")
        mm-automatic-display
        (remove "text/html" mm-automatic-display)
        gnus-buttonized-mime-types
        '("multipart/alternative"
          ".*/signed"
          "multipart/encrypted")))

;; Remove white space in filenames
(setq mm-file-name-rewrite-functions
      '(mm-file-name-trim-whitespace
        mm-file-name-collapse-whitespace
        mm-file-name-replace-whitespace))

;; Show-all-these-headers 
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

;; Order-of-headers 
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

;; ne-pas-demander-si-on-splitte-les-pa 
(setq message-send-mail-partially-limit nil)

;;; Html renderer
;;
;;
(setq shr-color-visible-luminance-min 75)
(setq shr-width nil) ; Use all screen.
(setq mm-text-html-renderer 'shr)
;(setq mm-text-html-renderer 'w3m)
;(setq mm-text-html-renderer 'gnus-article-html)
;(setq mm-inline-text-html-with-images t)

;; delete-incoming-mail-source-files-after-splitting 
;(setq mail-source-delete-incoming t)

;; passage-à-la-ligne-automatique 
(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

;;; Mail encryption.
;;
;;
(setq mml2015-use 'epg)
(setq mml2015-encrypt-to-self t)

;; Verify/Decrypt automatically
;; only if mml knows about the protocol used.
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

(setq gnus-inhibit-mime-unbuttonizing nil)
(setq gnus-buttonized-mime-types '("multipart/signed"
                                   "multipart/alternative"))

;; Automatically sign/encrypt replies to signed/encrypted mails. 
(setq gnus-message-replysign t)
(setq gnus-message-replyencrypt t)

;; Enable epa.
(add-hook 'message-mode-hook 'epa-mail-mode)

;; Suppression de la signature quand on quote. 
(setq message-cite-function 'message-cite-original-without-signature)

;; shimbun 
;; (add-to-list 'load-path "~/elisp/emacs-w3m/shimbun/")
;; (setq shimbun-server-additional-path '("~/elisp"))
;; (require 'nnshimbun)
;; (autoload 'gnus-group-make-shimbun-group "nnshimbun" nil t)

;; Integration dans dired
(require 'gnus-dired)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; fortune 
(add-hook 'gnus-article-mode-hook
          '(lambda ()
             (define-key gnus-article-mode-map "i" 'fortune-from-region)))
;; (add-hook 'message-setup-hook 'fortune-to-signature)
;; (message "Making new signature: %s" (fortune-to-signature "~/docs/ascii/misc/fortunes/usenet"))


;; Scoring 
;; The scoring system sorts articles and authors you read often to the beginning of the available mails.
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
                                        ;(gnus-killed-mark (from -1) (subject -3))))
                                        ;(gnus-kill-file-mark (from -9999)))
                                        ;(gnus-expirable-mark (from -1) (subject -1))
                                        ;(gnus-ancient-mark (subject -1))
                                        ;(gnus-low-score-mark (subject -1))
                                        ;(gnus-catchup-mark (subject -1))))

(setq gnus-score-decay-constant 1)      ;default = 3
(setq gnus-score-decay-scale 0.03)      ;default = 0.05

(setq gnus-decay-scores t)              ;(gnus-decay-score 1000)

;;Use a global score file to filter gmane spam articles. That is a really cool feature.
(setq gnus-global-score-files
      '("~/News/scores/all.SCORE"))

;; all.SCORE contains:
;; (("xref"
;;  ("gmane.spam.detected" -1000 nil s)))
(setq gnus-summary-expunge-below -999)

;; gnus-demon (start with `gnus-demon-init') 
;; Scan for new news
;(gnus-demon-add-handler 'gnus-demon-scan-news 5 1)

;; Scroll-other-window 
(define-key gnus-summary-mode-map (kbd "<C-M-down>") #'(lambda ()
                                                         (interactive)
                                                         (scroll-other-window 1)))

(define-key gnus-summary-mode-map (kbd "<C-M-up>") #'(lambda ()
                                                         (interactive)
                                                         (scroll-other-window -1)))

;; Default-directory-to-save-attached-files 
(setq mm-default-directory "~/download/")

;; timestamp 
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(setq gnus-group-line-format "%M\%S\%p\%P\%5y: %(%-40,40g%) %ud\n")

(defun gnus-user-format-function-d (headers)
  (declare (special gnus-tmp-group))
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

;; sort
;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-number
;;         gnus-thread-sort-by-date))

(gnus-compile)

;;; .gnus.el ends here

