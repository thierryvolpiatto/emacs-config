;; -*- mode: emacs-lisp -*-
;;; .gnus.el -- Laptop


;;; Code:

;(gnus-compile)

;; Search engine for imap and gmane (hit `G G' in group buffer)
(require 'nnir)

;; methode-par-defaut 
(setq gnus-select-method '(nntp "news.gmane.org"
                           (nnir-search-engine gmane)))

;; Secondary methods
(setq gnus-secondary-select-methods '((nnml "")))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;; See also:
;; `nnimap-fetch-partial-articles' and `A C' to see entire mail.
;(setq nnimap-fetch-partial-articles "text")

;; Mail-directory-for-gnus 
(setq nnml-directory "~/Mail")

;; Archivage-des-mails-envoyés 
;;(setq gnus-message-archive-group "nnml:sent-mail")
(setq gnus-message-archive-group '((when (message-news-p) "sent-news")))
      ;;       "sent-news"
      ;;       "nnml:sent-mail")))

;; config-pour-gmail 
(setq user-mail-address "thierry.volpiatto@gmail.com")
(setq user-full-name "thierry")

;;; Sending mail

;; config-gmail-avec-starttls 
;; (find-fline "/usr/share/emacs/24.0.50/lisp/mail/smtpmail.el" "Please")

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

(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))

;; Now-my-password-are-in-.authinfo 
;; (setq smtpmail-auth-credentials '(("smtp.gmail.com" 587 "my_email_as_login" "My_password")))

(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)

;; Registry
;(gnus-registry-initialize)

;; spam-for-news 
;; (require 'spam)
;; (spam-initialize)
;; (setq spam-log-to-registry t)
;; (setq spam-directory "~/Mail/probably-spam/")
;; (setq gnus-spam-process-newsgroups
;;       '(("^gmane\\." ((spam spam-use-gmane)))
;;         ("^gwene\\." ((spam spam-use-bogofilter)))
;;         ("^nnimap\\." ((spam spam-use-bogofilter)))))

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

;; utiliser-w3m-pour-les-messages-html 
(setq mm-text-html-renderer 'w3m)
;(setq mm-text-html-renderer 'gnus-article-html)
;(setq mm-inline-text-html-with-images t)

;; delete-incoming-mail-source-files-after-splitting 
;(setq mail-source-delete-incoming t)

;; passage-à-la-ligne-automatique 
(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

;; Posting-styles 
;; [EVAL] (info "(gnus) Posting Styles")
;; [EVAL] (info "(gnus) X-Face")
;; [EVAL] (info "(gnus) Face")

(setq gnus-posting-styles
      `((".*"
         ;(Face ,(gnus-face-from-file "~/.xfaces/2-face.png"))
         ;(x-face-file "~/.xfaces/2-face.png")
         (name "Thierry Volpiatto")
         (address "thierry.volpiatto@gmail.com")
         (signature-file "~/.signature"))))

;; Easypg 
 ;; use newer epg
(setq mml2015-use 'epg)
(setq mml2015-encrypt-to-self t)

;; verify/decrypt 
;; only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

(add-hook 'message-mode-hook 'epa-mail-mode)

;; suppression-de-la-signature-quand-on-quote 
(setq message-cite-function 'message-cite-original-without-signature)

;; shimbun 
;; (add-to-list 'load-path "~/elisp/emacs-w3m/shimbun/")
;; (setq shimbun-server-additional-path '("~/elisp"))
;; (require 'nnshimbun)
;; (autoload 'gnus-group-make-shimbun-group "nnshimbun" nil t)

;; integration-dans-dired? 
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
      '("~/gnus/scores/all.SCORE"))

;; all.SCORE contains:
;; (("xref"
;;  ("gmane.spam.detected" -1000 nil s)))
(setq gnus-summary-expunge-below -999)

;; gnus-demon 
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

;;; .gnus.el ends here

