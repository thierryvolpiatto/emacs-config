;;; mu4e-config.el --- Config for mu4e.

;;; Code:

(require 'mu4e)
(require 'mu4e-contrib)
(require 'helm-mu)
(require 'org-mu4e)
(require 'config-w3m)


;;; Message and smtp settings
;;
;;
;; Don't send to these address in wide reply.
(setq message-dont-reply-to-names '("notifications@github\\.com"
                                    ".*@noreply\\.github\\.com"
                                    "thierry\\.volpiatto@gmail\\.com"))

(setq user-mail-address "thierry.volpiatto@gmail.com")
(setq user-full-name "Thierry Volpiatto")
(setq mu4e-user-mail-address-list `(,user-mail-address "tvolpiatto@yahoo.fr"))

;; [smtpmail-async] Experimental, use `smtpmail-send-it' otherwise. 
(setq message-send-mail-function 'async-smtpmail-send-it
      ;smtpmail-debug-info t        ; Uncomment to debug
      ;smtpmail-debug-verb t        ; Uncomment to debug on server
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

;; Default settings.
;; This are default setting, they could be modified
;; by `tv-change-smtp-server' according to `tv-smtp-accounts'
;; and `gnus-posting-styles'.
(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Passage à la ligne automatique
;;
(defun tv/message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill)
  (epa-mail-mode 1))
(add-hook 'message-mode-hook 'tv/message-mode-setup)

(defvar tv-smtp-accounts
  '(("thierry.volpiatto@gmail.com"
     (:server "smtp.gmail.com"
      :port 587
      :name "Thierry Volpiatto"))
    ("tvolpiatto@yahoo.fr"
     (:server "smtp.mail.yahoo.com"
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
  "Change mail account to send this mail."
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

;; Ne pas demander si on splitte les pa 
(setq message-send-mail-partially-limit nil)


;;; Mu4e settings
;;

;;; Default
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

;;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-headers-skip-duplicates t)

;;; Signature
(setq mu4e-compose-signature t)

;;; encryption
(define-key mu4e-view-mode-map [remap mu4e-view-verify-msg-popup] 'epa-mail-verify)

;;; Setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '(("/drafts"                    . ?d)
        ("/Gmail/INBOX"               . ?i)
        ("/Gmail/github-emacs-helm"   . ?h)
        ("/Gmail/emacs-helm"          . ?e)
        ("/Gmail/Emacs development"   . ?E)
        ("/Gmail/Friends"             . ?f)
        ("/Gmail/[Gmail].Sent Mail"   . ?s)
        ("/Gmail/[Gmail].Trash"       . ?t)
        ("/Gmail/[Gmail].Spam"        . ?!)
        ("/Gmail/[Gmail].All Mail"    . ?a)
        ("/Yahoo/Trash"               . ?p)
        ("/Yahoo/Inbox"               . ?y)))

(setq mu4e-bookmarks
      '(("date:1w..now helm AND NOT flag:trashed" "Last 7 days helm messages"                                                                           ?h)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Unread messages"               ?u)
        ("date:today..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Today's messages"          ?t)
        ("date:1d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Yesterday and today messages" ?y)
        ("date:7d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Last 7 days"                  ?w)
        ("mime:image/* AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam AND NOT maildir:/Gmail/[Gmail].All Mail" "Messages with images"         ?p)))

(add-hook 'mu4e-compose-mode-hook 'tv/message-mode-setup) ; loaded from .gnus.el

;;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;;; Updating
;;
;;
;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap -q -u Basic")

;;; Automatic updates.
;(setq mu4e-update-interval 600)

;;; Make a full update all the
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

;;; Attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;;; Allow queuing mails
(setq smtpmail-queue-mail  nil  ;; start in non-queuing mode
      smtpmail-queue-dir   "~/Maildir/queue/")

;;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
            '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Decorate mu main view
(defun mu4e-main-mode-font-lock-rules ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([a-zA-Z]\\{1,2\\}\\)\\]" nil t)
      (add-text-properties (match-beginning 1) (match-end 1) '(face font-lock-variable-name-face)))))
(add-hook 'mu4e-main-mode-hook 'mu4e-main-mode-font-lock-rules)

;;; Handle quoted text added with `message-mark-inserted-region' (`C-c M-m')
(add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)

(defun tv/mu4e-browse-url ()
  (interactive)
  (browse-url (w3m-active-region-or-url-at-point)))
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'tv/mu4e-browse-url)

(defadvice w3m-goto-next-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (goto-char (next-single-property-change
                (point) 'w3m-anchor-sequence))))

(defadvice w3m-goto-previous-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (goto-char (previous-single-property-change
                (point) 'w3m-anchor-sequence))))

(define-key mu4e-view-mode-map (kbd "C-i") 'w3m-next-anchor)
(define-key mu4e-view-mode-map (kbd "M-<tab>") 'w3m-previous-anchor)

;;; A simplified and more efficient version of `article-translate-strings'.
;;
;; Transform also in headers.
(defun mu4e~view-translate-strings (map)
  "Translate all string in the the article according to MAP.
MAP is an alist where the elements are on the form (\"from\" \"to\")."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (dolist (elem map)
        (let* ((key  (car elem))
               (from (if (characterp key) (string key) key))
               (to   (cdr elem)))
          (save-excursion
            (while (search-forward from nil t)
              (replace-match to))))))))

(defun mu4e-view-treat-dumbquotes ()
    "Translate M****s*** sm*rtq**t*s and other symbols into proper text.
Note that this function guesses whether a character is a sm*rtq**t* or
not, so it should only be used interactively.

Sm*rtq**t*s are M****s***'s unilateral extension to the
iso-8859-1 character map in an attempt to provide more quoting
characters.  If you see something like \\222 or \\264 where
you're expecting some kind of apostrophe or quotation mark, then
try this wash."
  (interactive)
  (with-current-buffer mu4e~view-buffer
    (mu4e~view-translate-strings
     '((128 . "EUR") (130 . ",") (131 . "f") (132 . ",,")
       (133 . "...") (139 . "<") (140 . "OE") (145 . "`")
       (146 . "'") (147 . "``") (148 . "\"") (149 . "*")
       (150 . "-") (151 . "--") (152 . "~") (153 . "(TM)")
       (155 . ">") (156 . "oe") (180 . "'")))))

;;; Same as `article-remove-cr' (W-c) but simplified and more efficient.
;;
;; Not sure it is needed in mu4e though.
(defun mu4e-view-remove-cr ()
  "Remove trailing CRs and then translate remaining CRs into LFs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (if (eolp)
            (replace-match "" t t)
            (replace-match "\n" t t))))))

;;; Show Smileys
;;
(add-hook 'mu4e-view-mode-hook 'smiley-buffer)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
