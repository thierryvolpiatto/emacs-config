;;; mu4e-config.el --- Config for mu4e.

;;; Code:

(require 'mu4e-contrib)
(require 'config-w3m)
(require 'mu4e-patch)

;;; Message and smtp settings
;;
;;
;; Don't send to these address in wide reply.
(setq mu4e-compose-reply-ignore-address
      '("notifications@github\\.com"
        ".*@noreply\\.github\\.com"
        "thierry\\.volpiatto@gmail\\.com"
        "thievol05@zoho\\.eu"))

(setq user-mail-address "thievol05@zoho.eu")
(setq user-full-name "Thierry Volpiatto")

;; [smtpmail-async] Experimental, use `smtpmail-send-it' otherwise.
;; To debug use `smtpmail-send-it'
(setq message-send-mail-function 'smtpmail-send-it
      ;; smtpmail-debug-info t        ; Uncomment to debug
      ;; smtpmail-debug-verb t        ; Uncomment to debug on server
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

;; Default settings.
(setq smtpmail-default-smtp-server "smtp.zoho.eu"
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "smtp.zoho.eu"
      smtpmail-smtp-service 587)

;; Passage à la ligne automatique
;;
(defun tv/message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill)
  (epa-mail-mode 1))
(add-hook 'message-mode-hook 'tv/message-mode-setup)

;; Contexts (setup smtp servers)
;;
(setq mu4e-compose-context-policy 'ask-if-none
      mu4e-context-policy 'pick-first
      mu4e-contexts
      `(,(make-mu4e-context
           :name "Gmail"
           :enter-func (lambda () (mu4e-message "Switch to Gmail"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Gmail" (mu4e-message-field msg :maildir))))
           :vars '((smtpmail-smtp-user           . "thierry.volpiatto@gmail.com")
                   (smtpmail-default-smtp-server . "smtp.gmail.com")
                   (smtpmail-smtp-server         . "smtp.gmail.com")
                   (smtpmail-smtp-service        . 587)
                   (mail-reply-to                . "thierry.volpiatto@gmail.com")
                   (user-mail-address            . "thierry.volpiatto@gmail.com")
                   (user-full-name               . "Thierry Volpiatto")
                   (mu4e-compose-signature       . t)))
         ,(make-mu4e-context
          :name "Zoho"
          :enter-func (lambda () (mu4e-message "Switch to Zoho"))
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/Zoho" (mu4e-message-field msg :maildir))))
          :vars '((smtpmail-smtp-user           . "thievol05@zoho.eu")
                  (smtpmail-default-smtp-server . "smtp.zoho.eu")
                  (smtpmail-smtp-server         . "smtp.zoho.eu")
                  (smtpmail-smtp-service        . 587)
                  (mail-reply-to                . "thievol05@zoho.eu")
                  (user-mail-address            . "thievol05@zoho.eu")
                  ;; Not sure that works, once sending once from
                  ;; webmail, it seems I am allowed to send from any
                  ;; VPN but perhaps this may be needed? Note that
                  ;; gmail doesn't add such headers when sending from webmail.
                  ;; (message-default-headers      "User-Agent: Zoho Mail\nX-Mailer: Zoho Mail\n")
                  (user-full-name               . "Thierry Volpiatto")
                  (mu4e-compose-signature       . t)))
         ,(make-mu4e-context
           :name "Yahoo"
           :enter-func (lambda () (mu4e-message "Switch to Yahoo"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Yahoo" (mu4e-message-field msg :maildir))))
           :vars '((smtpmail-smtp-user           . "tvolpiatto@yahoo.fr")
                   (smtpmail-default-smtp-server . "smtp.mail.yahoo.com")
                   (smtpmail-smtp-server         . "smtp.mail.yahoo.com")
                   (smtpmail-smtp-service        . 587)
                   (mail-reply-to                . "tvolpiatto@yahoo.fr")
                   (user-mail-address            . "tvolpiatto@yahoo.fr")
                   (user-full-name               . "Thierry Volpiatto")
                   (mu4e-compose-signature       . t)))))

(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))

;; Ne pas demander si on splitte les pa 
(setq message-send-mail-partially-limit nil)


;;; Mu4e settings
;;

;;; Default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-compose-complete-addresses nil)
(setq mu4e-completing-read-function 'completing-read)
(setq mu4e-view-show-addresses t)
(setq mu4e-headers-include-related nil)

;;; Html rendering
(setq mu4e-view-prefer-html t)
(setq mu4e-html2text-command (cond ((fboundp 'w3m)
                                    ;; Use emacs-w3m
                                    (lambda ()
                                      (w3m-region (point-min) (point-max))))
                                   ((executable-find "w3m")
                                    ;; Use w3m shell-command
                                    "w3m -T text/html")
                                   (t 'html2text)))

(setq mail-user-agent      'mu4e-user-agent
      read-mail-command    'mu4e
      gnus-dired-mail-mode 'mu4e-user-agent)

(define-key mu4e-main-mode-map "q"   'quit-window)
(define-key mu4e-main-mode-map "Q"   'mu4e-quit)
(define-key mu4e-main-mode-map "\C-s" 'helm-mu)

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
      '(("/drafts"                         . ?d)
        ("/Gmail/INBOX"                    . ?i)
        ("/Zoho/INBOX.Github.Emacs-helm"   . ?h)
        ("/Zoho/INBOX.Github.Emacs-mu4e"   . ?m)
        ("/Gmail/emacs-helm"               . ?e)
        ("/Gmail/Friends"                  . ?f)
        ("/Gmail/[Gmail].Sent Mail"        . ?s)
        ("/Gmail/[Gmail].Trash"            . ?t)
        ("/Gmail/[Gmail].Spam"             . ?!)
        ("/Yahoo/Inbox"                    . ?y)))

(setq mu4e-bookmarks
      '(("date:1w..now helm AND NOT flag:trashed" "Last 7 days helm messages" ?h)
        ("date:1d..now helm AND NOT flag:trashed" "Yesterday and today helm messages" ?b)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam \
AND NOT maildir:/Zoho/Spam AND NOT maildir:/Yahoo/Bulk\\ Mail" "Unread messages" ?u)
        ("date:today..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam \
AND NOT maildir:/Zoho/Spam AND NOT maildir:/Yahoo/Bulk\\ Mail" "Today's messages" ?t)
        ("date:1d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam \
AND NOT maildir:/Zoho/Spam AND NOT maildir:/Yahoo/Bulk\\ Mail" "Yesterday and today messages" ?y)
        ("date:7d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam \
AND NOT maildir:/Zoho/Spam AND NOT maildir:/Yahoo/Bulk\\ Mail" "Last 7 days" ?w)
        ("mime:image/* AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam \
AND NOT maildir:/Zoho/Spam AND NOT maildir:/Yahoo/Bulk\\ Mail" "Messages with images" ?p)))

(add-hook 'mu4e-compose-mode-hook 'tv/message-mode-setup)

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
    (cl-incf tv/mu4e-counter)))
(add-hook 'mu4e-update-pre-hook #'tv/mu4e-update-mail-quick-or-full)

;;; Attempt to show images when viewing messages
;; (setq mu4e-view-show-images t
;;       mu4e-view-image-max-width 800)

;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
            '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Setup queue mail dir
;;
;; `smtpmail-queue-mail' is set by NetworkManager.el.
(setq smtpmail-queue-dir "~/Maildir/queue/")

;;; Handle quoted text added with `message-mark-inserted-region' (`C-c M-m')
(add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)
;;; Show Smileys
(add-hook 'mu4e-view-mode-hook 'smiley-buffer)

(defun tv/curl-url-retrieve (url)
  (with-temp-buffer
    (call-process "curl" nil t nil "-s" "-L" url)
    (buffer-string)))

(defun tv/mu4e-show-patch-other-frame (url)
  (let ((contents "")
        (bufname (file-name-nondirectory url)))
    (if (buffer-live-p (get-buffer bufname))
        (progn (switch-to-buffer-other-frame bufname)
               (view-mode))
      (setq contents (tv/curl-url-retrieve url))
      (switch-to-buffer-other-frame (get-buffer-create bufname))
      (erase-buffer)
      (save-excursion (insert contents))
      (diff-mode)
      (view-mode))))

(defun tv/mu4e-browse-url-or-show-patch (arg)
  (interactive "P")
  (require 'helm-net)
  (let ((url (w3m-active-region-or-url-at-point)))
    (when url
      (if (string-match "\\.\\(patch\\|diff\\)\\'" url)
          (tv/mu4e-show-patch-other-frame (if arg (concat url "?w=1") url))
        (browse-url url)))))
(define-key mu4e-view-mode-map (kbd "C-c C-c") 'tv/mu4e-browse-url-or-show-patch)

(defadvice w3m-goto-next-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (let ((pos (next-single-property-change
                (point) 'w3m-anchor-sequence)))
      (and pos (goto-char pos)))))

(defadvice w3m-goto-previous-anchor (before go-to-end-of-anchor activate)
  (when (w3m-anchor-sequence)
    (let ((pos (previous-single-property-change
                (point) 'w3m-anchor-sequence)))
      (and pos (goto-char pos)))))

(defun tv/mu4e-next-anchor ()
  (interactive)
  (require 'w3m)
  (or (w3m-next-anchor)
      (let ((pos (point)))
        (when (eq (get-text-property (point) 'face)
                  'mu4e-link-face)
          (setq pos (next-single-property-change (point) 'face)))
        (let ((next-url (and pos
                             (or (text-property-any
                                  pos (point-max)
                                  'face 'mu4e-link-face)
                                 (text-property-any
                                  (point-min) (point-max)
                                  'face 'mu4e-link-face)))))
          (and next-url (goto-char next-url))))))

(defun tv/mu4e-previous-anchor ()
  (interactive)
  (require 'helm-lib)
  (require 'w3m)
  (or (w3m-previous-anchor)
      (let ((prev-url (save-excursion
                        (helm-awhile (re-search-backward
                                      ffap-url-regexp nil t)
                          (goto-char (match-beginning 0))
                          (when (eq (get-text-property
                                     (point) 'face)
                                    'mu4e-link-face)
                            (cl-return (point)))
                          (goto-char it)))))
        (and prev-url (goto-char prev-url)))))

(define-key mu4e-view-mode-map (kbd "<C-tab>")   'tv/mu4e-next-anchor)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'tv/mu4e-previous-anchor)
(define-key mu4e-view-mode-map (kbd "X")         'mu4e-view-save-attachment-multi)

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

;; Refresh main buffer when sending queued mails
(defun tv/advice-smtpmail-send-queued-mail ()
  (when (and mu4e~main-buffer-name
             (eq major-mode 'mu4e-main-mode))
    (with-current-buffer mu4e~main-buffer-name
      (revert-buffer))))
(advice-add 'smtpmail-send-queued-mail :after #'tv/advice-smtpmail-send-queued-mail)

;; Org links
(require 'org-mu4e)
(define-key mu4e-view-mode-map (kbd "C-c C-l") 'org-store-link)

(provide 'mu4e-config)

;;; mu4e-config.el ends here
