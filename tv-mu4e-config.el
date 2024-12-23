;;; tv-mu4e-config.el --- Config for mu4e. -*- lexical-binding: t -*-

;;; Commentary:

;; WARNING: Don't rename this file!
;; This file should not be renamed to mu4e-config.el as mu4e have now
;; a file named like this.

;;; Code:

(require 'mu4e-contrib)
(require 'mu4e-view-gnus nil t)
(require 'gnus-and-mu4e)

(setq mail-user-agent      'mu4e-user-agent
      read-mail-command    'mu4e
      gnus-dired-mail-mode 'mu4e-user-agent)


;;; Message and smtp settings
;;
;; Use Mu4e to compose new mail.
(define-key global-map [remap compose-mail] 'mu4e-compose-new)

;; Contexts (setup smtp servers)
;;
(setq mu4e-compose-context-policy 'ask-if-none
      mu4e-context-policy 'pick-first
      mu4e-contexts
      `(,(make-mu4e-context
           :name "Posteo"
           :enter-func (lambda () (mu4e-message "Switch to Posteo"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Posteo" (mu4e-message-field msg :maildir))))
           :vars '((smtpmail-smtp-user           . "thievol@posteo.net")
                   (smtpmail-default-smtp-server . "posteo.de")
                   (smtpmail-smtp-server         . "posteo.de")
                   (smtpmail-smtp-service        . 587)
                   (user-mail-address            . "thievol@posteo.net")
                   (user-full-name               . "Thierry Volpiatto")
                   (mu4e-compose-signature       . t)
                   (mu4e-sent-folder             . "/sent")
                   (mu4e-trash-folder            . "/trash")
                   (mu4e-drafts-folder           . "/drafts")))))

(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                    mu4e-contexts)))


;;; Mu4e settings
;;

;;; Default
(setq mu4e-maildir "~/Maildir"
      mu4e-compose-complete-addresses nil
      mu4e-completing-read-function 'completing-read
      mu4e-view-show-addresses t
      mu4e-headers-include-related nil) ; Can be toggled with "W".

;; See (info "(mu4e) Refiling messages")
(defvar tv:mu4e-subject-alist '(("\\[djcb/mu\\]" . "/Posteo/github-mu"))
  "List of subjects and their respective refile folders.")

(defun tv:mu4e-refile-folder-function (msg)
  "Set the refile folder for MSG."
  (let ((subject (mu4e-message-field msg :subject))
        (maildir (mu4e-message-field msg :maildir)))
    (cl-loop for (reg . dir) in tv:mu4e-subject-alist
             when (string-match reg subject)
             return dir
             finally return "/archive")))

(setq mu4e-refile-folder 'tv:mu4e-refile-folder-function)

(with-eval-after-load 'gnus-art
  (fset 'gnus-article-press-button 'mu4e-scroll-up))

;; Disable mu4e-modeline-mode when quitting.
(defun tv:mu4e-quit-window ()
  (interactive)
  (quit-window)
  (when (fboundp 'mu4e-modeline-mode)
    (mu4e-modeline-mode -1)))

(define-key mu4e-main-mode-map "q"   'tv:mu4e-quit-window)
(define-key mu4e-main-mode-map "Q"   'mu4e-quit)
(define-key mu4e-main-mode-map "\C-s" 'helm-mu)
(define-key mu4e-main-mode-map [remap mu4e-headers-search] 'tv:mu4e-headers-search)
(define-key mu4e-main-mode-map "u" 'mu4e-update-index)

(setq mu4e-headers-skip-duplicates t)

;;; Signature
(setq mu4e-compose-signature t)

;;; Shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Posteo/Drafts"         . ?d)
        ("/Posteo/INBOX"          . ?i)
        ("/Posteo/Sent"           . ?s)
        ("/Posteo/Trash"          . ?t)
        ("/Posteo/Emacs-devel"    . ?e)
        ("/Posteo/Emacs-bug"      . ?b)
        ("/Posteo/github-helm"    . ?h)
        ("/Posteo/github-mu"      . ?m)
        ("/Posteo/github-async"   . ?a)
        ("/Posteo/Spam-Log"       . ?l)
        ("/Posteo/Pub"            . ?p)
        ("/Posteo/Fortuneo"       . ?f)))

;;; Bookmarks
(setq mu4e-bookmarks
      '((:name
         "Unread messages"
         :query "flag:unread AND NOT flag:trashed"
         :key ?u)
        (:name
         "Unread messages from Helm"
         :query "flag:unread AND NOT flag:trashed AND maildir:/Posteo/github-helm"
         :key ?h)
        (:name
         "Today's messages"
         :query "date:today..now AND NOT flag:trashed"
         :key ?t)
        (:name
         "Yesterday and today messages"
         :query "date:1d..now AND NOT flag:trashed"
         :key ?y)
        (:name
         "Last week messages"
         :query "date:7d..now AND NOT flag:trashed"
         :key ?w)
        (:name
         "Last month messages"
         :query "date:1m..now AND NOT flag:trashed"
         :key ?m)
        ))

(defun tv:mu4e-headers-search ()
  "Add a query reminder in `mu4e-headers-search' prompt."
  (interactive)
  (mu4e-search
   nil
   (format "Search for%s: "
           (propertize
            " " 'display (propertize "(from:date:flag:prio:mime:maildir:and/not)"
                                     'face '(:foreground "DimGray"))))))

(defun tv:mu4e-mark-all-for-something ()
  (interactive)
  (save-excursion
    (while (not (eobp)) (mu4e-headers-mark-for-something))))

(defun tv:mu4e-mark-similar-messages-for-something ()
  (interactive)
  (let ((regexp (mu4e~headers-from-or-to (mu4e-message-at-point)))
        (count 0))
    (when regexp
      (save-excursion
        (while (re-search-forward (regexp-quote regexp) nil t)
          (mu4e-headers-mark-for-something)
          (cl-incf count)))
      (message "%s messages marked for something" count))))

(define-key mu4e-search-minor-mode-map (kbd "s") 'tv:mu4e-headers-search)
(define-key mu4e-headers-mode-map (kbd "C-c A") 'tv:mu4e-mark-all-for-something)
(define-key mu4e-headers-mode-map (kbd "C-c S") 'tv:mu4e-mark-similar-messages-for-something)

(add-hook 'mu4e-compose-mode-hook 'tv:message-mode-setup)

;;; Use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/download")

;;; Updating
;;
;;
;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "mbsync -a -V")
;; (setq mu4e-change-filenames-when-moving t) ;; Use only with mbsync!

(setq mu4e-get-mail-command "offlineimap -q -u Basic")

;;; Make a full update all the
;; `tv:mu4e-max-number-update-before-toggling' mail retrievals.
(defvar tv:mu4e-counter 10) ; Ensure a full update on startup.
(defvar tv:mu4e-max-number-update-before-toggling 10)
(defvar tv:mu4e-get-mail-command-full "offlineimap -u Basic")
(defvar tv:mu4e-get-mail-command-quick "offlineimap -q -u Basic")
(defun tv:mu4e-update-mail-quick-or-full ()
  (if (>= tv:mu4e-counter
          tv:mu4e-max-number-update-before-toggling)
      (progn
        (setq mu4e-get-mail-command tv:mu4e-get-mail-command-full)
        (setq tv:mu4e-counter 0))
    (setq mu4e-get-mail-command tv:mu4e-get-mail-command-quick)
    (cl-incf tv:mu4e-counter)))
(add-hook 'mu4e-update-pre-hook #'tv:mu4e-update-mail-quick-or-full)

;;; Automatic updates.
;(setq mu4e-update-interval 600)

(setq mu4e-view-inhibit-images t)

;; View html message in firefox (type aV)
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;; Setup queue mail dir
;;
(setq smtpmail-queue-dir "~/Maildir/queue/")

(if (boundp 'mu4e-view-rendered-hook)
    (progn
      (add-hook 'mu4e-view-rendered-hook 'mu4e-mark-region-code 100)
      (add-hook 'mu4e-view-rendered-hook 'smiley-buffer 100))
  ;; Handle quoted text added with `message-mark-inserted-region' (`C-c M-m')
  (add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)
  ;; Show Smileys
  (add-hook 'mu4e-view-mode-hook 'smiley-buffer))

(define-key mu4e-view-mode-map (kbd "C-c C-c") 'tv:browse-url-or-show-patch)

;;; A simplified and more efficient version of `article-translate-strings'.
;;
;; Transform also in headers.
(defun tv:mu4e-view-translate-strings (map)
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
  (with-current-buffer mu4e-view-buffer-name
    (tv:mu4e-view-translate-strings
     '((128 . "EUR") (130 . ",") (131 . "f") (132 . ",,")
       (133 . "...") (139 . "<") (140 . "OE") (145 . "`")
       (146 . "'") (147 . "``") (148 . "\"") (149 . "*")
       (150 . "-") (151 . "--") (152 . "~") (153 . "(TM)")
       (155 . ">") (156 . "oe") (180 . "'")))))

;; Crypto
(setq mu4e-compose-crypto-policy '(sign-all-messages))

(define-key mu4e-compose-mode-map (kbd "C-c e") 'mml-secure-encrypt)

;; Refresh main buffer when sending queued mails
(defun tv:advice-smtpmail-send-queued-mail ()
  (when (and mu4e-main-buffer-name
             (eq major-mode 'mu4e-main-mode))
    (with-current-buffer mu4e-main-buffer-name
      (revert-buffer))))
(advice-add 'smtpmail-send-queued-mail :after #'tv:advice-smtpmail-send-queued-mail)

;; Org links
(define-key mu4e-view-mode-map (kbd "C-c C-l") 'org-store-link)

(setq mu4e-headers-thread-last-child-prefix '("L" . "╰► "))
(setq mu4e-headers-precise-alignment t)

(define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-all-unread-read)
(define-key mu4e-headers-mode-map (kbd "<down>") 'mu4e-headers-next)
(define-key mu4e-headers-mode-map (kbd "<up>") 'mu4e-headers-prev)

(defun tv:mu4e-remove-buttons-in-reply (original-fn &rest args)
  (if current-prefix-arg
      (delete-region (point) (point-max))
    (save-excursion
      (message-goto-body)
      (while (re-search-forward "^[[]\\{2\\}.*[]]\\{2\\}" nil t)
        (replace-match "")))
    (apply original-fn args)))
(add-function :around mu4e-compose-cite-function #'tv:mu4e-remove-buttons-in-reply)

(when (boundp 'mu4e-search-minor-mode-map)
  (define-key mu4e-search-minor-mode-map (kbd "S") nil))

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

;; Ignore these pesty gnus buttons.
(setq w3m-handle-non-anchor-buttons nil)

(defun tv:mu4e-next-anchor ()
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

(defun tv:mu4e-previous-anchor ()
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

(define-key mu4e-view-mode-map (kbd "<C-tab>")   'tv:mu4e-next-anchor)
(define-key mu4e-view-mode-map (kbd "<backtab>") 'tv:mu4e-previous-anchor)
(define-key mu4e-view-mode-map (kbd "C-c v") 'mu4e-view-open-attachment)

(with-eval-after-load 'mu4e-vars
  (set-face-attribute 'mu4e-region-code nil :extend t))

(provide 'tv-mu4e-config)

;;; tv-mu4e-config.el ends here
