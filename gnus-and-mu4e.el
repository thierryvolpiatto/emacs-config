;;; gnus-and-mu4e.el --- Common setting for gnus and mu4e -*- lexical-binding: t -*-

;;; code:

;;; Message and smtp settings
;;
;;
(setq user-mail-address "thievol@posteo.net")
(setq user-full-name "Thierry Volpiatto")

;; Don't send to these addresses in wide reply.
;; See (info "(message) Wide Reply")
(setq message-dont-reply-to-names
      '("notifications@github\\.com"
        ".*@noreply\\.github\\.com"
        "thievol@posteo\\.net"))

;; [smtpmail-async] Experimental, use `smtpmail-send-it' otherwise.
;; To debug use `smtpmail-send-it'
(setq message-send-mail-function 'smtpmail-send-it
      ;; smtpmail-debug-info t        ; Uncomment to debug
      ;; smtpmail-debug-verb t        ; Uncomment to debug on server
      mail-specify-envelope-from t ; Use from field to specify sender name.
      mail-envelope-from 'header)  ; otherwise `user-mail-address' is used. 

;; Default settings.
(setq smtpmail-default-smtp-server "posteo.de"
      smtpmail-smtp-user user-mail-address
      smtpmail-smtp-server "posteo.de"
      smtpmail-smtp-service 587)

(defun tv:message-mode-setup ()
  ;; (setq tv:message-pre-winconf (current-window-configuration))
  (setq fill-column 72)
  (turn-on-auto-fill)
  (epa-mail-mode 1)
  (define-key epa-mail-mode-map (kbd "C-c C-e l") 'helm-list-epg-keys))
(add-hook 'message-mode-hook 'tv:message-mode-setup)

;; Ne pas demander si on splitte les pa 
(setq message-send-mail-partially-limit nil)

;;; mm-* settings
;;
(setq mm-file-name-rewrite-functions
      '(mm-file-name-delete-control
        mm-file-name-delete-gotchas
        mm-file-name-trim-whitespace
        mm-file-name-collapse-whitespace
        mm-file-name-replace-whitespace))

;; Html renderer
(setq mm-text-html-renderer 'shr)
(setq shr-color-visible-luminance-min 75)
(setq shr-use-colors nil)

(setq mm-inline-text-html-with-w3m-keymap nil
      mm-html-inhibit-images t
      gnus-inhibit-images t)

;; Default directory to save attached files 
(setq mm-default-directory "~/download/")

;;; Encryption
;;
;; force choosing key (completion).
;; (setq mm-encrypt-option 'guided)

;; `mml-secure-openpgp-encrypt-to-self' will encrypt to self only when
;; using mml* functions but not if for some reasons I use epa*, using
;; encrypt-to in gpg.conf ensure epa and mml encrypt to self.
(setq mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-signers '("0EC56D141D16EF93") ; priv
      )

;; Using 'known for `mm-verify-option' may hang mu4e or gnus forever
;; if the key is not found.
(setq mm-verify-option 'known
      mm-decrypt-option 'known)

;;; Patchs
;;
(defun tv:curl-url-retrieve (url)
  (with-temp-buffer
    (call-process "curl" nil t nil "-s" "-L" url)
    (buffer-string)))

(defun tv:show-patch-other-frame (url)
  (let ((contents "")
        (bufname (file-name-nondirectory url)))
    (if (buffer-live-p (get-buffer bufname))
        (progn (switch-to-buffer-other-frame bufname)
               (view-mode))
      (setq contents (tv:curl-url-retrieve url))
      (switch-to-buffer-other-frame (get-buffer-create bufname))
      (erase-buffer)
      (save-excursion (insert contents))
      (diff-mode)
      (view-mode))))

(defun tv:browse-url-or-show-patch (arg)
  (interactive "P")
  (require 'helm-net)
  (let ((url (shr-url-at-point nil)))
    (when url
      (if (string-match "\\.\\(patch\\|diff\\)\\'" url)
          (tv:show-patch-other-frame (if arg (concat url "?w=1") url))
        (browse-url url)))))

;; For some reasons some mails have many null characters at the end,
;; most of the time these emails come from emacs developers.
(defun tv:delete-null-chars-from-gnus ()
  "Delete null characters in gnus article buffer.
Such characters are represented by \"^@\" chars.
They are most of the time at the end of mails sent with Gnus or Rmail.
See https://en.wikipedia.org/wiki/Null_character."
  (save-excursion
    (let ((inhibit-read-only t))
      (message-goto-body)
      ;; WARNING: (emacs bug#44486)
      ;; Using ^@ instead of \0 corrupt emacs-lisp buffers
      ;; containing special characters such as "à" and may be
      ;; others (unicode), this doesn't happen in lisp-interaction
      ;; buffers i.e. scratch.
      (while (re-search-forward "\0" nil t)
        (replace-match "")))))
(add-hook 'gnus-part-display-hook 'tv:delete-null-chars-from-gnus)

(defun tv:autocrypt-import-key ()
  "Import key from autocrypt header to gpg keyring.
Try to import the key only if an autocrypt header is found and if
sender is not one of `autocrypt-peers'.  Called interactively with a
prefix arg import the key even if sender is member of
`autocrypt-peers'.

Mu4e and Gnus hang forever when a key is not found and mail is
signed. When this happen, importing the key from the autocrypt header,
if one may help."
  (interactive)
  (require 'epg)
  (require 'autocrypt)
  ;; `message-fetch-field' removes the newlines, so use `mail-fetch-field'.
  (let ((data (mail-fetch-field "Autocrypt" nil t))
        (from (message-sendmail-envelope-from)))
    (when data
      (with-temp-buffer
        (insert data)
        (goto-char (point-min))
        (delete-region (point-at-bol) (point-at-eol))
        (insert "-----BEGIN PGP PUBLIC KEY BLOCK-----\n")
        (goto-char (point-max))
        (insert "\n-----END PGP PUBLIC KEY BLOCK-----")
        (tv:epg-import-keys-region (point-min) (point-max))))))
;; (add-hook 'gnus-article-decode-hook 'tv:autocrypt-import-key)

(defun tv:epg-import-keys-region (start end)
  "Same as `epa-import-keys-region' but less verbose and BTW faster."
  (let ((context (epg-make-context epa-protocol)))
    (message "Autocrypt importing gpg key...")
    (condition-case err
	(progn
	  (epg-import-keys-from-string context (buffer-substring start end))
          (message "Autocrypt importing gpg key done"))
      (error "Importing from autocrypt failed: %s" (cadr err)))))

(defun tv:remove-cr ()
  (when (save-excursion
          (message-goto-body)
          (re-search-forward "\C-m$" nil t))
    (article-remove-cr)))
(add-hook 'gnus-part-display-hook 'tv:remove-cr)


(provide 'gnus-and-mu4e)

;;; gnus-and-mu4e.el ends here
