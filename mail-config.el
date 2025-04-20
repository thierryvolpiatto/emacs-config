;;; mail-config.el --- Common setting for gnus and mu4e -*- lexical-binding: t -*-

;;; code:

;; Fontify patches in gnus
(require 'gnus-patch)
(advice-add 'gnus-article-prepare-display :after #'gnus-patch:article-treat-patch)

;;; Message and smtp settings
;;
;;
(setq user-mail-address "thievol@posteo.net")
(setq user-full-name "Thierry Volpiatto")

;; Don't send to these addresses in wide reply.
;; See (info "(message) Wide Reply")
(setq message-dont-reply-to-names
      '("notifications@github\\.com"
        ".*@noreply\\.github\\.com"))

;; Forward sent messages to myself. Posteo/Sent is therefore no more
;; synchronised, sent messages are copied to /sent locally. This have
;; the benefit of having all sent mails saved and encrypted on posteo
;; server. And if I really want to have my sent mails in /Posteo/Sent
;; I can create a filter on Posteo server to move my own emails to /Sent.
(setq message-default-mail-headers (format "Bcc: %s\n" user-mail-address))

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

;; Save mime parts
(defun tv:gnus-mime-parts ()
  (with-current-buffer gnus-article-buffer
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (not (eobp))
               for part = (get-text-property (point) 'gnus-data)
               for index = (get-text-property (point) 'gnus-part) 
               when (and part (numberp index))
               collect (cons (or (mm-handle-filename part)
                                 (format "mime-part-%02d" index))
                             part)
               do (forward-line 1)))))

(defun tv:gnus-save-mime-parts ()
  (interactive nil gnus-summary-mode gnus-article-mode)
  (let* ((helm-comp-read-use-marked t)
         (parts (tv:gnus-mime-parts))
         (files (completing-read "Save mime part(s): " (mapcar 'car parts) nil t)))
    (when files
      (dolist (f files)
        (mm-save-part-to-file
         (assoc-default f parts) (expand-file-name f mm-default-directory))))))

;; We can't use directly `completing-read' because the signature of
;; `gnus-completing-read' which funcall this is not the same (missing predicate). 
(defun tv:gnus-emacs-completing-read (prompt collection &optional require-match
                                                          _initial-input history def)
  "Call standard `completing-read-function'."
  (completing-read prompt collection nil require-match nil history def))

(with-eval-after-load 'gnus-sum
  (setq gnus-completing-read-function #'tv:gnus-emacs-completing-read))

;; Html renderer (shr)
(setq mm-text-html-renderer (if (fboundp 'w3m) 'w3m 'shr))
(setq shr-color-visible-luminance-min 75)
(setq shr-use-colors nil)

;; I use C-c C-c to browse url and RET for scrolling.
(with-eval-after-load 'shr
  (define-key shr-map (kbd "RET") nil))

(setq mm-inline-text-html-with-w3m-keymap nil
      mm-html-inhibit-images t
      gnus-inhibit-images t)

;; Default directory to save attached files 
(setq mm-default-directory "~/download/")

;;; Encryption
;;
;; Keybinding for signing/encrypting:
;; `C-c C-m s o' and `C-c C-m c o'.

;; To always sign emails
;; (add-hook 'message-setup-hook 'mml-secure-message-sign)
;; To always encrypt emails
;; (add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; force choosing key (completion).
;; (setq mm-encrypt-option 'guided)

;; `mml-secure-openpgp-encrypt-to-self' will encrypt to self only when
;; using mml* functions but not if for some reasons I use epa*, using
;; encrypt-to in gpg.conf ensure epa and mml encrypt to self.
(setq mml-secure-openpgp-encrypt-to-self nil ; I use encrypt-to in gpg.conf.
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-signers '("0EC56D141D16EF93")
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
  (interactive "P" gnus-article-mode)
  (require 'helm-net)
  (let ((url (w3m-active-region-or-url-at-point)))
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


(provide 'mail-config)

;;; mail-config.el ends here
