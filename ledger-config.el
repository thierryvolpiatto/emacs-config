;;; ledger-config.el - extend ledger.el

;; Code:

(define-key ledger-mode-map (kbd "C-c a l") 'ledger-align-device)
(defvar ledger-default-device "€")

;; «Redefine-ledger-reconcile-visit» (to ".Redefine-ledger-reconcile-visit")
(defun ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (equal (car where) "/dev/stdin")
      (switch-to-buffer-other-window ledger-buf)
      (goto-char (cdr where)))))

;; «Align-euro-device» (to ".Align-euro-device")
;;;###autoload
(defun ledger-align-device (&optional column)
  (interactive "p")
  (when (= column 1) (setq column 48))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ledger-default-device nil t)
      (backward-char)
      (let ((col (current-column))
            (beg (point))
            target-col len)
        (skip-chars-forward (concat "-" ledger-default-device "0-9,."))
        (setq len (- (point) beg))
        (setq target-col (- column len))
        (if (< col target-col)
            (progn
              (goto-char beg)
              (insert (make-string (- target-col col) ? )))
            (move-to-column target-col)
            (if (looking-back "  ")
                (delete-char (- col target-col))
                (skip-chars-forward "^ \t")
                (delete-horizontal-space)
                (insert "  ")))
        (forward-line)))))

;; «ledger-position-at-point» (to ".ledger-position-at-point")
;;;###autoload
(defun ledger-position (arg)
  "Show ledger balance, with prefix-arg insert it at point."
  (interactive "P")
  (let* ((bal (with-temp-buffer
               (apply #'call-process "ledger" nil t nil
                      (list "-C" "bal" "socgen"))
               (split-string (buffer-string) "\n" t)))
         (result (car (last bal))))
    (when (string-match "€ [0-9.]*" result)
      (setq result (match-string 0 result))
      (if arg
          (insert (format "[%s]" result))
          (message "ledger balance: %s" result)))))

(defadvice ledger-reconcile-refresh (after align-euros () activate)
  "Align euros in reconcile buffer when refreshing with `C-l'."
  (save-excursion
    (let ((inhibit-read-only t))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)€" 1 1 nil))))

(defun ledger-reverse-date-from-regexp (regexp)
  (with-current-buffer (find-file-noselect (getenv "LEDGER_FILE"))
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let* ((dt     (match-string-no-properties 0))
             (split  (reverse (split-string dt "/")))
             (new-dt (mapconcat 'identity split "/")))
        (delete-region (point-at-bol) (point))
        (insert new-dt)))))

;;;###autoload
(defun ledger-reverse-date-to-us ()
  (interactive)
  (ledger-reverse-date-from-regexp
   "^[0-9]\\{2\\}/[0-9]\\{2\\}/[0-9]\\{4\\}"))

;;;###autoload
(defun ledger-reverse-date-to-fr ()
  (interactive)
  (ledger-reverse-date-from-regexp
   "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}"))

;;;###autoload
(defun ledger-add-expense (date payee categorie type amount)
  (interactive
   (list (read-string "Date: " (format-time-string "%Y/%m/%d"))
         (read-string "Payee: ")
         (helm-comp-read "Categorie: " (ledger-collect-categories))
         (helm-comp-read "Type: " '("Visa" "Check" "Tip" "Prelevement"))
         (read-string "Amount: ")))
  (let ((ledger-file (getenv "LEDGER_FILE"))
        numcheck defnumcheck)
    (with-current-buffer (find-file-noselect ledger-file)
      (goto-char (point-max))
      (when (string= type "Check")
        (setq defnumcheck (save-excursion
                            (when
                                (re-search-backward
                                 "\\(^[0-9]\\{4\\}/[0-9/]*\\)\\(.*\\)\\(\([0-9]*\)\\)" nil t)
                              (replace-regexp-in-string "\(\\|\)" "" (match-string 3)))))
        (setq defnumcheck (int-to-string (1+ (string-to-number defnumcheck))))
        (setq numcheck (read-string "CheckNumber: " defnumcheck)))
      (insert (concat
               date " " payee (or (and numcheck (concat " (" numcheck ")")) "") "\n    "
               "Expenses:" categorie (make-string 8 ? ) "€ " amount "\n    "
               "Liabilities:Socgen:" type "\n\n"))
      (goto-char (point-min))
      (ledger-align-device 1)
      (save-buffer)
      (pop-to-buffer ledger-file))))

;;;###autoload
(defun ledger-add-income (date payee categorie account amount)
  (interactive
   (list (read-string "Date: " (format-time-string "%Y/%m/%d"))
         (read-string "Payee: ")
         (helm-comp-read "Categorie: " (ledger-collect-categories))
         (helm-comp-read "Account: " '("Socgen:Checking" "Socgen:Prelevement")) ;; TODO add completion here
         (read-string "Amount: ")))
  (let ((ledger-file (getenv "LEDGER_FILE")))
    (with-current-buffer (find-file-noselect ledger-file)
      (goto-char (point-max))
      (insert (concat
               date " " payee "\n    "
               (if (string= account "Socgen:Checking") "Assets:" "Liabilities:")
               account (make-string 8 ? ) "€ "
               (if (string= account "Socgen:Checking")
                   amount (int-to-string (- (string-to-number amount))))
               "\n    Income:" categorie "\n\n"))
      (goto-char (point-min))
      (ledger-align-device 1)
      (save-buffer)
      (pop-to-buffer ledger-file))))

(defun ledger-collect-categories ()
  (let ((categories '("Alimentation" "Impots"
                      "Auto:gasoil" "Auto:garage"
                      "Voyages" "Escalade"
                      "Livres" "Informatique"
                      "Loisirs" "Divers"
                      "Loyers:immovar" "Loyers:big"))
        result)
    (with-current-buffer (find-file-noselect (getenv "LEDGER_FILE"))
      (goto-char (point-min))
      (save-excursion
        (while
            (re-search-forward
             "\\(^ *Expenses\\|Income\\):\\([^ €0-9\n]*\\)" (point-max) t)
          (setq result (match-string 2))
          (unless (or (member result categories)
                      (string= result ""))
            (push result categories))))
      categories)))

;;;###autoload
(defun ledger-point-entries-in-buffer ()
  "Point entries from point to end of buffer.
Like C-c C-e but on all entries.
If entries are already pointed, skip."
  (interactive)
  (while (re-search-forward "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}" nil t)
    (forward-char 1) (unless (looking-at "[*]") (insert "* "))))

(defvar csv2ledger-default-input-dir "~/Téléchargements/")
(defvar csv2ledger-default-output-dir "~/finance")
;;;###autoload
(defun csv2ledger (account infile ofile)
  (interactive (list (completing-read "Account: " '("Socgen" "Paypal" "livretA"))
                     (read-file-name "Input cvs file: "
                                     csv2ledger-default-input-dir
                                     nil nil nil (lambda (f)
                                                   (or (file-directory-p f)
                                                       (string= (file-name-extension f) "csv"))))
                     (read-file-name "Output file (.dat): "
                                     csv2ledger-default-output-dir
                                     nil nil nil (lambda (f)
                                                   (or (file-directory-p f)
                                                       (string= (file-name-extension f) "dat"))))))
  (let ((ibuf (find-file-noselect infile))
        (obuf (find-file-noselect ofile))
        curpos beg ov)
    (with-current-buffer obuf
      (setq curpos (point))
      (goto-char (point-max))
      (setq beg (point))
      (text-mode))
    (with-current-buffer ibuf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[0-9]+/" nil t)
          (let* ((split (split-string (buffer-substring (point-at-bol) (point-at-eol)) ";" t))
                 (date (car split))
                 (payee (nth 2 split))
                 (amountstr (replace-regexp-in-string "," "." (nth 3 split)))
                 (amountnum (string-to-number amountstr))
                 (deb (< amountnum 0))
                 (cred (> amountnum 0)))
            (setq amountstr (replace-regexp-in-string "-" "" amountstr))
            (with-current-buffer obuf
              (save-excursion 
                (insert
                 (concat date " * " payee "\n    "
                         (if deb
                             (format "Expenses:unknown    € %s\n    Liabilities:Socgen\n\n" amountstr)
                             (format "Assets:%s:Checking    € %s\n    Income\n\n" account amountstr))))))))))
    (with-current-buffer obuf
      (ledger-mode)
      (remove-overlays)
      (setq ov (make-overlay beg (point-max)))
      (overlay-put ov 'face '((:background "DarkSlateGray"))))
    (switch-to-buffer obuf)
    (ledger-reverse-date-to-us)))

;;;###autoload
(defun ledger-exchange-point-an-mark-or-overlay ()
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)
      (helm-aif (overlays-at (point))
          (cond ((eq (overlay-start (car it)) (point))
                 (goto-char (next-overlay-change (point)))
                 (forward-line -1))
                (t
                 (goto-char (previous-overlay-change (point)))))
        (and (delq nil (overlay-lists))
             (goto-char (next-overlay-change (point)))))))
(define-key ledger-mode-map (kbd "C-x C-x") 'ledger-exchange-point-an-mark-or-overlay)

(defvar ledger-previous-window-configuration nil)
(defadvice ledger-reconcile (before save-winconf activate)
  (setq ledger-previous-window-configuration (current-window-configuration)))

(defadvice ledger-reconcile-quit (after restore-winconf activate)
  (set-window-configuration ledger-previous-window-configuration))

(provide 'ledger-config)

;;; ledger-config.el ends here
