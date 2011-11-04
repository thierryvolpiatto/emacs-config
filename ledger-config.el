;;; ledger-config.el - extend ledger.el

;; Code:

(require 'ledger)

(define-key ledger-mode-map (kbd "C-c a l") 'ledger-align-device)
(setq ledger-default-device "€")

;; «Redefine-ledger-reconcile-visit» (to ".Redefine-ledger-reconcile-visit")
(defun ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (equal (car where) "/dev/stdin")
      (switch-to-buffer-other-window ledger-buf)
      (goto-char (cdr where)))))

;; «Align-euro-device» (to ".Align-euro-device")
(defun ledger-align-device (&optional column)
  (interactive "p")
  (if (= column 1)
      (setq column 48))
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
      (forward-line))))

;; «ledger-position-at-point» (to ".ledger-position-at-point")
(defun ledger-position-at-point ()
  (interactive)
  (let* ((bal (with-temp-buffer
               (apply #'call-process "ledger" nil t nil
                      (list "-C" "bal" "socgen"))
               (split-string (buffer-string) "\n" t)))
         (result (car (last bal))))
    (string-match "€ [0-9.]*" result)
    (setq result (match-string 0 result))
    (insert (concat "[" result "]"))))

(defadvice ledger-reconcile-refresh (after align-euros () activate)
  "Align euros in reconcile buffer when refreshing with `C-l'."
  (save-excursion
    (let ((inhibit-read-only t))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)€" 1 1 nil))))


(defun ledger-add-expense-from-org ()
  "Add expense from current line of a org table created with csv2org"
  (interactive)
  (let* ((line-list   (split-string (buffer-substring-no-properties
                                   (point-at-bol) (point-at-eol)) "|" t))
         (fdate       (replace-regexp-in-string "^ *\\| *$" "" (nth 0 line-list)))
         (split-date  (split-string fdate "/"))
         (date        (mapconcat 'identity (reverse split-date) "/"))
         (payee       (replace-regexp-in-string "^ *\\| *$" "" (nth 2 line-list)))
         (amount      (replace-regexp-in-string "^ *\\| *$" "" (nth 4 line-list)))
         (categorie   (anything-comp-read "Categorie: " (ledger-collect-categories)))
         (type        (anything-comp-read "Type: " '("Visa" "Check" "Tip" "Prelevement")))
         (ledger-file (getenv "LEDGER_FILE"))
         numcheck defnumcheck)
    (with-current-buffer (find-file-noselect ledger-file)
      (goto-char (point-max))
      (setq amount (replace-regexp-in-string "-" "" amount))
      (insert (concat
               date " "
               payee
               "\n    "
               "Expenses:" categorie (make-string 8 ? ) "€ " amount "\n    "
               "Liabilities:Socgen:" type "\n\n"))
      (goto-char (point-min))
      (ledger-align-device 1)
      (save-buffer))))
      ;(find-file-other-window ledger-file))))
(define-key org-mode-map (kbd "<f5> -") 'ledger-add-expense-from-org)

(defun ledger-reverse-date-to-us ()
  (interactive)
  (with-current-buffer (find-file-noselect (getenv "LEDGER_FILE"))
    (goto-char (point-min))
    (while (re-search-forward "^[0-9]\\{2\\}/[0-9]\\{2\\}/[0-9]\\{4\\}" nil t)
      (let* ((dt     (match-string-no-properties 0))
             (split  (reverse (split-string dt "/")))
             (new-dt (mapconcat 'identity split "/")))
        (delete-region (point-at-bol) (point))
        (insert new-dt)))))

(defun ledger-add-expense (date payee categorie type amount)
  (interactive
   (list (read-string "Date: " (tv-cur-date-string :separator "/"))
         (read-string "Payee: ")
         (anything-comp-read "Categorie: " (ledger-collect-categories))
         (anything-comp-read "Type: " '("Visa" "Check" "Tip" "Prelevement"))
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

(defun ledger-add-income-from-org ()
  "Add income from current line of a org table created with csv2org"
  (interactive)
  (let* ((line-list   (split-string (buffer-substring-no-properties
                                     (point-at-bol) (point-at-eol)) "|" t))
         (fdate       (replace-regexp-in-string "^ *\\| *$" "" (nth 0 line-list)))
         (split-date  (split-string fdate "/"))
         (date        (mapconcat 'identity (reverse split-date) "/"))
         (payee       (replace-regexp-in-string "^ *\\| *$" "" (nth 2 line-list)))
         (amount      (replace-regexp-in-string "^ *\\| *$" "" (nth 3 line-list)))
         (categorie   (anything-comp-read "Categorie: " (ledger-collect-categories)))
         (account     (anything-comp-read "Account: " '("Socgen:Checking" "Socgen:Prelevement")))
         (ledger-file (getenv "LEDGER_FILE")))
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
      (save-buffer))))
      ;(find-file-other-window ledger-file))))
(define-key org-mode-map (kbd "<f5> +") 'ledger-add-income-from-org)

(defun ledger-add-income (date payee categorie account amount)
  (interactive
   (list (read-string "Date: " (tv-cur-date-string :separator "/"))
         (read-string "Payee: ")
         (anything-comp-read "Categorie: " (ledger-collect-categories))
         (anything-comp-read "Account: " '("Socgen:Checking" "Socgen:Prelevement")) ;; TODO add completion here
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
                      "Auto:Gasoil" "Auto:Garage"
                      "Voyages" "Escalade"
                      "Livres" "Informatique"
                      "Loisirs" "Divers"
                      "Loyers:Immovar" "Loyers:Big"))
         result)
    (with-current-buffer (find-file-noselect (getenv "LEDGER_FILE"))
      (goto-char (point-min))
      (while
          (re-search-forward
           "\\(^ *Expenses\\|Income\\):\\([^ €0-9\n]*\\)" (point-max) t)
        (setq result (match-string 2))
        (unless (or (member result categories)
                    (string= result ""))
          (push result categories))))
    categories))

(defun ledger-point-entries-in-buffer ()
  "Point entries from point to end of buffer.
Like C-c C-e but on all entries.
If entries are already pointed, skip."
  (interactive)
  (while (re-search-forward "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}" nil t)
    (forward-char 1) (unless (looking-at "[*]") (insert "* "))))

(provide 'ledger-config)

;;; ledger-config.el ends here
