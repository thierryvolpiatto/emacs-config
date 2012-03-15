;;; csv2org.el --- 
;; 
;; Author: thierry
;; Maintainer: 
;; 
;; Created: dim. avril 26 19:42:22 2009 (+0200)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; User's variables
(defvar csv2org-output-dir "~/finance/csv2org/")

(defun csv2org (fname output-file)
  "Convert a csv file to an org table."
  (interactive (list (helm-c-read-file-name "CsvFile: ")
                     (helm-c-read-file-name "OutputFile: "
                                                :initial-input
                                                (format "%s%s-csv2org.org"
                                                        csv2org-output-dir
                                                        (tv-cur-date-string)))))
  (csv2org-1 fname output-file))

(defun csv2org-1 (fname output-file)
  "Convert a csv file to an org table."
  (if (string= (file-name-extension fname) "csv")
      (with-current-buffer (find-file-noselect output-file)
        (erase-buffer)
        (insert-file-contents fname)
        (goto-char (point-min))
        (let* ((info-list (split-string (buffer-substring (point) (point-at-eol))";"))
               (account (nth 0 info-list))
               (from (nth 1 info-list))
               (to (nth 2 info-list))
               (nentries (nth 3 info-list))
               (date (nth 4 info-list))
               (bal (nth 5 info-list)))
          (delete-region (point) (point-at-eol))
          ;; make the info section
          (insert "* Infos\n")
          (insert (format "  - Date: %s\n" date))
          (insert (format "  - Account%s\n" account))
          (insert (format "  - From %s to %s\n" from to))
          (insert (format "  - Entries: %s\n" nentries))
          (insert (format "  - Solde: %s\n" (propertize bal 'face 'traverse-match-face)))
          (insert "* Balance\n")
          ;; replace all ; with |
          (save-excursion
            (while (re-search-forward ";" nil t) (replace-match "\|")))
          (while (re-search-forward "^\n$" nil t) (forward-line))
          ;; Insert all | at bol
          (save-excursion
            (while (not (eobp))
              (beginning-of-line)
              (insert "|")
              (forward-line)))
          ;; remove long entry in column
          (while (or (search-forward "de l'opÃ©ration" nil t)
                     (search-forward "de l'opération" nil t))
            (replace-match ""))
          (end-of-line)
          ;; Add the P column
          (save-excursion
            (insert "|P\n")
            ;; Insert narrow column and active formulas
            (insert "|||<6>|=|=|")
            ;; add a column for negative entries
            (while (re-search-forward "\\(\|-[0-9]\\)" nil t)
              (let ((num (match-string 0)))
                (delete-char (- 0 (length num)))
                (insert (concat "|" num)))))
          ;; Replace Devise with Debit
          (save-excursion
            (while (re-search-forward "Devise" nil t)
              (replace-match "Debit"))
            ;; Remove all EUR
            (while (re-search-forward "EUR" nil t)
              (replace-match ""))
            ;; replace all ,
            (while (re-search-backward "," nil t)
              (replace-match "\."))
            ;; replace montant with credit
            (while (re-search-forward "Montant" nil t)
              (replace-match "Credit")))
          ;; Go to end and insert keyword and formulas
          (goto-char (point-max))
          (setq nentries (int-to-string (+ (string-to-number nentries) 2)))
          (insert (format "\n\n#+TBLFM: $4=vsum(@3$4..@%s$4);EN::$5=vsum(@3$5..@%s$5)\n#+STARTUP: align"
                          nentries
                          nentries)))
        (save-buffer)
        (kill-buffer))
      (message "Hoops!it seem %s is not a csv file!" fname)))

(defun csv2org-fast (csvfname)
  (let ((outfile (format "%s%s-csv2org.org"
                         csv2org-output-dir
                         (tv-cur-date-string))))
    (if (string= (file-name-extension csvfname) "csv")
        (progn
          (when (file-exists-p outfile)
          (delete-file outfile))
          (csv2org-1 csvfname outfile)
          (find-file outfile))
        (error "Error: %s is not a valid csv file"
               (file-name-nondirectory csvfname)))))
    
;; Provide
(provide 'csv2org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; csv2org.el ends here
