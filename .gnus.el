;;; .gnus.el -- Laptop


;;; Code:

;;; Search engine for imap and gmane (hit `G G' in group buffer)
;;
(require 'nnir)

;; Don't read/write to the .newrc file, go straight to the *.eld.
(setq gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil)

;;; Gnus methods
;;
;;
;; Default method
(setq gnus-select-method '(nntp "news.gmane.org"
                           (nnir-search-engine gmane)))

;;; .gnus.el ends here

