;;; org-config-thierry.el --- My config for org
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; 
;; Created: jeu. avril  2 14:10:06 2009 (+0200)
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

;;; Code:

;(require 'org)
;; auto-fill-mode 
;; (set to 78 in files)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Use-enter-to-follow-links 
(setq org-return-follows-link t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/org")) 

;; Todo-rules 
;; (find-node "(org)Fast access to TODO states")
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "INPROGRESS(i)" "DONE(d)" "CANCELED(c)" "DEFERRED(s)")))

(setq org-todo-keyword-faces
      '(("TODO"      .  ((:foreground "red")))
        ("INPROGRESS" . ((:foreground "yellow")))
        ("BUGREPORT" . ((:foreground "VioletRed4" :weight bold)))
        ("FIXED" . ((:foreground "SpringGreen4" :weight bold)))
        ("DEFERRED"  . shadow)
        ("CANCELED"  . ((:foreground "blue" :weight bold)))))
        

(setq org-log-done 'time)
(setq org-use-fast-todo-selection t)
(setq org-reverse-note-order t)

;; Tags-setting 
;; (info "(org)Setting tags")
(setq org-tag-alist '(("entrainement" . ?E)
                      ("climbing" . ?c)
                      ("Equipement" . ?e)
                      ("github" . ?d)
                      ("crypt" . ?C)
                      ("travel" . ?t)))

;; org-capture
(setq org-capture-templates
      '(("W" "BROWSER" entry      (file+headline "~/org/notes.org" "Firefox")      "* BROWSER %?\n %:description\n  (created: %U)\n\n  %c\n\n  %i" :prepend t)
        ("t" "Todo" entry         (file+headline "~/org/agenda.org" "Tasks")       "** TODO %?\n  %i\n  %a"                                        :prepend t)
        ("R" "Report" entry       (file+headline "~/org/agenda.org" "Development") "** REPORT %?\n  %i\n  %a"                                      :prepend t)
        ("n" "Notes" entry        (file+headline "~/org/notes.org" "General")      "* %T %?\n\n  %i\n  %a"                                         :prepend t)
        ("i" "Idea" entry         (file+headline "~/org/notes.org" "New Ideas")    "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("E" "Entrainement" entry  (file+headline "~/org/notes.org" "Entrainement") "* %T %?\n\n  %i\n  %a"                                        :prepend t)
        ("l" "Lisp" entry         (file+headline "~/org/notes.org" "Notes elisp")  "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("p" "Python" entry       (file+headline "~/org/notes.org" "Notes python") "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("e" "Emacs" entry        (file+headline "~/org/notes.org" "Memo Emacs")   "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("s" "Stump" entry        (file+headline "~/org/notes.org" "Memo Stumpwm") "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("L" "Linux" entry        (file+headline "~/org/notes.org" "Memo Linux")   "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("g" "Gentoo" entry       (file+headline "~/org/notes.org" "Notes Gentoo") "* %^{Title}\n  %i\n  %a"                                       :prepend t)
        ("w" "Web" entry          (file+headline "~/org/notes.org" "Web links")    "* %u %c \n\n%i"                                                :prepend t)))

(global-set-key (kbd "C-c r") 'org-capture)

;; org-annotation-helper 
(require 'org-annotation-helper)

;; Diary-integration-in-org 
(setq org-agenda-diary-file "~/org/diary.org") ; org diary file (use i in agenda).
(setq org-agenda-include-diary t) ; show also content of regular diary file.

;; Insinuate-appt 
;(require 'appt)
(org-agenda-to-appt)
;; When use 'r' (rebuild agenda) reload appt
(add-hook 'org-agenda-mode-hook #'(lambda ()
                                    (setq appt-time-msg-list nil)
                                    (org-agenda-to-appt)))

(setq appt-display-format 'window) ;echo, window, nil
(appt-activate 1)
(global-set-key (kbd "<f5> d a") 'appt-add)

;; Subtasks 
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; leave-a-blank-line-when-insert-new-item 
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil)))

;; Insert-cyclic-entries-in-ledger 
(defun tv-org-add-ledger-entry-from-todo (payee amount)
  (with-current-buffer (find-file-noselect (getenv "LEDGER_FILE"))
    (save-excursion
      (ledger-add-entry (concat
                         (format-time-string "%Y/%m/%d")
                         " " payee " " amount)))))

(define-key org-mode-map (kbd "<f11> o") 'helm-org-headlines)
(define-key org-mode-map (kbd "<f11> k") 'helm-org-keywords)
                                             
;; Colorize-Diary's-entries-in-agenda 
(defvar tv-diary-regexp "^ *[Dd]iary")
(defvar tv-diary-done-regexp "^ *[Dd]iary.* *==> *done$") 
(defun tv-org-propertize-diary-entries ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((re-search-forward tv-diary-done-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face 'tv-org-diary-done)))
              ((re-search-forward tv-diary-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face tv-org-diary))))
        (forward-line 1)))))
(add-hook 'org-finalize-agenda-hook 'tv-org-propertize-diary-entries)

(defface tv-org-diary '((t (:foreground "Yellow3" :underline t)))
  "*Face for diary entry in org agenda."
  :group 'org)

(defface tv-org-diary-done '((t (:foreground "darkgrey")))
  "*Face for finished diary entry in org agenda."
  :group 'org)

(defun tv-org-propertize-note-entry ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (re-search-forward "^ +note" nil t)
        (add-text-properties (match-beginning 0) (point-at-eol) '(face '((:foreground "magenta"))))))))
(add-hook 'org-finalize-agenda-hook 'tv-org-propertize-note-entry)


(set-face-attribute 'org-agenda-date-weekend nil :foreground "red")

;; org-crypt 
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-key "59F29997")
(setq org-crypt-disable-auto-save 'encrypt)
(define-key org-mode-map (kbd "C-c e") 'org-encrypt-entry)
(define-key org-mode-map (kbd "C-c d") 'org-decrypt-entry)

(define-key org-agenda-mode-map (kbd "C-c M") 'org-agenda-month-view)

;; fontify source code
(setq org-src-fontify-natively t)

;; Use C-c on a org block like
;; #+BEGIN: image :file "~/Pictures/Claude_et_Nathalie.JPG"
;; #+END

(defun org-dblock-write:image (params)
  (let ((file (plist-get params :file)))
    (clear-image-cache file)
    (insert-image (create-image file) )))

;; Bugfix: Incompatibility with bidi.
(add-hook 'org-mode-hook #'(lambda () (setq bidi-display-reordering nil)))

;;; setting for org-refile full completion
;;
;; (setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-use-outline-path t)
;; (setq org-refile-targets '((nil :maxlevel . 8)))

;; Always show full path of files 
(setq org-link-file-path-type 'absolute)

;; Org babel
(require 'ob-sh)
(require 'ob-emacs-lisp)


(provide 'org-config-thierry)

;;; org-config-thierry.el ends here
