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

;; Use-my-func-with-ido-and-not-org-iswitchb 
(defvar my-org-files '("agenda.org" "notes.org" "journal.org" "pwdthierry.org"))
(defun tv-find-org-files (fname)
  (interactive (list (anything-comp-read "OrgFiles: "
                                         (if (bufferp (get-buffer "*Org Agenda*"))
                                             (cons "*Org Agenda*" my-org-files)
                                             my-org-files))))
  (when fname
    (if (equal fname "*Org Agenda*")
        (display-buffer fname)
        (find-file (expand-file-name fname org-directory)))))

(global-set-key (kbd "C-c b") 'tv-find-org-files)

;; Todo-rules 
;; (find-node "(org)Fast access to TODO states")
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)" "INPROGRESS(i)" "DEFERRED(s)")))
        ;; (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        ;; (sequence "INPROGRESS(i)" "DEFERRED(s)")))

(setq org-todo-keyword-faces
      '(("TODO"      .  traverse-match-face)
        ("INPROGRESS" . traverse-regex-face)
        ("BUG" . (:foreground "VioletRed4" :weight bold))
        ("FIXED" . (:foreground "SpringGreen4" :weight bold))
        ("DEFERRED"  . shadow)
        ("CANCELED"  . (:foreground "blue" :weight bold))))
        

(setq org-log-done 'time)
(setq org-use-fast-todo-selection t)
(setq org-reverse-note-order t)

;; Tags-setting 
;; (find-node "(org)Setting tags")
(setq org-tag-alist '(("@home" . ?h)
                      ("running" . ?r)
                      ("velo" . ?v)
                      ("@climbing" . ?c)
                      ("buy" . ?b)
                      ("laptop" . ?l)
                      ("pc" . ?p)
                      ("dev" . ?d)
                      ("crypt" . ?C)
                      ("mail" . ?M)
                      ("phone" . ?P)
                      ("bank" . ?B)
                      ("bug" . ?E)
                      ("travel" . ?t)))

;; Insinuate-remember 
(org-remember-insinuate)
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
(setq org-remember-store-without-prompt t)
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Org-remember-templates 
;; (find-node "(org)Remember templates")
(setq org-remember-templates
      '(("BROWSER" ?W "* BROWSER %?\n %:description\n  (created: %U)\n\n  %c\n\n  %i" "~/org/notes.org" "Firefox")
        ("Todo" ?t "** TODO %?\n  %i\n  %a" "~/org/agenda.org" "Tasks")
        ("Report" ?R "** REPORT %?\n  %i\n  %a" "~/org/agenda.org" "Development")
        ("Notes" ?n "* %U %?\n\n  %i\n  %a" "~/org/notes.org" "General")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "New Ideas")
        ("Lisp" ?l "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes elisp")
        ("Python" ?p "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes python")
        ("Emacs" ?e "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Emacs")
        ;("BmkAnnotations" ?b "* %^{Title}\n  %i\n  %a" "~/org/bmkannotations.org" "Bookmark Annotations")
        ("Stump" ?s "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Stumpwm")
        ("Linux" ?L "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Memo Linux")
        ("Gentoo" ?g "* %^{Title}\n  %i\n  %a" "~/org/notes.org" "Notes Gentoo")
        ("Web" ?w "* %u %c \n\n%i" "~/org/notes.org" "Web links")))

;; Org-capture 
;; imported from remember templates
(when (require 'org-capture nil t)
  (global-set-key (kbd "C-c r") 'org-capture)
  (setq org-capture-templates
        '(("W" "BROWSER" entry (file+headline "~/org/notes.org" "Firefox") "* BROWSER %?
 %:description
  (created: %U)

  %c

  %i" :prepend t) ("t" "Todo" entry (file+headline "~/org/agenda.org" "Tasks") "** TODO %?
  %i
  %a" :prepend t) ("R" "Report" entry (file+headline "~/org/agenda.org" "Development") "** REPORT %?
  %i
  %a" :prepend t) ("n" "Notes" entry (file+headline "~/org/notes.org" "General") "* %U %?

  %i
  %a" :prepend t) ("i" "Idea" entry (file+headline "~/org/notes.org" "New Ideas") "* %^{Title}
  %i
  %a" :prepend t) ("l" "Lisp" entry (file+headline "~/org/notes.org" "Notes elisp") "* %^{Title}
  %i
  %a" :prepend t) ("p" "Python" entry (file+headline "~/org/notes.org" "Notes python") "* %^{Title}
  %i
  %a" :prepend t) ("e" "Emacs" entry (file+headline "~/org/notes.org" "Memo Emacs") "* %^{Title}
  %i
  %a" :prepend t) ("s" "Stump" entry (file+headline "~/org/notes.org" "Memo Stumpwm") "* %^{Title}
  %i
  %a" :prepend t) ("L" "Linux" entry (file+headline "~/org/notes.org" "Memo Linux") "* %^{Title}
  %i
  %a" :prepend t) ("g" "Gentoo" entry (file+headline "~/org/notes.org" "Notes Gentoo") "* %^{Title}
  %i
  %a" :prepend t) ("w" "Web" entry (file+headline "~/org/notes.org" "Web links") "* %u %c 

%i" :prepend t))))

;; org-annotation-helper 
(require 'org-annotation-helper)

;; Diary-integration-in-org 
(setq org-agenda-diary-file "~/org/diary.org") ; org diary file (use i in agenda).
(setq org-agenda-include-diary t) ; show also content of regular diary file.

;; Insinuate-appt 
(require 'appt)
(setq appt-time-msg-list nil)
(org-agenda-to-appt)
;; When use 'r' (rebuild agenda) reload appt
(add-hook 'org-agenda-mode-hook #'(lambda ()
                                    (setq appt-time-msg-list nil)
                                    (org-agenda-to-appt)))
(setq appt-audible t)
(setq appt-display-format nil) ;echo, window, nil
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
  (interactive)
  (save-excursion
    (find-file "~/finance/financehg/ledger.dat")
    (ledger-add-entry (concat
                       (replace-regexp-in-string "\\." "/" (tv-cur-date-string))
                       " "
                       payee
                       " "
                       amount))))

;; Use-anything-in-org 
;; (defun anything-org-headlines-only ()
;;   (interactive)
;;   (anything-other-buffer 'anything-c-source-org-headline "*org headlines*"))

(define-key org-mode-map (kbd "<f11> o") 'anything-org-headlines)
(define-key org-mode-map (kbd "<f11> k") 'anything-org-keywords)
                                             
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
(define-key org-mode-map (kbd "C-c e") 'org-encrypt-entry)
(define-key org-mode-map (kbd "C-c d") 'org-decrypt-entry)

(define-key org-agenda-mode-map (kbd "C-c M") 'org-agenda-month-view)

;; org-w3m 
;; Use C-c C-x C/M-w in w3m
(require 'org-w3m)

;; fontify source code
(setq org-src-fontify-natively t)

;; Provide 
(provide 'org-config-thierry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-config-thierry.el ends here
