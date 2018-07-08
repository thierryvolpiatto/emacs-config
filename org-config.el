;;; org-config.el --- My config for org
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

(setq org-directory "~/org")

;; auto-fill-mode 
;; (set to 78 in files)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Use-enter-to-follow-links 
(setq org-return-follows-link t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
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

(setq org-tag-alist '(("entrainement")
                      ("climbing")
                      ("equipement")
                      ("running")
                      ("bike")
                      ("vtt")
                      ("montagne")
                      ("cascade")
                      ("github")
                      ("helm")
                      ("async")
                      ("crypt")
                      ("home")
                      ("travel")))

;; org-capture
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-capture-templates
      '(("t" "Todo" entry         (file+headline  "~/org/agenda.org" "Tasks")        "** TODO %?\n    %i\n  %a" :prepend t)
        ("n" "Notes" entry        (file+headline  "~/org/notes.org"  "General")      "** %T %?\n\n    %i\n"     :prepend t)
        ("E" "Entrainement" entry (file+headline  "~/org/notes.org"  "Entrainement") "** %T %?\n\n    %i\n"     :prepend t)
        ("H" "Helm" entry         (file+headline  "~/org/notes.org"  "Helm")         "** %^{Title}\n  %i\n  %a" :prepend t)
        ("l" "Lisp" entry         (file+headline  "~/org/notes.org"  "Elisp")        "** %^{Title}\n  %i\n  %a" :prepend t)
        ("p" "Python" entry       (file+headline  "~/org/notes.org"  "Python")       "** %^{Title}\n  %i\n  %a" :prepend t)
        ("b" "Bash" entry         (file+headline  "~/org/notes.org " "Bash")         "** %^{Title}\n  %i\n  %a" :prepend t)
        ("L" "Linux" entry        (file+headline  "~/org/notes.org"  "Linux")        "** %^{Title}\n  %i\n  %a" :prepend t)))

;; org-annotation-helper 
;; (use-package org-annotation-helper)

;; Diary-integration-in-org 
(setq org-agenda-include-diary t) ; show also content of regular diary file.

;; Subtasks 
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; leave-a-blank-line-when-insert-new-item 
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil)))

(defun tv/insert-org-src-keyword (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (insert "#+begin_src\n")
    (goto-char end)
    (forward-line 1)
    (insert "\n#+end_src")))

(add-hook 'org-mode-hook 
	  (lambda ()
	    (define-key org-mode-map (kbd "<f11> o") 'helm-org-in-buffer-headings)
	    (define-key org-mode-map (kbd "<f11> k") 'tv/insert-org-src-keyword)))
                                             
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
      (while (re-search-forward "^ +note" nil t)
        (add-text-properties (match-beginning 0) (point-at-eol)
                             '(face '((:foreground "magenta"))))))))
(add-hook 'org-finalize-agenda-hook 'tv-org-propertize-note-entry)


(add-hook 'org-agenda-mode-hook
	  (lambda () (set-face-attribute 'org-agenda-date-weekend nil :foreground "red")))

;; org-crypt 
(use-package org-crypt
    :config
  (progn
    (org-crypt-use-before-save-magic)
    (setq org-crypt-key "59F29997")
    (setq org-crypt-disable-auto-save t) ;'encrypt)
    (define-key org-mode-map (kbd "C-c e") 'org-encrypt-entry)
    (define-key org-mode-map (kbd "C-c d") 'org-decrypt-entry)))

;; fontify source code
(setq org-src-fontify-natively t)

;; Always show full path of files 
(setq org-link-file-path-type 'absolute)

;; Invisible edits
(setq org-catch-invisible-edits 'smart)

;; Org babel
(use-package ob-emacs-lisp
    :config
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (emacs-lisp . t))))

(define-key org-mode-map (kbd "<M-up>") 'tv-scroll-up)
(define-key org-mode-map (kbd "<M-down>") 'tv-scroll-down)
(define-key org-mode-map (kbd "<C-M-up>") 'tv-scroll-other-up)
(define-key org-mode-map (kbd "<C-M-down>") 'tv-scroll-other-down)
(define-key org-mode-map (kbd "C-d") 'tv-delete-char)

(provide 'org-config)

;;; org-config.el ends here
