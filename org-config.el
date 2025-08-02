;;; org-config.el --- My config for org -*- lexical-binding: t -*-
;; 

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
(setq org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-capture-use-agenda-date t)
(setq org-capture-templates
      '(("t" "Todo"   entry (file+headline  "~/org/agenda.org" "Tasks")   "** TODO %?\n    %i\n  %a" :prepend t)
        ("n" "Notes"  entry (file+headline  "~/org/notes.org"  "General") "** %^{Title}\n  %i\n  %a" :prepend t)
        ("h" "Helm"   entry (file+headline  "~/org/notes.org"  "Helm")    "** %^{Title}\n  %i\n  %a" :prepend t)
        ("e" "Emacs"  entry (file+headline  "~/org/notes.org"  "Emacs")   "** %^{Title}\n  %i\n  %a" :prepend t)
        ("l" "Lisp"   entry (file+headline  "~/org/notes.org"  "Elisp")   "** %^{Title}\n  %i\n  %a" :prepend t)
        ("p" "Python" entry (file+headline  "~/org/notes.org"  "Python")  "** %^{Title}\n  %i\n  %a" :prepend t)
        ("b" "Bash"   entry (file+headline  "~/org/notes.org " "Bash")    "** %^{Title}\n  %i\n  %a" :prepend t)
        ("L" "Linux"  entry (file+headline  "~/org/notes.org"  "Linux")   "** %^{Title}\n  %i\n  %a" :prepend t)))

;; Org-protocol
;; See https://orgmode.org/worg/org-contrib/org-protocol.html for
;; setting it up.
;; (require 'org-protocol)

;; org-annotation-helper 
;; (require 'org-annotation-helper)

;; Diary-integration-in-org 
(setq org-agenda-include-diary t) ; show also content of regular diary file.
;; (setq org-agenda-diary-file "~/org/diary.org")

;; Subtasks 
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; leave-a-blank-line-when-insert-new-item 
(setq org-blank-before-new-entry '((heading . t)
                                   (plain-list-item . nil)))

(defun tv:insert-org-src-keyword ()
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        (forward-line -1)
        (insert "#+begin_src ")
        (save-excursion
          ;; (insert "\n")
          (goto-char end)
          (end-of-line)
          (insert "\n#+end_src")))
    (insert "#+begin_src ")
    (save-excursion (insert "\n#+end_src"))))

(defun tv:org-headings (arg)
  (interactive "P")
  (if (eq major-mode 'org-mode)
      (helm-org-in-buffer-headings arg)
    (helm-org-agenda-files-headings arg)))

(add-hook 'org-mode-hook 
	  (lambda ()
	    (define-key org-mode-map (kbd "<f11> o") 'tv:org-headings)
	    (define-key org-mode-map (kbd "<f11> k") 'tv:insert-org-src-keyword)))
                                             
;; Colorize-Diary's-entries-in-agenda 
(defvar tv:diary-regexp "^ *[Dd]iary")
(defvar tv:diary-done-regexp "^ *[Dd]iary.* *==> *done$") 
(defun tv:org-propertize-diary-entries ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((re-search-forward tv:diary-done-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face 'tv:org-diary-done)))
              ((re-search-forward tv:diary-regexp (point-at-eol) t)
               (add-text-properties
                (point-at-bol) (point-at-eol) '(face tv:org-diary))))
        (forward-line 1)))))
(add-hook 'org-finalize-agenda-hook 'tv:org-propertize-diary-entries)

(defface tv:org-diary '((t (:foreground "Yellow3" :underline t)))
  "*Face for diary entry in org agenda."
  :group 'org)

(defface tv:org-diary-done '((t (:foreground "darkgrey")))
  "*Face for finished diary entry in org agenda."
  :group 'org)

(defun tv:org-propertize-note-entry ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "^ +note" nil t)
        (add-text-properties (match-beginning 0) (point-at-eol)
                             '(face '((:foreground "magenta"))))))))
(add-hook 'org-finalize-agenda-hook 'tv:org-propertize-note-entry)


(add-hook 'org-agenda-mode-hook
	  (lambda () (set-face-attribute 'org-agenda-date-weekend nil :foreground "red")))

;; org-crypt 
(org-crypt-use-before-save-magic)
(setq org-crypt-key "08FDB07A7433A7F2")
(setq org-crypt-disable-auto-save t)
(define-key org-mode-map (kbd "C-c e") 'org-encrypt-entry)
(define-key org-mode-map (kbd "C-c d") 'org-decrypt-entry)

;; fontify source code
(setq org-src-fontify-natively t)

;; Always show full path of files 
(setq org-link-file-path-type 'absolute)

;; Invisible edits
(setq org-catch-invisible-edits 'smart)

(setq org-show-context-detail '((default . local)))

;; Org babel
(with-eval-after-load 'ob-emacs-lisp
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (emacs-lisp . t))))

(define-key org-mode-map (kbd "<M-up>")     'tv:scroll-up)
(define-key org-mode-map (kbd "<M-down>")   'tv:scroll-down)
(define-key org-mode-map (kbd "<C-M-up>")   'tv:scroll-other-up)
(define-key org-mode-map (kbd "<C-M-down>") 'tv:scroll-other-down)
(define-key org-mode-map (kbd "C-d")        'tv:delete-char)
(define-key org-mode-map (kbd "<C-return>") nil)
(define-key org-mode-map (kbd "C-,")        nil)
(define-key org-mode-map (kbd "<C-return>") nil)
(define-key org-mode-map (kbd "<M-right>")  nil)
(define-key org-mode-map (kbd "<M-left>")   nil)
(define-key org-mode-map (kbd "C-c C-i") 'org-table-insert-row)

;; Disable org-persist
(when (> emacs-major-version 28)
  (with-eval-after-load 'org-persist
    (setq org-element-cache-persistent nil)
    ;; Thanks Colin!
    (defun zz/advice--org-persist (old-fn &rest args)
      (let (user-init-file)
        (apply old-fn args)))
    (advice-add 'org-persist-clear-storage-maybe :around #'zz/advice--org-persist)
    (when org-persist--refresh-gc-lock-timer
      (cancel-timer org-persist--refresh-gc-lock-timer))))

;; Imenu
(setq org-imenu-depth 4)


(provide 'org-config)

;;; org-config.el ends here
