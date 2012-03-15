;;; .emacs-config-w3m.el -- config w3m for thievol


;; Code:
(require 'w3m)
;; test 
;; Kill-other-windows-when-using-w3m- 
;(add-hook 'w3m-mode-hook 'delete-other-windows)

;; Elscreen tabs
;; Use the w3m tabs instead of elscreen one's.
(when (locate-library "elscreen.el")
  (defun tv-elscreen-remove-tab ()
    "remove the tab on the top of screen."
    (interactive)
    (setq elscreen-display-tab nil)
    (elscreen-notify-screen-modification 'force))

  (defun tv-elscreen-display-tab ()
    "Show the tab on the top of screen."
    (interactive)
    (setq elscreen-display-tab t)
    (elscreen-notify-screen-modification 'force))

  (add-hook 'w3m-mode-hook #'(lambda () (call-interactively 'tv-elscreen-remove-tab)))
  (defadvice w3m-quit (after restore-el-tabs () activate)
    (call-interactively 'tv-elscreen-display-tab))
  (defadvice w3m-close-window (after restore-el-tabs () activate)
    (call-interactively 'tv-elscreen-display-tab))
  (defadvice w3m (after close-el-tabs () activate)
    (call-interactively 'tv-elscreen-remove-tab)))

;; default-save-directory 
(setq w3m-default-save-directory "~/download/")

;; w3m-namazu 
;(setq w3m-namazu-default-index "~/namazu-index/")

;; localisation-w3m 
(setq w3m-coding-system 'utf-8
      w3m-language "french"
      w3m-output-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)
 
;; w3m-par-defaut 

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;; w3m-region 
(autoload 'w3m-region "w3m"
  "Render region in current buffer and replace with result." t)

(autoload 'w3m-toggle-inline-image "w3m"
  "Toggle the visibility of an image under point." t)


;; w3m-homepage 
(setq w3m-home-page "http://www.google.fr")

;; webjump-sites 
(setq webjump-sites
      '(("pythonlib" .  "http://docs.python.org/lib/genindex.html")
        ("pythondoc" . "http://docs.python.org/index.html")
        ("tkinter-tuto" . "http://infohost.nmt.edu/tcc/help/pubs/tkinter/")
        ("pygtk-tuto" . "http://www.pygtk.org/pygtk2tutorial/index.html")
        ("gentooforumfr" .  "http://www.gentoo.fr/forum/index.php")
        ("gentoowikifr" . "http://fr.gentoo-wiki.com/Accueil")
        ("gentoosearch" . "http://cosearch.googlepages.com/GentooSearchFrancais.html")
        ("delicious" . "http://del.icio.us/thiedlecques")
        ("gentooman" . "http://www.gentoo.org/doc/fr/handbook/handbook-amd64.xml?full=1#book_part1")
        ("emacsmanfr" . "http://www.linux-france.org/article/appli/emacs/manuel/html/index.html")
        ("emacswiki" . "http://www.emacswiki.org/cgi-bin/wiki/SiteMap")
        ("wikipedia" . "http://fr.wikipedia.org/wiki/Accueil")
        ("planner" . "http://wjsullivan.net/static/doc/planner/html_node/index.html")
        ("elispcode" . "http://www.emacswiki.org/cgi-bin/wiki/Cat%c3%a9gorieCode")
        ("elisp-reference-manual" . "http://www.gnu.org/software/emacs/elisp/html_node/index.html")
        ))

;; search-google 
;;;###autoload
(defun search-word (word)
  "Initial input use by defaut thing-at-point.
With prefix arg open another window to diplay result"
  (interactive (list (read-string "GoogleSearch: "
                                  (thing-at-point 'sexp)
                                  'minibuffer-history)))
  (w3m-browse-url (concat "http://www.google.com/search?hl=fr&ie=ISO-8859-1&q=" word)
                  (if current-prefix-arg t)))



;; search-gmane 
(defun tv-search-gmane (query &optional group author)
  (interactive (list
                (read-from-minibuffer "Query: ")
                (helm-comp-read "Group: "
                                    '("gmane.emacs.gnus.general"
                                      "gmane.emacs.gnus.cvs"
                                      "gmane.emacs.gnus.user"
                                      "gmane.emacs.help"
                                      "gmane.emacs.devel"
                                      "gmane.emacs.bugs"
                                      "gmane.lisp.clisp.devel"
                                      "gmane.lisp.clisp.general"
                                      "gmane.lisp.emacs-cl"
                                      "gmane.linux.gentoo.devel"
                                      "gmane.linux.gentoo.user"
                                      "gmane.linux.gentoo.cvs"
                                      "gmane.emacs.planner.general"
                                      "gmane.emacs.muse.general"
                                      "gmane.emacs.dvc.devel"
                                      "gmane.emacs.python-mode")
                                    :must-match t)
                (read-from-minibuffer "Author(Optional): ")))
  (browse-url (concat "http://search.gmane.org/?query="
                          query
                          "&author="
                          author
                          "&group="
                          group
                          "&sort=relevance&DEFAULTOP=and&TOPDOC=80&xP=Zemac&xFILTERS=A"
                          author
                          "---A")))

;; enable-cookies-in-w3m 
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)

;; bookmark-in-delicious 

;; With prefix arg, add also to w3m-bookmarks
(define-key w3m-mode-map "\C-x\C-a" 'w3m-add-delicious-bookmark)

;; active-submit-form-when-no-button 
(define-key w3m-mode-map "\C-c\C-c" 
  '(lambda ()
     (interactive)
     (if (member 'w3m-href-anchor (text-properties-at (point)))
         (w3m-view-this-url)
       (w3m-submit-form))))

;; Find-jargon-definition-at-point 
(defun tv-jargon-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (w3m-browse-url (concat "http://www.babylon.com/definition/" word "/English"))))

;; w3m-antenna 
(autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)

;; netscape-vs-firefox 
(setq browse-url-netscape-program "firefox")

;; Scroll-only-one-line-with-arrow-keys 
(defun w3m-scroll-up-one ()
  (interactive)
  (scroll-up 1))

(defun w3m-scroll-down-one ()
  (interactive)
  (scroll-down 1))

(define-key w3m-mode-map (kbd "<M-down>") 'w3m-scroll-up-one)
(define-key w3m-mode-map (kbd "<M-up>") 'w3m-scroll-down-one)

;; Change-tabs-easily 
(define-key w3m-mode-map (kbd "M-<right>") 'w3m-next-buffer)

(define-key w3m-mode-map (kbd "M-<left>") 'w3m-previous-buffer)

;; emacswiki-shortcut 
(add-to-list 'w3m-uri-replace-alist '("\\`ewi:" w3m-search-uri-replace "emacswiki"))

;; Remove-trailing-white-space-in-w3m-buffers 
(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))


;; Provide
(provide 'config-w3m)

;;; .emacs-config-w3m.el ends here


