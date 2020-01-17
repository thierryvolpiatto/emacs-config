;;; init.el --- emacs configuration. -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)

(setq inhibit-startup-echo-area-message "thierry")

;;; Melpa/Elpa
;;
;; Emacs-26
(unless (boundp 'package-quickstart)
  (load-file (expand-file-name "early-init.el" user-emacs-directory))
  ;; Initialize packages after setting package-archives
  ;; to feed package-archive-contents with all archives.
  (package-initialize))

;; Emacs-27
(when (boundp 'package-quickstart)
  (setq package-quickstart t))

(defun tv/fix-selected-packages ()
  (interactive)
  (package-initialize)
  (package--save-selected-packages (package--find-non-dependencies)))


;;; load-path
;;
(dolist (i '("~/elisp/"
             "~/elisp/magit/lisp"
             "~/elisp/with-editor"
             "~/elisp/Emacs-wgrep"
	     "~/elisp/autoconf-mode"
	     "~/elisp/desktop-file-utils"
	     "~/elisp/emacs-wget"
	     "~/elisp/tex-utils"
	     "~/elisp/ledger-mode"
             "~/elisp/helm-extensions"
             "~/elisp/google-maps.el"
             "~/elisp/emacs-w3m"
             "~/.emacs.d/themes/"
	     "~/.emacs.d/emacs-config/"
	     ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (file-name-as-directory i) t))

;;; Emacs customize have it's own file
;;
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;;; VC
;;
;; Possible values for vc backends: (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
(setq vc-handled-backends '(RCS))
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
                   vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; Use-package.
;;
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)


;;; Global settings
;;
;;  Global bindings
(global-set-key (kbd "C-z")   nil) ; Disable `suspend-frame'.
(global-set-key (kbd "<f11>") nil)
(global-set-key (kbd "C-c R") (lambda () (interactive) (revert-buffer t t)))
(global-set-key [remap save-buffers-kill-terminal] 'tv/stop-emacs) ; C-x C-c

;; y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop/restart emacs
(defun tv/stop-emacs-1 ()
  (if (daemonp)
      (save-buffers-kill-emacs)
    (save-buffers-kill-terminal)))

(defun tv/stop-emacs (arg)
  "Close emacs, with a prefix arg restart it.
Restart works only on graphic display."
  (interactive "P")
  (let ((confirm-kill-emacs (unless (and arg (display-graphic-p))
                              'y-or-n-p))
        (kill-emacs-query-functions
         (if (and arg (display-graphic-p))
             (append (list
                      (lambda ()
                        (when (y-or-n-p (format "Really restart %s? "
                                                (capitalize (invocation-name))))
                          (add-hook 'kill-emacs-hook
                                    (lambda ()
                                      (call-process-shell-command
                                       (format "(%s &)"
                                               ;; eselect-emacs.sh
                                               ;; should have kept
                                               ;; only one of
                                               ;; emacs/remacs.
                                               (or (executable-find "emacs")
                                                   (executable-find "remacs")))))
                                    t))))
                     kill-emacs-query-functions)
           kill-emacs-query-functions)))
    (tv/stop-emacs-1)))

;; Add-newline-at-end-of-files
(setq require-final-newline t)

;; Limite-max-lisp
(setq max-lisp-eval-depth 40000
      max-specpdl-size    100000)

;; Increase GC
(setq gc-cons-threshold 20000000)

;; Disable bidi
(setq-default bidi-display-reordering nil)

;; column-number in mode-line.
(column-number-mode 1)

;; Environment variables
;;
;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

;; For eshell env settings.
(setenv "STARDICT_DATA_DIR" "~/.stardict/dic")

;; Coding system.
(prefer-coding-system 'utf-8)

;; Themes
(defvar tv/theme-directory "~/.emacs.d/themes/")
(unless (< emacs-major-version 24)
  (setq custom-theme-directory tv/theme-directory))

;; Load my favourite theme.
(add-hook 'emacs-startup-hook (lambda () (load-theme 'naquadah)))

;; Pas-de-dialog-gtk
(setq use-file-dialog nil)

;;; emacs-backup-config
;;
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Trash
;; `move-file-to-trash' doesn't use `substitute-in-file-name' to extract
;; value of XDG_DATA_HOME, so ensure this is unset in emacs.
(setenv "XDG_DATA_HOME")
;; (setq delete-by-moving-to-trash t)

;; Start-emacs-server
;;
(require 'server)
(add-hook 'after-init-hook (lambda ()
                             (unless (or (daemonp) (server-running-p))
                               (setq server-name (format "emacs-%s" emacs-version))
                               (server-start)
                               (setq server-raise-frame t))))

;; Copy/paste
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil
      select-enable-clipboard t
      select-enable-primary t)

;; Enable-commands-disabled-by-default
(put 'narrow-to-region 'disabled nil)          ; C-x n n
(put 'narrow-to-page 'disabled nil)            ; C-x n p
(put 'scroll-left 'disabled nil)               ; C-x > or <
(put 'downcase-region 'disabled nil)           ; C-x C-l
(put 'upcase-region 'disabled nil)             ; C-x C-u
(put 'set-goal-column 'disabled nil)           ; C-x C-n ==> disable with C-u
(put 'dired-find-alternate-file 'disabled nil) ; a in dired

;; setup-minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; History variables
(setq history-delete-duplicates t)
(setq history-length            100) ; default is 30.

(setq line-move-visual                 nil
      ;; completion-cycle-threshold       t ; always cycle, no completion buffers.
      report-emacs-bug-no-explanations t
      comint-prompt-read-only          t
      uniquify-buffer-name-style       nil
      register-preview-delay           nil
      inhibit-startup-message          t
      message-log-max                  1000
      kill-ring-max                    80
      mark-ring-max                    60
      global-mark-ring-max             200)

;; Disable indent-tabs-mode
(setq-default indent-tabs-mode nil)


;;; Compatibility
;;
;;
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in cl-case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

;; Fix compatibility with emacs 24.3.
;; Avoid rebuilding all the autoloads just for this when switching to 24.3.
(unless (fboundp 'function-put)
  (defalias 'function-put
    ;; We don't want people to just use `put' because we can't conveniently
    ;; hook into `put' to remap old properties to new ones.  But for now, there's
    ;; no such remapping, so we just call `put'.
    (lambda (f prop value) (put f prop value))
    "Set function F's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, F can only be a symbol, not a lambda expression."))

;; Fix slow helm frame popup in emacs-26 helm issue #1976
(when (= emacs-major-version 26)
  (setq x-wait-for-event-timeout nil))

;; Don't beep even with visible-bell (debian)
(setq ring-bell-function 'ignore)


;;; Use package declarations

;;; Info
;;
(use-package info
  :config
  (progn
    ;; Additional info directories
    (add-to-list 'Info-directory-list "/usr/local/share/info")
    (add-to-list 'Info-directory-list "/usr/share/info")
    (add-to-list 'Info-directory-list "~/elisp/info")
    (add-to-list 'Info-directory-list "~/elisp/emacs-w3m/doc")
    (add-to-list 'Info-directory-list "~/elisp/magit/Documentation")
    ;; Fancy faces in info.
    (defface tv/info-ref-item
      '((((background dark)) :background "DimGray" :foreground "Gold")
        (((background light)) :background "firebrick" :foreground "LightGray"))
      "Face for item stating with -- in info." :group 'Info :group 'faces)

    (defvar tv/info-title-face 'tv/info-ref-item)
    (defvar tv/info-underline 'underline)
    (defvar info-unicode-quote-start (string 8216))
    (defvar info-unicode-quote-end (string 8217))
    (defvar info-unicode-quoted-regexp (format "[%s]\\([^%s%s]+\\)[%s]"
                                               info-unicode-quote-start
                                               info-unicode-quote-start
                                               info-unicode-quote-end
                                               info-unicode-quote-end
                                               ))
    (defun tv/font-lock-doc-rules ()
      (font-lock-add-keywords
       nil `(("[^][\\s`]\\([^[](`'+\\)`']?[^][\\s']?" 1 font-lock-type-face)
             (,info-unicode-quoted-regexp 1 font-lock-type-face)
             ("^ --.*$" . tv/info-title-face)
             ("[_]\\([^_]+\\)[_]" 1 tv/info-underline)
             ("[\"]\\([^\"]*\\)[\"]" . font-lock-string-face)
             ("\\*Warning:\\*" . font-lock-warning-face)
             ("^ *\\([*•]\\) " 1 font-lock-variable-name-face)
             ("^[[:upper:],]\\{2,\\}$" . font-lock-comment-face)
             ("^[[:upper]][a-z- ]*:" . font-lock-variable-name-face)
             )))

    (add-hook 'Info-mode-hook 'tv/font-lock-doc-rules)
    (define-key Info-mode-map [remap Info-index] 'helm-info-at-point)))

;;; Helm
;;
(require 'init-helm)

;;; Term - ansi-term
;;
(use-package term
  :config
  (progn
    ;; Kill buffer after C-d in ansi-term.
    (defadvice term-sentinel (after kill-buffer activate)
      (kill-buffer))
    (defun tv/term ()
      (interactive)
      (ansi-term "/bin/bash"))
    (defadvice term-command-hook (before decode-string)
      (setq string (decode-coding-string string locale-coding-system)))
    (when (version< emacs-version "24.3.50.1") (ad-activate 'term-command-hook))
    ;; Retrieve emacs-25 behavior with 26+.
    (when (and (boundp 'term-char-mode-buffer-read-only)
               (boundp 'term-char-mode-point-at-process-mark))
      (setq term-char-mode-point-at-process-mark nil
            term-char-mode-buffer-read-only nil)))
  :bind ("<f11> t" . tv/term))

;; Browse url
;;
;;
(use-package browse-url
  :config
  (progn
    (setq browse-url-firefox-program "firefox")
    (setq browse-url-browser-function 'helm-browse-url-firefox)))

;;; Ediff
;;
(use-package ediff
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally
          ediff-show-ancestor         nil)))

;;; Help
;;
(use-package help
  :config
  (progn
    ;; Fix curly quotes in emacs-25
    (and (boundp 'text-quoting-style)
         (setq text-quoting-style 'grave))
    ;; Since they use pp-buffer, it is not possible to override pp so
    ;; we need to duplicate the whole function with modifications and
    ;; override the original by advice.
    ;; Test: (describe-variable 'load-history)
    (defun tv/describe-variable (variable &optional buffer frame)
      (interactive
       (let ((v (variable-at-point))
	     (enable-recursive-minibuffers t)
             (orig-buffer (current-buffer))
	     val)
         (setq val (completing-read
                    (if (symbolp v)
                        (format
                         "Describe variable (default %s): " v)
                      "Describe variable: ")
                    #'help--symbol-completion-table
                    (lambda (vv)
                      ;; In case the variable only exists in the buffer
                      ;; the command we switch back to that buffer before
                      ;; we examine the variable.
                      (with-current-buffer orig-buffer
                        (or (get vv 'variable-documentation)
                            (and (boundp vv) (not (keywordp vv))))))
                    t nil nil
                    (if (symbolp v) (symbol-name v))))
         (list (if (equal val "")
	           v (intern val)))))
      (let (file-name)
        (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
        (unless (frame-live-p frame) (setq frame (selected-frame)))
        (if (not (symbolp variable))
	    (message "You did not specify a variable")
          (save-excursion
	    (let ((valvoid (not (with-current-buffer buffer (boundp variable))))
	          (permanent-local (get variable 'permanent-local))
	          val val-start-pos locus)
              ;; Extract the value before setting up the output buffer,
              ;; in case `buffer' *is* the output buffer.
	      (unless valvoid
	        (with-selected-frame frame
	          (with-current-buffer buffer
		    (setq val (symbol-value variable)
		          locus (variable-binding-locus variable)))))
	      (help-setup-xref (list #'describe-variable variable buffer)
			       (called-interactively-p 'interactive))
	      (with-help-window (help-buffer)
	        (with-current-buffer buffer
	          (prin1 variable)
	          (setq file-name (find-lisp-object-file-name variable 'defvar))

	          (if file-name
		      (progn
		        (princ (format-message
                                " is a variable defined in `%s'.\n"
                                (if (eq file-name 'C-source)
                                    "C source code"
                                  (file-name-nondirectory file-name))))
		        (with-current-buffer standard-output
		          (save-excursion
			    (re-search-backward (substitute-command-keys
                                                 "`\\([^`']+\\)'")
                                                nil t)
			    (help-xref-button 1 'help-variable-def
					      variable file-name)))
		        (if valvoid
			    (princ "It is void as a variable.")
		          (princ "Its ")))
		    (if valvoid
		        (princ " is void as a variable.")
		      (princ (substitute-command-keys "'s ")))))
	        (unless valvoid
	          (with-current-buffer standard-output
		    (setq val-start-pos (point))
		    (princ "value is")
		    (let ((line-beg (line-beginning-position))
		          (print-rep
		           (let ((rep
			          (let ((print-quoted t)
                                        (print-circle t))
				    (cl-prin1-to-string val))))
			     (if (and (symbolp val) (not (booleanp val)))
			         (format-message "`%s'" rep)
			       rep))))
		      (if (< (+ (length print-rep) (point) (- line-beg)) 68)
		          (insert " " print-rep)
		        (terpri)
                        ;; >>>>>>>>>>>adviced block
                        (cl-letf (((symbol-function 'pp)
                                   (lambda (object &optional stream)
                                     (let ((fn (lambda (ob &optional stream)
                                                 (princ (pp-to-string ob)
                                                        (or stream standard-output))
                                                 (terpri)))
                                           (print-circle t)
                                           prefix suffix map-fn looping)
                                       (cond ((ring-p object)
                                              (setq looping nil))
                                             ((consp object)
                                              (setq prefix "\n("
                                                    suffix ")"
                                                    map-fn 'mapc
                                                    looping t))
                                             ((vectorp object)
                                              (setq prefix "\n["
                                                    suffix "]"
                                                    map-fn 'mapc
                                                    looping t))
                                             ((hash-table-p object)
                                              (setq prefix (format "#s(hash-table size %s test %s rehash-size %s rehash-threshold %s data\n"
                                                                   (hash-table-size object)
                                                                   (hash-table-test object)
                                                                   (hash-table-rehash-size object)
                                                                   (hash-table-rehash-threshold object))
                                                    suffix ")"
                                                    map-fn 'maphash
                                                    fn `(lambda (k v &optional stream)
                                                          (funcall ,fn k stream)
                                                          (funcall ,fn v stream))
                                                    looping t)))
                                       (if looping
                                           (progn
                                             (insert prefix)
                                             (funcall map-fn fn object)
                                             (cl-letf (((point) (1- (point))))
                                               (insert suffix)))
                                         (funcall fn object stream))))))

                          (pp val))
                          ;; >>>>>>>>>>>>>>>>>>>>>>>>>
                          ;; Remove trailing newline.
                          (and (= (char-before) ?\n) (delete-char -1)))
		      (let* ((sv (get variable 'standard-value))
			     (origval (and (consp sv)
				           (condition-case nil
					       (eval (car sv))
					     (error :help-eval-error))))
                             from)
		        (when (and (consp sv)
                                   (not (equal origval val))
                                   (not (equal origval :help-eval-error)))
		          (princ "\nOriginal value was \n")
		          (setq from (point))
                          (cl-prin1 origval)
                          (save-restriction
                            (narrow-to-region from (point))
                            (save-excursion (pp-buffer)))
		          (if (< (point) (+ from 20))
			      (delete-region (1- from) from)))))))
	        (terpri)
	        (when locus
	          (cond
                    ((bufferp locus)
                     (princ (format "Local in buffer %s; "
                                    (buffer-name buffer))))
                    ((terminal-live-p locus)
                     (princ (format "It is a terminal-local variable; ")))
                    (t
                     (princ (format "It is local to %S" locus))))
	          (if (not (default-boundp variable))
		      (princ "globally void")
		    (let ((global-val (default-value variable)))
		      (with-current-buffer standard-output
		        (princ "global value is ")
		        (if (eq val global-val)
			    (princ "the same.")
		          (terpri)
                          ;; Fixme: pp can take an age if you happen to
                          ;; ask for a very large expression.  We should
                          ;; probably print it raw once and check it's a
                          ;; sensible size before prettyprinting.  -- fx
		          (let ((from (point)))
                            (cl-prin1 global-val)
                            (save-restriction
                              (narrow-to-region from (point))
                              (save-excursion (pp-buffer)))
                            ;; See previous comment for this function.
                            ;; (help-xref-on-pp from (point))
			    (if (< (point) (+ from 20))
			        (delete-region (1- from) from)))))))
                  (terpri))

                ;; If the value is large, move it to the end.
	        (with-current-buffer standard-output
	          (when (> (count-lines (point-min) (point-max)) 10)
                    ;; Note that setting the syntax table like below
                    ;; makes forward-sexp move over a `'s' at the end
                    ;; of a symbol.
		    (set-syntax-table emacs-lisp-mode-syntax-table)
		    (goto-char val-start-pos)
                    ;; The line below previously read as
                    ;; (delete-region (point) (progn (end-of-line) (point)))
                    ;; which suppressed display of the buffer local value for
                    ;; large values.
		    (when (looking-at "value is") (replace-match ""))
		    (save-excursion
		      (insert "\n\nValue:")
		      (set (make-local-variable 'help-button-cache)
		           (point-marker)))
		    (insert "value is shown ")
		    (insert-button "below"
			           'action help-button-cache
			           'follow-link t
			           'help-echo "mouse-2, RET: show value")
		    (insert ".\n")))
                (terpri)

                (let* ((alias (condition-case nil
                                  (indirect-variable variable)
                                (error variable)))
                       (obsolete (get variable 'byte-obsolete-variable))
                       (watchpoints (get-variable-watchers variable))
		       (use (car obsolete))
		       (safe-var (get variable 'safe-local-variable))
                       (doc (or (documentation-property
                                 variable 'variable-documentation)
                                (documentation-property
                                 alias 'variable-documentation)))
                       (extra-line nil))

                  ;; Mention if it's a local variable.
	          (cond
	            ((and (local-variable-if-set-p variable)
		          (or (not (local-variable-p variable))
			      (with-temp-buffer
			        (local-variable-if-set-p variable))))
                     (setq extra-line t)
                     (princ "  Automatically becomes ")
		     (if permanent-local
		         (princ "permanently "))
		     (princ "buffer-local when set.\n"))
	            ((not permanent-local))
	            ((bufferp locus)
		     (setq extra-line t)
		     (princ
		      (substitute-command-keys
		       "  This variable's buffer-local value is permanent.\n")))
	            (t
		     (setq extra-line t)
                     (princ (substitute-command-keys
			     "  This variable's value is permanent \
if it is given a local binding.\n"))))

                  ;; Mention if it's an alias.
                  (unless (eq alias variable)
                    (setq extra-line t)
                    (princ (format-message
                            "  This variable is an alias for `%s'.\n"
                            alias)))

                  (when obsolete
                    (setq extra-line t)
                    (princ "  This variable is obsolete")
                    (if (nth 2 obsolete)
                        (princ (format " since %s" (nth 2 obsolete))))
		    (princ (cond ((stringp use) (concat ";\n  " use))
			         (use (format-message ";\n  use `%s' instead."
                                                      (car obsolete)))
			         (t ".")))
                    (terpri))

                  (when watchpoints
                    (setq extra-line t)
                    (princ "  Calls these functions when changed: ")
                    (princ watchpoints)
                    (terpri))

	          (when (member (cons variable val)
                                (with-current-buffer buffer
                                  file-local-variables-alist))
		    (setq extra-line t)
		    (if (member (cons variable val)
                                (with-current-buffer buffer
                                  dir-local-variables-alist))
		        (let ((file (and (buffer-file-name buffer)
                                         (not (file-remote-p
                                               (buffer-file-name buffer)))
                                         (dir-locals-find-file
                                          (buffer-file-name buffer))))
                              (is-directory nil))
		          (princ (substitute-command-keys
			          "  This variable's value is directory-local"))
                          (when (consp file) ; result from cache
                            ;; If the cache element has an mtime, we
                            ;; assume it came from a file.
                            (if (nth 2 file)
                                ;; (car file) is a directory.
                                (setq file (dir-locals--all-files (car file)))
                              ;; Otherwise, assume it was set directly.
                              (setq file (car file)
                                    is-directory t)))
                          (if (null file)
                              (princ ".\n")
                            (princ ", set ")
                            (princ (substitute-command-keys
                                    (cond
                                      (is-directory "for the directory\n  `")
                                      ;; Many files matched.
                                      ((and (consp file) (cdr file))
                                       (setq file (file-name-directory (car file)))
                                       (format "by one of the\n  %s files in the directory\n  `"
                                               dir-locals-file))
                                      (t (setq file (car file))
                                         "by the file\n  `"))))
			    (with-current-buffer standard-output
			      (insert-text-button
			       file 'type 'help-dir-local-var-def
                               'help-args (list variable file)))
			    (princ (substitute-command-keys "'.\n"))))
		      (princ (substitute-command-keys
			      "  This variable's value is file-local.\n"))))

	          (when (memq variable ignored-local-variables)
		    (setq extra-line t)
		    (princ "  This variable is ignored as a file-local \
variable.\n"))

                  ;; Can be both risky and safe, eg auto-fill-function.
	          (when (risky-local-variable-p variable)
		    (setq extra-line t)
		    (princ "  This variable may be risky if used as a \
file-local variable.\n")
		    (when (assq variable safe-local-variable-values)
		      (princ (substitute-command-keys
                              "  However, you have added it to \
`safe-local-variable-values'.\n"))))

	          (when safe-var
                    (setq extra-line t)
		    (princ "  This variable is safe as a file local variable ")
		    (princ "if its value\n  satisfies the predicate ")
		    (princ (if (byte-code-function-p safe-var)
			       "which is a byte-compiled expression.\n"
			     (format-message "`%s'.\n" safe-var))))

                  (if extra-line (terpri))
	          (princ "Documentation:\n")
	          (with-current-buffer standard-output
		    (insert (or doc "Not documented as a variable."))))

                ;; Make a link to customize if this variable can be customized.
	        (when (custom-variable-p variable)
	          (let ((customize-label "customize"))
		    (terpri)
		    (terpri)
		    (princ (concat "You can " customize-label " this variable."))
		    (with-current-buffer standard-output
		      (save-excursion
		        (re-search-backward
		         (concat "\\(" customize-label "\\)") nil t)
		        (help-xref-button 1 'help-customize-variable variable))))
                  ;; Note variable's version or package version.
	          (let ((output (describe-variable-custom-version-info variable)))
		    (when output
		      (terpri)
		      (terpri)
		      (princ output))))

	        (with-current-buffer standard-output
                  ;; Return the text we displayed.
	          (buffer-string))))))))
    (advice-add 'describe-variable :override #'tv/describe-variable)))

;;; comment
;;
(use-package newcomment
  :config
  (progn
    ;; Change the behavior of `M-;' by commenting line.
    ;; Much simpler than emacs-25 `comment-line'.
    (defun comment--advice-dwim (old--fn &rest args)
      (if (region-active-p)
          (apply old--fn args)
        (save-excursion
          (goto-char (point-at-bol))
          (push-mark (point-at-eol) t t)
          (apply old--fn args))
        (indent-region (point-at-bol) (point-at-eol))
        (forward-line 1)
        (back-to-indentation)))
    (advice-add 'comment-dwim :around 'comment--advice-dwim)))

;;; Woman/man
;;
(use-package woman
  :config
  (setq woman-use-own-frame nil))

(use-package man
    :config
  (setq Man-notify-method 'pushy)
  (defun tv/advice--Man-highlight-references (&optional xref-man-type)
    (unless Man-arguments
      (setq Man-arguments ""))
    (if (string-match "-k " Man-arguments)
        (progn
          (Man-highlight-references0 nil Man-reference-regexp 1
                                     'Man-default-man-entry
                                     (or xref-man-type 'Man-xref-man-page))
          (Man-highlight-references0 nil Man-apropos-regexp 1
                                     'Man-default-man-entry
                                     (or xref-man-type 'Man-xref-man-page)))
      ;; Highlight references from top to bottom, not only from "SEE
      ;; ALSO" section.
      (Man-highlight-references0 nil Man-reference-regexp 1
                                 'Man-default-man-entry
                                 (or xref-man-type 'Man-xref-man-page))
      (Man-highlight-references0 Man-synopsis-regexp Man-header-regexp 0 2
                                 'Man-xref-header-file)
      (Man-highlight-references0 Man-files-regexp Man-normal-file-regexp 0 0
                                 'Man-xref-normal-file)))
  (advice-add 'Man-highlight-references :override #'tv/advice--Man-highlight-references))

;; show-paren-mode
;;
(use-package paren
  :config
  (progn
    (show-paren-mode 1)
    (setq show-paren-ring-bell-on-mismatch t)))

(use-package electric
  :config (electric-indent-mode -1))

;;; auto-compression-mode
;;
(use-package jka-cmpr-hook
  :config (auto-compression-mode 1))

;;; Image file
;;
(use-package image-file
  :config (auto-image-file-mode 1))

;;; Rst-mode
;;
(use-package rst
  :config
  (add-hook 'rst-mode-hook 'auto-fill-mode))

;;; Shell script
;;
(use-package sh-script
  :bind (:map sh-mode-map
              ("RET" . newline-and-indent)
              ("C-h f" . helm-info-bash)))

;;; Auto-conf
;;
(use-package autoconf-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.ac\\'\\|configure\\.in\\'" . autoconf-mode)))

(use-package autotest-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.at\\'" . autotest-mode)))

;;; Desktop-entry-mode
;;
(use-package desktop-entry-mode
  :load-path "~/elisp/desktop-file-utils/"
  :commands 'desktop-entry-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode)))

;;; Wget
;;
;;
(use-package wget
  :config
  (progn
    (use-package w3m-wget)
    ;; Use wget in eshell.
    (defun eshell/wget (url)
      (wget url))))

;;; Winner
;;
(use-package winner
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*mu4e-loading*"
                                ))
  (winner-mode 1))

;;; Time
;;
(use-package time
  :config
  ;; World-time
  (when (eq display-time-world-list t) ; emacs-26+
    (setq display-time-world-list
          (let ((nyt (format-time-string "%z" nil "America/New_York"))
                (gmt (format-time-string "%z" nil "Europe/London")))
            (if (string-equal nyt gmt)
                legacy-style-world-list
              zoneinfo-style-world-list))))
  (add-to-list 'display-time-world-list '("Greenwich" "Greenwich"))
  (add-to-list 'display-time-world-list '("Australia/Sydney" "Sydney"))
  (add-to-list 'display-time-world-list '("Australia/Melbourne" "Melbourne"))
  (add-to-list 'display-time-world-list '("Australia/Canberra" "Canberra"))
  (add-to-list 'display-time-world-list '("America/Chicago" "Chicago"))
  (add-to-list 'display-time-world-list '("America/Denver" "Denver"))
  (add-to-list 'display-time-world-list '("America/Los_Angeles" "Los_Angeles/Seattle"))
  (add-to-list 'display-time-world-list '("America/Denver" "Moab"))
  (add-to-list 'display-time-world-list '("America/Vancouver" "Vancouver"))
  (add-to-list 'display-time-world-list '("America/Montreal" "Montreal"))
  (add-to-list 'display-time-world-list '("America/New_York" "Ottawa"))
  (add-to-list 'display-time-world-list '("Europe/Moscow" "Moscow"))
  (add-to-list 'display-time-world-list '("Europe/Berlin" "Berlin"))
  (add-to-list 'display-time-world-list '("Europe/Oslo" "Oslo"))
  (add-to-list 'display-time-world-list '("Europe/Lisbon" "Lisbon"))
  (add-to-list 'display-time-world-list '("Europe/Athens" "Athens"))
  (add-to-list 'display-time-world-list '("Asia/Dubai" "Dubai"))
  (add-to-list 'display-time-world-list '("Asia/Tokyo" "Tokyo"))
  (add-to-list 'display-time-world-list '("Hongkong" "Hongkong"))
  (add-to-list 'display-time-world-list '("Indian/Antananarivo" "Antananarivo"))
  (add-to-list 'display-time-world-list '("Indian/Reunion" "Reunion"))
  
  (setq display-time-24hr-format   t
        display-time-day-and-date  t
        display-time-use-mail-icon t
        display-time-string-forms
        '( ;; date
          (if (and (not display-time-format) display-time-day-and-date)
              (format-time-string "[%a%e %b " now)
            "")
          ;; time
          (concat
           (propertize
            (format-time-string (or display-time-format
                                    (if display-time-24hr-format " %H:%M" " %-I:%M%p"))
                                now)
            'face '((:foreground "green"))
            'help-echo (format-time-string " %a %b %e, %Y" now))
           (and time-zone " (") time-zone (and time-zone ")")
           "]")
          ;; cpu load average
          ;; (if (and load (not (string= load "")))
          ;;     (format " [Cpu:%s%%%%] " load) "")
          ""
          ;; mail
          ""))
  (display-time))

;;; Frame and window config.
;;
;;
;; My current-font:      [EVAL]: (assoc-default 'font (frame-parameters))
;; Choose a font:        [EVAL]: (progn (when (require 'helm-font) (helm 'helm-source-xfonts)))
;; Choose a color:       [EVAL]: (progn (when (require 'helm-color) (helm 'helm-source-colors)))
;; To reload .Xresources [EVAL]: (shell-command xrdb "~/.Xresources")

(use-package frame
  :config
  (progn
    (defvar tv/default-font (if (string= (invocation-name) "remacs")
                                "-*-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1"
                              ;; Use .Xdefaults config
                              (assoc-default 'font (frame-parameters))))
    (setq-default frame-background-mode 'dark)
    (setq initial-frame-alist '((fullscreen . maximized)))
    (setq frame-auto-hide-function 'delete-frame)
    
    (if (or (daemonp)
            (not (window-system))
            (< emacs-major-version 24))
        (setq default-frame-alist `((vertical-scroll-bars . nil)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (title . ,(format "%s-%s"
                                                      (capitalize (invocation-name))
                                                      emacs-version))
                                    (cursor-color . "red")))

      (setq default-frame-alist `((foreground-color . "Wheat")
                                  (background-color . "Gray19")
                                  ;; New frames go in right corner.
                                  (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
                                  (vertical-scroll-bars . nil)
                                  (title . ,(format "%s-%s"
                                                    (capitalize (invocation-name))
                                                    emacs-version))
                                  (tool-bar-lines . 0)
                                  (menu-bar-lines . 0)
                                  (font . ,tv/default-font)
                                  (cursor-color . "red")
                                  (fullscreen . nil)
                                  )))

    ;; Special buffer display.
    (add-hook 'window-setup-hook
              (lambda ()
                (setq special-display-regexps `(("\\*Help"
                                                 (minibuffer . nil)
                                                 (width . 80)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "Lightsteelblue1")
                                                 (foreground-color . "black")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ("\\*Compile-Log"
                                                 (minibuffer . nil)
                                                 (width . 85)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "Brown4")
                                                 (foreground-color . "black")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ("\\*Dict"
                                                 (minibuffer . nil)
                                                 (width . 80)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "LightSteelBlue")
                                                 (foreground-color . "DarkGoldenrod")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ))))))

(use-package window
  :no-require t
  ;; Don't split windows horizontally.
  :init (setq split-width-threshold nil)
  (use-package helm
    :config
    (helm-define-key-with-subkeys global-map (kbd "C-x ^") ?^ 'enlarge-window
                                  '((?ç . shrink-window)
                                    (?} . enlarge-window-horizontally)
                                    (?{ . shrink-window-horizontally))
                                  (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor" 'face 'minibuffer-prompt))
    (helm-define-key-with-subkeys global-map (kbd "C-x }") ?} 'enlarge-window-horizontally
                                  '((?^ . enlarge-window)
                                    (?ç . shrink-window)
                                    (?{ . shrink-window-horizontally))
                                  (propertize "^:Enl.ver, }:Enl.hor, ç:Shr.ver, {:Shr.hor" 'face 'minibuffer-prompt)))
    :bind (("C-x C-²" . delete-window)
           ("C-x C-&" . delete-other-windows)
           ("C-x C-é" . split-window-vertically)
           ("C-x C-\"" . split-window-horizontally)))

;;; Use `net-utils-run-simple' in net-utils fns.
;;
(use-package net-utils
  :config
  (progn
    (defun ping (host)
      "Ping HOST.
If your system's ping continues until interrupted, you can try setting
`ping-program-options'."
      (interactive "sPing host: ")
      (let ((options
             (if ping-program-options
                 (append ping-program-options (list host))
               (list host))))
        (net-utils-run-simple
         (concat "Ping" " " host)
         ping-program
         options)))

    (defun run-dig (host)
      "Run dig program."
      (interactive "sLookup host: ")
      (net-utils-run-simple
       (concat "** "
               (mapconcat 'identity
                          (list "Dig" host dig-program)
                          " ** "))
       dig-program
       (list host)))))

;;; Async
;;
(use-package async
  :config
  (progn
    ;; Dired async.
    (use-package dired-async :config (dired-async-mode 1))
    ;; Smtp async.
    (use-package smtpmail-async
      :commands 'async-smtpmail-send-it)
    ;; Byte compilation async.
    (use-package async-bytecomp
      :config
      (setq async-bytecomp-allowed-packages 'all))))

;;; Org
;;
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c k" . org-capture))
  :config (use-package org-config))

;;; Emms
;;
(use-package emms
  :ensure t
  :defer t
  :config (use-package emms-vlc-config))

;;; Dired
;;
(use-package dired
  :init (progn
          (setq dired-dwim-target t)
          (setq dired-auto-revert-buffer t)
          (setq dired-backup-overwrite nil) ; nil, always, ask.
          (setq dired-isearch-filenames 'dwim)
          (setq dired-listing-switches (purecopy "-alh")))
  :config
  (use-package dired-extension)
  (use-package wdired
    :config (setq wdired-use-dired-vertical-movement 'sometimes))
  :defer t)

;;; htmlize
;;
(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'htmlize-region "htmlize" nil t)
(autoload 'htmlize-file "htmlize" nil t)
(autoload 'htmlize-many-files "htmlize" nil t)
(autoload 'htmlize-many-files-dired "htmlize" nil t)

;;; google-maps
;;
(use-package google-maps
  :init (setq google-maps-static-default-zoom 10)
  :bind ("<f5> g m" . google-maps))

;;; tv-utils fns
;;
(use-package tv-utils
  :commands (tv/eval-region tv/restore-scratch-buffer)
  :init (progn
          (bind-key "C-M-!" 'tv/eval-region lisp-interaction-mode-map) 
          (bind-key "C-M-!" 'tv/eval-region emacs-lisp-mode-map))
  :config (advice-add 'view-echo-area-messages :around 'tv/view-echo-area-messages)
  :bind (("M-\""                  . tv/insert-double-quote)
         ("C-M-`"                 . tv/insert-double-backquote)
         ("C-M-("                 . tv/move-pair-forward)
         ("C-M-\""                . tv/insert-double-quote-and-close-forward)
         ("C-M-)"                 . tv/insert-pair-and-close-forward)
         ("<f5> c"                . tv/toggle-calendar)
         ([remap kill-whole-line] . tv/kill-whole-line)
         ([remap kill-line]       . tv/kill-line)
         ([remap delete-char]     . tv/delete-char)
         ([remap c-electric-delete-forward] . tv/delete-char)
         ("C-<"                   . other-window-backward)
         ("C->"                   . other-window-forward)
         ([C-left]                . screen-top)
         ([C-right]               . screen-bottom)
         ("<M-down>"              . tv/scroll-down)
         ("<M-up>"                . tv/scroll-up)
         ("<C-M-down>"            . tv/scroll-other-down)
         ("<C-M-up>"              . tv/scroll-other-up)))

;;; Ledger
;;
(use-package ledger-mode
  :init (setenv "LEDGER_PAGER" "cat")
  :commands (ledger-mode csv2ledger)
  :config (use-package ledger-config
            :init
            (require 'helm-lib)
            (require 'helm-mode)))

;;; Rectangle
;;
(use-package rectangle-utils
  :commands (rectangle-utils-menu
             rectangle-utils-copy-rectangle
             rectangle-utils-insert-at-right
             rectangle-utils-extend-rectangle-to-end
             rectangle-utils-extend-rectangle-to-space-or-paren
             rectangle-utils-extend-rectangle-to-space-or-dot
             rectangle-utils-extend-rectangle-to-regexp)
  :bind (("C-x r e"       . rectangle-utils-extend-rectangle-to-end)
         ("C-x r h"       . rectangle-utils-menu)
         ("C-x r <right>" . rectangle-utils-insert-at-right)))

;;; Align-let
;;
(use-package align-let
    :commands (align-let align-let-region)
    :bind (("C-c C-a" . align-let)))

;;; Smallurl
;;
(use-package smallurl
  :commands (smallurl smallurl-replace-at-point))

;;; Zop-to-char
;;
(use-package zop-to-char
  :commands (zop-to-char zop-up-to-char)
  :init
  (progn
    (setq zop-to-char-prec-keys '(left ?\C-b ?\M-a)
          zop-to-char-next-keys '(right ?\C-f ?\M-e)))
  :bind ([remap zap-to-char] . zop-to-char))

;;; Iedit
;;
(use-package iedit
  :ensure t
  :config
  (defun iedit-narrow-to-defun (arg)
    (interactive "P")
    (require 'iedit)
    (save-window-excursion
      (save-restriction
        (narrow-to-defun)
        (iedit-mode arg))))
  :bind (("C-²" . iedit-narrow-to-defun)
         ("C-;" . iedit-mode)
         :map isearch-mode-map
         ("C-;" . iedit-mode-from-isearch)))

(use-package iedit-rect
  :bind (([C-return] . iedit-rectangle-mode)
         :map ctl-x-r-map
         ("RET" . iedit-rectangle-mode)))

;;; Lacarte
;;
(autoload 'lacarte-get-overall-menu-item-alist "lacarte")

;;; Iterator
;;
(use-package iterator)

;;; pcomplete
;;
(use-package pcomplete-extension)

;;; Migemo
;;
(use-package migemo
  :init
  (setq migemo-command          "cmigemo"
        migemo-options          '("-q" "-e")
        migemo-dictionary       "/usr/share/cmigemo/utf-8/migemo-dict"
        migemo-user-dictionary  nil
        migemo-regex-dictionary nil
        migemo-coding-system    'utf-8-unix
        migemo-isearch-enable-p nil)
  :disabled t)

;;; Magit
;;
;; Magit when installed from git contains also git-commit and
;; git-rebase so no need to install them as dependency.
;;
(use-package magit
  :commands (magit-status magit-status-internal magit-blame)
  :init
  (bind-key "<f2>" 'magit-status)
  (use-package magit-branch
    :bind ("C-c b" . magit-checkout))
  (setq git-commit-fill-column             70
        git-commit-summary-max-length      56
        auto-revert-verbose                nil
        magit-auto-revert-immediately
        (null (and (boundp 'auto-revert-use-notify)
                   auto-revert-use-notify))
        magit-revision-show-gravatars nil
        magit-uniquify-buffer-names   nil)
  (add-hook 'git-commit-setup-hook (lambda () (setq-local adaptive-fill-mode nil)))
  :config
  (bind-key "C"    'magit-commit-add-log magit-diff-mode-map)
  (bind-key "C-]"  'magit-toggle-margin magit-log-mode-map)
  ;; Press RET while in branch manager to checkout branches as
  ;; before.
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref))
  (add-to-list 'magit-visit-ref-behavior 'create-branch)
  ;; Recognize sudo french/english password prompt in shell
  ;; commands.
  ;; Use the nth99 submatch to pass the match to auth-source.
  (add-to-list 'magit-process-password-prompt-regexps
               "^\\[sudo\\] [Mm]ot de passe de \\(?99:.*[^ ]\\).?:.*$")
  (add-to-list 'magit-process-password-prompt-regexps
               "^\\[sudo\\] [Pp]assword for \\(?99:.*\\): ?$")
  (add-to-list 'magit-process-find-password-functions
               (lambda (key)
                 (tv/get-passwd-from-auth-sources key :port "sudo")))
  :no-require t)

;;; Emamux
;;
(use-package emamux
  :ensure t
  :init (setq emamux:completing-read-type 'helm)
  :config (setq emamux:get-buffers-regexp
                "^\\(buffer[0-9]+\\): +\\([0-9]+\\) +\\(bytes\\): +[\"]\\(.*\\)[\"]"
                emamux:show-buffers-with-index nil)
  :bind (("C-c y" . emamux:yank-from-list-buffers)
         ("C-c s" . emamux:send-command)))

;;; Eldoc
;;
(use-package eldoc
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'eshell-mode-hook 'turn-on-eldoc-mode))
  :config
  ;; This is for emacs-25+ (fixes the new idiot quoting style).
  (when (fboundp 'elisp--highlight-function-argument)
    (defun tv/before-elisp--highlight-function-argument (old--fn &rest args)
      (let ((sym    (nth 0 args))
            (argstr (substitute-command-keys (nth 1 args)))
            (index  (nth 2 args))
            (prefix (nth 3 args)))
        (funcall old--fn sym argstr index prefix)))
    (advice-add 'elisp--highlight-function-argument
                :around #'tv/before-elisp--highlight-function-argument)))

(use-package eldoc-eval
    :preface (defvar eldoc-in-minibuffer-mode nil)
    :diminish eldoc-mode
    :config
    (progn
      (eldoc-in-minibuffer-mode 1)
      (defadvice edebug-eval-expression (around with-eldoc activate)
        "This advice enable eldoc support."
        (interactive (list (with-eldoc-in-minibuffer
                               (read-from-minibuffer
                                "Eval: " nil read-expression-map t
                                'read-expression-history))))
        ad-do-it)))

;;; Python config
;;
;;
(use-package python
  :no-require t
  :init
  (progn
    (setq
     gud-pdb-command-name "ipdb"
     python-shell-interpreter "ipython"
     python-shell-interpreter-args "-i --autoindent"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
    (setq python-eldoc-setup-code
          "def __PYDOC_get_help(obj):
    try:
        import inspect
        try:
            str_type = basestring
            argspec_function = inspect.getargspec
        except NameError:
            str_type = str
            argspec_function = inspect.getfullargspec
        if isinstance(obj, str_type):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if callable(obj):
            try:
                sig = inspect.formatargspec(*argspec_function(obj))
            except:
                sig = None
        else:
            sig = None
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(*argspec_function(target))
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
        sig = ''
    if sig:
        doc = '\\n'.join([sig,doc])
    return doc
")
    (add-hook 'python-mode-hook
              (lambda ()
                (define-key python-mode-map (kbd "C-m") 'newline-and-indent))))
  :bind ("<f11> p" . python-shell-switch-to-shell))

;;; Tramp-config
;;
(use-package tramp
  :defer t
  :no-require t
  :config
  (progn
    ;; scp is better for copying large files but not working with many
    ;; files.
    (setq tramp-default-method "ssh")
    ;; (setq tramp-verbose 6) ; See `helm-tramp-verbose' in init-helm.

    ;; No messages
    (setq tramp-message-show-message nil)

    (setq tramp-use-ssh-controlmaster-options nil)

    ;; Allow connecting as root on all remote Linux machines except this one.
    ;; Use e.g /sudo:host:/path
    (add-to-list 'tramp-default-proxies-alist
                 '("\\`thievol\\'" "\\`root\\'" "/ssh:%h:"))

    (add-to-list 'tramp-default-proxies-alist
                 '("\\`thievolrem\\'" "\\`root\\'" "/ssh:%h:"))

    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))

    ;; (when (boundp 'tramp-save-ad-hoc-proxies)
    ;;   (setq tramp-save-ad-hoc-proxies t))

    ;; Connect to my freebox as 'freebox' user.
    (add-to-list 'tramp-default-user-alist
                 '("ftp" "\\`mafreebox\\.freebox\\.fr\\'" "freebox"))))

;;; Ange-ftp
;;
(use-package ange-ftp
  :init
  (progn
    ;; Following used to work with previous emacs version but is now broken.
    ;; (setq ange-ftp-try-passive-mode t)
    ;; (setq ange-ftp-passive-host-alist '(("mafreebox.freebox.fr" . "on")))
    ;; So use now directly pftp.
    (setq ange-ftp-ftp-program-name "pftp"))
  :no-require t)

;;; Calendar and diary
;;
(use-package calendar
  :config
  (progn
    (setq diary-file "~/.emacs.d/diary")
    (unless (fboundp 'fancy-diary-display) ; Fix emacs-25.
      (defalias 'fancy-diary-display 'diary-fancy-display))
    (defface tv/calendar-blocks
      '((t (:background "ForestGreen")))
      "Face used to highlight diary blocks in calendar."
      :group 'calendar)
    (defface tv/calendar-blocks-1
      '((t (:background "DarkOliveGreen")))
      "Face used to highlight diary blocks in calendar."
      :group 'calendar)
    (setq calendar-date-style 'european)
    (setq calendar-mark-diary-entries-flag t)
    (setq calendar-mark-holidays-flag t)
    (setq holiday-bahai-holidays nil)
    (setq holiday-hebrew-holidays nil)
    (setq holiday-islamic-holidays nil)
    (setq holiday-oriental-holidays nil)

    (setq diary-display-function 'diary-fancy-display)
    (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
    (add-hook 'initial-calendar-window-hook 'mark-diary-entries)
    (setq mark-holidays-in-calendar t)
    (setq diary-number-of-entries 4)

    ;; calendar-date-style is set [HERE]:
    (setq calendar-week-start-day 1
          calendar-day-name-array
          ["Dimanche" "Lundi" "Mardi"
           "Mercredi" "Jeudi" "Vendredi" "Samedi"]
          ;; FIXME there is a bug in calendar that break diary when
          ;; abbreviated names are different than US.
          ;; calendar-day-abbrev-array
          ;; ["Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"]
          ;; calendar-day-header-array
          ;; ["Di" "Lu" "Ma" "Me" "Je" "Ve" "Sa"]
          ;; calendar-month-abbrev-array
          ;; ["Jan" "Fév" "Mar" "Avr" "Mai" "Juin" "Juil" "Aou" "Sep" "Oct" "Nov" "Déc"]
          calendar-month-name-array
          ["Janvier" "Février" "Mars" "Avril"
           "Mai" "Juin" "Juillet" "Août" "Septembre"
           "Octobre" "Novembre" "Décembre"])

    (defvar holiday-french-holidays nil
      "French holidays")

    (setq holiday-french-holidays
          `((holiday-fixed 1 1 "Jour de l'an")
            (holiday-fixed 2 14 "Fête des amoureux")
            (holiday-fixed 5 1 "Fête du travail")
            (holiday-fixed 5 8 "Victoire")
            (holiday-float 5 0 -1 "Fête des Mères")
            (holiday-float 6 0 3 "Fête des Pères")
            (holiday-fixed 7 14 "Fête nationale")
            (holiday-fixed 8 15 "Assomption")
            (holiday-fixed 10 31 "Halloween")
            (holiday-easter-etc -47 "Mardi Gras")
            (holiday-fixed 11 11 "Armistice")
            (holiday-fixed 11 1 "Toussaint")
            (holiday-fixed 12 25 "Noël")
            (holiday-easter-etc 0 "Pâques")
            (holiday-easter-etc 1 "Pâques")
            (holiday-easter-etc 39 "Ascension")
            (holiday-easter-etc 49 "Pentecôte")
            (holiday-easter-etc 50 "Pentecôte")
            (holiday-float 3 0 -1 "Heure d'été")
            (holiday-float 10 0 -1 "Heure d'hiver")))

    (setq calendar-holidays `(,@holiday-solar-holidays
                              ,@holiday-french-holidays))

    (defun tv/calendar-diary-or-holiday (arg)
      "A single command for diary and holiday entries."
      ;; Assume diary and holidays are shown in calendar.
      (interactive "p")
      (let* ((ovs (overlays-at (point)))
             (props (cl-loop for ov in ovs
                             for prop = (cadr (overlay-properties ov))
                             when (memq prop '(diary holiday diary-anniversary
                                                     tv/calendar-blocks tv/calendar-blocks-1))
                             collect prop)))
        (cond ((and (or (memq 'diary props)
                        (memq 'tv/calendar-blocks props)
                        (memq 'tv/calendar-blocks-1 props)
                        (memq 'diary-anniversary props))
                    (memq 'holiday props))
               (cl-letf (((symbol-function 'message) #'ignore))
                 (diary-view-entries arg))
               (calendar-cursor-holidays))
              ((or (memq 'diary props)
                   (memq 'tv/calendar-blocks props)
                   (memq 'tv/calendar-blocks-1 props)
                   (memq 'diary-anniversary props))
               (cl-letf (((symbol-function 'message) #'ignore))
                 (diary-view-entries arg)))
              ((memq 'holiday props)
               (calendar-cursor-holidays))
              (t (message "Nothing special on this date")))))

    (define-key calendar-mode-map (kbd "C-<right>") 'calendar-forward-month)
    (define-key calendar-mode-map (kbd "C-<left>")  'calendar-backward-month)
    (define-key calendar-mode-map (kbd "RET")       'tv/calendar-diary-or-holiday)
    (use-package appt
      :config
      (progn
        (setq appt-display-format 'echo ; Values: 'echo, 'window or nil.
              appt-warning-time-regexp "warn ?\\([0-9]+\\)") 
        (add-hook 'emacs-startup-hook 'appt-activate))))
  :defer t)

;;; Bookmarks
;;
(use-package bookmark
  :no-require t
  :init
  (progn
    (add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)
    (setq bookmark-bmenu-toggle-filenames nil)
    (setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
    (setq bookmark-automatically-show-annotations nil))
  :config
  (progn
    (defun tv/advice--bookmark-write-file (file)
      "Write `bookmark-alist' to FILE."
      (let ((reporter (make-progress-reporter
		       (format "Saving bookmarks to file %s..." file))))
        (with-current-buffer (find-file-noselect file)
          (let ((vc (cond
		     ((null bookmark-version-control) nil)
		     ((eq 'never bookmark-version-control) 'never)
		     ((eq 'nospecial bookmark-version-control) version-control)
		     (t t))))
            (when (version-control-safe-local-p vc)
              (setq-local version-control vc)))
          (goto-char (point-min))
          (condition-case err
              (progn
                (delete-region (point-min) (point-max))
                (let ((coding-system-for-write
                       (or coding-system-for-write
                           bookmark-file-coding-system
                           'utf-8-emacs))
                      (print-length nil)
                      (print-level nil)
                      ;; See bug #12503 for why we bind `print-circle'.  Users
                      ;; can define their own bookmark types, which can result in
                      ;; arbitrary Lisp objects being stored in bookmark records,
                      ;; and some users create objects containing circularities.
                      (print-circle t))
                  (insert "(")
                  ;; Rather than a single call to `pp' we make one per bookmark.
                  ;; Apparently `pp' has a poor algorithmic complexity, so this
                  ;; scales a lot better.  bug#4485.
                  (dolist (i bookmark-alist) (pp i (current-buffer)))
                  (insert ")\n")
                  ;; Make sure the specified encoding can safely encode the
                  ;; bookmarks.  If it cannot, suggest utf-8-emacs as default.
                  (with-coding-priority '(utf-8-emacs)
                    (setq coding-system-for-write
                          (select-safe-coding-system (point-min) (point-max)
                                                     (list t coding-system-for-write))))
                  (goto-char (point-min))
                  (bookmark-insert-file-format-version-stamp coding-system-for-write)
                  (setq bookmark-file-coding-system coding-system-for-write)
                  (save-buffer)))
            (file-error (message "Can't write %s" file)))
          (kill-buffer (current-buffer)))
        (progress-reporter-done reporter)))
    (advice-add 'bookmark-write-file :override #'tv/advice--bookmark-write-file)
    (and (boundp 'bookmark-bmenu-use-header-line)
         (setq bookmark-bmenu-use-header-line nil))))

;;; git-gutter-mode
;;
(use-package git-gutter
  :init
  (progn
    (customize-set-variable 'git-gutter:update-interval 2) ; Activate live update timer.
    (setq git-gutter:hide-gutter t) ; Always a 0 width margin when no changes.
    (bind-key [remap vc-dir] 'git-gutter:popup-hunk)
    ;; Stage current hunk
    (bind-key [remap vc-create-tag] 'git-gutter:stage-hunk)
    ;; Revert current hunk
    (bind-key (kbd "C-x v r") 'git-gutter:revert-hunk))
  :diminish git-gutter-mode
  :config
  (progn
    (defun tv/git-gutter:popup-diff-quit ()
      (interactive)
      (with-selected-window (get-buffer-window git-gutter:popup-buffer)
        (View-quit)))
    ;; (setq git-gutter:diff-option "-b")
    (global-git-gutter-mode) ; Enable live update.
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v n") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v p") ?p 'git-gutter:previous-hunk '((?n . git-gutter:next-hunk)))
    (helm-define-key-with-subkeys
        global-map (kbd "C-x v d") nil 'git-gutter:popup-hunk '((?n . git-gutter:next-hunk)
                                                                (?d . git-gutter:next-hunk)
                                                                (?p . git-gutter:previous-hunk)
                                                                (?q . tv/git-gutter:popup-diff-quit)))))

;;; Addressbook
;;
(use-package addressbook-bookmark
  :commands (addressbook-turn-on-mail-completion
             addressbook-bookmark-set
             addressbook-mu4e-bookmark
             addressbook-bmenu-edit
             addressbook-bookmark-jump))

;;; W3m
;;
(use-package w3m
  :commands (w3m-toggle-inline-image w3m-region w3m-browse-url)
  :init (require 'config-w3m)
  :bind
  (("<f7> h" . w3m)
   :map w3m-mode-map
   ("F" . w3m-view-url-with-browse-url)
   ("M-<right>" . w3m-next-buffer)
   ("M-<left>" . w3m-previous-buffer)
   ("V" . helm-w3m-bookmarks)
   ("M" . w3m-view-url-with-browse-url)
   ("M-q" . tv/w3m-fill-region-or-paragraph)
   ("<down>" . next-line)
   ("<up>" . previous-line)
   :map w3m-lynx-like-map
   ("S-<right>" . w3m-view-this-url-new-session)))

;;; Mu4e
;;
(use-package mu4e
  :config
  (progn (require 'mu4e-config)
         (addressbook-turn-on-mail-completion))
  :commands (mu4e compose-mail)
  :bind ("<f8>" . mu4e))

;;; Auth-source
;;
(use-package auth-source
  :no-require t
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

;;; esh-toggle
;;
(use-package esh-toggle
  :commands (eshell-toggle-cd eshell-toggle)
  :bind (("<f11> e c" . eshell-toggle-cd)
         ("<f11> e t" . eshell-toggle)))

;;; Whitespace-mode
;;
(use-package whitespace
  :commands 'whitespace-mode
  :config (progn
            (add-to-list 'whitespace-style 'lines-tail)
            (setq whitespace-line-column 80))
  :bind ("C-c W" . whitespace-mode))

;;; align-let
;;
(use-package align-let
  :commands 'align-let-keybinding
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'align-let-keybinding)
    (add-hook 'lisp-interaction-mode-hook 'align-let-keybinding)
    (add-hook 'lisp-mode-hook 'align-let-keybinding))
  :config
  (progn
    (put 'setq-local 'align-let 'setq)
    (put 'cl-psetq 'align-let 'setq)
    (put 'helm-set-local-variable 'align-let 'setq))
  :disabled t)

;;; sqlite-dump
;;
(use-package sqlite-dump
  :commands 'sqlite-dump
  :config (progn
            (modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
            (add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))
            (setq sql-sqlite-program "sqlite3")))

;;; Checkdoc
;;
(use-package checkdoc-batch
  :commands (checkdoc-batch checkdoc-batch-files))

;;; markdown-mode
;;
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mdpp$" . markdown-mode)))

(use-package ffap
  :config
  ;; See issue #2003 in helm
  (setq ffap-url-unwrap-remote '("ftp" "file"))
  (when (> emacs-major-version 24)
    ;; See issue #1716 in helm.
    (setcdr (assq 'file ffap-string-at-point-mode-alist)
            '("--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))))

;;; Eshell-config
;;
(use-package eshell
  :commands (eshell eshell-command)  
  :init
  (progn
    ;; Eshell-prompt
    (setq eshell-prompt-function
          (lambda nil
            (let ((pwd (eshell/pwd)))
              (with-temp-buffer
                (let* ((default-directory (file-name-as-directory pwd))
                       (proc (process-file
                              "git" nil t nil
                              "symbolic-ref" "HEAD" "--short"))
                       (id (propertize (if (= (user-uid) 0) " # " " $ ")
                                       'face 'default))
                       detached branch status)
                  (unless (= proc 0)
                    (erase-buffer)
                    (setq detached t)
                    (setq proc (process-file
                                "git" nil t nil
                                "rev-parse" "--short" "HEAD")))
                  (if (= proc 0)
                      (progn
                        (setq branch (replace-regexp-in-string
                                      "\n" "" (buffer-string)))
                        (erase-buffer)
                        (setq proc (process-file
                                    "git" nil t nil "status" "--porcelain"))
                        (setq status (pcase (buffer-string)
                                       ((and str (guard (and (not (string= str ""))
                                                             (= proc 0))))
                                        (if (string-match "\\`[?]" str) "?" "*"))
                                       (_ "")))
                        (format "%s@%s:%s(%s%s)%s"
                                (getenv "USER") (system-name)
                                (propertize (abbreviate-file-name pwd) 'face 'italic)
                                (propertize (format
                                             "%s%s"
                                             (if detached "detached@" "")
                                             branch)
                                            'face '((:foreground "red")))
                                (propertize status
                                            'face `((:foreground
                                                     ,(if (string= "?" status)
                                                          "OrangeRed" "gold1"))))
                                id))
                    (format "%s@%s:%s%s"
                            (getenv "USER") (system-name)
                            (propertize (abbreviate-file-name pwd) 'face 'italic)
                            id)))))))
    (setq eshell-password-prompt-regexp
      "\\(\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|[Mm]\\(?:ot de passe\\|ật khẩu\\)\\|pa\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\).*:.*\\'")

    ;; Compatibility 24.2/24.3
    (unless (or (fboundp 'eshell-pcomplete)
                (>= emacs-major-version 27))
      (defalias 'eshell-pcomplete 'pcomplete))
    (unless (or (fboundp 'eshell-complete-lisp-symbol)
                (>= emacs-major-version 27))
      (defalias 'eshell-complete-lisp-symbol 'lisp-complete-symbol))

    (add-hook 'eshell-mode-hook (lambda ()
                                  (setq eshell-pwd-convert-function (lambda (f)
                                                                      (if (file-equal-p (file-truename f) "/")
                                                                          "/" f)))
                                  ;; This is needed for eshell-command (otherwise initial history is empty).
                                  (eshell-read-history eshell-history-file-name)
                                  ;; Helm completion with pcomplete
                                  (setq eshell-cmpl-ignore-case t
                                        eshell-hist-ignoredups t)
                                  (eshell-cmpl-initialize)
                                  (unless (>= emacs-major-version 27)
                                    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                    ;; Helm lisp completion
                                    (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
                                    ;; Helm completion on eshell history.
                                    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
                                  ;; Eshell prompt
                                  (set-face-attribute 'eshell-prompt nil :foreground "Green")))

    ;; Eshell history size
    (setq eshell-history-size 1000) ; Same as env var HISTSIZE.

    ;; Eshell-banner
    (setq eshell-banner-message "")

    ;; Eshell-et-ansi-color
    (ignore-errors
      (dolist (i (list 'eshell-handle-ansi-color
                       'eshell-handle-control-codes
                       'eshell-watch-for-password-prompt))
        (add-to-list 'eshell-output-filter-functions i)))

    ;; Eshell-save-history-on-exit
    ;; Possible values: t (always save), 'never, 'ask (default)
    (setq eshell-save-history-on-exit t)

    ;; Eshell-directory
    (setq eshell-directory-name "/home/thierry/.emacs.d/eshell/")

    ;; Eshell-visual
    (setq eshell-term-name "eterm-color")
    (with-eval-after-load "em-term"
      (dolist (i '("tmux" "htop" "ipython" "alsamixer" "git-log" "w3mman"))
        (add-to-list 'eshell-visual-commands i)))
    ;; Eshell modifiers
    (with-eval-after-load "em-pred"
      (defun tv/advice--eshell-pred-substitute (&optional repeat)
        "Return a modifier function that will substitute matches."
        (let ((delim (char-after))
	      match replace end)
          (forward-char)
          (setq end (eshell-find-delimiter delim delim nil nil t)
	        match (buffer-substring-no-properties (point) end))
          (goto-char (1+ end))
          (setq end (or (eshell-find-delimiter delim delim nil nil t) (point))
	        replace (buffer-substring-no-properties (point) end))
          (goto-char (1+ end))
          (if repeat
	      (lambda (lst)
	        (mapcar
	         (lambda (str)
	           (let ((i 0))
		     (while (setq i (string-match match str i))
		       (setq str (replace-match replace t nil str))))
	           str)
                 lst))
            (lambda (lst)
	      (mapcar
               (lambda (str)
	         (if (string-match match str)
		     (setq str (replace-match replace t nil str)))
	         str)
               lst)))))
      ;; Allow empty string in substitution e.g. echo foo.el(:gs/.el//)
      (advice-add 'eshell-pred-substitute :override #'tv/advice--eshell-pred-substitute)
      ;; Fix echo, perhaps using as alias *echo is even better.
      (setq eshell-plain-echo-behavior t)))
  :bind ("C-!" . eshell-command))

;;; linum-relative
;;
(use-package linum-relative
    :disabled t
    :commands (linum-relative-mode
               helm-linum-relative-mode
               linum-relative-toggle
               linum-relative-global-mode))

(use-package display-line-numbers
    :commands (display-line-numbers-mode
               global-display-line-numbers-mode)
    :config
    (setq display-line-numbers-type 'relative))

;;; Outline-mode
;;
(use-package outline
  :defer t
  :requires helm
  :config
  (progn
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-p")
                                  ?p 'outline-previous-visible-heading
                                  '((?n . outline-next-visible-heading)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-n")
                                  ?n 'outline-next-visible-heading
                                  '((?p . outline-previous-visible-heading)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-f")
                                  ?f 'outline-forward-same-level
                                  '((?b . outline-backward-same-level)))
    (helm-define-key-with-subkeys outline-mode-map (kbd "C-c C-b")
                                  ?b 'outline-backward-same-level
                                  '((?f . outline-forward-same-level)))))

;;; Flyspell
;;
(use-package ispell
  :init
  (progn
    (setq-default ispell-program-name "aspell")
    (setq ispell-local-dictionary "francais"))
  :config
  (progn
    (defun tv/toggle-flyspell (arg)
      "Toggle `flyspell-mode'." 
      (interactive "P")
      (if (and flyspell-mode (null arg))
	  (progn
	    (flyspell-mode -1)
	    (message "Flyspell Mode disabled"))
        (flyspell-mode 1)
        (unwind-protect
            (progn
              (when (fboundp 'helm-autoresize-mode)
                (helm-autoresize-mode 1))
              (let ((dic (completing-read "Dictionary: " '("english" "francais"))))
                (ispell-change-dictionary dic)
                (flyspell-delete-all-overlays)
                (message "Starting new Ispell process aspell with %s dictionary..." dic)))
          (when (fboundp 'helm-autoresize-mode)
            (helm-autoresize-mode -1))))))
  :bind ("C-c @" . tv/toggle-flyspell))

;;; Webjump
;;
(use-package webjump
  :bind ("<f7> j" . webjump))

;;; Semantic
;;
(use-package semantic
  :config
  (progn
    (add-hook 'semantic-mode-hook
              ;; My patched lisp/cedet/semantic/bovine/el.el.
              (lambda ()
                (load-file "~/elisp/el.el")
                (when (fboundp 'semantic-default-elisp-setup)
                  (semantic-default-elisp-setup))))
    (semantic-mode 1))
  :disabled t)

;;; Which function
;;
(use-package which-func
  :commands 'which-function
  :config
  (progn
    (defun tv/which-func ()
      (interactive)
      (message "[%s]" (which-function))))
  :bind (:map emacs-lisp-mode-map
              ("C-c ?" . tv/which-func)))

;;; Shell
;;
(use-package shell
  :requires helm
  :config
  (progn
    (defun comint--advice-send-eof (&rest _args)
      (let ((win (selected-window)))
        (kill-buffer) (delete-window win)))
    (advice-add 'comint-send-eof :after 'comint--advice-send-eof))
  :bind (("<f11> s h" . shell)
         :map shell-mode-map
         ("M-p" . helm-comint-input-ring)))

;;; Ielm
;;
(use-package ielm
  :bind ("<f11> i" . ielm))

;;; Elisp/lisp
;;
(use-package lisp-mode
  :config
  (progn
    ;; Try to have same indentation in both 24, 25 and 26.
    (defun tv/common-lisp-indent-function (indent-point state)
      (if (save-excursion (goto-char (elt state 1))
                          (looking-at "(cl-\\([Ll][Oo][Oo][Pp]\\)"))
          (common-lisp-indent-function-1 indent-point state)
        (lisp-indent-function indent-point state)))
    
    ;; Fix indentation in CL loop.
    (setq lisp-indent-function #'tv/common-lisp-indent-function
          lisp-simple-loop-indentation 1
          lisp-loop-keyword-indentation 6
          lisp-loop-forms-indentation 6)

    (defun goto-scratch ()
      (interactive)
      (switch-to-buffer "*scratch*"))

    ;; Fix indentation in cl-flet and cl-labels
    (use-package cl-indent
      :config (let ((l '((flet ((&whole 4 &rest (&whole 1 &lambda &body)) &body))
                         (cl-flet* . flet)
                         (labels . flet)
                         (cl-flet . flet)
                         (cl-labels . flet)
                         (cl-macrolet . flet)
                         )))
                (dolist (el l)
                  (put (car el) 'common-lisp-indent-function
                       (if (symbolp (cdr el))
                           (get (cdr el) 'common-lisp-indent-function)
                         (car (cdr el)))))))

    ;; Add fontification to some functions
    (cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
      (font-lock-add-keywords
       mode
       '(("(\\<\\(cl-dolist\\)\\>" 1 font-lock-keyword-face))))

    (defvar tv/autofill-modes '(emacs-lisp-mode
                                lisp-interaction-mode
                                sh-mode))
    (defun tv/point-in-comment-p (pos)
      "Returns non-nil if POS is in a comment."
      (eq 'comment (syntax-ppss-context (syntax-ppss pos))))

    (defun tv/point-in-docstring-p (pos)
      "Returns non-nil if POS is in a docstring."
      (and (eq 'string (syntax-ppss-context (syntax-ppss pos)))
           (eq (get-text-property (point) 'face) 'font-lock-doc-face)))

    (defun tv/turn-on-auto-fill-mode-maybe ()
      "Enable auto-fill-mode only in comments or docstrings.
Variable adaptive-fill-mode is disabled when a docstring field is detected."
      (when (memq major-mode tv/autofill-modes)
        (let ((in-docstring (tv/point-in-docstring-p (point))))
          (setq adaptive-fill-mode (not in-docstring))
          (auto-fill-mode
           (if (or (tv/point-in-comment-p (point))
                   in-docstring)
               1 -1)))))
    ;; Maybe turn on auto-fill-mode when a comment or docstring field
    ;; is detected. Ensure the hook is appended otherwise things like
    ;; eldoc-eval will not work.
    (add-hook 'post-command-hook #'tv/turn-on-auto-fill-mode-maybe t))

  :bind (("<f11> s c" . goto-scratch)
         ("<S-f12>" . cancel-debug-on-entry)
         :map
         emacs-lisp-mode-map
         ("RET" . newline-and-indent)
         ("C-c C-c b" . byte-compile-file)
         ("<next>" . forward-page)
         ("<prior>" . backward-page)
         ("C-M-j" . backward-kill-sexp)
         ("C-x C-m e" . pp-macroexpand-last-sexp)
         :map
         lisp-interaction-mode-map
         ("RET" . newline-and-indent)
         ("C-M-j" . backward-kill-sexp)
         ("C-x C-m e" . pp-macroexpand-last-sexp)
         :map
         lisp-mode-map
         ("RET" . newline-and-indent)))

;;; face-remap - font size <C-fn-up/down>.
;;
(use-package face-remap
  :bind (("<C-prior>" . text-scale-decrease)
         ("<C-next>" . text-scale-increase)))

;;; Elp instrument
;;
(use-package elp
  :config
  (progn
    (defun tv/advice-elp-results (old--fn &rest args)
      (let ((inhibit-read-only t))
        (with-current-buffer (if elp-recycle-buffers-p
                                 (get-buffer-create elp-results-buffer)
                               (generate-new-buffer elp-results-buffer))
          (special-mode)
          (apply old--fn args)))))
  (advice-add 'elp-results :around 'tv/advice-elp-results))

;;; Org toc for github
;;
(use-package toc-org
  :commands (toc-org-insert-toc)  
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-enable))

;;; Powerline
;;
(use-package powerline
  :config
  (progn
    (use-package helm-ls-git)
    ;; Will appear in face0 once helm-ls-git is loaded
    (defpowerline powerline-git
      (when (and (buffer-file-name (current-buffer))
                 (fboundp 'helm-ls-git--branch)
                 (helm-ls-git-root-dir))
        (if (and window-system (not powerline-gui-use-vcs-glyph))
            (format " Git:%s" (format-mode-line '(:eval (helm-ls-git--branch))))
          (format " %s %s"
                  (char-to-string #x21af) ; Needs a one line height char.
                  (format-mode-line '(:eval (helm-ls-git--branch)))))))

    (setq powerline-gui-use-vcs-glyph t)
    
    (defun tv/powerline-default-theme ()
      "Setup the default mode-line."
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                              (mode-line (if active 'mode-line 'mode-line-inactive))
                              (face0 (if active 'powerline-active0 'powerline-inactive0))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left (intern (format "powerline-%s-%s"
                                                              (powerline-current-separator)
                                                              (car powerline-default-separator-dir))))
                              (separator-right (intern (format "powerline-%s-%s"
                                                               (powerline-current-separator)
                                                               (cdr powerline-default-separator-dir))))
                              (lhs (list (powerline-raw mode-line-remote face0 'l)
                                         (powerline-raw "%*" face0 'l)
                                         (when powerline-display-buffer-size
                                           (powerline-buffer-size face0 'l))
                                         (when powerline-display-mule-info
                                           (powerline-raw mode-line-mule-info face0 'l))
                                         (powerline-buffer-id mode-line-buffer-id 'l)
                                         (when (and (boundp 'which-func-mode) which-func-mode)
                                           (powerline-raw which-func-format nil 'l))
                                         (powerline-raw " " face0)
                                         ;; (funcall separator-left face0 face1)
                                         (powerline-raw "%4l" face1 'l)
                                         (powerline-raw ":" face1 'l)
                                         (powerline-raw "%3c" face1 'r)
                                         ;; (funcall separator-left face1 face0)
                                         (powerline-raw " " face0)
                                         (powerline-raw "%6p" face0 'l)
                                         ;; (funcall separator-left face0 face1)
                                         (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                           (powerline-raw erc-modified-channels-object face1 'l))
                                         (powerline-major-mode face1 'l)
                                         (powerline-process face1)
                                         (powerline-minor-modes face1 'l)
                                         (powerline-narrow face1 'l)
                                         (powerline-raw " " face1)
                                         ;; (funcall separator-left face1 face2)
                                         (powerline-git face2 'r)
                                         (when (bound-and-true-p nyan-mode)
                                           (powerline-raw (list (nyan-create)) face2 'l))))
                              (rhs (list (powerline-raw global-mode-string face2 'r)
                                         ;; (funcall separator-right face2 face1)
                                         (unless window-system
                                           (powerline-raw (char-to-string #xe0a1) face1 'l))
                                         (when powerline-display-hud
                                           (powerline-hud face2 face1)))))
                         (concat (powerline-render lhs)
                                 (powerline-fill face2 (powerline-width rhs))
                                 (powerline-render rhs)))))))
    (tv/powerline-default-theme)
    (global-set-key [mode-line mouse-1] 'ignore)
    (global-set-key [mode-line mouse-2] 'ignore)
    (global-set-key [mode-line mouse-3] 'ignore)
    (setq mode-line-default-help-echo nil)
    (add-hook 'focus-in-hook 'force-mode-line-update))
  :ensure t)

;;; Rectangle edit
;;
(use-package rectangle-edit :commands 'rectangle-edit)

;;; Bash-completion
;;
(use-package bash-completion
  :commands 'bash-completion-dynamic-complete
  :init
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  :ensure t)

;;; Pcmpl-git (For Eshell)
;;
;; Seems that bash-completion and pcmpl can cohabit.
;; No subcommands completion with pcmpl in eshell though.
(use-package pcmpl-git :ensure t)

;;; Log-view
;;
(use-package log-view
  :config
  (defun tv/log-view-fontify ()
    (font-lock-add-keywords nil '(("^revision [0-9.]*" . font-lock-comment-face)
                                  ("[a-zA-Z ]*:" . font-lock-type-face))))
  (add-hook 'log-view-mode-hook 'tv/log-view-fontify))

;;; Wgrep
;;
(use-package wgrep-helm
  :config (setq wgrep-enable-key "\C-x\C-q"))

;;; Slime
;;
;; (use-package slime
;;     :ensure t
;;     :init
;;   (progn
;;     (setq inferior-lisp-program "/usr/bin/sbcl"
;;           slime-net-coding-system 'utf-8-unix
;;           slime-contribs '(slime-fancy)
;;           slime-scratch-file "~/.emacs.d/slime-scratch.lisp")
;;     ;; common-lisp-info
;;     (require 'cl-info)
;;     (add-to-list 'Info-additional-directory-list "~/elisp/info/gcl-info/"))
;;     :bind (("<f11> l r" . slime)
;;            ("<f11> l s" . slime-scratch)
;;            ("<f11> l l" . helm-slime-list-connections)
;;            :map slime-repl-mode-map
;;            ("C-i" . helm-slime-complete)
;;            :map slime-scratch-mode-map
;;            ("C-i" . helm-slime-complete)))

;;; Geiser
;;
;; (use-package geiser
;;     :config
;;     (setq geiser-completion--module-list-func
;;           (completion-table-dynamic 'geiser-completion--module-list)
;;           geiser-completion--symbol-list-func
;;           (completion-table-dynamic 'geiser-completion--symbol-list)))

;;; Sly
(use-package sly
  :disabled t
  :ensure t
  :config
  (setq sly-completing-read-function 'completing-read)
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-hook 'sly-mode-hook (lambda () (sly-symbol-completion-mode -1))))
  
;;; psession
;;
(use-package psession
  :config
  (psession-savehist-mode 1)
  (psession-mode 1)
  (psession-autosave-mode 1)
  (bind-key "C-x p s" 'psession-save-winconf)
  (bind-key "C-x p d" 'psession-delete-winconf)
  (bind-key "C-x p j" 'psession-restore-winconf))

;;; Gnus
;;
(use-package gnus
  :disabled t  
  :config
  (setq gnus-init-file "~/.emacs.d/.gnus")
  :bind ("<f7> m" . gnus))

;;; Undo-tree
;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (defun git-gutter:undo-tree-undo (&rest _args)
    (when git-gutter-mode
      (run-with-idle-timer 0.1 nil 'git-gutter)))
  (advice-add 'undo-tree-undo :after 'git-gutter:undo-tree-undo)
  (advice-add 'undo-tree-redo :after 'git-gutter:undo-tree-undo)

  (global-undo-tree-mode 1))


;; Kill buffer and windows
(defun tv/kill-buffer-and-windows (arg)
  "Kill current-buffer and delete its window.
With a prefix arg ask with completion which buffer to kill."
  (interactive "P")
  (let* ((buffer (if arg
                     (read-buffer "Kill buffer: " (current-buffer) t)
                   (current-buffer)))
         (windows (get-buffer-window-list buffer nil t)))
    (when (kill-buffer buffer)
      (dolist (win windows)
        (when (window-live-p win)
          (ignore-errors (delete-window win)))))))
(helm-define-key-with-subkeys global-map (kbd "C-x k") ?k 'tv/kill-buffer-and-windows)

;; Fix issue with the new :extend face attribute in emacs-27
;; Prefer to extend to EOL as in previous emacs.
(defun tv/extend-faces-matching (regexp)
  (cl-loop for f in (face-list)
           for face = (symbol-name f)
           when (and (string-match regexp face)
                     (eq (face-attribute f :extend t 'default)
                         'unspecified))
           do (set-face-attribute f nil :extend t)))

(when (fboundp 'set-face-extend)
  (with-eval-after-load "magit"
    (tv/extend-faces-matching "\\`magit"))
  (with-eval-after-load "helm"
    (tv/extend-faces-matching "\\`helm")))

;; Fix unreadable diff/ediff in emacs-27
(when (>= emacs-major-version 27)
  (with-eval-after-load 'diff-mode
    (set-face-attribute 'diff-refine-added nil :background 'unspecified)
    (set-face-attribute 'diff-refine-removed nil :background 'unspecified)
    (set-face-attribute 'diff-refine-changed nil :background 'unspecified))
  (with-eval-after-load 'ediff-init
    (set-face-attribute 'ediff-fine-diff-A nil :background 'unspecified)
    (set-face-attribute 'ediff-fine-diff-B nil :background 'unspecified))
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil :extend t))
  (set-face-attribute 'region nil :extend t))

;; diff buffers read-only
(setq diff-default-read-only t)

;; Link now scratch buffer to file
(tv/restore-scratch-buffer)

;;; init.el ends here
