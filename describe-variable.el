;;; describe-variable.el --- Advice describe-variable -*- lexical-binding: t -*-

;;; Code:

(require 'help)
(require 'help-fns)
(require 'pp)

(defvar describe-variable--offset-value 800)

(defun tv/pp (object &optional stream)
  (let ((fn (lambda (ob &optional stream)
              (princ (pp-to-string ob)
                     (or stream standard-output))
              (terpri)))
        (print-quoted t)
        (print-circle t)
        prefix suffix map-fn looping
        (standard-output (or stream (current-buffer))))
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
        (with-current-buffer standard-output
          (insert prefix)
          (funcall map-fn fn object)
          (cl-letf (((point) (1- (point))))
            (insert suffix)))
      (funcall fn object stream))))

(defun tv/pp-value-in-help ()
  (interactive)
  (let ((inhibit-read-only t)
        (sym (save-excursion (goto-char (point-min)) (symbol-at-point)))
        beg end)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Value: ?$" nil t)
        (forward-line 1)
        (setq beg (point))
        (setq end (save-excursion
                    (goto-char (point-max))
                    (button-start (previous-button (point) t))))))
    (when (and beg end)
      (message "Prettifying value...")
      (goto-char beg)
      (delete-region beg end)
      (tv/pp (symbol-value sym) (current-buffer))
      (message "Prettifying value done"))))

(defun tv/describe-variable (variable &optional buffer frame)
  "Optimized `describe-variable' version.
It is based on emacs-28.2 version of `describe-variable'.
Large values are not pretty printed."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
         val)
     (setq val (completing-read
                (format-prompt "Describe variable" (and (symbolp v) v))
                #'help--symbol-completion-table
                (lambda (vv)
                  (or (get vv 'variable-documentation)
                      (and (not (keywordp vv))
                           ;; Since the variable may only exist in the
                           ;; original buffer, we have to look for it
                           ;; there.
                           (buffer-local-boundp vv orig-buffer))))
                t nil nil
                (if (symbolp v) (symbol-name v))))
     (list (if (equal val "")
               v (intern val)))))
  (let (file-name)
    (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
    (unless (frame-live-p frame) (setq frame (selected-frame)))
    (if (not (symbolp variable))
        (user-error "You didn't specify a variable")
      (save-excursion
        (let ((valvoid (not (with-current-buffer buffer (boundp variable))))
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

              (princ (if file-name
                         (progn
                           (princ (format-message
                                   " is a variable defined in `%s'.\n\n"
                                   (if (eq file-name 'C-source)
                                       "C source code"
                                     (help-fns-short-filename file-name))))
                           (with-current-buffer standard-output
                             (setq help-mode--current-data
                                   (list :symbol variable
                                         :type (if (eq file-name 'C-source)
                                                   'variable
                                                 'defvar)
                                         :file file-name))
                             (save-excursion
                               (re-search-backward (substitute-command-keys
                                                    "`\\([^`']+\\)'")
                                                   nil t)
                               (help-xref-button 1 'help-variable-def
                                                 variable file-name)))
                           (if valvoid
                               "It is void as a variable."
                             "Its "))
                       (with-current-buffer standard-output
                         (setq help-mode--current-data (list :symbol variable
                                                             :type 'variable)))
                       (if valvoid
                           " is void as a variable."
                         (substitute-command-keys "'s ")))))
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
                    (let ((buf (current-buffer)))
                      (with-temp-buffer
                        (lisp-mode-variables nil)
                        (set-syntax-table emacs-lisp-mode-syntax-table)
                        (insert print-rep)
                        (unless (> (- (point-max) (point-min)) describe-variable--offset-value)
                          (pp-buffer))
                        (let ((pp-buffer (current-buffer)))
                          (with-current-buffer buf
                            (insert-buffer-substring pp-buffer)))))
                    ;; Remove trailing newline.
                    (and (= (char-before) ?\n) (delete-char -1)))
                  (let* ((sv (get variable 'standard-value))
                         (origval (and (consp sv)
                                       (condition-case nil
                                           (eval (car sv) t)
                                         (error :help-eval-error))))
                         from)
                    (when (and (consp sv)
                               (not (equal origval val))
                               (not (equal origval :help-eval-error)))
                      (princ "\nOriginal value was \n")
                      (setq from (point))
                      (if (and (symbolp origval) (not (booleanp origval)))
                          (let* ((rep (cl-prin1-to-string origval))
                                 (print-rep (format-message "`%s'" rep)))
                            (insert print-rep))
                        (cl-prin1 origval))
                      (save-restriction
                        (narrow-to-region from (point))
                        (unless (> (- (point) from) describe-variable--offset-value)
                          (save-excursion (pp-buffer))))
                      (if (< (point) (+ from 20))
                          (delete-region (1- from) from)))))))
            (terpri)
            (when locus
              (cond
               ((bufferp locus)
                (princ (format "Local in buffer %s; "
                               (buffer-name buffer))))
               ((terminal-live-p locus)
                (princ "It is a terminal-local variable; "))
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
                      ;; FIXED:
                      ;; Fixme: pp can take an age if you happen to
                      ;; ask for a very large expression.  We should
                      ;; probably print it raw once and check it's a
                      ;; sensible size before prettyprinting.  -- fx
                      (let ((from (point)))
                        (cl-prin1 global-val)
                        (save-restriction
                          (narrow-to-region from (point))
                          (unless (> (- (point) from) describe-variable--offset-value)
                            (save-excursion (pp-buffer))))
                        ;; See previous comment for this function.
                        ;; (help-xref-on-pp from (point))
                        (if (< (point) (+ from 20))
                            (delete-region (1- from) from)))))))
              (terpri))

            ;; If the value is large, move it to the end.
            (with-current-buffer standard-output
              (when (or (> (- (point-max) (point-min)) describe-variable--offset-value)
                        (> (count-lines (point-min) (point-max)) 10))
                ;; Note that setting the syntax table like below
                ;; makes forward-sexp move over a `'s' at the end
                ;; of a symbol.
                (set-syntax-table emacs-lisp-mode-syntax-table)
                (goto-char val-start-pos)
                (when (looking-at "value is") (replace-match ""))
                (save-excursion
                  (insert "\n\nValue:")
                  (setq-local help-button-cache (point-marker)))
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
                   (doc (or (documentation-property
                             variable 'variable-documentation)
                            (documentation-property
                             alias 'variable-documentation))))

              (with-current-buffer standard-output
                (insert (or doc "Not documented as a variable.")))

              ;; Output the indented administrative bits.
              (with-current-buffer buffer
                (help-fns--run-describe-functions
                 help-fns-describe-variable-functions variable))

              (with-current-buffer standard-output
                ;; If we have the long value of the variable at the
                ;; end, remove superfluous empty lines before it.
                (unless (eobp)
                  (while (looking-at-p "\n")
                    (delete-char 1)))))

            (with-current-buffer standard-output
              ;; Return the text we displayed.
              (buffer-string))))))))


(advice-add 'describe-variable :override #'tv/describe-variable)

(provide 'describe-variable)

;;; describe-variable.el ends here
