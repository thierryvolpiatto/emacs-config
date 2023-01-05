;;; describe-variable.el --- Advice describe-variable

;;; Code:

(require 'help-fns)
(require 'pp)

(defun tv/pp (object &optional stream)
  (let ((fn (lambda (ob &optional stream)
              (princ (pp-to-string ob)
                     (or stream standard-output))
              (terpri)))
        (print-quoted t)
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
      (funcall fn object stream))))

;; This is the Emacs-28.2 version of `describe-variable' but with the
;; inefficient block of code using `pp-buffer' replaced with tv/pp.
;; With advice:
;; (benchmark-run 1 (describe-variable 'load-history))
;; (0.938096707 0 0.0)
;; Without advice:
;; (benchmark-run 1 (describe-variable 'load-history))
;; (12.380366787 0 0.0)
(defun tv/describe-variable (variable &optional buffer frame)
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
                    (tv/pp val)
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
                      (tv/pp origval)
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
		      (let ((from (point)))
                        (tv/pp global-val)
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
