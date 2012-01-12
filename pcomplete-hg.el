;; Time-stamp: <2012-01-11 07:09:23 thierry>

(defun* pcomplete-get-hg-commands (&key com opts spec)
  (with-temp-buffer
    (let (h1 h2 h3)
      (if spec
          (progn
            (apply #'call-process "hg" nil t nil
                   (list "-v" "help" spec))
            (setq h3 (buffer-string)))
          (call-process "hg" nil t nil
                        "-v" "help")
          (setq h1 (buffer-string))
          (erase-buffer)
          (apply #'call-process "hg" nil t nil
                 (list "-v" "help" "mq"))
          (setq h2 (buffer-string)))
      (erase-buffer)
      (if spec (insert h3) (insert (concat h1 h2)))
      (goto-char (point-min))
      (let (args coms sargs)
        (if spec
            (setq sargs (loop while (re-search-forward "\\(-[a-zA-Z]\\|--[a-zA-Z]+\\) *" nil t)
                              collect (match-string 1)))
            (save-excursion
              (setq coms
                    (loop while (re-search-forward "^ \\([a-z]+\\) *" nil t)
                          collect (match-string 1))))
            (setq args
                  (loop while (re-search-forward "\\(-[a-zA-Z]\\|--[a-zA-Z]+\\) *" nil t)
                        collect (match-string 1))))
        (cond (spec sargs)
              (com coms)
              (opts args)
              (t (list args coms)))))))

(defvar pcomplete-hg-commands-cache nil)
(defvar pcomplete-hg-glob-opts-cache nil)
(defun pcomplete/hg ()
  (let* ((cur      (pcomplete-arg 'first))
         (avder    (pcomplete-arg 0))
         (last     (pcomplete-arg 'last))
         (all      (pcomplete-get-hg-commands))
         (commands (or pcomplete-hg-commands-cache
                       (setq pcomplete-hg-commands-cache
                             (cadr all))))
         (options  (or pcomplete-hg-glob-opts-cache
                       (setq pcomplete-hg-glob-opts-cache
                             (car all))))
         (special  (pcomplete-get-hg-commands :spec avder)))
    (cond ((and (string-match-p "-" last)
                (member avder commands))
           (while (pcomplete-here special)))
          ((string-match-p "-" last)
           (pcomplete-here options))
          ((and (string= cur "hg")
                (not (string= cur avder)))
           (pcomplete-here commands)))
    (while (pcomplete-here (pcomplete-entries) nil 'identity))))
          
(provide 'pcomplete-hg)

;;; pcomplete-hg.el ends here
