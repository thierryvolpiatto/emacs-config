;;; googlecl.el - elisp UI for googlecl commands.

;;; Code:

(defvar google-db-file "~/.emacs.d/elisp-objects/gpicasa-album-list.el")

(defun google-create-album-1 (dir)
  (assert (and (file-directory-p dir)
               (not (file-symlink-p dir)))
          nil (format "Error: `%s' is not a directory" dir))
  (lexical-let ((album (car (last (split-string dir "/")))))
    (message "Syncing `%s' album to google..." album)
    (start-process-shell-command
     "googlecl" nil
     (format "google picasa create --title %s %s*.jpg"
             album
             (file-name-as-directory dir)))
    (set-process-sentinel (get-process "googlecl")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (message "Syncing `%s' album to google done." album))))))

(defun google-create-album (dir)
  (interactive (list (helm-c-read-file-name "Directory: "
                                                :test 'file-directory-p
                                                :initial-input "/home/thierry/Pictures/")))
  (google-create-album-1 dir))

(defvar gpicasa-album-list nil)
(defun google-update-album-list-db ()
  (setq gpicasa-album-list nil)
  (let ((comp-file (concat google-db-file "c")))
    (when (file-exists-p comp-file) (delete-file comp-file)))
  (lexical-let ((album-list ()))
    (start-process-shell-command "gpicasa-list" nil "google picasa list-albums")
    (set-process-filter (get-process "gpicasa-list")
                        #'(lambda (process output)
                            (setq album-list (cons output album-list))))
    (set-process-sentinel (get-process "gpicasa-list")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (loop for album in (split-string (car album-list) "\n" t)
                                   collect (split-string album ",") into ls-album
                                   finally do
                                   (progn
                                     (setq gpicasa-album-list ls-album)
                                     (dump-object-to-file 'gpicasa-album-list google-db-file))))))))
                                       
(defun google-insert-link-to-album-at-point (arg)
  (interactive "P")
  (when arg (google-update-album-list-db))
  (let ((comp-file (concat google-db-file "c")))
    (while (not (file-exists-p comp-file)) (sit-for 0.1))
    (unless gpicasa-album-list (load-file comp-file)))
  (let ((album (helm-comp-read "Album: " gpicasa-album-list)))
    (insert (car album))))

(defun google-post-image-to-album-1 (file album)
  (start-process-shell-command
   "gpicasa-post" nil
   (format "google picasa post --src %s %s"
           (shell-quote-argument file)
           album))
  (lexical-let ((album-dir album)
                (fname     file)) 
    (set-process-sentinel (get-process "gpicasa-post")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (message "`%s' pushed to `%s'" fname album-dir))))))


(defun google-post-image-to-album (arg)
  (interactive "P")
  (when arg (google-update-album-list-db))
  (let ((comp-file (concat google-db-file "c"))
        (album (helm-comp-read "Album: " (loop for i in gpicasa-album-list collect (car i))))
        (file  (helm-c-read-file-name "File: " :initial-input "~/Pictures")))
    (while (not (file-exists-p comp-file)) (sit-for 0.1))
    (unless gpicasa-album-list (load-file comp-file))
    (google-post-image-to-album-1 file album)))

(defun google-ls-album (arg)
  (interactive "P")
  (when arg (google-update-album-list-db))
  (let ((comp-file (concat google-db-file "c")))
    (while (not (file-exists-p comp-file)) (sit-for 0.1))
    (unless gpicasa-album-list (load-file comp-file)))
  (let* ((album (helm-comp-read "Album: " (mapcar 'car gpicasa-album-list)))
         (ls-album (with-temp-buffer
                     (apply #'call-process "google" nil t nil
                            (list "picasa" "list" album))
                     (loop for a in (split-string (buffer-string) "\n" t)
                           for line = (split-string a "," t)
                           collect (cons (car line) (cadr line))))))
    (helm :sources '((name . "Google albums")
                     (candidates . ls-album)
                     (action . (("View online" . browse-url)
                                ("copy link to kill-ring" . kill-new)))
                     (persistent-action . browse-url)))))


(defun google-post-document ()
  (interactive)
  (lexical-let ((document (helm-c-read-file-name "Document: ")))
    (start-process-shell-command "google-docs-post" nil
                                 (format "google docs upload --src %s"
                                         (shell-quote-argument document)))
    (set-process-sentinel (get-process "google-docs-post")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (message "`%s' pushed to google document done" document))))))

(provide 'googlecl)

;;; googlecl.el ends here.
