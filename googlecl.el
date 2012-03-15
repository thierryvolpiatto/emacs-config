;;; googlecl.el - elisp UI for googlecl commands.

;;; Code:

(defvar google-db-file "~/.emacs.d/elisp-objects/gpicasa-album-list.el")
(defun google-create-album (dir)
  (interactive (list (helm-c-read-file-name "Directory: "
                                                :test 'file-directory-p
                                                :initial-input "/home/thierry/Pictures/")))
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

(defun google-post-image-to-album (arg)
  (interactive "P")
  (when arg (google-update-album-list-db))
  (let ((comp-file (concat google-db-file "c")))
    (while (not (file-exists-p comp-file)) (sit-for 0.1))
    (unless gpicasa-album-list (load-file comp-file)))
  (lexical-let ((album (helm-comp-read "Album: " (loop for i in gpicasa-album-list collect (car i))))
                (file  (helm-c-read-file-name "File: " :initial-input "~/Pictures")))
    (start-process-shell-command
     "gpicasa-post" nil
     (format "google picasa post --src %s %s"
             (shell-quote-argument file)
             album))
    (set-process-sentinel (get-process "gpicasa-post")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (message "`%s' pushed to `%s'" file album))))))


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
