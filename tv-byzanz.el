;;; tv-byzanz.el --- Record Emacs screencast with byzanz. -*- lexical-binding: t -*- 
;;

;;; Code:

(defgroup byzanz-record nil "Record screencast."
          :group 'multimedia)

;;;###autoload
(defun byzanz-record (file)
  "Record a screencast with byzanz."
  (interactive "FRecord to file: ")
  (let* ((height  (number-to-string (+ (frame-pixel-height)
                                       ;; minibuf+mode-line
                                       40)))
         (width   (number-to-string (frame-pixel-width)))
         (process (start-process "byzanz" "*byzanz log*"
                                 "byzanz-record"
                                 "--exec=sleep 1000000"
                                 "--delay=5"
                                 "-c" "-w" width "-h" height file)))
    (byzanz-record-mode 1)
    (set-process-sentinel
     process (lambda (_proc event)
               (when (string= event "finished\n")
                 (byzanz-record-mode -1)
                 (message "Screencast recorded to `%s'" file))))))

(defun byzanz-record-stop ()
  "Stop byzanz recording.
Don't bind this to global-map but to `byzanz-record-mode-map' instead."
  (interactive)
  (call-process "killall" nil nil nil "sleep"))

(defvar byzanz-record-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<f12>") 'byzanz-record-stop)
    map))

(define-minor-mode byzanz-record-mode
    "A minor mode to stop byzanz-record."
  :group 'byzanz-record
  :global t
  (message "Byzanz started recording, hit `S-<f12>' to stop"))
(put 'byzanz-record-mode 'no-helm-mx t)

(provide 'tv-byzanz)

;;; tv-byzanz.el ends here
