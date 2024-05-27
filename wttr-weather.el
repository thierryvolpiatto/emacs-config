;;; wttr-weather.el --- wttr.in weather report. -*- lexical-binding: t -*- 
;;

;;; Code:

(require 'ansi-color)

(defvar wttr-weather-history nil)
(defvar wttr-weather-default-location "Guillestre")
(defvar wttr-weather-last-location nil)

;;;###autoload
(defun wttr-weather (place)
  "Weather forecast with wttr.in.
With a prefix arg refresh buffer if some.
See <https://github.com/chubin/wttr.in>."
  (interactive (list (read-string (format "Place (%s): "
                                          wttr-weather-default-location)
                                  nil
                                  'wttr-weather-history
                                  wttr-weather-default-location)))
  (let ((buf (get-buffer-create (format "*wttr.in %s*" place))))
    (switch-to-buffer buf)
    (when current-prefix-arg
      (set (make-local-variable 'wttr-weather-last-location) nil))
    (unless wttr-weather-last-location
      (wttr-weather-update place)
      (wttr-weather-mode)
      (set (make-local-variable 'wttr-weather-last-location) place))))

(defun wttr-weather-update (place)
  (let* ((inhibit-read-only t)
         ansi
         (data
          (with-temp-buffer
            (call-process
             "curl" nil '(t t) nil
             "-s" (format "fr.wttr.in/%s?m" (replace-regexp-in-string " " "+" place)))
            (goto-char (point-min))
            ;; Try to replace 256 colors seq like this
            ;; "\033[38;5;226m" => "\033[33m" or sequence ending with
            ;; ;5m (animated) which Emacs-28 replace by a crapy box.
            ;; "\033[38;5;228;5m" => "\033[33m".
            ;; Thanks to Jim Porter for explanations in Emacs bug#54774.
            (while (re-search-forward "\\(38;5;\\([0-9]+\\);?[0-9]?\\)m" nil t)
              ;; If we have ansi sequences, that's mean we had weather
              ;; output, otherwise we have a simple message notifying
              ;; weather report is not available.
              (setq ansi t)
              ;; Need a 256 color ansi library, emacs supports only basic
              ;; ansi colors as now, so replace all 38;5 foreground
              ;; specs by simple ansi sequences.
              ;; Emacs-29 supports 256 color but still have bad support for
              ;; animated ansi sequences, so better use basic colors.
              (replace-match (pcase (match-string 2)
                               ("190"            "31")  ;; red
                               ((or "118" "154") "32")  ;; green
                               ((or "226" "228") "33")  ;; yellow
                               ("202"            "34")  ;; blue
                               ("214"            "35")  ;; magenta
                               ((or "220" "111") "36")  ;; cyan
                               ("208"            "37")  ;; white
                               (_                "0"))  ;; Avoid box face
                             t t nil 1))
            (ansi-color-apply (buffer-string)))))
    (erase-buffer)
    (save-excursion
      (if data
          (insert data)
        ;; Probaly check error status instead (it is 52).
        (insert "Empy reply from server"))
      (forward-line -1)
      (when (and ansi ; Keep notification when no weather report.
                 (re-search-backward "^$" nil t))
        (delete-region (point) (point-max))))
    (while (re-search-forward "\\s\\" (pos-eol) t) (replace-match ""))
    (goto-char (pos-eol))
    (insert (format-time-string " le %d/%m/%Y à %H:%M:%S"))))

(defun wttr-weather-revert-fn (_ignore-auto _no_confirm)
  (wttr-weather-update wttr-weather-last-location))

(define-derived-mode wttr-weather-mode
    special-mode "wttr"
    "Mode used to display wttr-weather buffer."
  (make-local-variable 'wttr-weather-last-location)
  (set (make-local-variable 'revert-buffer-function) 'wttr-weather-revert-fn))
(put 'wttr-weather-mode 'no-helm-mx t)

(provide 'wttr-weather)

;;; wttr-weather.el ends here
