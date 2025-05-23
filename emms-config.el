;;; emms-config.el --- emms config. -*- lexical-binding: t -*-

;;; Code:

(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-source-file-default-directory "/home/thierry/Musique")

;; Needed when installing from source.
(require 'emms-auto nil t)

;; Add lexbind cookie to emms-cache to shutup warnings in emacs-31.
(with-eval-after-load 'emms-cache
  (defun tv:advice-emms-cache-save (old--fn)
    (let ((header (concat ";;; .emms-cache -*- mode: emacs-lisp; coding: "
                          (symbol-name emms-cache-file-coding-system)
                          "; lexical-binding: t -*-\n"))) 
      (funcall old--fn)
      (with-current-buffer (find-file-noselect emms-cache-file)
        (goto-char (point-min))
        (delete-region (point) (pos-eol))
        (insert header)
        (save-buffer)
        (kill-buffer))))
  (advice-add 'emms-cache-save :around #'tv:advice-emms-cache-save))

(emms-all)

;; Setup `emms-player-list'.
(setq emms-player-list '(emms-player-mpv emms-player-vlc emms-player-vlc-playlist))
;; (emms-default-players)

(setq emms-player-mpv-parameters
      ;; "--no-video" for mp4's.
      '("--no-video" "--no-terminal" "--force-window=no" "--audio-display=no"))

;; «enable-emms-scoring» (to ".enable-emms-scoring")
(setq emms-score-enabled-p t)

;; «Start-browser-with-album» (to ".Start-browser-with-album")
(setq emms-browser-default-browse-type 'info-album)

;; «default-action-for-bookmark-streams» (to ".default-action-for-bookmark-streams")
(setq emms-stream-default-action "play")

(add-to-list 'emms-info-functions 'emms-info-mp3info)
(setq emms-browser-default-covers '("~/.emacs.d/emms/cover_small.jpg"))

;; «Mode-line» (to ".Mode-line")
(setq emms-mode-line-icon-color "Gold1")
(setq emms-mode-line-icon-before-format "[")
(setq emms-mode-line-format " `%s'")
(setq emms-playing-time-display-format " %s] ")
(defun emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (let* ((track (emms-playlist-current-selected-track))
         (cur-track (emms-track-description track))
         (all (emms-info-track-description (emms-playlist-current-selected-track))))
    (format emms-mode-line-format
            (propertize (truncate-string-to-width cur-track 20 nil nil "⃨")
                        'face 'font-lock-type-face
                        'help-echo all))))

(defun tv:emms-mode-line-icon-function ()
  (let* ((pls  (emms-mode-line-playlist-current))
         (icon (if (string-match "\\` *[`]http://" pls) "📻" "🎵")))
    (concat " " emms-mode-line-icon-before-format icon pls)))

(setq emms-mode-line-mode-line-function 'tv:emms-mode-line-icon-function)

(emms-mode-line 1)

(defun tv:emms-volume--pulse-get-volume ()
  (with-temp-buffer
    (call-process "pactl" nil t nil "list" "sinks")
    (goto-char (point-min))
    (when (re-search-forward "^[\t ]*Volume.?:.*/ *\\([0-9]*\\)% */" nil t)
      (string-to-number (match-string 1)))))
(advice-add 'emms-volume--pulse-get-volume :override #'tv:emms-volume--pulse-get-volume)
(setq emms-volume-change-function #'emms-volume-pulse-change)

;; «Bindings» (to ".Bindings")

(global-set-key (kbd "<f6> r")  'emms-streams)
(helm-define-key-with-subkeys
    global-map (kbd "<f6> +")
    ?+ 'emms-volume-raise '((?- . emms-volume-lower)))
(helm-define-key-with-subkeys
    global-map (kbd "<f6> -")
    ?- 'emms-volume-lower '((?+ . emms-volume-raise)))
(global-set-key (kbd "<f6> b")  'emms-smart-browse)
(global-set-key (kbd "<f6> s")  'emms-stop)
(global-set-key (kbd "<f6> RET")'emms-start)
(global-set-key (kbd "<f6> c")  'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> p")  'emms-pause)
(global-set-key (kbd "<f6> >")  'emms-next)
(global-set-key (kbd "<f6> <")  'emms-previous)
(global-set-key (kbd "<f6> m")  'emms-mode-line-toggle)

;; «Update-mpd-directory» (to ".Update-mpd-directory")

(defun tv:emms-update-and-clean-cache ()
  (interactive)
  (when emms-cache-db
    (clrhash emms-cache-db)
    (and (file-exists-p emms-cache-file)
         (delete-file emms-cache-file))
    (and (file-exists-p emms-history-file)
         (delete-file emms-history-file))
    (with-current-buffer (find-file-noselect emms-cache-file)
      (save-buffer))
    (emms-add-directory-tree "~/Musique/")))


(defun tv:emms-track-simple-description (track)
  "Simple function to give a user-readable description of a track.
If it's a file track, just return the file name.  Otherwise,
return the type and the name with a colon in between.
Hex-encoded characters in URLs are replaced by the decoded
character."
  (let ((type (emms-track-type track)))
    (cond ((eq 'file type)
           (file-name-sans-extension
            (file-name-nondirectory (emms-track-name track))))
          ((eq 'url type)
           (emms-format-url-track-name (emms-track-name track)))
          (t (concat (symbol-name type)
                     ": " (emms-track-name track))))))
(setq emms-track-description-function 'tv:emms-track-simple-description)

;; Switch to xfce presentation mode
(defun tv:emms-xfce-presentation-mode-1 (val)
  (call-process "xfconf-query" nil nil nil
                "xfconf-query" "-c" "xfce4-power-manager"
                "-p" "/xfce4-power-manager/presentation-mode"
                "-s" val))

(defun tv:emms-player-start-hook ()
  (tv:emms-xfce-presentation-mode-1 "true"))

(defun tv:emms-player-stop-hook ()
  (tv:emms-xfce-presentation-mode-1 "false"))

(add-hook 'emms-player-started-hook 'tv:emms-player-start-hook)
(add-hook 'emms-player-stopped-hook 'tv:emms-player-stop-hook)
(add-hook 'emms-player-finished-hook 'tv:emms-player-stop-hook)

;; Fix error:
;; emms-info-native error processing
;; http://europe1.lmn.fm/europe1.mp3:
;; (file-missing Opening input file Aucun fichier ou dossier de ce nom /home/thierry/.emacs.d/emacs-config/http:/europe1.lmn.fm/europe1.mp3)
(defun tv:advice-emms-info-native--find-stream-type (filename)
  (unless (string-match-p "\\`http" filename)
    (pcase (file-name-extension filename)
      ("ogg" 'vorbis)
      ("opus" 'opus)
      ("flac" 'flac)
      ("mp3" 'mp3)
      ("spc" 'spc)
      (_ nil))))
(advice-add 'emms-info-native--find-stream-type :override #'tv:advice-emms-info-native--find-stream-type)

(provide 'emms-config)

;;; emms-config.el ends here
