;;; emms-vlc-config.el --- config for emms using vlc.

;;; Code:

(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-source-file-default-directory "/home/thierry/Musique")

(emms-devel)
(emms-default-players)

(setq emms-player-list '(emms-player-vlc-playlist
                         emms-player-vlc
                         emms-player-mplayer
                         emms-player-mpg321
                         emms-player-ogg123))

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
(setq emms-mode-line-format " %s")
(setq emms-playing-time-display-format " %s]")
(defun emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (format emms-mode-line-format
          (propertize "Emms playing"
                      'help-echo
                      (emms-track-description
                       (emms-playlist-current-selected-track)))))

(defun tv-emms-mode-line-icon-function ()
  (setq emms-mode-line-icon-image-cache
        `(image :type xpm :ascent center :data ,(concat "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c " emms-mode-line-icon-color  "\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\"};")))
  (concat " "
          emms-mode-line-icon-before-format
          (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-playlist-current)))

(setq emms-mode-line-mode-line-function 'tv-emms-mode-line-icon-function)

(emms-mode-line 1)

;; «Bindings» (to ".Bindings")

(global-set-key (kbd "<f6> r")  'emms-streams)
(global-set-key (kbd "<f6> +")  'emms-volume-raise)
(global-set-key (kbd "<f6> -")  'emms-volume-lower)
(global-set-key (kbd "<f6> b")  'emms-smart-browse)
(global-set-key (kbd "<f6> t")  'emms-player-mpd-show)
(global-set-key (kbd "<f6> s")  'emms-stop)
(global-set-key (kbd "<f6> RET")'emms-start)
(global-set-key (kbd "<f6> c")  'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> p")  'emms-pause)
(global-set-key (kbd "<f6> >")  'emms-next)
(global-set-key (kbd "<f6> <")  'emms-previous)
(global-set-key (kbd "<XF86AudioNext>")  'emms-next)
(global-set-key (kbd "<XF86AudioPrev>")  'emms-previous)
(global-set-key (kbd "<XF86AudioPlay>")  'emms-pause)
(global-set-key (kbd "<f6> m")  'emms-mode-line-toggle)

;; «Update-mpd-directory» (to ".Update-mpd-directory")

(defun tv-emms-update-and-clean-cache ()
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


(provide 'emms-vlc-config)

;;; emms-vlc-config.el ends here
