;;; emms-alsaplayer-config.el --- 
;; 
;; Author: 
;; Maintainer: 
;; 
;; Created: mar. mars 24 15:37:17 2009 (+0100)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;; (tv-ee-index-create) 

;;;;«INDEX»
;;; «.basic-config»                          (to "basic-config")
;;; «.require»                               (to "require")
;;; «.player-list»                           (to "player-list")
;;; «.conf-alsaplayer»                       (to "conf-alsaplayer")
;;; «.config-mplayer»                        (to "config-mplayer")
;;; «.reload-emms-cache»                     (to "reload-emms-cache")
;;; «.format-par-defaut-playlists»           (to "format-par-defaut-playlists")
;;; «.dossier-musique-par-defaut»            (to "dossier-musique-par-defaut")
;;; «.pour-avoir-des-images-dans-le-browser» (to "pour-avoir-des-images-dans-le-browser")
;;; «.show-all-files-                        (no-streamlists,-etc)» (to "show-all-files-(no-streamlists,-etc)")
;;; «.only-stream-playlists»                 (to "only-stream-playlists")
;;; «.affichage-d'infos-dans-la-modeline»    (to "affichage-d'infos-dans-la-modeline")
;;; «.keys-bindings-pour-emms»               (to "keys-bindings-pour-emms")
;;; «.bookmark-streams-»                     (to "bookmark-streams-")
;;; «.All-my-code-patched-in-emms»           (to "All-my-code-patched-in-emms")
;;; «.Completion-for-artists»                (to "Completion-for-artists")
;;; «.Save-artist-as-favorite-artist»        (to "Save-artist-as-favorite-artist")
;;; «.Completion-on-global-tags»             (to "Completion-on-global-tags")
;;; «.Lastfm-config»                         (to "Lastfm-config")
;;; «.provide»                               (to "provide")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; «basic-config» (to ".basic-config")
(add-to-list 'load-path "~/elisp/emms/")
(add-to-list 'load-path "~/elisp/emms/lisp/")
(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")

;; «require» (to ".require")
(require 'emms-auto)
(require 'emms-setup)
(emms-devel)
(emms-default-players)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-streams)
(require 'emms-info)

;; «player-list» (to ".player-list")
(setq emms-player-list (list 'emms-player-alsaplayer
                             'emms-player-mplayer
                             'emms-player-ogg123))

;; «conf-alsaplayer» (to ".conf-alsaplayer")
(define-emms-simple-player alsaplayer
    '(file url playlist streamlist)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".flac" ".pls" ".m3u" "http://"))
  "alsaplayer"
  "--quiet" "--nosave")

;; «config-mplayer» (to ".config-mplayer")
(define-emms-simple-player mplayer '(file url streamlist)
  (regexp-opt '("mms://" ".ogg" ".mp3" ".wav" ".flac" ".pls" ".m3u" "http://"))
  "mplayer" "-nocache") 

;; «reload-emms-cache» (to ".reload-emms-cache")
(defun tv-emms-update-and-clean-cache ()
  (interactive)
  (clrhash emms-cache-db)
  (delete-file "~/.emacs.d/emms/emms-cache")
  (delete-file "~/.emacs.d/emms/emms-history")
  (emms-add-directory-tree "~/mpd/music"))

;; «format-par-defaut-playlists» (to ".format-par-defaut-playlists")
(setq emms-source-playlist-default-format "pls")

;; «dossier-musique-par-defaut» (to ".dossier-musique-par-defaut")
(setq emms-default-music-directory "~/mpd/music")

;; «pour-avoir-des-images-dans-le-browser» (to ".pour-avoir-des-images-dans-le-browser")
;;(image par defaut si il n'y a pas 
;;d'image nommée cover_small.jpg dans le dossier de l'album)
(setq emms-browser-default-covers
      (list "/home/thierry/mpd/covers/cover_small.jpg" nil nil))

;; «show-all-files-(no-streamlists,-etc)» (to ".show-all-files-(no-streamlists,-etc)")
(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))

;; «only-stream-playlists» (to ".only-stream-playlists")
;; (emms-browser-make-filter
;;  "sky-radios" (emms-browser-filter-only-dir "~/playlists/"))

;; «affichage-d'infos-dans-la-modeline» (to ".affichage-d'infos-dans-la-modeline")
(require 'emms-mode-line)
(emms-mode-line 1)
(require 'emms-playing-time)
(emms-playing-time 1)

(defun tv-stop-mpd ()
  (interactive)
  (if emms-player-playing-p
      (emms-stop)
    (shell-command "alsaplayer --stop")))

;; «keys-bindings-pour-emms» (to ".keys-bindings-pour-emms")
(global-set-key (kbd "<f6> r") 'emms-streams)
(global-set-key (kbd "<f6> +") 'emms-volume-raise)
(global-set-key (kbd "<f6> -") 'emms-volume-lower)
(global-set-key (kbd "<f6> b") 'emms-smart-browse)
(global-set-key (kbd "<f6> s") 'emms-stop)
(global-set-key (kbd "<f6> c") 'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> l b") 'emms-lastfm-radio-ban)
(global-set-key (kbd "<f6> l l") 'emms-lastfm-radio-love)
(global-set-key (kbd "<f6> l a") 'emms-play-lastfm-similar-artists)
(global-set-key (kbd "<f6> l g") 'emms-play-lastfm-tag)

;; «bookmark-streams-» (to ".bookmark-streams-")
(setq emms-stream-default-action "play")

;; «All-my-code-patched-in-emms» (to ".All-my-code-patched-in-emms")
;; «Completion-for-artists» (to ".Completion-for-artists")

;; «Save-artist-as-favorite-artist» (to ".Save-artist-as-favorite-artist")
(defvar emms-favorites-artists-file "~/.emacs.d/emms/emms-favourites-artists.el")
(defvar emms-favorites-artists nil)
(defun emms-add-artist-to-favorites ()
  "Save artist as favorite artist"
  (interactive)
  (if (file-exists-p emms-favorites-artists-file)
      (load-file emms-favorites-artists-file))
  (let* ((orig-list (emms-lastfm-read-artist t))
         (new-artist (car orig-list)))
    (if (and (not (member new-artist emms-favorites-artists))
             (not (member new-artist (cdr orig-list))))
        (progn
          (save-excursion
            (find-file emms-favorites-artists-file)
            (goto-char (point-min))
            (erase-buffer)
            (insert "(setq emms-favorites-artists '(")
            (add-to-list 'emms-favorites-artists new-artist) 
            (dolist (i emms-favorites-artists)
              (insert (concat "\"" i "\"" "\n")))
            (insert "))\n\n ;;; end-of-file")
            (indent-region (point-min) (point-max))
            (save-buffer)
            (kill-buffer (current-buffer))
            (load-file emms-favorites-artists-file)
            (message "<%s> added to your favorites artist"
                     (propertize new-artist
                                 'face 'emms-playlist-track-face))))
        (message "This artist is already bookmarked"))))
  

(defun emms-lastfm-read-artist (&optional onlylist)
  "Read an artist name from the user."
  (let ((artists nil))
    (when (boundp 'emms-cache-db)
      (maphash
       #'(lambda (file track)
           (let ((artist (emms-track-get track 'info-artist)))
             (when artist
               (add-to-list 'artists artist))))
       emms-cache-db))
    (if onlylist
        artists
        (if artists
            (progn
              (when (file-exists-p emms-favorites-artists-file)
                (load emms-favorites-artists-file)
                (when emms-favorites-artists
                  (setq artists (append emms-favorites-artists (cdr artists)))))
              (emms-completing-read "Artist: " artists))
            (read-string "Artist: ")))))

;; «Completion-on-global-tags» (to ".Completion-on-global-tags")

(defvar emms-tag-file "~/.emacs.d/emms/emms-globals-tags.el")
(defvar emms-lastfm-global-tag-list nil)
(defvar emms-tags-url "http://ws.audioscrobbler.com/2.0/?method=tag.getTopTags&")
(defvar emms-tags-apikey nil
  "Put here your apikey you can get it on the website of lastfm
http://www.lastfm.fr/api/show?service=276
after subscribe and login to this service")

(defun emms-retrieve-global-tags ()
  "Fetch a list of tags from lastfm"
  (url-retrieve-synchronously
   (concat
   emms-tags-url
   emms-tags-apikey)))

(defun emms-lastfm-global-tags-list (buffer)
  "Parse the html buffer and create a list of tags"
  (with-current-buffer (get-buffer buffer)
    (goto-char (point-min))
    (let ((list-lines nil)
          (end (point-max))
          (bufstr (buffer-string))
          (list-word nil)
          (kword))
      (setq list-lines (split-string bufstr "\n"))
      (dolist (i list-lines)
        (when (string-match "<name>[a-zA-Z0-9]+</name>" i)
          (setq kword (replace-regexp-in-string "<name>"
                                                ""
                                                (match-string 0 i)))
          (setq kword (replace-regexp-in-string "</name>"
                                                ""
                                                kword))
          (push kword list-word)))
      list-word)))

(defun emms-setup-global-tag-file (file)
  "Create a file-cache containing all the lastfm global tags"
  (interactive "sFile: ")
  (message "Wait retrieving list from lastfm...")
  (sit-for 2)
  (let ((buf (emms-retrieve-global-tags))
        (list-tags))
    (setq list-tags (emms-lastfm-global-tags-list buf))
    (save-excursion
      (find-file file)
      (erase-buffer)
      (goto-char (point-min))
      (insert "(setq emms-lastfm-global-tag-list '(")
      (dolist (i list-tags)
        (insert (concat "\"" i "\"" "\n")))
      (insert "))\n\n;;; end of file")
      (indent-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (load file))

(defun emms-play-lastfm-tag (tag)
  (interactive (list (emms-completing-read "MusicType: "
                                           (if (not emms-lastfm-global-tag-list)
                                               (if (file-exists-p emms-tag-file)
                                                   (progn
                                                     (load emms-tag-file)
                                                     emms-lastfm-global-tag-list)
                                                   (emms-setup-global-tag-file emms-tag-file)
                                                   emms-lastfm-global-tag-list)
                                               emms-lastfm-global-tag-list))))
  (emms-lastfm-radio (concat "lastfm://globaltags/" tag)))

;; «Lastfm-config» (to ".Lastfm-config")

(setq emms-lastfm-auth
      (auth-source-user-or-password  '("login" "password")
                                     "ws.audioscrobbler.com:80"
                                     "http"))

(setq emms-lastfm-username (car emms-lastfm-auth))
(setq emms-lastfm-password (cadr emms-lastfm-auth))

(setq emms-tags-apikey "api_key=b25b959554ed76058ac220b7b2e0a026")
;;(setq emms-lastfm-submission-verbose-p t)
;;(emms-lastfm-enable)


;; «provide» (to ".provide")
(provide 'emms-alsaplayer-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emms-alsaplayer-config.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
