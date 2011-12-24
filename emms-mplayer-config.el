;;; emms-mplayer-config.el --- 
;; 
;; Filename: emms-mplayer-config.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: lun jan 12 08:42:23 2009 (+0100)
;; Version: 
;; Last-Updated: mar fév 24 14:41:15 2009 (+0100)
;;           By: thierry
;;     Update #: 28
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Code:

;; «Basic-config» (to ".Basic-config")


(require 'emms-setup)
(require 'emms-player-vlc)
(emms-minimalistic)
(emms-devel)

(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-source-file-default-directory "~/mpd/music")
(setq emms-setup-default-player-list '(emms-player-mplayer
                                       emms-player-mplayer-playlist
                                       emms-player-vlc
                                       emms-player-mpg321
                                       emms-player-ogg123
                                       ))

(emms-default-players)

;; «enable-emms-scoring» (to ".enable-emms-scoring")
(setq emms-score-enabled-p t)

;; «Start-browser-with-album» (to ".Start-browser-with-album")
(setq emms-browser-default-browse-type 'info-album)

;; «default-action-for-bookmark-streams» (to ".default-action-for-bookmark-streams")
(setq emms-stream-default-action "play")

;; «Album-covers» (to ".Album-covers")

;;pour avoir des images dans le browser
;;(image par defaut si il n'y a pas 
;;d'image nommée cover_small.jpg dans le dossier de l'album)
(setq emms-browser-default-covers
      (list "/home/thierry/mpd/covers/cover_small.jpg" nil nil))

;; «Scroll-settings» (to ".Scroll-settings")
(setq scroll-up-aggressively 0.0)
(setq scroll-down-aggressively 0.0)

;; «Browser-filters» (to ".Browser-filters")

;; «show-all-files-(no-streamlists,-etc)» (to ".show-all-files-(no-streamlists,-etc)")
(emms-browser-make-filter
 "all-files" (emms-browser-filter-only-type 'file))

;; «show-only-tracks-in-one-folder» (to ".show-only-tracks-in-one-folder")
;; (emms-browser-make-filter
;;  "Sky_radios" (emms-browser-filter-only-dir "~/mpd/playlists"))

;; «Mode-line» (to ".Mode-line")
;(require 'emms-mode-line)
(emms-mode-line 1)

;; «Bindings» (to ".Bindings")

(global-set-key (kbd "<f6> r") 'emms-streams)
(global-set-key (kbd "<f6> +") 'emms-volume-raise)
(global-set-key (kbd "<f6> -") 'emms-volume-lower)
(global-set-key (kbd "<f6> b") 'emms-smart-browse)
(global-set-key (kbd "<f6> s") 'emms-stop)
(global-set-key (kbd "<f6> \r") 'emms-start)
(global-set-key (kbd "<f6> c") 'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> d") 'emms-mode-line-toggle)
(global-set-key (kbd "<f6> <right>") 'emms-next)
(global-set-key (kbd "<f6> <left>") 'emms-previous)


(defun tv-emms-update-and-clean-cache ()
  (interactive)
  (and emms-cache-db
       (clrhash emms-cache-db))
  (ignore-errors
    (delete-file "~/.emacs.d/emms/emms-cache")
    (delete-file "~/.emacs.d/emms/emms-history"))
  (emms-add-directory-tree "~/mpd/music"))

(provide 'emms-mplayer-config)


;;; emms-mplayer-config.el ends here
