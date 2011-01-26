;;; emms-mpd-config.el --- 
;; 
;; Filename: emms-mpd-config.el
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

;;; Code:

;; «Basic-config» (to ".Basic-config")

(setq emms-score-file "~/.emacs.d/emms/scores")
(setq emms-stream-bookmarks-file "~/.emacs.d/emms/emms-streams")
(setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-source-file-default-directory "~/mpd/music")

(when (require 'emms-setup nil t)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-streams)
  (require 'emms-info)
  (emms-devel)
  (emms-default-players)
  (require 'emms-mode-line)
  (require 'emms-player-mpd))

;; «Disable-Last-Fm» (to ".Disable-Last-Fm")
;(emms-lastfm-disable)

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

;; «Time-elapsed-in-current-track» (to ".Time-elapsed-in-current-track")
;;(require 'emms-playing-time)
;;(emms-playing-time 1)

;; «MPD-config» (to ".MPD-config")
(require 'emms-player-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-music-directory emms-source-file-default-directory)
(add-to-list 'emms-info-functions 'emms-info-mpd)


(setq emms-player-mpd-supported-regexp
      (concat "\\(file\\|http\\|lastfm\\|mms\\|mmsh\\|mmst|mmsu\\):.*"
              (mapconcat 'regexp-quote '("mp3" "mp2" "ogg" "oga" "wav"
                                         "au" "aiff" "aif" "aac" "amf"
                                         "dsm" "far" "gdm" "imf" "it"
                                         "med" "mod" "mtm" "s3m" "stm"
                                         "stx" "ult" "uni" "xm" "16sv"
                                         "3g2" "3gp" "4xm" "8svx" "aa3"
                                         "aac" "ac3" "afc" "aif" "aifc"
                                         "aiff" "al" "alaw" "amr" "anim"
                                         "apc" "ape" "asf" "asx" "atrac"
                                         "au" "aud" "avi" "avm2" "avs" "bap"
                                         "bfi" "c93" "cak" "cin" "cmv"
                                         "cpk" "daud" "dct" "divx"
                                         "dts" "dv" "dvd" "dxa" "eac3"
                                         "film" "flac" "flc" "fli"
                                         "fll" "flx" "flv" "g726"
                                         "gsm" "gxf" "iss" "m1v" "m2v"
                                         "m2t" "m2ts" "m4a" "m4v" "mad"
                                         "mj2" "mjpeg" "mjpg" "mka" "mkv"
                                         "mlp" "mm" "mmf" "mov" "mp+" "mp1"
                                         "mp2" "mp3" "mp4" "mpc" "mpeg"
                                         "mpg" "mpga" "mpp" "mpu" "mve"
                                         "mvi" "mxf" "nc" "nsv" "nut" "nuv"
                                         "oga" "ogm" "ogv" "ogx" "oma" "ogg"
                                         "omg" "psp" "pva" "qcp" "qt" "r3d"
                                         "ra" "ram" "rl2" "rm" "rmvb" "roq"
                                         "rpl" "rvc" "shn" "smk" "snd" "sol"
                                         "son" "spx" "str" "swf" "tgi" "tgq"
                                         "tgv" "thp" "ts" "tsp" "tta" "xa"
                                         "xvid" "uv" "uv2" "vb" "vid" "vob"
                                         "voc" "vp6" "vmd" "wav" "wma" "wmv"
                                         "wsaud" "wsvga" "wv" "wve")
                         "\\|")))


;; «Amix-and-PCM» (to ".Amix-and-PCM")
;;j'utilise PCM avec amix pour mon volume
(setq emms-volume-amixer-control "PCM")


;; «stop-mpd» (to ".stop-mpd")
(defun tv-stop-mpd ()
  (interactive)
  (if emms-player-playing-p
      (emms-stop)
    (shell-command "mpc stop")))

;; «Bindings» (to ".Bindings")

(global-set-key (kbd "<f6> r") 'emms-streams)
(global-set-key (kbd "<f6> +") 'emms-volume-raise)
(global-set-key (kbd "<f6> -") 'emms-volume-lower)
(global-set-key (kbd "<f6> b") 'emms-smart-browse)
(global-set-key (kbd "<f6> t") 'emms-player-mpd-show)
(global-set-key (kbd "<f6> s") 'tv-stop-mpd)
(global-set-key (kbd "<f6> \r") 'emms-start)
(global-set-key (kbd "<f6> c") 'emms-browser-clear-playlist)
(global-set-key (kbd "<f6> p") 'emms-player-mpd-pause)
(global-set-key (kbd "<f6> >") 'emms-player-mpd-next)
(global-set-key (kbd "<f6> <") 'emms-player-mpd-previous)
(global-set-key (kbd "<f6> d") 'emms-mode-line-toggle)

;; «Update-mpd-directory» (to ".Update-mpd-directory")

(defun tv-emms-update-and-clean-cache ()
  (interactive)
  (clrhash emms-cache-db)
  (delete-file "~/.emacs.d/emms/emms-cache")
  (delete-file "~/.emacs.d/emms/emms-history")
  (emms-add-directory-tree "~/mpd/music"))


(provide 'emms-mpd-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emms-mpd-config.el ends here
