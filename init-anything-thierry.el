;;; init-anything-thierry.el --- My startup file for anything. 
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; URL: 
;; Keywords: 
;; Compatibility: 
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
;; 
;;; Commentary: 
;; 
;; Personal configuration to start anything.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'anything-config)

;;;; Test Sources or new anything code. 
;;   !!!WARNING EXPERIMENTAL!!!
;;

;;;; Extensions

;;;; Mercurial-Qpatchs
(require 'anything-mercurial)

;;;; Delicious-bookmarks-tv
(require 'anything-delicious)

;;;; anything-ipython
;;
;;
;; (require 'anything-ipython)

;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))

;; (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)
;; (define-key py-shell-map (kbd "M-<tab>") 'anything-ipython-complete)
;; (define-key py-mode-map (kbd "C-c M") 'anything-ipython-import-modules-from-buffer)


;;;; Anything-faces

(defface anything-tv-header '((t (:background "#22083397778B" :foreground "white" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

;;; Anything-command-map
;;
;;
(define-key anything-command-map (kbd "q")   'anything-qpatchs)
(define-key anything-command-map (kbd "v")   'anything-eev-anchors)
(define-key anything-command-map (kbd "d")   'anything-delicious)
(define-key anything-command-map (kbd "y e") 'anything-yaoddmuse-emacswiki-edit-or-view)
(define-key anything-command-map (kbd "y p") 'anything-yaoddmuse-emacswiki-post-library)
(define-key anything-command-map (kbd "g")   'anything-apt)


;;; Global-map
;;
;;
(global-set-key (kbd "M-x") 'anything-M-x)
(global-set-key (kbd "M-y") 'anything-show-kill-ring)
(global-set-key (kbd "C-c f") 'anything-recentf)
(global-set-key (kbd "C-x C-f") 'anything-find-files)
(global-set-key (kbd "C-c <SPC>") 'anything-all-mark-rings)
(global-set-key (kbd "C-x r b") 'anything-bookmark-ext) ; Replace regular bookmark binding.
(global-set-key (kbd "C-h r")   'anything-info-emacs)
(global-set-key (kbd "C-c C-b") 'anything-browse-code)
(global-set-key (kbd "C-:")     'anything-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")     'anything-calcul-expression)
(global-set-key (kbd "C-c h f") 'anything-info-at-point)
(global-set-key (kbd "C-c g")   'anything-google-suggest)
(global-set-key (kbd "C-c y")   'anything-yahoo-suggest)
(global-set-key (kbd "M-g s")   'anything-do-grep)
(define-key global-map [remap insert-register] 'anything-register)
(define-key global-map [remap list-buffers] 'anything-buffers-list)

;; Lisp complete or indent.
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'anything-lisp-completion-at-point-or-indent)
(define-key emacs-lisp-mode-map [remap indent-for-tab-command] 'anything-lisp-completion-at-point-or-indent)

;; lisp complete.
(define-key lisp-interaction-mode-map [remap completion-at-point] 'anything-lisp-completion-at-point)
(define-key emacs-lisp-mode-map [remap completion-at-point] 'anything-lisp-completion-at-point)

;;; Describe-key-bindings
;;
;;
;(require 'descbinds-anything)
;(descbinds-anything-install)            ; C-h b, C-x C-h

;; My-anything-describe-keybindings
(require 'anything-describe-keybindings)
(anything-describe-bindings-mode 1)

;;; Anything-variables
;;
;;
(setq anything-google-suggest-use-curl-p            t
      anything-kill-ring-threshold                  1
      anything-raise-command                        "wmctrl -xa %s"
      ;anything-allow-skipping-current-buffer        nil
      anything-yaoddmuse-use-cache-file             t
      anything-scroll-amount                        1
      anything-candidate-number-limit               100
      anything-quick-update                         t
      anything-candidate-separator                  (propertize (make-string 42 ?-) 'face '((:foreground "red")))
      anything-header-face                          'anything-tv-header
      anything-selection-face                       '((:background "ForestGreen"))
      anything-idle-delay                           0.1
      anything-input-idle-delay                     0.1
      anything-ff-avfs-directory                    "~/.avfs"
      anything-ff-history-max-length                100
      anything-su-or-sudo                           "sudo"
      anything-c-kill-ring-max-lines-number         5
      anything-ff-transformer-show-only-basename    t
      anything-c-default-external-file-browser      "thunar"
      ;anything-c-pdfgrep-default-read-command       "evince --page-label=%p '%f'"
      ;anything-surfraw-default-browser-function     'tv-browse-url-w3m
      anything-c-etags-use-regexp-search            nil
      anything-c-use-adaptative-sorting             t
      ;anything-save-configuration-functions         '(set-frame-configuration . current-frame-configuration)
      ;anything-c-google-suggest-default-browser-function 'browse-url-uzbl
      )

;;; Debugging
;;
;;
(defun anything-switch-to-log ()
  (interactive)
  (switch-to-buffer "*Anything Log*"))

(defun anything-toggle-debug ()
  (interactive)
  (setq anything-debug (not anything-debug))
  (message "Anything Debug is now %s" (if anything-debug "Enabled" "Disabled")))

;;; Enable ac-mode
;;
;;
(ac-mode 1)

(provide 'init-anything-thierry)

;;; init-anything-thierry.el ends here
