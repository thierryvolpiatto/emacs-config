;;; jabber-config.el --- 
;; 
;; Filename: jabber-config.el
;; Description: 
;; Author: thierry
;; Maintainer: 
;; Created: ven jan 16 13:55:13 2009 (+0100)
;; Version: 
;; Last-Updated: ven jan 16 13:55:29 2009 (+0100)
;;           By: thierry
;;     Update #: 1
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
;; 
;;; Code:

(require 'jabber)

(defvar emacs-jabber-user nil)
(defvar emacs-jabber-password nil)
(defun emacs-jabber-authentify ()
  "Authentify user from .authinfo file.
`path' is the path of you .authinfo file, usually ~/.authinfo."
  (let ((emacs-jabber-auth
         (auth-source-user-or-password  '("login" "password")
                                        "smtp.gmail.com"
                                        "587")))
    (when emacs-jabber-auth
      (setq emacs-jabber-user (car emacs-jabber-auth)
            emacs-jabber-password (cadr emacs-jabber-auth))
      nil)))

(defun tv-load-jabber ()
  (interactive)
  (emacs-jabber-authentify)
  ;; setup account
  ;; config for 0.8 (multi account support)
  (setq jabber-account-list
        `((,(format "%s/emacs" emacs-jabber-user)
           (:password . ,emacs-jabber-password))))

  ;; setup connection
  (setq jabber-network-server "talk.google.com"
        jabber-port 5222)

  (define-key jabber-chat-mode-map (kbd "\C-x s f") 'jabber-ft-send)

  ;; Change status if no activity in emacs
  (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

  ;; Store all messages
  (setq jabber-history-enabled t)

  ;; get smileys support in jabber
  (require 'autosmiley)
  (add-hook 'jabber-chat-mode-hook 'autosmiley-mode)

  ;; highlight URLs
  (add-hook 'jabber-chat-mode-hook 'goto-address))

(provide 'jabber-config)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jabber-config.el ends here
