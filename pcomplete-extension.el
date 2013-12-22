;;; pcomplete-extension.el --- additional completion for pcomplete

;; Copyright (C) 2010 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'pcomplete)

;;; Hg completion
;;
;;
(cl-defun pcomplete-get-hg-commands (&key com opts spec)
  (with-temp-buffer
    (let (h1 h2 h3)
      (if spec
          (progn
            (apply #'call-process "hg" nil t nil
                   (list "-v" "help" spec))
            (setq h3 (buffer-string)))
          (call-process "hg" nil t nil
                        "-v" "help")
          (setq h1 (buffer-string))
          (erase-buffer)
          (apply #'call-process "hg" nil t nil
                 (list "-v" "help" "mq"))
          (setq h2 (buffer-string)))
      (erase-buffer)
      (if spec (insert h3) (insert (concat h1 h2)))
      (goto-char (point-min))
      (let (args coms sargs)
        (if spec
            (setq sargs (cl-loop while (re-search-forward "\\(-[a-zA-Z]\\|--[a-zA-Z]+\\) *" nil t)
                                 collect (match-string 1)))
            (save-excursion
              (setq coms
                    (cl-loop while (re-search-forward "^ \\([a-z]+\\) *" nil t)
                             collect (match-string 1))))
            (setq args
                  (cl-loop while (re-search-forward "\\(-[a-zA-Z]\\|--[a-zA-Z]+\\) *" nil t)
                           collect (match-string 1))))
        (cond (spec sargs)
              (com coms)
              (opts args)
              (t (list args coms)))))))

(defvar pcomplete-hg-commands-cache nil)
(defvar pcomplete-hg-glob-opts-cache nil)
(defun pcomplete/hg ()
  (let* ((cur      (pcomplete-arg 'first))
         (avder    (pcomplete-arg 0))
         (last     (pcomplete-arg 'last))
         (all      (pcomplete-get-hg-commands))
         (commands (or pcomplete-hg-commands-cache
                       (setq pcomplete-hg-commands-cache
                             (cadr all))))
         (options  (or pcomplete-hg-glob-opts-cache
                       (setq pcomplete-hg-glob-opts-cache
                             (car all))))
         (special  (pcomplete-get-hg-commands :spec avder)))
    (cond ((and (string-match-p "-" last)
                (member avder commands))
           (while (pcomplete-here special)))
          ((string-match-p "-" last)
           (pcomplete-here options))
          ((string-match-p "qqueue" avder)
           (let ((queues (with-temp-buffer
                           (apply #'call-process "hg" nil t nil
                                  (list "qqueue" "-l"))
                           (cl-loop for i in (split-string (buffer-string) "\n" t)
                                    append (list (car (split-string i " (")))))))
             (while (pcomplete-here queues))))
          ((and (string= cur "hg")
                (not (string= cur avder)))
           (pcomplete-here commands)))
    (while (pcomplete-here (pcomplete-entries) nil 'identity))))

;;; Find completion
;;
;;
(defun pcomplete/find ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((and (pcomplete-match "^-" 'last)
                (string= "find" prec))
           (pcomplete-opt "HLPDO"))
          ((pcomplete-match "^-" 'last)
           (while (pcomplete-here
                   '("-amin" "-anewer" "-atime" "-cmin" "-cnewer" "-context"
                     "-ctime" "-daystart" "-delete" "-depth" "-empty" "-exec"
                     "-execdir" "-executable" "-false" "-fls" "-follow" "-fprint"
                     "-fprint0" "-fprintf" "-fstype" "-gid" "-group"
                     "-help" "-ignore_readdir_race" "-ilname" "-iname"
                     "-inum" "-ipath" "-iregex" "-iwholename"
                     "-links" "-lname" "-ls" "-maxdepth"
                     "-mindepth" "-mmin" "-mount" "-mtime"
                     "-name" "-newer" "-nogroup" "-noignore_readdir_race"
                     "-noleaf" "-nouser" "-nowarn" "-ok"
                     "-okdir" "-path" "-perm" "-print"
                     "-print0" "-printf" "-prune" "-quit"
                     "-readable" "-regex" "-regextype" "-samefile"
                     "-size" "-true" "-type" "-uid"
                     "-used" "-user" "-version" "-warn"
                     "-wholename" "-writable" "-xdev" "-xtype"))))
          ((string= "-type" prec)
           (while (pcomplete-here (list "b" "c" "d" "p" "f" "l" "s" "D"))))
          ((string= "-xtype" prec)
           (while (pcomplete-here (list "b" "c" "d" "p" "f" "l" "s"))))
          ((or (string= prec "-exec")
               (string= prec "-execdir"))
           (while (pcomplete-here* (funcall pcomplete-command-completion-function)
                                   (pcomplete-arg 'last) t))))
    (while (pcomplete-here (pcomplete-entries) nil 'identity))))

;;; Sudo
;;
;; Allow completing other commands entered after sudo
(defun pcomplete/sudo ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((string= "sudo" prec)
           (while (pcomplete-here*
                   (funcall pcomplete-command-completion-function)
                   (pcomplete-arg 'last) t))))))

;;; Ls
;;
(defun pcomplete/ls ()
  (let ((prec (pcomplete-arg 'last -1)))
    (cond ((and (pcomplete-match "^-\\{2\\}" 'last)
                (string= "ls" prec))
           (while (pcomplete-here
                   '("--all" "--almost-all" "--author"
                     "--escape" "--block-size=" "--ignore-backups" "--color="
                     "--directory" "--dired" "--classify" "--file-type"
                     "--format=" "--full-time" "--group-directories-first"
                     "--no-group" "--human-readable" "--si"
                     "--dereference-command-line-symlink-to-dir"
                     "--hide=" "--indicator-style=" "--inode" "--ignore="
                     "--dereference" "--numeric-uid-gid" "--literal" "--indicator-style="
                     "--hide-control-chars" "--show-control-chars"
                     "--quote-name" "--quoting-style=" "--reverse" "--recursive"
                     "--size" "--sort=" "--time=" "--time-style=--tabsize="
                     "--width=" "--context" "--version" "--help"))))
          ((and (pcomplete-match "^-\\{1\\}" 'last)
                (string= "ls" prec))
           (pcomplete-opt "aAbBcCdDfFgGhHiIklLmnNopqQrRsStTuUvwxXZ1")))
  (while (pcomplete-here (pcomplete-entries) nil 'identity))))

(provide 'pcomplete-extension)

;;; pcomplete-hg.el ends here
