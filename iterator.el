;;; iterator.el --- A library to create and use elisp iterators objects.

;; Author: Thierry Volpiatto <thierry dot volpiatto at gmail dot com>

;; Copyright (C) 2009 ~ 2010 Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 23.1+

;; X-URL: http://mercurial.intuxication.org/hg/elisp-iterators

;; This file is not part of GNU Emacs. 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Code:

(require 'cl)

(defsubst* iter-position (item seq &key (test 'eq))
  "Get position of ITEM in SEQ.
A simple replacement of CL `position'."
  (loop for i in seq for index from 0
     when (funcall test i item) return index))

(defmacro iter-list (seq)
  "Return an iterator from SEQ."
  `(lexical-let ((lis ,seq))
     (lambda ()
       (let ((elm (car lis)))
         (setq lis (cdr lis))
         elm))))

(defun iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(defun* iter-sub-next (seq elm &key (test 'eq))
  "Create iterator from position of ELM to end of SEQ."
  (lexical-let* ((pos      (ioccur-position elm seq :test test))
                 (sub      (nthcdr (1+ pos) seq))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (ioccur-iter-next iterator))))

(defun* iter-sub-prec (seq elm &key (test 'eq))
  "Create iterator from position of ELM to beginning of SEQ."
  (lexical-let* ((rev-seq  (reverse seq))
                 (pos      (ioccur-position elm rev-seq :test test))
                 (sub      (nthcdr (1+ pos) rev-seq))
                 (iterator (ioccur-iter-list sub)))
     (lambda ()
       (ioccur-iter-next iterator))))

(defun iter-circular (seq)
  "Infinite iteration on SEQ."
  (lexical-let ((it (iter-list seq))
                (lis seq))
    (lambda ()
      (let ((elm (iter-next it)))
        (or elm
            (progn (setq it (iter-list lis)) (iter-next it)))))))

(defun* iter-sub-prec-circular (seq elm &key (test 'eq))
  "Infinite reverse iteration of SEQ starting at ELM."
  (lexical-let* ((rev-seq  (reverse seq))
                 (pos      (iter-position elm rev-seq :test test))
                 (sub      (append (nthcdr (1+ pos) rev-seq) (subseq rev-seq 0 pos)))
                 (iterator (iter-list sub)))
     (lambda ()
       (let ((elm (iter-next iterator)))
         (or elm
             (progn (setq iterator (iter-list sub)) (iter-next iterator)))))))

(defun* iter-sub-next-circular (seq elm &key (test 'eq))
  "Infinite iteration of SEQ starting at ELM."
  (lexical-let* ((pos      (iter-position elm seq :test test))
                 (sub      (append (nthcdr (1+ pos) seq) (subseq seq 0 pos)))
                 (iterator (iter-list sub)))
     (lambda ()
       (let ((elm (iter-next iterator)))
         (or elm (progn
                   (setq iterator (iter-list sub))
                   (iter-next iterator)))))))


(defmacro iter-apply-fun-on-list (fun seq)
  "Create an iterator that apply function FUN on each elm of SEQ."
  `(lexical-let ((lis ,seq)
                 (fn ,fun))
     (lambda ()
       (let ((elm (if (car lis)
                      (funcall fn (car lis)))))
         (setq lis (cdr lis))
         elm))))


(defmacro iter-scroll-list (seq size)
  "Create an iterator of the subseq of the cdr of SEQ ending to SIZE."
  `(lexical-let* ((lis ,seq)
                  (end ,size))
     (lambda ()
       (let ((sub (subseq lis 0 end)))
         (setq lis (cdr lis))
         (if (< (length lis) end)
             (setq end (- end 1)))
         (remove nil sub)))))

(defmacro iter-scroll-up (seq elm size)
  `(lexical-let* ((pos (position (car (last ,elm)) ,seq))
                  (sub (reverse (subseq ,seq 0 pos)))
                  (iterator (iter-scroll-list sub ,size)))
     (lambda ()
       (reverse (iter-next iterator)))))

(defmacro iter-scroll-down (seq elm size)
  `(lexical-let* ((pos (position (car (last ,elm)) ,seq))
                  (sub (subseq ,seq pos))
                  (iterator (iter-scroll-list sub ,size)))
     (lambda ()
       (iter-next iterator))))
                  
;;; Provide
(provide 'iterator)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; iterator.el ends here
