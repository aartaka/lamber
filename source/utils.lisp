;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defvar nil-var '(lambda (thn) (lambda (els) els)))
(defvar false-var nil-var)
(defvar true-var '(lambda (thn) (lambda (els) thn)))

(defun tree-find (thing tree)
  (typecase tree
    ;; FIXME: Doesn't handle dotted lists.
    (list (some (lambda (e) (tree-find thing e)) tree))
    (t (equal thing tree))))

(defmacro define-generic (name (&rest args) &body (documentation . body))
  (assert (stringp documentation))
  `(defgeneric ,name (,@(mapcar #'first (mapcar #'uiop:ensure-list args)))
     (:documentation ,documentation)
     (:method (,@args)
       ,@body)))
