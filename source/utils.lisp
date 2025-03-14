;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

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

(defun memqual-string (item list)
  (member item list
          :test (lambda (a b)
                  (ignore-errors (string-equal a b)))))
