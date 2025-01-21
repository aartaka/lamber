;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defun tree-find (thing tree)
  (typecase tree
    ;; FIXME: Doesn't handle dotted lists.
    (list (some (lambda (e) (tree-find thing e)) tree))
    (t (equal thing tree))))
