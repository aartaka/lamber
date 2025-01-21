;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defun tonum (num)
  (funcall (funcall num #'1+) 0))

(defun tobool (bool)
  (funcall (funcall bool t) nil))

(defun tochar (char)
  (code-char (tonum char)))

(defun tocons (term &optional converter)
  (loop with cons = term
        while (handler-case
                  (tobool cons)
                (error () t))
        for (first second) = (funcall cons (lambda (f) (lambda (s) (list f s))))
        collect (if converter
                    (funcall converter first)
                    first)
        do (setf cons second)))

(defun tostring (str)
  (coerce (tocons str #'tochar) 'string))

;; (tochar
;;  (run-with-lib #p"~/git/lamber/test.lmb"
;;                #p"~/git/lamber/lib/"))

;; (%eval-process
;;  (lambda-ify
;;   (read #p"~/git/lamber/lib/3-list.lmb")))

;; (tonum
;;  (eval
;;   (read #p"~/git/lamber/lib/2-numbers.lmb")))
