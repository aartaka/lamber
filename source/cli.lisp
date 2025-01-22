;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defun entry-point ()
  (let* ((args uiop:*command-line-arguments*)
         (libs (cons (asdf:system-relative-pathname "lamber" "lib/")
                     (mapcar #'uiop:parse-native-namestring (butlast args 2))))
         (type (car (last args)))
         (executable (car (last args 2)))
         (result (run-with-lib
                  (or (ignore-errors (uiop:parse-native-namestring executable))
                      executable)
                  libs)))
    (flet ((print-typed (result type)
             (cond
               ((memqual-string type '("int" "integer" "number"))
                (princ (tonum result)))
               ((memqual-string type '("str" "string"))
                (print (tostring result)))
               ((memqual-string type '("bool" "boolean"))
                (princ (if (tobool result) "true" "false")))
               ((memqual-string type '("char" "character"))
                (format t "'~a'" (tochar result))))))
      (print-typed result type)
      (fresh-line))))
