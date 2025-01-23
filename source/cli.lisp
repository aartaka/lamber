;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defun entry-point ()
  (handler-case
      (handler-bind
          ((warning #'muffle-warning))
        (let* ((args uiop:*command-line-arguments*)
               (libs (cons (asdf:system-relative-pathname "lamber" "lib/")
                           (mapcar #'uiop:parse-native-namestring (butlast args 2))))
               (type (car (last args)))
               (executable (car (last args 2)))
               (result (run-with-lib
                        (if (uiop:file-exists-p (uiop:parse-native-namestring executable))
                            (uiop:parse-native-namestring executable)
                            executable)
                        libs)))
          (flet ((print-typed (result type)
                   (cond
                     ((memqual-string type '("int" "integer" "number"))
                      (princ (tonum result)))
                     ((memqual-string type '("str" "string"))
                      (princ (tostring result)))
                     ((memqual-string type '("bool" "boolean"))
                      (princ (if (tobool result) "true" "false")))
                     ((memqual-string type '("char" "character"))
                      (format t "'~a'" (tochar result))))))
            (fresh-line)
            (ignore-errors (print-typed result type)) (fresh-line))))
    (error (e)
      (let ((*print-case* :downcase)
            (*package* (find-package :lamber)))
        (format t "~&~a Aborting!~&" e)))))
