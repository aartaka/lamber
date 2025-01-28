;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defun strip-end (list)
  (if (memqual-string (first list) '("end" "then" "."))
      (rest list)
      list))

(defgeneric %read (in)
  (:method ((in string))
    (with-input-from-string (s in)
      (%read s)))
  (:method ((in pathname))
    (with-open-file (s in)
      (%read s)))
  (:method ((in stream))
    (%read (uiop:slurp-stream-forms in)))
  (:method ((list list))
    (cond
      ((memqual-string (first list) '("let" "def" "define" "local" "var" "alias"))
       (let ((name (second list))
             (value-list (nthcdr (if (memqual-string (third list) '("=" ":=" "<-")) 3 2)
                                 list)))
         (multiple-value-bind (value rest)
             (%read value-list)
           (multiple-value-bind (body remaining)
               (%read rest)
             (values `(let ((,name ,value))
                        ,body)
                     remaining)))))
      ((memqual-string (first list) '("if" "when"))
       (multiple-value-bind (condition rest)
           (%read (rest list))
         (multiple-value-bind (then rest)
             (%read (if (ignore-errors (string-equal "then" (first rest)))
                        (rest rest)
                        rest))
           (if (string-equal (first rest) "else")
               (multiple-value-bind (else remaining)
                   (%read (rest rest))
                 (values
                  `(if ,condition
                       ,then
                       ,else)
                  remaining))
               (values
                `(if ,condition
                     ,then
                     ,nil-var)
                rest)))))
      ((memqual-string (first list) '("fn" "lambda" "function"))
       (let ((args (second list)))
         (multiple-value-bind (body next)
             (%read (nthcdr 2 list))
           (values
            `(lambda (,@(uiop:ensure-list args))
               ,body)
            next))))
      (t
       (loop for elem in list
             until (ignore-errors (memqual-string elem '("let" "def" "define" "local" "var" "alias" "if" "then" "else" "end" ".")))
             collect elem into prefix
             finally (let* ((suffix (nthcdr (length prefix) list))
                            (suffix (strip-end suffix))
                            (prefix (mapcar (lambda (x)
                                              (if (consp x) (%read x) x))
                                            prefix))
                            (prefix (if (= 1 (length prefix))
                                        (first prefix)
                                        prefix)))
                       (return (values prefix suffix))))))))

(defun get-escaped-char (char)
  (case char
    (#\a #\Bell)
    (#\b #\Backspace)
    (#\n #\Newline)
    (#\r #\Return)
    (#\t #\Tab)
    (t char)))

(defun read-quoted-char (stream char)
  (declare (ignorable char))
  (loop for c = (read-char stream)
        if (char= #\\ c)
          do (let ((next (read-char stream)))
               (setf c (get-escaped-char next)))
        do (let ((next (read-char stream)))
             (assert (char= #\' next)
                     () "Character literal can only be one char long (around '~c~c)" c next))
        do (return c)))

(defun read-string (stream char)
  (declare (ignorable char))
  (loop for c = (read-char stream)
        until (char= #\" c)
        if (char= #\\ c)
          collect (get-escaped-char (read-char stream))
            into str
        else
          collect c
            into str
        finally (return (coerce str 'string))))

(defun read-square-bracket (stream char)
  (declare (ignorable char))
  (let ((list (read-delimited-list #\] stream t)))
    (labels ((format-cons (cons)
               (when cons
                 (if (eq '|\|| (car cons))
                     (second cons)
                     `(|cons| ,(car cons)
                              ,(or (format-cons (cdr cons)) '|nil|))))))
      (format-cons list))))

(defun read-colon (stream char)
  (declare (ignorable char))
  (loop for form = (cl:read stream nil nil t)
        for next-char = (loop for char = (peek-char nil stream nil nil)
                              while (and (not (null char))
                                         (char= #\Space char))
                              do (read-char stream nil nil)
                              finally (return (peek-char nil stream nil nil)))
        collect form
        until (memqual-string next-char '(#\Newline "." ")" "nil"))))

(defun read-comma (stream char)
  (declare (ignorable stream char))
  (values))

(defun read-end-period (stream char)
  (declare (ignorable stream char))
  '|end|)

(defun read (in)
  (let ((*readtable* (copy-readtable *readtable*))
        (*package* (find-package :lamber)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\' #'read-quoted-char t)
    (set-macro-character #\" #'read-string)
    (set-macro-character #\[ #'read-square-bracket nil)
    (set-macro-character #\: #'read-colon)
    (set-macro-character #\, #'read-comma)
    (set-macro-character #\. #'read-end-period t)
    (set-macro-character #\] nil)
    (set-macro-character #\| nil)
    (%read in)))
