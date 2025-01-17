(in-package :lamber)

(defun memqual-string (item list)
  (member item list
          :test (lambda (a b)
                  (ignore-errors (string-equal a b)))))

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
      ((memqual-string (first list) '("let" "def" "local" "var"))
       (let ((name (second list))
             (value-list (nthcdr (if (memqual-string (third list) '("=" ":=" "<-")) 3 2)
                                 list)))
         (multiple-value-bind (value rest)
             (if (consp (first value-list))
                 (values (first value-list) (rest value-list))
                 (%read value-list))
           (multiple-value-bind (body remaining)
               (%read rest)
             (values `(let ((,name ,value))
                        ,body)
                     remaining)))))
      ((ignore-errors (string-equal (first list) "if"))
       (multiple-value-bind (condition rest)
           (%read (rest list))
         (multiple-value-bind (then rest)
             (%read (if (ignore-errors (string-equal "then" (first rest)))
                        (rest rest)
                        rest))
           (assert (string-equal (first rest) "else"))
           (multiple-value-bind (else remaining)
               (%read (rest rest))
             (values
              `(if ,condition
                   ,then
                   ,else)
              remaining)))))
      ((memqual-string (first list) '("fn" "lambda" "function"))
       (let ((args (second list)))
         (multiple-value-bind (body next)
             (%read (nthcdr 2 list))
           (values `(lambda (,@(uiop:ensure-list args))
                      ,body)
                   next))))
      ((consp (first list))
       (values (%read (first list)) (rest list)))
      (t
       (loop for elem in list
             until (ignore-errors (memqual-string elem '("let" "def" "local" "var" "if" "then" "else" "." "end")))
             collect elem into prefix
             finally (let* ((suffix (nthcdr (length prefix) list))
                            (prefix (if (= 1 (length prefix))
                                        (first prefix)
                                        prefix))
                            (suffix (if (memqual-string (first suffix) '("." "end" "then"))
                                        (rest suffix)
                                        suffix)))
                       (return (values prefix suffix))))))))

(defun read-quoted-char (stream char)
  (declare (ignorable char))
  (loop for c = (read-char stream)
        if (char= #\\ c)
          do (setf c (read-char stream))
        do (assert (char= #\' (read-char stream))
                   () "Character literal can only be one char long")
        do (return c)))

(defun read-square-bracket (stream char)
  (declare (ignorable char))
  (let ((list (read-delimited-list #\] stream t)))
    (labels ((format-cons (cons)
               (when cons
                 (if (eq '|\|| (car cons))
                     (second cons)
                     `(|cons| ,(car cons) ,(format-cons (cdr cons)))))))
      (format-cons list))))

(defun read-curly-brace (stream char)
  (declare (ignorable char))
  (let ((list (read-delimited-list #\} stream t)))
    (loop with acc = (first list)
          for (op arg . rest) on (rest list) by #'cddr
          while (or op arg)
          do (setf acc (list op acc arg))
          finally (return acc))))

(defun read-colon (stream char)
  (declare (ignorable char))
  (with-input-from-string (s (read-line stream))
    (uiop:slurp-stream-forms s)))

(defun read-comma (stream char)
  (declare (ignorable stream char))
  (values))

(defun read (in)
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\' #'read-quoted-char)
    (set-macro-character #\[ #'read-square-bracket nil)
    (set-macro-character #\{ #'read-curly-brace nil)
    (set-macro-character #\: #'read-colon)
    (set-macro-character #\, #'read-comma)
    (set-macro-character #\] nil)
    (set-macro-character #\} nil)
    (set-macro-character #\| nil)
    (set-macro-character #\. nil)
    (%read in)))

;; (read "local a = 3
;; local b = 'a'
;; local x = function : a
;;  local hello : 3
;;  if 3 then
;;   whateeeeeeever
;;  else
;;   if 3 then
;;     whatever again
;;   else
;;    try other thing .
;; x [a b c | d]")
