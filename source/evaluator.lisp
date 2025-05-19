;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defgeneric %lambda-ify (thing)
  (:method ((thing function))
    thing)
  (:method ((thing symbol))
    thing)
  (:method ((thing (eql 'type)))
    `(lambda (x) x))
  (:method ((thing (eql t)))
    '|true|)
  (:method ((thing (eql nil)))
    '|nil|)
  (:method ((thing string))
    (loop with acc = '|nil|
          for char across (reverse thing)
          do (setf acc `(lambda (z) (z ,(%lambda-ify char) ,acc)))
          finally (return acc)))
  (:method ((thing character))
    (%lambda-ify (char-code thing)))
  (:method ((thing integer))
    #'(lambda (f)
        #'(lambda (x)
            (loop repeat (1+ thing)
                  for res = x then (funcall f res)
                  finally (return res)))))
  (:method ((thing ratio))
    (%lambda-ify
     `(|cons| ,(numerator thing) ,(denominator thing))))
  (:method ((thing cons))
    (case (first thing)
      (let (destructuring-bind (let ((name value)) body)
               thing
             (declare (ignorable let))
             `((lambda (,name)
                 ,(%lambda-ify body))
               ,(if (tree-find name value)
                    ;; Automatic recursive functions with Z-combinator
                    (let ((recur (gensym (uiop:strcat (string name) "-recur"))))
                      ;; Z-combinator (what an unfortunate name...)
                      `((lambda (f)
                          ((lambda (x)
                             (f (lambda (y)
                                  ((x x) y))))
                           (lambda (x)
                             (f (lambda (y)
                                  ((x x) y))))))
                        (lambda (,recur)
                          ,(%lambda-ify (subst recur name value)))))
                    (%lambda-ify value)))))
      (if (destructuring-bind (if cond then else)
              thing
            (declare (ignorable if)
                     (cl:optimize space))
            `((,(%lambda-ify cond)
               (lambda (,(gensym)) ,(%lambda-ify then))
               (lambda (,(gensym)) ,(%lambda-ify else))))))
      (type (destructuring-bind (type (&rest args) body)
                thing
              (declare (ignorable type)
                       (cl:optimize space))
              (%lambda-ify `(lambda (,@args) ,body))))
      (lambda (destructuring-bind (lambda (arg &rest args) body)
                  thing
                (declare (ignorable lambda)
                         (cl:optimize space))
                `(lambda (,arg)
                   ,(%lambda-ify
                     (if args
                         `(lambda (,@args) ,body)
                         body)))))
      (t (mapcar #'%lambda-ify thing)))))

(defun %process-applications (term)
  (cond
    ((and (consp term)
          (eq 'lambda (first term)))
     `(lambda ,(second term)
        (declare (ignorable ,@(second term)))
        ,(%process-applications (third term))))
    ((consp term)
     (if (> (length term) 2)
         (%process-applications
          `((,(first term)
             ,(second term))
            ,@(rest (rest term))))
         `(funcall ,(%process-applications (first term))
                   ,(%process-applications (second term)))))
    (t term)))

(defun lambda-ify (term)
  (%process-applications (%lambda-ify term)))

(defun eval (term)
  (multiple-value-bind (optimized type)
      (optimize term)
    (declare (cl:optimize space (safety 0) (debug 0)))
    (values (cl:eval (lambda-ify optimized)) type)))

(defun run-with-lib (in &optional lib)
  (let* ((main (etypecase in
                 ((or pathname
                      (satisfies uiop:file-exists-p))
                  (open in))
                 (string (make-string-input-stream in))
                 (stream in)))
         (lib-files (reduce
                     #'append
                     (mapcar
                      (lambda (l)
                        (if (uiop:directory-exists-p l)
                            (uiop:directory-files l)
                            (uiop:ensure-list l)))
                      (uiop:ensure-list lib))))
         (lib-files (sort lib-files #'string-lessp
                          :key #'pathname-name))
         (stream (apply #'make-concatenated-stream
                        (reduce (lambda (a e)
                                  (append a (list (make-string-input-stream (string #\Newline)) e)))
                                (append (mapcar #'open lib-files)
                                        (list main))
                                :initial-value '()))))

    (multiple-value-bind (read rest)
        (read stream)
      (if rest
          (warn "Some forms weren't processed. Did you add a redundant 'end'/'.' before '~{~a~^ ~}'?"
                (subseq rest 0 (min (length rest) 10)))
          (eval read)))))
