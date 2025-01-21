;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(defgeneric lambda-ify (thing)
  (:method ((thing (eql '|true|)))
    true-var)
  (:method ((thing (eql '|false|)))
    false-var)
  (:method ((thing (eql '|nil|)))
    nil-var)
  (:method ((thing symbol))
    thing)
  (:method ((thing string))
    (loop with acc = nil-var
          for char across (reverse thing)
          do (setf acc `(lambda (z) (z ,(lambda-ify char) ,acc)))
          finally (return acc)))
  (:method ((thing character))
    (lambda-ify (char-code thing)))
  (:method ((thing integer))
    (loop with acc = 'zero
          repeat thing
          do (setf acc (list 'f acc))
          finally (return `(lambda (f) (lambda (zero) ,acc)))))
  (:method ((thing cons))
    (case (first thing)
      (let (destructuring-bind (let ((name value)) body)
               thing
             (declare (ignorable let))
             `((lambda (,name)
                 ,(lambda-ify body))
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
                          ,(lambda-ify (subst recur name value)))))
                    (lambda-ify value)))))
      (if (destructuring-bind (if cond then else)
              thing
            (declare (ignorable if))
            `((,(lambda-ify cond)
               (lambda (,(gensym)) ,(lambda-ify then))
               (lambda (,(gensym)) ,(lambda-ify else))))))
      (lambda (destructuring-bind (lambda (arg &rest args) body)
                  thing
                (declare (ignorable lambda))
                `(lambda (,arg)
                   ,(lambda-ify
                     (if args
                         `(lambda (,@args) ,body)
                         body)))))
      (t (mapcar #'lambda-ify thing)))))

(defun %eval-process (term)
  (cond
    ((and (consp term)
          (eq 'lambda (first term)))
     `(lambda ,(second term)
        ,(%eval-process (third term))))
    ((consp term)
     (if (> (length term) 2)
         (%eval-process
          `((,(first term)
             ,(second term))
            ,@(rest (rest term))))
         `(funcall ,(%eval-process (first term))
                   ,(%eval-process (second term)))))
    (t term)))

(defun eval (term)
  (cl:eval (%eval-process (lambda-ify term))))

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
                        (sort (if (uiop:directory-exists-p l)
                                  (uiop:directory-files l)
                                  (uiop:ensure-list l))
                              #'string-lessp
                              :key #'pathname-name))
                      (uiop:ensure-list lib)))))
    (eval (read (apply #'make-concatenated-stream
                       (reduce (lambda (a e)
                                 (append a (list (make-string-input-stream (string #\Newline)) e)))
                               (append (mapcar #'open lib-files)
                                       (list main))
                               :initial-value '()))))))
