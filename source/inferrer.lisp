;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(define-generic type-infer ((tree (eql '|nil|)) &optional sym-types defined-types)
  "Infer the DEFINED-TYPE returned by TREE.
Return
1. The TREE itself, modified when necessary.
2. It's type, where inferrable.
3. And possibly modified SYM-TYPES.

Looks up the types of symbols in SYM-TYPES whenever necessary.
DEFINED-TYPES are types defined so far.
NIL is always an always defined 'anything' type.
Raises warnings if there are type mismatches."
  (declare (ignorable defined-types))
  (values tree nil sym-types))

(defmethod type-infer ((tree (eql '|false|)) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|bool| sym-types))
(defmethod type-infer ((tree (eql '|true|)) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|bool| sym-types))

(defmethod type-infer ((tree integer) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|int| sym-types))

(defmethod type-infer ((tree character) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|char| sym-types))

(defmethod type-infer ((tree string) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|str| sym-types))

(defmethod type-infer ((tree symbol) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree (cdr (assoc tree sym-types)) sym-types))

(defun merge-sym-types (types1 types2)
  "Merge the TYPES1 and TYPES2 type alists.
Warn if there are mismatches."
  ;; TODO
  )

(defun infer-let (tree &optional sym-types defined-types)
  (destructuring-bind (let ((name value)) body)
      tree
    (declare (ignorable let))
    (cond
      ((eq 'type value)
       (type-infer
        body sym-types `((,name . 0) ,@defined-types)))
      ((and (consp value)
            (eq 'type (first value)))
       ;; What a spaghetti dish, yummy!
       (destructuring-bind (type (&rest args) body)
           value
         (declare (ignorable type))
         (let* ((constructor `(lambda (,@args) ,body))
                (defined-types `((,name . ,(length args)) ,@defined-types))
                (dummies (mapcar #'gensym args))
                (sym-types
                  `((,name . (|fn| (,@dummies)
                                   `(,type ,@dummies)))
                    ,@(mapcar #'(lambda (arg dummy)
                                  (cons arg `(|fn| ((,type ,@dummies)) ,dummy)))
                              args dummies)
                    ,@sym-types)))
           (multiple-value-bind (body-expr body-type final-sym-types)
               (type-infer body sym-types defined-types)
             (values `(let ((,name ,constructor)) ,body-expr)
                     body-type
                     (merge-sym-types sym-types final-sym-types))))))
      (t (multiple-value-bind (expr type new-sym-types)
             (type-infer value sym-types defined-types)
           (let ((merged-types (merge-sym-types `((,name ,type))
                                                (merge-sym-types sym-types new-sym-types))))
             (multiple-value-bind (body body-type new-new-sym-types)
                 (type-infer body merged-types defined-types)
               (values `(let ((,name ,expr))
                          ,body)
                       body-type
                       (merge-sym-types merged-types new-new-sym-types)))))))))

(defmethod type-infer ((tree cons) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (let ((head (first tree)))
    (cond
      ((eql 'let head)
       (infer-let tree sym-types defined-types))
      ((and (assoc head defined-types)
            (zerop (cdr (assoc head defined-types)))
            (second tree)
            (symbolp (second tree)))
       (destructuring-bind (type sym)
           tree
         (values sym type `((,sym ,type) ,@sym-types))))
      ((and (assoc head defined-types)
            (plusp (cdr (assoc head defined-types))))
       (unless (second tree)
         (warn "Constructed types (like ~a) should have arguments" head))
       (loop for sub in (rest tree)
             for (expr type syms)
               = (multiple-value-list (type-infer sub))
             collect type into types
             collect expr into exprs
             do (setf sym-types (merge-sym-types sym-types syms))
             finally (values `(,head ,@exprs) `(,head ,@types) sym-types)))
      ;; TODOTODOTODO
      )))
