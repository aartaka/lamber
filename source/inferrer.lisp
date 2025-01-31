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
                (dummies (mapcar (lambda (arg) (gensym (string arg)))
                                 args))
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
                     body-type final-sym-types)))))
      ;; TODO: Function types based on use of args in body, and
      ;; removal of arg types when function types is inferred.
      (t (multiple-value-bind (expr type new-sym-types)
             (type-infer value sym-types defined-types)
           (let ((augmented-types (cons `(,name . ,type) new-sym-types)))
             (multiple-value-bind (body body-type new-sym-types)
                 (type-infer body augmented-types defined-types)
               (values `(let ((,name ,expr))
                          ,body)
                       body-type
                       new-sym-types))))))))

(defun infer-lambda (tree &optional sym-types defined-types)
  (let ((old-sym-types (copy-tree sym-types)))
    (destructuring-bind (lambda (&rest args) body)
        tree
      (declare (ignorable lambda))
      (multiple-value-bind (body return-type new-sym-types)
          (type-infer body sym-types defined-types)
        (values
         body
         `(|fn| (,@(mapcar (lambda (arg)
                             (cdr (assoc arg new-sym-types)))
                           args))
                ,return-type)
         old-sym-types)))))

(defun types-compatible-p (type1 type2)
  (or (null type1)
      (null type2)
      (eq type1 type2)
      (and (listp type1)
           (listp type2)
           (every #'types-compatible-p type1 type2))))

(defmethod type-infer ((tree cons) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (let ((head (first tree)))
    (cond
      ((eq 'let head)
       (infer-let tree sym-types defined-types))
      ((eq 'lambda head)
       (infer-lambda tree sym-types defined-types))
      ((and (assoc head defined-types)
            (zerop (cdr (assoc head defined-types)))
            (second tree)
            (symbolp (second tree)))
       (destructuring-bind (type sym)
           tree
         (values sym type `((,sym . ,type) ,@sym-types))))
      ((and (assoc head defined-types)
            (zerop (cdr (assoc head defined-types))))
       (multiple-value-bind (expr type syms)
           (type-infer (second tree) sym-types defined-types)
         (unless (types-compatible-p head type)
           (warn "Types ~a and and ~a are not compatible for ~a" head type tree))
         (values expr type syms)))
      ((and (assoc head defined-types)
            (plusp (cdr (assoc head defined-types))))
       (unless (second tree)
         (warn "Constructed types (like ~a) should have arguments" head))
       (loop for sub in (rest tree)
             for (expr type syms)
               = (multiple-value-list (type-infer sub))
             collect type into types
             collect expr into exprs
             do (setf sym-types (append syms sym-types))
             finally (values `(,head ,@exprs) `(,head ,@types) sym-types)))
      ;; Means it's a function, right?
      ((symbolp head)
       (let ((found-type (assoc head sym-types)))
         (when found-type
           (destructuring-bind (fn (&rest arg-types) return-type)
               (cdr found-type)
             (declare (ignorable fn))
             (loop for arg-type in arg-types
                   for i from 0 below (length (rest tree))
                   for (expr type syms)
                     = (multiple-value-list (type-infer (elt (rest tree) i)))
                   unless (types-compatible-p arg-type type)
                     do (warn "Argument ~s to ~a has type mismatch: ~a vs. expected ~a"
                              i head type arg-type)
                   finally (return (values tree return-type syms)))))))
      (t (error "Expression ~s is not parseable" tree)))))

;; (type-infer '(+ 1 "hello") '((+ . (|fn| (|int| |int|) |int|))) '((|int| . 0) (|char| . 0)))

;; (type-infer (read "
;; fn (augend addend)
;;   int (fn (f x)
;;     (int augend) f ((int addend) f x))") '() '((|int| . 0)))
