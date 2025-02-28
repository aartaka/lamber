;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

;; TODO: e.g. infer return types of F & G as |bool|-s
;; def disjoin fn (f g x)
;;   or (f x) (g x) .
;; TODO: Handle nesting better in general
;; TODO: Variable types
;; TODO: Union/intersection types
;; TODO: Introduce the |fn| type and use it on everything FUNCALL-ed

(in-package :lamber)

(define-generic type-infer ((tree symbol) &optional sym-types defined-types)
  "Infer the DEFINED-TYPE returned by TREE.
Return
1. The TREE itself, modified when necessary.
2. It's type, where inferrable.
3. And possibly modified SYM-TYPES.

Looks up the types of symbols in SYM-TYPES whenever necessary.
DEFINED-TYPES are types defined so far.
NIL is an always defined 'anything' type.
Raises warnings if there are type mismatches."
  (declare (ignorable defined-types))
  (values tree (cdr (assoc tree sym-types)) sym-types))

(defmethod type-infer ((tree integer) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|int| sym-types))

(defmethod type-infer ((tree character) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|char| sym-types))

(defmethod type-infer ((tree string) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  (values tree '|str| sym-types))

(defun infer-let (tree &optional sym-types defined-types)
  (destructuring-bind (let ((name value)) body)
      tree
    (declare (ignorable let))
    (cond
      ((eq 'type value)
       (multiple-value-bind (body-expr body-type final-sym-types)
           (type-infer body sym-types `((,name . 0) ,@defined-types))
         (let ((val (gensym "val")))
           (values `(let ((,name (lambda (,val) ,val)))
                      ,body-expr)
                   body-type final-sym-types))))
      ((and (consp value)
            (eq 'type (first value)))
       ;; What a spaghetti dish, yummy!
       (destructuring-bind (type (&rest args) constructor-body)
           value
         (declare (ignorable type))
         (let* ((constructor `(lambda (,@args) ,constructor-body))
                (defined-types `((,name . ,(length args)) ,@defined-types))
                (dummies (mapcar (lambda (arg) (gensym (string arg)))
                                 args))
                (sym-types
                  `((,name . (|fn| (,@dummies)
                                   (,name ,@dummies)))
                    ,@(mapcar #'(lambda (arg dummy)
                                  `(,arg . (|fn| ((,name ,@dummies)) ,dummy)))
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
  (destructuring-bind (lambda (&rest args) body)
      tree
    (declare (ignorable lambda))
    (multiple-value-bind (body return-type sym-types)
        (type-infer body sym-types defined-types)
      (values
       `(lambda (,@args) ,body)
       `(|fn| (,@(mapcar (lambda (arg)
                           (cdr (assoc arg sym-types)))
                         args))
              ,return-type)
       sym-types))))

(defun types-compatible-p (type1 type2)
  (or (null type1)
      (null type2)
      (eq type1 type2)
      (and (listp type1)
           (listp type2)
           (every #'types-compatible-p type1 type2))))

(defun merge-sym-types (types1 types2)
  (flet ((assoc-ref (item alist)
           (cdr (assoc item alist))))
    (loop with types2 = (remove-if #'null types2 :key #'cdr)
          for (sym . type) in types1
          for type2 = (assoc-ref sym types2)
          if (null type2)
            collect (cons sym type) into types
          else if (and (null type) type2)
                 collect (assoc sym types2) into types
          else if (and type2 (not (types-compatible-p type type2)))
                 do (warn "Types ~a and ~a are incompatible for ~a" type type2 sym)
          else
            collect (cons sym type) into types
            finally (return (loop for pair in types2
                                  do (pushnew pair types :key #'car)
                                  finally (return types))))))

(defun get-sym-types (args sym-types defined-types)
  (loop for i from 0 below (length args)
        for (expr type syms)
          = (multiple-value-list (type-infer (elt args i) sym-types defined-types))
        collect (cons expr type) into arg+type
        do (setf sym-types (merge-sym-types sym-types syms))
        finally (return (values arg+type sym-types))))

(defun check-function-arg-types (tree type sym-types defined-types)
  (destructuring-bind (fn (&rest arg-types) return-type)
      type
    (declare (ignorable fn))
    (multiple-value-bind (arg+type arg-sym-types)
        (get-sym-types (rest tree) sym-types defined-types)
      (loop for (arg-expr . arg-type) in arg+type
            for i below (length arg+type)
            collect arg-expr into arg-exprs
            unless (types-compatible-p arg-type (elt arg-types i))
              do (warn "Argument ~s to ~a has type mismatch: ~a vs. expected ~a"
                       i (first tree) arg-type (elt arg-types i))
            finally (return (values (cons (first tree) arg-exprs)
                                    (if (= (length arg+type) (length arg-types))
                                        return-type
                                        `(|fn| ,(subseq arg-types (length arg+type)) ,return-type))
                                    (merge-sym-types sym-types arg-sym-types)))))))

(defmethod type-infer ((tree cons) &optional sym-types defined-types)
  (declare (ignorable defined-types))
  sym-types defined-types
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
         (declare (ignorable type))
         ;; ;; Useless for explicit type casts?
         ;; (unless (types-compatible-p head type)
         ;;   (warn "Types ~a and and ~a are not compatible for ~a" head type tree))
         (values expr head syms)))
      ((and (assoc head defined-types)
            (plusp (cdr (assoc head defined-types))))
       (unless (second tree)
         (warn "Constructed types (like ~a) should have arguments" head))
       (loop for sub in (rest tree)
             for (expr type syms)
               = (multiple-value-list (type-infer sub sym-types defined-types))
             collect type into types
             collect expr into exprs
             do (setf sym-types (append syms sym-types))
             finally (return (values `(,head ,@exprs) `(,head ,@types) sym-types))))
      ;; Means it's a function, right?
      ((symbolp head)
       (let ((found-type (assoc head sym-types)))
         (cond
           ((and found-type (consp (cdr found-type)) (eq '|fn| (cadr found-type)))
            (check-function-arg-types tree (cdr found-type) sym-types defined-types))
           (t (values tree nil sym-types)))))
      ((listp head)
       (multiple-value-bind (head-expr head-type head-syms)
           (type-infer head sym-types defined-types)
         (declare (ignorable head-type))
         (multiple-value-bind (arg+type sym-types)
             (get-sym-types (rest tree) sym-types defined-types)
           (setf sym-types (merge-sym-types head-syms sym-types))
           (cond
             ((and (listp head-type)
                   (eq '|fn| (first head-type))
                   (second head-type))
              (check-function-arg-types (cons head-expr (rest tree)) head-type
                                        sym-types defined-types))
             ((symbolp head-expr)
              (type-infer (cons head-expr (mapcar #'car arg+type)) sym-types defined-types))
             (t (values (cons head-expr (mapcar #'car arg+type)) nil sym-types))))))
      (t (values tree nil sym-types)))))
