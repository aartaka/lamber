;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :lamber)

(define-generic plug-dummy-for-lib ((tree null))
  "Plug the dummy value (zero/false/nil) for when there's no code to run."
  (warn "No code to evaluate, doing a dry checking run on a library")
  0)

(defmethod plug-dummy-for-lib ((tree t))
  tree)

(defmethod plug-dummy-for-lib ((tree cons))
  (if (eq 'let (first tree))
      (destructuring-bind (let ((name value)) body)
          tree
        (declare (ignorable let))
        `(,let ((,name ,value))
           ,(plug-dummy-for-lib body)))
      tree))

(define-generic tree-shake ((tree t))
  "Remove all the unused `let'-bound functions recursively."
  tree)

(defmethod tree-shake ((tree cons))
  (if (eq 'let (first tree))
      (destructuring-bind (let ((name value)) body)
          tree
        (declare (ignorable let))
        (if (not (tree-find name body))
            (tree-shake body)
            `(,let ((,name ,value))
               ,(tree-shake body))))
      (mapcar #'tree-shake tree)))

(define-generic de-alias ((tree t) &optional (aliases '()))
  "Warn about all the unbound variables."
  (declare (ignorable aliases))
  tree)

(defmethod de-alias ((tree symbol) &optional (aliases '()))
  (loop with sym = tree
        for alias = (cdr (assoc sym aliases))
        until (null alias)
        do (setf sym alias)
        finally (return sym)))

(defmethod de-alias ((tree cons) &optional (aliases '()))
  (case (first tree)
    (let (destructuring-bind (let ((name value)) body)
             tree
           (declare (ignorable let))
           (cond
             ((eq 'type value)
              `(let ((,name ,value))
                 ,(de-alias body aliases)))
             ((symbolp value)
              (de-alias body `((,name . ,value) ,@aliases)))
             ((assoc name aliases)
              `(,let ((,name ,value))
                 ,(de-alias body (remove name aliases :key #'car))))
             (t `(,let ((,name ,(de-alias value aliases)))
                   ,(de-alias body aliases))))))
    (lambda (destructuring-bind (lambda (&rest args) body)
                tree
              (declare (ignorable lambda))
              `(lambda (,@args)
                 ,(de-alias body (remove-if (lambda (alias)
                                              (member alias args))
                                            aliases :key #'car)))))
    (t (mapcar (lambda (subtree)
                 (de-alias subtree aliases))
               tree))))

(define-generic warn-on-unbound ((tree t) &optional (enclosing-function "toplevel") path)
  "Warn about all the unbound variables."
  (declare (ignorable tree enclosing-function path))
  tree)

(defmethod warn-on-unbound ((tree symbol) &optional (enclosing-function "toplevel") path)
  (if (or (member tree path)
          (memqual-string tree
                          '("let" "def" "define" "local" "var" "alias"
                            "if" "when" "then" "else"
                            "fn" "lambda" "function"
                            "type" "constructor"
                            "end" ".")))
      tree
      (progn
        (warn "Symbol ~a appears to be unbound in ~a" tree enclosing-function)
        tree)))

(defmethod warn-on-unbound ((tree cons) &optional (enclosing-function "toplevel") path)
  (case (first tree)
    (let (destructuring-bind (let ((name value)) body)
             tree
           (declare (ignorable let))
           ;; NOTE: (cons name path) is because the let-bound function
           ;; might be recursive (I know it's not a cool thing to
           ;; encode this in IR syntax, but oh well.)
           `(,let ((,name ,(warn-on-unbound value name (cons name path))))
              ,(warn-on-unbound body enclosing-function (cons name path)))))
    ((lambda type)
     (destructuring-bind (lambda (&rest args) body)
         tree
       (declare (ignorable lambda))
       `(lambda (,@args)
          ,(warn-on-unbound body enclosing-function (append args path)))))
    (t (mapcar (lambda (subtree)
                 (warn-on-unbound subtree enclosing-function path))
               tree))))

(define-generic warn-on-suspicious-applications ((tree t) &optional (enclosing-function "toplevel") arg-counts)
  "Find the suspiciously long applications or applications not matching the arglists."
  (declare (ignorable enclosing-function arg-counts))
  tree)

(defmethod warn-on-suspicious-applications ((tree cons) &optional (enclosing-function "toplevel") arg-counts)
  (let ((head (first tree)))
    (if (eq 'let head)
        (destructuring-bind (let ((name value)) body)
            tree
          (declare (ignorable let))
          (let ((arg-counts (if (and (listp value)
                                     (eq 'lambda (first value)))
                                (cons (cons name (length (second value)))
                                      arg-counts)
                                arg-counts)))
            `(,let ((,name ,(warn-on-suspicious-applications value name arg-counts)))
               ,(warn-on-suspicious-applications body enclosing-function arg-counts))))
        (cond
          ((and (symbolp head)
                (assoc head arg-counts)
                (> (length (rest tree))
                   ;; NOTE: An imperfect heuristic for curried
                   ;; functions. Any other number?
                   (+ 2 (cdr (assoc head arg-counts)))))
           ;; FIXME: Should I call the parameters, like PL designers?
           (warn "Too many arguments (~d) to ~a in ~a, did you forget to wrap some arguments in colons or parentheses?"
                 (length (rest tree)) head enclosing-function))
          (t (mapcar (lambda (subtree)
                       (warn-on-suspicious-applications subtree enclosing-function arg-counts))
                     tree))))))

(define-generic warn-on-shadowing ((tree t) &optional path)
  "Check whether any of the functions override the ones defined earlier."
  (declare (ignorable path))
  tree)

(defmethod warn-on-shadowing ((tree cons) &optional path)
  (let ((head (first tree)))
    (if (eq 'let head)
        (destructuring-bind (let ((name value)) body)
            tree
          (declare (ignorable let))
          (when (find name path)
            (warn "Function ~a is redefined/shadowed to another value near ~a"
                  name (car path)))
          `(,let ((,name ,value))
             ,(warn-on-shadowing body (cons name path))))
        (mapcar (lambda (subtree)
                  (warn-on-shadowing subtree path))
                tree))))

(define-generic dry-run-p ((tree t))
  "Check whether we're running in dry run mode."
  nil)

(defmethod dry-run-p ((tree cons))
  (let ((head (first tree)))
    (if (eq 'let head)
        (dry-run-p (third tree))
        nil)))

(defun optimize (tree)
  (multiple-value-bind (expr return-type all-types)
      (tree-shake
       (de-alias
        (plug-dummy-for-lib tree)))
    (declare (ignorable all-types))
    (let ((optimized
            (tree-shake
             (tree-shake
              (tree-shake
               (warn-on-shadowing
                (warn-on-suspicious-applications
                 (warn-on-unbound expr))))))))
      (if (dry-run-p optimized)
          '|nil|
          (values optimized return-type)))))
