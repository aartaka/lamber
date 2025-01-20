(in-package :lamber)

(defvar nil-var '(lambda (thn) (lambda (els) els)))
(defvar false-var nil-var)
(defvar true-var '(lambda (thn) (lambda (els) thn)))

(defun memqual-string (item list)
  (member item list
          :test (lambda (a b)
                  (ignore-errors (string-equal a b)))))

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
      ((memqual-string (first list) '("let" "def" "local" "var" "alias"))
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
           (loop with acc = body
                 for arg in (reverse (uiop:ensure-list args))
                 do (setf acc `(lambda (,arg) ,acc))
                 finally (return (values acc next))))))
      (t
       (loop for elem in list
             until (ignore-errors (memqual-string elem '("let" "def" "local" "var" "alias" "if" "then" "else" "end" ".")))
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
                     `(lambda (z)
                        (z ,(car cons)
                           ,(or (format-cons (cdr cons)) nil-var)))))))
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
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\' #'read-quoted-char)
    (set-macro-character #\[ #'read-square-bracket nil)
    (set-macro-character #\: #'read-colon)
    (set-macro-character #\, #'read-comma)
    (set-macro-character #\. #'read-end-period)
    (set-macro-character #\] nil)
    (set-macro-character #\| nil)
    (%read in)))

(defun tree-find (thing tree)
  (typecase tree
    ;; FIXME: Doesn't handle dotted lists.
    (list (some (lambda (e) (tree-find thing e)) tree))
    (t (equal thing tree))))

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
    (loop with zero = (gensym "zero")
          with f = (gensym "f")
          with acc = zero
          repeat thing
          do (setf acc (list f acc))
          finally (return `(lambda (,f) (lambda (,zero) ,acc)))))
  (:method ((thing cons))
    (case (first thing)
      (let (destructuring-bind (let ((name value)) body)
               thing
             (declare (ignorable let))
             ;; XXX: Tree-shaking. More (2? 3?) passes won't hurt, but
             ;; let's have this for now.
             (if (not (tree-find name body))
                 (lambda-ify body)
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
                        (lambda-ify value))))))
      (if (destructuring-bind (if cond then else)
              thing
            (declare (ignorable if))
            `((,(lambda-ify cond)
               (lambda (,(gensym)) ,(lambda-ify then))
               (lambda (,(gensym)) ,(lambda-ify else))))))
      (lambda (destructuring-bind (lambda (arg) body)
                  thing
                (declare (ignorable lambda))
                `(lambda (,arg)
                   ,(lambda-ify body))))
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
                        (if (uiop:directory-exists-p l)
                            (uiop:directory-files l)
                            (uiop:ensure-list l)))
                      (uiop:ensure-list lib))))
         (lib-files (sort lib-files #'string-lessp
                          :key #'pathname-name)))
    (eval (read (apply #'make-concatenated-stream
                       (reduce (lambda (a e)
                                 (append a (list (make-string-input-stream (string #\Newline)) e)))
                               (append (mapcar #'open lib-files)
                                       (list main))
                               :initial-value '()))))))
