(in-package :caten/lang)

(a/defmacro progn (&rest forms)
  "`(progn &rest forms)`"
  (let ((forms (map 'list #'a/macroexpand-all forms)))
    `(_%progn ,@forms)))

(a/defun _%progn (&rest forms)
    "Creates a body"
  nil)

(macrolet ((def (builtin-name expr-name lisp-name &optional (unary nil))
             `(progn
                (a/defun ,builtin-name (a b)
                    ,(format nil "`(~a a b)`" builtin-name)
                  (caten/ajit:make-expr ,expr-name a b))
                (a/defmacro ,lisp-name (&rest forms)
                    ,(format nil "`(~a &rest forms)`" lisp-name)
                  (if (and ',unary (= (length forms) 1))
                      (list ',unary (car forms))
                      (flet ((binary (a b) (list ',builtin-name a b)))
                        (reduce #'binary forms)))))))
  (def _%add :+ +)
  (def _%sub :- - _%neg)
  (def _%mul :* *)
  (def _%div :/ / _%recip))
