(in-package :caten/lang)

(macrolet ((def (builtin-name expr-name lisp-name &optional (unary nil) (logical-p nil))
             (assert (typep expr-name 'caten/ajit:op/expr))
             `(progn
                (a/defun ,builtin-name (ctx a b)
                         ,(format nil "`(~a a b)`" builtin-name)
                  (multiple-value-bind (lhs-forms lhs-expr) (stash-forms ctx a (gensym "_LHS"))
                    (multiple-value-bind (rhs-forms rhs-expr) (stash-forms ctx b (gensym "_RHS"))
                      (make-parsed-form
                       (append lhs-forms rhs-forms)
                       (caten/ajit:make-expr ,expr-name lhs-expr rhs-expr)
                       ,(if logical-p
                            `(make-const-buffer :bool)
                            `(parsed-form-type a))))))
                (a/defmacro ,lisp-name (&rest forms)
                    ,(format nil "`(~a &rest forms)`" lisp-name)
                  (if (and ',unary (= (length forms) 1))
                      (list ',unary (car forms))
                      (flet ((binary (a b) (list ',builtin-name a b)))
                        (reduce #'binary forms)))))))
  (def _%add :+ +)
  (def _%sub :- - _%neg)
  (def _%mul :* *)
  (def _%div :/ / _%recip)
  (def _%eq :== = nil t)
  (def _%< :< < nil t)
  (def _%<= :<= <= nil t)
  (def _%> :> > nil t)
  (def _%>= :>= >= nil t))

(a/defmacro progn (&rest forms)
    "`(progn &rest forms)`"
  (let ((forms (map 'list #'a/macroexpand-all forms)))
    `(_%progn ,@forms)))

(a/defmacro if (test then &optional else)
    "`(if test then &optional else)`"
  (if else
      `(_%if ,test ,then ,else)
      `(_%if ,test ,then)))

(a/defmacro when (test &rest forms)
    "`(when test &body forms)`"
  `(_%if ,test (progn ,@forms)))

(a/defmacro unless (test &rest forms)
    "`(unless test &body forms)`"
  `(when (not ,test) ,@forms))

(a/defmacro cond (&rest clauses)
    "`(cond &rest clauses)`"
  (labels ((expand (form)
             (if (= (length form) 1)
                 `(when ,(caar form) ,@(cdar form))
                 `(if ,(caar form)
                      (progn ,@(cdar form))
                      ,(expand (cdr form))))))
    (expand clauses)))

(a/defmacro loop (&rest keyword-and-forms)
    "
Implements: https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
`(loop &rest keyword-and-forms)`"
  ;; Reference: https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
  (error "NOT READY!"))

;; How to implement List?
;; How to implement String?
