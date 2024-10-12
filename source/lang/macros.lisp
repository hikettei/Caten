(in-package :caten/lang)

;; Binary Primitive Operations
(macrolet ((def (builtin-name expr-name lisp-name &optional (unary nil) (logical-p nil))
             (assert (typep expr-name 'caten/ajit:op/expr))
             `(progn
                (a/defun ,builtin-name (ctx a b)
                         ,(format nil "`(~a a b)`" builtin-name)
                  (multiple-value-bind (lhs-forms lhs-expr) (stash-forms ctx a (gensym "_LHS") t)
                    (multiple-value-bind (rhs-forms rhs-expr) (stash-forms ctx b (gensym "_RHS") t)
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
  (def _%>= :>= >= nil t)
  (def _%and :AND and)
  (def _%or :OR or)
  (def _%xor :XOR xor)
  (def _%mod :% mod)
  (def _%max :max max)
  (def _%min :min min))

;; Unary Primitive Operations
(macrolet ((def (builtin-name expr-name lisp-name)
             (assert (typep expr-name 'caten/ajit:op/expr))
             `(progn
                (a/defun ,builtin-name (ctx x)
                  ,(format nil "`(~a a)`" builtin-name)
                  (multiple-value-bind (forms expr) (stash-forms ctx x (gensym "_UNARY") t)
                    (make-parsed-form
                     forms
                     (caten/ajit:make-expr ,expr-name expr)
                     (parsed-form-type x))))
                (a/defmacro ,lisp-name (x)
                    ,(format nil "`(~a a)`" lisp-name)
                  (list ',builtin-name x)))))
  (def _%not :not not)
  (def _%sin :sin sin)
  (def _%log2 :log2 log2)
  (def _%exp2 :exp2 exp2)
  (def _%sqrt :sqrt sqrt))

(a/defmacro progn (&rest forms)
    "`(progn &rest forms)`"
  (let ((forms (map 'list #'a/macroexpand-all forms)))
    `(_%progn ,@forms)))

(a/defmacro if (test then &optional else)
    "`(if test then &optional else)`"
  (if else
      `(_%if ,test (progn ,then) (progn ,else))
      `(_%if ,test (progn ,then))))

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

(a/defmacro let (bindings &rest body)
    "`(let bindings &rest body)`"
  (labels ((explore (rest-forms)
             (if rest-forms
                 (multiple-value-bind (var form) (values (car (car rest-forms)) (cdr (car rest-forms)))
                   (assert (= (length form) 1) () "let: Only single expression allowed in binding")
                   (assert (symbolp var) () "let: Variable name must be a symbol")
                   `(when t (_%setf :t ,var ,@form) ,(explore (cdr rest-forms))))
                 `(progn ,@body))))
    (explore bindings)))

(a/defmacro dotimes (var-count &optional (result) &rest body)
    "
```
(dotimes (var count &optional result) &rest body)
```

Iterates the body over the range of [0, count). The form returns `result` if specified. The variable `var` is bound to the current iteration index.
"
  (multiple-value-bind (var count) (apply #'values var-count)
    `(let ((,var 0))
       (_%while (< ,var ,count) (progn (setf ,var (+ 1 ,var)) ,@body ,(or result var))))))

(a/defmacro setf (place value)
    "`(setf place value)`"
  ;; [TODO] (setf (aref ...))
  `(_%setf :nil ,place ,value))

(a/defmacro loop (&rest keyword-and-forms)
    "
TODO: Implements: https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
`(loop &rest keyword-and-forms)`"
  ;; Reference: https://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
  (error "NOT READY!"))

;; Return
;; Values
