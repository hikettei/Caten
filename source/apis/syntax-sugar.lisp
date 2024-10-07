(in-package :caten/apis)
;;(defpackage :caten/ops)
;; TODO: Doing an operator overloading at `c::+` and `c::-` (is it a good idea?)
;; Note that doing this in caten/apis package will decrease the performance even the generid methods are inlined.

(defclass TC (Func)
  ((iterators :initarg :iterators :accessor tc-iterators)
   (expr :initarg :expr :accessor tc-expr)
   (st :initarg :st :accessor tc-st)
   (where :initarg :where :accessor tc-where)))

(defmethod forward ((op TC) &rest inputs)
  (dolist (iterator (tc-iterators op))
    (assert (typep iterator 'cons) () "TC: Each iterator should be a cons cell.")
    (assert (keywordp (car iterator)) () "TC: The key of the iterator should be a keyword.")
    (flet ((shape-p (x) (or (numberp x) (symbolp x) (tensor-p x))))
      (if (eql (car iterator) :~)
          (assert (every #'shape-p (cdr iterator)) () "TC: The shape should be a number, a symbol, or a tensor.")
          (assert (shape-p (cdr iterator)) () "TC: The shape should be a number, a symbol, or a tensor."))))
  ;; need what?
  ;; iterators and range
  ;; :~ = list of shape
  ;; other args -> (keyword . shape)
  (apply #'%solve-st nil (tc-st op) nil nil inputs))

(defmethod backward ((op TC) &optional prev-grad))

(defmethod lower ((op TC) &rest inputs)
  (with-context
      (_ (make-node :EINOPS :TC (list (gensym)) inputs :expr (tc-expr op) :iterators (tc-iterators op)))))

(defun form->expr (form iteration-vars variables)
  (flet ((explore (x) (form->expr x iteration-vars variables)))
    (cond
      ((keywordp form) form)
      ((symbolp form)
       (if (find form iteration-vars)
           `(caten/ajit::make-expr :Const ',form)
           `(caten/ajit::make-expr :Const ,form)))
      ((numberp form) `(caten/ajit::make-expr :Const ,form))
      ((listp form)
       (assert (symbolp (car form)) () "form->expr: The first element of the form should be a symbol.")
       (let ((op (intern (symbol-name (car form)) "KEYWORD")))
         (if (eql op :~)
             (flet ((op (x y)
                      `(caten/ajit::make-expr :CONS ,x ,y)))
               (let ((var (second form))
                     (args (map 'list #'explore (cddr form))))
                 (assert (symbolp var) () "from-expr: ~ is defined as: (~ Tensor_Name[Symbol] &rest args...) butgot: ~a" form)
                 (assert (find (intern (symbol-name var) "KEYWORD") variables :key #'at-name) () "from-expr: The tensor ~a is not declared. form: ~a" var form)
                 `(progn
                    (caten/ajit::make-expr :TAKE ',var ,(reduce #'op args)))))
             (let ((args (map 'list #'explore (cdr form))))
               (progn
                 (assert (typep op 'caten/ajit::op/expr) () "The function ~a is not an valid EXPR. See caten/ajit:op/expr" op)
                 (cond
                   ((find op `(:NEG :SIN :LOG2 :EXP2 :SQRT :NOT :RECIP))
                    (assert (= (length args) 1) () "from->expr: ~a is defined as: (~a x), butgot: ~a " op op form)
                    (assert (not (some #'keywordp args)) () "from->expr: keyword is not an argument for ~a. form=~a" op form)
                    `(caten/ajit::make-expr ,op ,(car args)))
                   ((find op `(:< :<= :> :>= :== :!= :Cast))
                    (assert (= (length args) 2) () "from->expr: ~a is defined as: (~a x y) butgot: ~a" op op form)
                    (when (eql op :cast)
                      (assert (keywordp (second args)) () "from-expr: CAST is defined as: (CAST x dtype) butgot ~a" form)
                      (assert (typep (second args) 'dtype-t) () "from-expr: CAST is defined as: (CAST x dtype) butgot ~a" form))
                    (when (not (eql op :cast))
                      (assert (not (some #'keywordp args)) () "from->expr: keyword is not an argument for ~a. form=~a" op form))
                    `(caten/ajit::make-expr ,op ,@args))
                   ((find op `(:+ :- :* :/ :MAX :MIN :AND :OR :XOR))
                    (assert (> (length args) 1) () "from->expr: ~a is defined as: (~a x y z ...) butgot: ~a (at least the length > 1)" op op form)
                    (assert (not (some #'keywordp args)) () "from->expr: keyword is not an argument for ~a. form=~a" op form)
                    (flet ((op (x y)
                             `(caten/ajit::make-expr ,op ,x ,y)))
                      (reduce #'op args)))
                   (T
                    (error "The op ~a is not supported." op)))))))))))

(defmacro tc ((type &key (where nil)) form)
  "
```
(tc (type &key (where nil)) form)
```

[TODO] Docs

### Syntax

- (~ tensor_name shape...) to aref

```
(funcall
 (tc (\"IN[B IP H W] Weight[OP IP KH KW] -> OUT[B OP H W]\") (+ (~ out b op h w) (* (~ in b ip (+ h kw) (+ w kw)) (~ weight op ip kh kw))))
 in weight)
```

- How to support VM? -> doing (compile nil body)
- How to implement autodiff?
```
"
  (let* ((st (%parse-st type))
         (input-vars (map 'list (compose #'intern #'symbol-name #'at-name) (st-bf st)))
         (iteration-vars
           (remove-duplicates
            (loop for s in (flatten (append (map 'list #'at-shape (st-bf st)) (map 'list #'at-shape (st-aft st))))
                  collect (intern (symbol-name s)))))
         (variables (append (st-aft st) (st-bf st))))
    `(lambda (,@input-vars)
       (declare (type tensor ,@input-vars))
       (st ,type (,@input-vars))
       (forward
        (make-instance
         'TC
         :where ,type
         :st ,(%st->list st)
         :iterators
         (with-st-bind (,type ,@input-vars)
           (append
            ',where
            (list
             ,@(loop for i in iteration-vars
                     collect `(cons ',i ,i)))))
         :expr
         ,(form->expr form iteration-vars variables))
        ,@input-vars))))


(defun einsum (formula &rest operands)
  (declare (type list formula operands))
  (error "deprecated"))
