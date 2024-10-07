(in-package :caten/apis)
;;(defpackage :caten/ops)
;; TODO: Doing an operator overloading at `c::+` and `c::-` (is it a good idea?)
;; Note that doing this in caten/apis package will decrease the performance even the generid methods are inlined.

(defclass TC (Func)
  ((iterators :initarg :iterators :accessor tc-iterators)
   (expr :initarg :expr :accessor tc-expr)
   (st :initarg :st :accessor tc-st)
   (inputs :initarg :inputs :accessor tc-inputs)
   (outputs :initarg :outputs :accessor tc-outputs)
   (where :initarg :where :accessor tc-where)))
;; Infinite-Rank is not working not.
(defmethod lower-into-lisp ((op TC))
  (with-slots ((expr expr) (inputs inputs) (outputs outputs) (iterators iterators)) op
    (let* ((infinite (find "~" iterators :key (compose #'symbol-name #'car) :test #'equalp))
           (infinite-idx
             (loop for i in (cdr infinite)
                   collect (cons (gensym) i)))
           (iterators (append infinite-idx (loop for idx in iterators
                                                 unless (equalp (symbol-name (car idx)) "~")
                                                   collect idx))))
      `(lambda (,@inputs)
         (with-st-bind (,(tc-where op) (map 'list #'make-tensor (map 'list #'buffer-shape (list ,@inputs))))
           ,(labels ((explore (dim)
                       (if (= dim -1)
                           `(setf
                             ,(read-from-string (caten/ajit:render-expr (caten/ajit:default-device :lisp) (caten/ajit:expr-x expr)))
                             ,(read-from-string (caten/ajit:render-expr (caten/ajit:default-device :lisp) expr)))
                           (let ((iter (nth dim iterators)))
                             `(dotimes (,(car iter) ,(cdr iter))
                                ,(explore (1- dim)))))))
              (explore (1- (length iterators)))))))))

(defmethod print-object ((op TC) stream)
  (format stream "<TC: ~a>" (tc-where op)))

(defmethod forward ((op TC) &rest inputs)
  (dolist (iterator (tc-iterators op))
    (assert (typep iterator 'cons) () "TC: Each iterator should be a cons cell.")
    (assert (symbolp (car iterator)) () "TC: The key of the iterator should be a symbol.")
    (flet ((shape-p (x) (or (numberp x) (symbolp x) (tensor-p x))))
      (if (equalp (symbol-name (car iterator)) "~")
          (assert (every #'shape-p (cdr iterator)) () "TC: The shape should be a number, a symbol, or a tensor. ~a" iterator)
          (assert (shape-p (cdr iterator)) () "TC: The shape should be a number, a symbol, or a tensor. ~a" iterator))))
  (apply #'%solve-st nil (tc-st op) nil nil inputs))
(defmethod backward ((op TC) &optional prev-grad))
(defmethod lower ((op TC) &rest inputs &aux (body (lower-into-lisp op)))
  (with-context
      (_ (emit (make-node
                :EINOPS :TC (list (gensym)) (map 'list #'node->id inputs)
                :expr (tc-expr op) :iterators (tc-iterators op)
                :inputs (tc-inputs op) :outputs (tc-outputs op)
                :_lisp-code body)))))

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
                    (caten/ajit::make-expr :TAKE ',var ,(reduce #'op (reverse args))))))
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

  ;; iterators and range
  ;; :~ = list of shape
  ;; other args -> (keyword . shape)
- How to support VM? -> doing (compile nil body)
- How to implement autodiff?
```
"
  (let* ((st (%parse-st type))
         (input-vars (map 'list (compose #'intern #'symbol-name #'at-name) (st-bf st)))
         (output-vars (map 'list (compose #'intern #'symbol-name #'at-name) (st-aft st)))
         (iteration-vars
           (remove-duplicates
            (loop for s in (flatten (append (map 'list #'at-shape (st-bf st)) (map 'list #'at-shape (st-aft st))))
                  collect (intern (symbol-name s)))))
         (variables (append (st-aft st) (st-bf st))))
    `(lambda (,@input-vars)
       (declare (type tensor ,@input-vars))
       (st ,type (,@input-vars))
       (apply
        #'forward
        (make-instance
         'TC
         :where ,type
         :st ,(%st->list st)
         :inputs  ',input-vars
         :outputs ',output-vars
         :iterators
         (with-st-bind (,type ,@input-vars)
           (append
            ',where
            (list
             ,@(loop for i in iteration-vars
                     collect `(cons ',i ,i)))))
         :expr
         ,(form->expr form (append (map 'list #'car where) iteration-vars) variables))
        ;; View creations are not allowed
        (map
         'list
         #'!contiguous
         (list ,@input-vars))))))

(defun einsum (formula &rest operands)
  (declare (type list formula operands))
  (error "deprecated"))
