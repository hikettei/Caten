(in-package :caten/ir)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass ASTRelay (AType)
  ((class :initarg :class :type :keyword :reader astrelay-class)
   (ast :initarg :ast :accessor astrelay-ast :initform nil)))

(defmethod print-object ((obj ASTRelay) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ":class ~a" (astrelay-class obj))))

(defclass RenderOps ()
  ((is-empty :initform nil :initarg :is-empty))
  (:documentation "RenderOps is a class that represents the operation of rendering the node to the target language."))

(defun ast-type-map (out-form &rest args)
  (assert (keywordp out-form) () "out-form should be a keyword and determined in a compilation time.")
  (lambda (id->type node)
    (loop for arg in args
          for nth upfrom 0
          for var = (gethash (nth nth (node-reads node)) id->type) do
            (when (symbolp (nth nth (node-reads node)))
              (assert var () "The ~ath argument in the node ~a is not defined." nth node)
              (typecase arg
                (list
                 (assert
                  (or (when (typep var 'ASTRelay) (find (astrelay-class var) arg))
                      (when (find :EXPR arg) (typep var 'TensorRelay)))
                  () "The expected ast as ~ath argument is ~a, in ~a.~%Getting ~a" nth arg node var))
                (function
                 (funcall arg var))
                (otherwise (error "not implemented match case: ~a" arg)))))
     (list (make-instance 'ASTRelay :class out-form :ast node))))
;;;; Control Flows
(defnode (:Render :RANGE) (RenderOps)
         "
```
BIND <- RANGE(SIZE, STEP, idx=idx, dtype=dtype)
```

The node :RANGE will generate a variable named `idx` (with dtype which is an integer) which moves from zero to SIZE with a step of `STEP`.

The compiler will assume SIZE/STEP is always an integer, or a node typed :EXPR.
"
         :slots ((idx) (dtype))
         :type-relay #'(lambda (id->type node)
                         (funcall (ast-type-map :RANGE '(:EXPR) '(:EXPR)) id->type node)
                         (list (make-tensor-relay nil nil :int64 nil :value (getattr node :idx)))))

(defnode (:Render :FOR) (RenderOps)
         "
```
ID <- FOR(RANGE, BODY)
```

The node :FOR will iterate over the range and executes the `BODY` node.

- The variable `RANGE` is always a :RANGE node.
- The variable `BODY` is always a RenderOps node.

Also, once the ASTGraph is constructed the compiler will try to maximize the band depth. Users can access this information via the `band` attribute. Nodes marked as the same `band` has the same band id.

If the `parallel` attribute is set to a positive integer, the compiler will try to parallelize the loop with the specified number of band depth.
"
         :slots ((directive :initform nil) (band :initform nil))
         :type-relay (ast-type-map :FOR '(:EXPR) '(:PROGN :FOR :IF :EXPR :BARRIER)))

(defnode (:Render :IF) (RenderOps)
         "
```
ID <- IF(CONDITION, THEN)
```

The node `IF` will execute `then` only when condition is evaluated to True.

- The variable `CONDITION` is always an EXPR.
- The variable `THEN` is always RenderOps.
"
         :slots nil
         :type-relay (ast-type-map :IF '(:EXPR) '(:PROGN :FOR :IF :EXPR :BARRIER)))

(defnode (:Render :PROGN) (RenderOps)
         "
```
ID <- PROGN(S1, S2, ..., Sn)
```

The node `:PROGN` will execute nodes from S1 to Sn in sequence. S1 ~ Sn is a node which is a type of RenderOps.
"
         :slots nil
         :type-relay (ast-type-map :PROGN))

(defnode (:Render :FUNCTION) (RenderOps)
         "
Function node. Root of ASTGraph.

```
ID <- FUNCTION(BODY, name=symbol)
```

Accepts exactly one argument which is an AST node (RenderOps).
The slot `name` holds the function name (which is a symbol).
"
         :slots ((name :type symbol))
         :type-relay (ast-type-map :FUNCTION '(:PROGN :FOR :IF :EXPR :BARRIER)))

(defnode (:Render :BARRIER) (RenderOps)
         "
```
ID <- BARRIER()
```
Syncs the threads.
" :slots nil
  :type-relay (ast-type-map :BARRIER))

(defnode (:Render :EXPR) (RenderOps)
         "
```
ID <- EXPR(NODE)
```
"
         :slots nil
         :type-relay
         #'(lambda (id->type node)
             (list (copy-tensor-relay (gethash (car (node-reads node)) id->type)))))

(defnode (:Render :DEFINE-GLOBAL) (RenderOps)
         "
```
X <- (name=symbol)
```
Declares a buffer.
"
         :slots ((name) (dtype) (pointer-p :type boolean) (mode :type (member :io :read :write) :initform :io))
         :type-relay #'(lambda (id->type node)
                         (if (getattr node :pointer-p)
                             (funcall (ast-type-map :DEFINE-GLOBAL) id->type node)
                             (list (make-tensor-relay nil nil (getattr node :dtype) nil)))))

(defnode (:Render :DEFINE-LOCAL) (RenderOps)
         "
```
X <- (SIZE1, SIZE2)
```
Declares Declares SIZE1 x SIZE2 x ... memory on local.
"
         :slots ((dtype))
         :type-relay #'(lambda (id->type node)
                         (assert (every #'integerp (node-reads node)))
                         (funcall (ast-type-map :DEFINE-LOCAL) id->type node)))

(defnode (:JIT :Aref) (RenderOps)
         "
```
X <- Aref(Array, Index)
```
Reads a scalar value at index from global memory.
"
         :slots nil
         :type-relay #'(lambda (id->type node)
                         (let ((arg (gethash (car (node-reads node)) id->type)))
                           (assert arg () "First argument for :Aref should be a Tensor.")
                           (cond
                             ((and (typep arg 'ASTRelay) (eql (astrelay-class arg) :DEFINE-GLOBAL)) ;; Load buffer from DRAM
                              (list (make-tensor-relay nil nil (getattr (astrelay-ast arg) :dtype) nil)))
                             ((typep arg 'TensorRelay)
                              (list (make-tensor-relay nil nil (tensor-relay-dtype arg) nil)))
                             (T
                              (error "The first argument for :Aref should be either of :DEFINE-GLOBAL or TensorRelay"))))))

(defnode (:JIT :PolyAref) (RenderOps)
         "

```
X <- (Array, Stride1, Stride2, ..., Aff1, Aff2, ..., nrank=fixnum)
```
PolyAref reads a scalar value from the global buffer at the position computed from the index.

`index = Stride1*Aff1 + Stride2*Aff2 + ...`

To provide a blueprint for automatic optimization by a polyhedral compiler, all `:Aref` nodes in the `RenderGraph` must be replaced with `:PolyAref`, and it must be guaranteed that every memory access at each point is affine. Thus, each byoc backend does not necessary have to support rendering this node.

After optimization is complete, `:PolyAref` will be rewritten back into `:Aref` according to the following expression.

```
X <- Aref(Array, idx=Stride1*Aff1+Stride2+Aff2+...)
```"
         :slots ((nrank :type fixnum))
         :type-relay #'(lambda (id->type node)
                         (let ((arg (gethash (car (node-reads node)) id->type)))
                           (assert arg () "First argument for :PolyAref should be a Tensor. (ID=~a)" (car (node-reads node)))
                           (cond
                             ((and (typep arg 'ASTRelay) (eql (astrelay-class arg) :DEFINE-GLOBAL))
                              (list (make-tensor-relay nil nil (getattr (astrelay-ast arg) :dtype) nil)))
                             ((typep arg 'TensorRelay)
                              (list (make-tensor-relay nil nil (tensor-relay-dtype arg) nil)))
                             (T
                              (error "The first argument for :PolyAref should be either of :DEFINE-GLOBAL or TensorRelay"))))))

(defnode (:JIT :Swizzle) (RenderOps)
         "
```
X <- Swizzle(Array, index[0], index[1], ...)
```
Reads a scalar value from local buffer (DEFINE-LOCAL)
"
         :slots nil
         :type-relay #'(lambda (id->type node)
                         (let ((arg (gethash (car (node-reads node)) id->type)))
                           (cond
                             ((and (typep arg 'ASTRelay) (eql (astrelay-class arg) :DEFINE-LOCAL)) ;; Load buffer from DRAM
                              (list (make-tensor-relay nil nil (getattr (astrelay-ast arg) :dtype) nil)))
                             ((typep arg 'TensorRelay)
                              (list (make-tensor-relay nil nil (tensor-relay-dtype arg) nil)))
                             (T
                              (error "The first argument for :Swizzle should be either of :DEFINE-LOCAL or TensorRelay~% getting ~a" arg))))))

(defnode (:JIT :VECTOR) (RenderOps)
         "
```
X <- VECTOR(LocalVar, size1, size2, ...)
```
"
         :slots ((shape :type list))
         :type-relay #'(lambda (id->type node)
                         (let ((arg (gethash (car (node-reads node)) id->type)))
                           (assert (every #'integerp (cdr (node-reads node))))
                           (cond
                             ((and (typep arg 'ASTRelay) (eql (astrelay-class arg) :DEFINE-LOCAL))
                              (list (make-tensor-relay nil nil (getattr (astrelay-ast arg) :dtype) nil
                                                       :vectorize (cdr (node-reads node)))))
                             ((and (typep arg 'TensorRelay))
                              (list (make-tensor-relay nil nil (tensor-relay-dtype arg) nil
                                                       :vectorize (cdr (node-reads node)))))
                             (T
                              (error "The first argument for :VECTOR should be a SRAM Load (i.e.: :DEFINE-LOCAL)~%Getting ~a" arg))))))

(defnode (:JIT :SETF) () ;; TODO: Rename SETF -> STORE?
         "
```
ID <- SETF(AREF(TARGET, IDX), EXPR(...)) 
```
Writes the value of EXPR into the corresponding region of AREF.
"
         :type-relay (make-type-relay 0)) ;; TODO, (car (node-reads node)) is always :AREF or :EXPR

(defnode (:JIT :BIND) ()
         "
```
ID <- BIND(X, value=value)
```
The X is always EXPR otherwise Caten cannot tpsort ASTGraph. (TODO: Check this during type inference)
"
         :slots ((value))
         :type-relay (make-type-relay 0))
;; Note: More?
)
