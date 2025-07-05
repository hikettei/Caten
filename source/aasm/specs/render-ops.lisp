(in-package :caten/aasm)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass ASTRelay (AType)
  ((class :initarg :class :type :keyword :reader astrelay-class)))

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
                 (assert (find (astrelay-class var) arg) () "The expected ast as ~ath argument is ~a, in~%~a" nth arg node))
                (function
                 (funcall arg var))
                (otherwise (error "not implemented match case: ~a" arg)))))
     (list (make-instance 'ASTRelay :class out-form))))
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
         :type-relay (ast-type-map :RANGE '(:EXPR) '(:EXPR)))

(defnode (:Render :FOR) (RenderOps)
         "
```
ID <- FOR(RANGE, BODY)
```

The node :FOR will iterate over the range and executes the `BODY` node.

- The variable `RANGE` is always a :RANGE node.
- The variable `BODY` is always a RenderOps node.

This node also have an special attribute named `mark` for specifying the optimization strategy.

Mark  specifies the type of loop which is exploited by the compiler to optimize the code. It is user's responsibility to ensure the validity of the mark.

Mark should be one of the following based on the nature of the loop.

```
- coincident (which means the loop is parallelizable)
- reduction  (which means the loop is reduction)
- noopt      (which means the loop is not optimized)
```

Also, once the ASTGraph is constructed the compiler will try to maximize the band depth. Users can access this information via the `band` attribute. Nodes marked as the same `band` has the same band id.

If the `parallel` attribute is set to a positive integer, the compiler will try to parallelize the loop with the specified number of band depth.
"
         :slots ((mark :type (member :coincident :reduction :noopt) :initform :noopt)
                 (band :initform nil)
                 (parallel :initform 0 :type (integer 0)))
         :type-relay (ast-type-map :FOR '(:RANGE) '(:PROGN :FOR :IF :EXPR :BARRIER)))

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

(defnode (:Render :BARRIER) (RenderOps)
         "
```
ID <- BARRIER()
```
" :slots nil
  :type-relay (ast-type-map :BARRIER))

(defnode (:Render :EXPR) (RenderOps)
         "
```
ID <- EXPR(NODE)
```
"
         :slots nil
         :type-relay (ast-type-map :EXPR)) ;; [TODO] Verify!

(defnode (:Render :DEFINE-GLOBAL) (RenderOps)
         "
```
X <- ()
```
Declares a buffer.
"
         :slots ((dtype) (pointer-p :type boolean) (mode :type (member :io :read :write) :initform :io))
         :type-relay (ast-type-map :DEFINE-GLOBAL))
;;; JITOps
(defnode (:JIT :Aref) (RenderOps) ;; TODO: Rename Aref -> LOAD?
         "
```
X <- Aref(Array, Index)
```
"
         :slots nil
         :type-relay (ast-type-map :Aref))

(defnode (:JIT :SWIZZLE) (RenderOps)
         "
SWIZZLE(A) is corresponding with:

```
X <- A.[x|y|z|...]
```
Unlike `aref`, a position of the access is fixed.
"
         :slots ((index :type fixnum))
         :type-relay (ast-type-map :SWIZZLE))

(defnode (:JIT :SETF) () ;; TODO: Rename SETF -> STORE?
         "
```
ID <- SETF(AREF(TARGET, IDX), EXPR(...)) 
```
Writes the value of EXPR into the corresponding region of AREF.
"
         :type-relay (ast-type-map :SETF))

(defnode (:JIT :BIND) ()
         "
```
ID <- BIND(X, value=value)
```"
         :slots ((value))
         :type-relay (ast-type-map :BIND))

(defnode (:JIT :SPACE) () ;; TODO: Rename SPACE -> GID?
         "
Corresponds to:
```
[blockIdx|threadIdx].[rank]
```
"
         :slots ((level :type (member :block :thread))
                 (rank  :type (integer 0 3))
                 (dtype :type keyword)
                 (size))
         :type-relay (ast-type-map :SPACE))
         
(defnode (:Render :DEFINE-SHARED-MEMORY) () "Declares a shared memory in the kenrel."
         :slots ((dtype :type keyword) (size :type integer))
         :type-relay (ast-type-map :DEFINE-SHARED-MEMORY))

;; Note: More?
)
