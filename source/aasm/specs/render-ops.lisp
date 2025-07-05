(in-package :caten/aasm)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass ASTRelay (AType)
  nil)

(defclass RenderOps ()
  ((is-empty :initform nil :initarg :is-empty))
  (:documentation "RenderOps is a class that represents the operation of rendering the node to the target language."))
;;;; Control Flows
(defnode (:Render :RANGE) (RenderOps)
         "
```
BIND <- RANGE(SIZE, STEP, idx=idx, dtype=dtype)
```

The node :RANGE will generate a variable named `idx` (with dtype which is an integer) which moves from zero to SIZE with a step of `STEP`.

The compiler will assume SIZE/STEP is always an integer, or a node typed :EXPR.
"
         :slots ((idx) (dtype)))

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
                 (parallel :initform 0 :type (integer 0))))

(defnode (:Render :IF) (RenderOps)
         "
```
ID <- IF(CONDITION, THEN)
```

The node `IF` will execute `then` only when condition is evaluated to True.

- The variable `CONDITION` is always an EXPR.
- The variable `THEN` is always RenderOps.
"
         :slots nil)

(defnode (:Render :PROGN) (RenderOps)
         "
```
ID <- PROGN(S1, S2, ..., Sn)
```

The node `:PROGN` will execute nodes from S1 to Sn in sequence. S1 ~ Sn is a node which is a type of RenderOps.
"
         :slots nil)

(defnode (:Render :BARRIER) (RenderOps)
         "
```
ID <- BARRIER()
```
" :slots nil)

(defnode (:Render :EXPR) (RenderOps)
         "
```
ID <- EXPR(NODE)
```
"
         :slots nil)

(defnode (:Render :DEFINE-GLOBAL) (RenderOps)
         "
```
X <- ()
```
Declares a buffer.
"
         :slots ((dtype) (pointer-p :type boolean) (mode :type (member :io :read :write) :initform :io)))
;;; JITOps
(defnode (:JIT :Aref) (RenderOps) ;; TODO: Rename Aref -> LOAD?
         "
```
X <- Aref(Array, Index)
```
"
         :slots nil)

(defnode (:JIT :SWIZZLE) (RenderOps)
         "
SWIZZLE(A) is corresponding with:

```
X <- A.[x|y|z|...]
```
Unlike `aref`, a position of the access is fixed.
"
         :slots ((index :type fixnum)))

(defnode (:JIT :SETF) () ;; TODO: Rename SETF -> STORE?
         "
```
ID <- SETF(AREF(TARGET, IDX), EXPR(...)) 
```
Writes the value of EXPR into the corresponding region of AREF.
")

(defnode (:JIT :BIND) ()
         "
```
ID <- BIND(X, value=value)
```"
         :slots ((value)))

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
                 (size)))

(defnode (:Render :DEFINE-SHARED-MEMORY) () "Declares a shared memory in the kenrel."
         :slots ((dtype :type keyword) (size :type integer)))

(defnode (:Render :Function) () ;; [TODO] remove :function?
         ""
         :slots ((name :initform nil :type symbol)))
;; Note: More?
)
