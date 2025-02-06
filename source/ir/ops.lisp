(in-package :caten/ir)
;; = [Summary of TensorOps] ===========================================
;; UnaryOps   | {NEG, RECIP, SIN, EXP2, LOG2, SQRT, NOT}       | 7 Ops
;; BinaryOps  | {ADD, MUL, IDIV, AND, OR, XOR, MOVE, MAX, GCD} | 9 Ops
;; TernaryOps | {!=, <, WHERE, WMMA}                           | 4 Ops
;; Buffer     | {ALLOCATE, LOAD, STORE, VIEW}                  | 4 Ops
;; JIT        | {SPACE, AREF, SETF, BIND}                      | 4 Ops
;; +)__________________________________________________________________
;;                                                             | 28 Ops
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass TypedNode () ;; = it is possible to use the node as an AST.
  ((src-types :initarg :src-types :initform nil) (dst-types :initarg :dst-types :initform nil)))

(defclass JITAble () ;; = it is valid to pass the node to the scheduler.lisp
  ((_type_relay :initarg :_type_relay)
   (_read_views :initform nil :initarg :_read_views)
   (declare-type :initarg :declare-type :initform nil)
   (iterations :initarg :iterations :initform nil)
   (_lowering_history :initform nil :initarg :_lowering_history)) ;; a list of parent module names which is used to create the kernel name.
  (:documentation "This node is jitable.
- declare-type[boolean] When this option is set to T, it is necessary to declare the types of the variables included in. e.g.:
```
x = a + b;
->
float x = a + b;
when :declare-type = [x]
```
"))

(defclass UnaryOps ()
  nil
  (:documentation "
UnaryOps applies an operaton to the first given read value and overwrites the result to write.
```
out <- f(x)
```"))

(defnode (:UnaryOps :NEG) (UnaryOps JITAble TypedNode)
	 "The node :NEG flips the sign of the first read tensor, writing the result to the first write.
```
out = (-x);
```
")

(defnode (:UnaryOps :RECIP) (UnaryOps JITAble TypedNode)
	 "The node :RECIP computes the reciprocal of the first read tensor, writing the result to the first write.
```
out = (1/x);
```
")

(defnode (:UnaryOps :SIN) (UnaryOps JITAble TypedNode)
	 "The node :SIN computes sine of the first read tensor, writing the result to the first write.
```
out = sin(x);
```
")

(defnode (:UnaryOps :EXP2) (UnaryOps JITAble TypedNode)
	 "The node :EXP2 computes `exp2` of the first read tensor, writing the result to the first write.
```
out = exp2(x);
```
")

(defnode (:UnaryOps :LOG2) (UnaryOps JITAble TypedNode)
	 "The node :LOG2 computes `log2` of the first read tensor, writing the result to the first write.
```
out = log2(x);
```
")

(defnode (:UnaryOps :SQRT) (UnaryOps JITAble TypedNode)
	 "The node :SQRT computes square-root of the first read tensor, writing the result to the first write.
```
out = sqrt(x);
```
")

(defnode (:UnaryOps :NOT) (UnaryOps JITAble TypedNode)
	 "The node :NOT computes the logical-not of the given tensor if the input is a boolean, otherwise (integer) computes a bitwise-not.

```
out = not(x) (if boolean)
out = lognot(x) (if integer)
```
")

(defclass BinaryOps ()
  ((reduction :initarg :reduction :initform nil :type boolean)
   (wrap-around :initarg :wrap-around :initform nil :type boolean))
  (:documentation "
BinaryOps applies an operation to the two given read values and overwrites the result to write.
```
out <- f(x, y)
```
- reduction[boolean] When this option is set to T, the node overwrites the result to the first read. i.e.:
```
x <- f(x, y)
``
- wrap-around[boolean] When this option is set to T, it suggests that overflow may occur as a result of the computation. If the backend in use does not exhibit the behavior of wrapping around to the minimum value of the data type when the maximum value is exceeded, this needs to be implemented intentionally. (This behaviour is assumed by threefry2x32 as of this writing: 2024/9/16) Only the :ADD and :MUL requires this behaviour.
"))

(defnode (:BinaryOps :Add) (BinaryOps JITAble TypedNode)
	 "The node :ADD adds the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```")

(defnode (:BinaryOps :MUL) (BinaryOps JITAble TypedNode)
	 "The node :MUL multiplies the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```")

(defnode (:BinaryOps :MOD) (BinaryOps JITAble TypedNode)
	 "The node :MOD finds the reminder of the first tensor in `read` divided by the second tensor in `read`.
```
out <- x % y
```")

(defnode (:BinaryOps :IDIV) (BinaryOps JITAble TypedNode)
	 "The node :IDIV divides the first tensor in `read` by the second tensor in `read`, writing the result to the first `write`.
Unlike other BinaryOps, :IDIV assumes two tensors to be an integer typed tensor.
```
out <- x / y
```")

(defnode (:BinaryOps :AND) (BinaryOps JITAble TypedNode)
	 "The node :AND computes the bit-wise and of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-and.
```
out <- x && y (if boolean)
out <- x & y (if integer)
```")

(defnode (:BinaryOps :OR) (BinaryOps JITAble TypedNode)
	 "The node :OR computes the bit-wise or of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-or.
```
out <- x || y (if boolean)
out <- x | y (if integer)
```")

(defnode (:BinaryOps :XOR) (BinaryOps JITAble TypedNode)
	 "The node :XOR computes the bit-wise xor of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-xor.
```
out <- x ^ y (if boolean)
out <- x ^ y (if integer)
```")

(defnode (:BinaryOps :MOVE) (BinaryOps JITAble TypedNode)
	 "Moves the second read into the first read, setting the result to first write.
```
out <- move(x, y)
where move(x, y) is x = y
```
")

(defnode (:BinaryOps :MAX) (BinaryOps JITAble TypedNode)
	 "Computes the maximum value of two tensors in read, writing the result to the first write.
```
out <- max(x, y)
```")

(defnode (:BinaryOps :GCD) (BinaryOps)
	 "Computes the greatest common divisor of two integer tensors in read, writing the result to the first write.

(Note: This computation should only applied to scalar tensors, and used for only computing the dynamic shaped tensor indexing.)

```
out <- gcd(x, y)
```
")

(defnode (:BinaryOps :CAST) (BinaryOps JITAble TypedNode)
	 "
```
OUT <- CAST(OUT, X)
```
The node :CAST casts the first read tensor into `:dtype`, writing the result into the first write."
	 :slots ((dtype :type dtype-t)))

(defclass TernaryOps ()
  nil
  (:documentation "
TernaryOps applies an operation to the first three read tensor, writing the result to the first write.
```
out <- f(x, y, z)
```
"))

(defnode (:TernaryOps :!=) (TernaryOps JITAble TypedNode)
	 "Compares the second and third tensors in read with `not-equal`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y != z;
out = x;
```
")

(defnode (:TernaryOps :<) (TernaryOps JITAble TypedNode)
	 "Compares the second and third tensors in read with `<`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y < z;
out = x;
```
")

(defnode (:TernaryOps :WHERE) (TernaryOps JITAble TypedNode)
	 "If the result of the first read (boolean) is true, the second read is selected, and if false, the third read is selected and written to the first write. When optimizing in-place, note that the value of the second read is used as a placeholder since choosing the first read would result in a data type mismatch with write.
```
in_dtype = dtype_of(y);
out[in_dtype] = x[boolean] ? y[in_dtype] : z[in_dtype];
```
"
	 :placeholder 1)

(defnode (:TernaryOps :WMMA) (TernaryOps JITAble TypedNode)
	 "The node :WMMA is generated during optimization (simplifiers.lisp) by AJIT and represents a fused computation of :ADD and :MUL. WMMA is not generated during VM execution.

WMMA is used to optimize the gemm computation:
```
WMMA(c, a, b) is the equivalent to:
c += a * b      (if reduction = t)
out = c + a * b (if reduction = nil)
```"
	 :slots ((reduction)))

(defclass BufferOps ()
  nil
  (:documentation "BufferOps performs an operaton related to the buffer."))

(defnode (:Buffer :Allocate) (BufferOps JITAble TypedNode)
	 "Allocates a new matrix of scalar value in the VM.
```
out = allocate(*shape, *stride)
```

:Allocate is defined as described above. The first through `nrank`-th read scalar tensors represent the size of the Tensor, and from the `nrank`-th read to the last, they represent the stride of the allocated Tensor. (they can be sliced using `subseq`)

- dtype[dtype-t] dtype to allocate.
- nrank[(unsigned-byte 32)] a rank of tensor. If set to 0, allocates a scalar.
- from[symbol or buffer or null] If specified, instead of allocating, an already allocated Buffer is used. If a symbol is specified, a buffer is already defined in the variable table of GraphRuntime. If buffer is specified, use the buffer directly.
- pool[null or Buffer] A place to store the result of the previous allocation. Allocation will be performed only after this slot is set to nil, or size are different due to dynamic shape.
"
	 :slots ((nrank :type (unsigned-byte 32))
		 (dtype :type dtype-t)
		 (from :initform nil)
                 (pool :initform nil :type (or null Buffer))))

(defnode (:Buffer :LOAD) (BufferOps JITAble TypedNode)
	 "Fills the first tensor in `read` with `value`, writing the result into the first write. The first read can be either of tensor or scalar.
```
x[...] = value;
out = x;
```

- value[symbol or number] initial value.
"
	 :slots ((value)))

(defnode (:Buffer :STORE) (BufferOps BinaryOps JITAble TypedNode)
	 "Just like a :MOVE, moves the second tensor in read into the first tensor in read, writing the result to the first write.
(Note: :STORE can be removed in the future refactoring)
")

(defnode (:Buffer :VIEW) (BufferOps JITAble TypedNode)
	 "Creates a view object of the tensor in a first read.
`View object` can modify the multi-dimensional offset of tensors, strides, shapes, and strides without copying.
```
out = view(x, *shape-new, *upfrom, *below, *by, *stride-new)
```
upfrom and below describes the multi-dimensional offset of the tensor. Caten applies an operation to out in the range of `[upfrom, below)`. by indicates the step of stride. the out tensor is reinitialized with `shape-new` and stride-new`.
View has an attribute `broadcast[list]`, this indicates the stride of thecorresponding axis is recognised as 0 if set to T.

- nrank[(unsigned-byte 32)] the rank of viewed tensor.
- broadcast[list] broadcasting order.
- permute[list] is an optional parameter and does nothing in VM, but requires to apply Polyhedral Compiler. If the view was created in `caten/api:!permute`, set the argument to this attribute.
"
	 :slots ((nrank :type (unsigned-byte 32))
		 (broadcast :type list)
		 (permute :type list :initform nil)
                 (tr :initform nil)))

(defclass Indexing () nil)
(defnode (:Indexing :Index-Components) (Indexing JITAble)
	 "The node :INDEX-COMPONENTS Indicates which element-wise computation of the Tensor is being performed. Typically, it should return the argument used when performing Aref on the Tensor with the corresponding `strides`.

```
out <- index_components(x, *strides)
```
is compiled as:
```
for i=0..N
  for j=0..M
    out[i, j] = stride[0] * i + j;
```
")

) ;; eval-when
;; ~~ [Render Ops] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass RenderOps ()
  ((is-empty :initform nil :initarg :is-empty))
  (:documentation "RenderOps is a class that represents the operation of rendering the node to the target language."))
;;;; Control Flows
(defnode (:Render :RANGE) (RenderOps TypedNode)
         "
```
BIND <- RANGE(SIZE, STEP, idx=idx, dtype=dtype)
```

The node :RANGE will generate a variable named `idx` (with dtype which is an integer) which moves from zero to SIZE with a step of `STEP`.

The compiler will assume SIZE/STEP is always an integer, or a node typed :EXPR.
"
         :slots ((idx) (dtype)))

(defnode (:Render :FOR) (RenderOps TypedNode)
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
"
         :slots ((mark :type (member :coincident :reduction :noopt) :initform :noopt)
                 (band :initform nil)))

(defnode (:Render :IF) (RenderOps TypedNode)
         "
```
ID <- IF(CONDITION, THEN)
```

The node `IF` will execute `then` only when condition is evaluated to True.

- The variable `CONDITION` is always an EXPR.
- The variable `THEN` is always RenderOps.
"
         :slots nil)

(defnode (:Render :PROGN) (RenderOps TypedNode)
         "
```
ID <- PROGN(S1, S2, ..., Sn)
```

The node `:PROGN` will execute nodes from S1 to Sn in sequence. S1 ~ Sn is a node which is a type of RenderOps.
"
         :slots nil)

(defnode (:Render :BARRIER) (RenderOps TypedNode)
         "
```
ID <- BARRIER()
```
" :slots nil)

(defnode (:Render :EXPR) (RenderOps TypedNode) ;; TODO: Rename EXPR -> ALU?
         "
```
ID <- EXPR(NODE)
```
ALU
"
         :slots nil)

(defnode (:Render :DEFINE-GLOBAL) (RenderOps TypedNode)
         "
```
X <- ()
```
Declares a buffer.
"
         :slots ((dtype) (pointer-p :type boolean)))
;;; JITOps
(defnode (:JIT :Aref) (RenderOps TypedNode) ;; TODO: Rename Aref -> LOAD?
         "
```
X <- Aref(Array, Index)
```
"
         :slots nil)

(defnode (:JIT :SETF) (TypedNode) ;; TODO: Rename SETF -> STORE?
         "
```
ID <- SETF(AREF(TARGET, IDX), EXPR(...)) 
```
Writes the value of EXPR into the corresponding region of AREF.
")

(defnode (:JIT :BIND) (TypedNode)
         "
```
ID <- BIND(X, value=value)
```"
         :slots ((value)))

(defnode (:JIT :SPACE) (TypedNode) ;; TODO: Rename SPACE -> GID?
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

(defnode (:Render :DEFINE-SHARED-MEMORY) (TypedNode) "Declares a shared memory in the kenrel."
         :slots ((dtype :type keyword) (size :type integer)))

(defnode (:Render :Function) (TypedNode) ;; [TODO] remove :function?
         ""
         :slots ((name :initform nil :type symbol)))
;; Note: More?
)
