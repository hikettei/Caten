(in-package :caten/aasm)
;; = [Summary of ops in caten/aasm] ===================================
;; UnaryOps   | {NEG, RECIP, SIN, EXP2, LOG2, SQRT, NOT}       | 7 Ops
;; BinaryOps  | {ADD, MUL, IDIV, AND, OR, XOR, MOVE, MAX, GCD} | 9 Ops
;; TernaryOps | {!=, <, WHERE, WMMA}                           | 4 Ops
;; Buffer     | {ALLOCATE, LOAD, STORE, VIEW}                  | 4 Ops
;; Indexing   | {INDEX-COMPONENTS}                             | 1 Op(s)
;; JIT        | {SPACE}                                        | 1 OP(s)
;; +)__________________________________________________________________
;;                                                             | 26 Ops
(eval-when (:compile-toplevel :load-toplevel :execute)
;; ~~ TypeInference ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass TensorRelay (AType)
  ((shape :accessor tensor-relay-shape :initarg :shape :initform nil :type list)
   (stride :accessor tensor-relay-stride :initarg :stride :initform nil :type list)
   (dtype :accessor tensor-relay-dtype :initarg :dtype :type keyword)
   (views :accessor tensor-relay-views :initarg :views :initform nil :type list)
   (nrank :accessor tensor-relay-nrank :initarg :nrank :initform 0 :type fixnum)
   (value :accessor tensor-relay-value :initarg :value :initform nil)
   (inferred-permute :accessor tensor-relay-inferred-permute :initarg :permute :initform nil)
   (orig-buffer-shape :accessor tensor-relay-orig-buffer-shape :initarg :orig-shape :initform nil)
   (depend-idx-list :accessor tensor-relay-depend-idx-list :initarg :depend-idx-list :initform nil)
   (iterspace :accessor tensor-relay-iterspace :initarg :iterspace :initform nil)))

(defun make-tensor-relay (shape stride dtype views &key (value nil) (permute nil) (orig-shape nil) (depend-idx-list nil) (iterspace nil))
  (declare (type keyword dtype))
  (when (null views) (setf views (loop for s in shape collect nil)))
  (assert (= (length shape) (length stride) (length views)))
  (make-instance 'TensorRelay :shape shape :stride stride :dtype dtype :views views :value value :nrank (length shape) :permute permute :orig-shape orig-shape :depend-idx-list depend-idx-list :iterspace iterspace))

(defun copy-tensor-relay (relay)
  (declare (type TensorRelay relay))
  (make-tensor-relay (copy-list (tensor-relay-shape relay)) (copy-list (tensor-relay-stride relay)) (tensor-relay-dtype relay)
                     (copy-list (tensor-relay-views relay))
                     :value (tensor-relay-value relay)
                     :permute (copy-list (tensor-relay-inferred-permute relay))
                     :depend-idx-list (copy-list (tensor-relay-depend-idx-list relay))
                     :iterspace (tensor-relay-iterspace relay)))

(defun merge-with-initial-value (node-reads realized-args)
  (assert (= (length node-reads) (length realized-args)))
  (loop for nr in node-reads
        for rr in realized-args
        if (numberp nr) ;; i.e.: Constant
          collect nr
        else ;; i.e.: Symbolic
        collect (if rr (or (tensor-relay-value rr) nr) nr)))

(defun assert-verify-tensor-relay (id->type node &key (assert-scalar nil) (nthcdr 0))
  (mapc
   #'(lambda (x nth &aux (type (gethash x id->type)))
       (when type
         (assert (typep type 'TensorRelay) () "TensorIR only accepts TensorRelay typed variables.~%In the ~ath var of node ~a. ~% Got ~a~%" nth node type)
         (when assert-scalar
           (assert (= 0 (tensor-relay-nrank type)) () "In the ~ath variable of node ~a.~%This should be a scalar." nth node))))
   (nthcdr nthcdr (node-reads node))
   (nthcdr nthcdr (range 0 (length (node-reads node))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass JITAble ()
  ((_type_relay :initarg :_type_relay) ;; [TODO] removable
   (_read_views :initform nil :initarg :_read_views) ;; [TODO] Removable
   (_output_type :initform nil :initarg :_output_type) ;; [TODO] Removable
   (declare-type :initarg :declare-type :initform nil) ;; [TODO] Removable if we refactor codegen
   (iterations :initarg :iterations :initform nil) ;; [TODO] Removable if we refactor codegen
   (_lowering_history :initform nil :initarg :_lowering_history) ;; Responsible for determining the kernel_name
   ;; Metadata for Vectorize
   (parent-node-id :initform nil :initarg :parent-node-id) ;; [TODO] Removable
   (unroll-history :initform nil :initarg :unroll-history)) ;; [TODO] Removable
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

(defun make-type-relay (n)
  #'(lambda (id->type node &aux (type (gethash (nth n (node-reads node)) id->type)))
      (assert type () "The variable ~a is not defined in the graph?" (nth n (node-reads node)))
      (assert-verify-tensor-relay id->type node)
      (list
       (make-tensor-relay (copy-list (tensor-relay-shape type)) (copy-list (tensor-relay-stride type))
                          (tensor-relay-dtype type) (copy-list (tensor-relay-views type))
                          :value (when (eql (node-type node) :LOAD) (getattr node :value)) ;; Only :LOAD can create a value!
                          :permute (copy-list (tensor-relay-inferred-permute type)) :orig-shape (copy-list (tensor-relay-orig-buffer-shape type))))))

(defnode (:UnaryOps :NEG) (UnaryOps JITAble)
	 "The node :NEG flips the sign of the first read tensor, writing the result to the first write.
```
out = (-x);
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :RECIP) (UnaryOps JITAble)
	 "The node :RECIP computes the reciprocal of the first read tensor, writing the result to the first write.
```
out = (1/x);
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :SIN) (UnaryOps JITAble)
	 "The node :SIN computes sine of the first read tensor, writing the result to the first write.
```
out = sin(x);
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :EXP2) (UnaryOps JITAble)
	 "The node :EXP2 computes `exp2` of the first read tensor, writing the result to the first write.
```
out = exp2(x);
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :LOG2) (UnaryOps JITAble)
	 "The node :LOG2 computes `log2` of the first read tensor, writing the result to the first write.
```
out = log2(x);
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :SQRT) (UnaryOps JITAble)
	 "The node :SQRT computes square-root of the first read tensor, writing the result to the first write.
```
out = sqrt(x);
```
"
         :type-relay #'unary-type-relay)

(defnode (:UnaryOps :NOT) (UnaryOps JITAble)
	 "The node :NOT computes the logical-not of the given tensor if the input is a boolean, otherwise (integer) computes a bitwise-not.

```
out = not(x) (if boolean)
out = lognot(x) (if integer)
```
"
         :type-relay (make-type-relay 0))

(defnode (:UnaryOps :CAST) (UnaryOps JITAble)
	 "The node :CAST casts the first read tensor into `:dtype`, writing the result into the first write."
	 :slots ((dtype :type dtype-t))
         :type-relay #'(lambda (id->type node &aux (out (funcall (make-type-relay 0) id->type node)))
                         (setf (tensor-relay-dtype (car out)) (getattr node :dtype))
                         out))

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

(defnode (:BinaryOps :Add) (BinaryOps JITAble)
	 "The node :ADD adds the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :MUL) (BinaryOps JITAble)
	 "The node :MUL multiplies the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :MOD) (BinaryOps JITAble)
	 "The node :MOD finds the reminder of the first tensor in `read` divided by the second tensor in `read`.
```
out <- x % y
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :IDIV) (BinaryOps JITAble)
	 "The node :IDIV divides the first tensor in `read` by the second tensor in `read`, writing the result to the first `write`.
Unlike other BinaryOps, :IDIV assumes two tensors to be an integer typed tensor.
```
out <- x / y
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :AND) (BinaryOps JITAble)
	 "The node :AND computes the bit-wise and of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-and.
```
out <- x && y (if boolean)
out <- x & y (if integer)
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :OR) (BinaryOps JITAble)
	 "The node :OR computes the bit-wise or of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-or.
```
out <- x || y (if boolean)
out <- x | y (if integer)
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :XOR) (BinaryOps JITAble)
	 "The node :XOR computes the bit-wise xor of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-xor.
```
out <- x ^ y (if boolean)
out <- x ^ y (if integer)
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :MOVE) (BinaryOps JITAble)
	 "Moves the second read into the first read, setting the result to first write.
```
out <- move(x, y)
where move(x, y) is x = y
```
"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :MAX) (BinaryOps JITAble)
	 "Computes the maximum value of two tensors in read, writing the result to the first write.
```
out <- max(x, y)
```"
         :type-relay (make-type-relay 0))

(defnode (:BinaryOps :GCD) (BinaryOps)
	 "Computes the greatest common divisor of two integer tensors in read, writing the result to the first write.

(Note: This computation should only applied to scalar tensors, and used for only computing the dynamic shaped tensor indexing.)

```
out <- gcd(x, y)
```
"
         :type-relay (make-type-relay 0))

(defclass TernaryOps ()
  nil
  (:documentation "
TernaryOps applies an operation to the first three read tensor, writing the result to the first write.
```
out <- f(x, y, z)
```
"))

(defnode (:TernaryOps :!=) (TernaryOps JITAble)
	 "Compares the second and third tensors in read with `not-equal`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y != z;
out = x;
```
"
         :type-relay (make-type-relay 0))

(defnode (:TernaryOps :<) (TernaryOps JITAble)
	 "Compares the second and third tensors in read with `<`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y < z;
out = x;
```
"
         :type-relay (make-type-relay 0))

(defnode (:TernaryOps :WHERE) (TernaryOps JITAble)
	 "If the result of the first read (boolean) is true, the second read is selected, and if false, the third read is selected and written to the first write. When optimizing in-place, note that the value of the second read is used as a placeholder since choosing the first read would result in a data type mismatch with write.
```
in_dtype = dtype_of(y);
out[in_dtype] = x[boolean] ? y[in_dtype] : z[in_dtype];
```
"
	 :placeholder 1
         :type-relay (make-type-relay 1))

(defnode (:TernaryOps :WMMA) (TernaryOps JITAble)
	 "The node :WMMA is generated during optimization (simplifiers.lisp) by AJIT and represents a fused computation of :ADD and :MUL. WMMA is not generated during VM execution.

WMMA is used to optimize the gemm computation:
```
WMMA(c, a, b) is the equivalent to:
c += a * b      (if reduction = t)
out = c + a * b (if reduction = nil)
```"
	 :slots ((reduction))
         :type-relay (make-type-relay 0))

(defclass BufferOps ()
  nil
  (:documentation "BufferOps performs an operaton related to the buffer."))

(defnode (:Buffer :Allocate) (BufferOps JITAble)
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
                 (pool :initform nil :type (or null Buffer)))
         :type-relay #'(lambda (id->type node)
                         (assert-verify-tensor-relay id->type node :assert-scalar t)
                         (let ((args (merge-with-initial-value (node-reads node) (map 'list #'(lambda (x) (or (gethash x id->type) x)) (node-reads node))))
                               (nrank (getattr node :nrank)))
                           (assert (= (length (node-reads node)) (* 2 nrank)) () "Failed to verify :ALLOCATE. Invaild number of node-reads (~a)" node)
                           (list (make-tensor-relay (subseq args 0 nrank) (subseq args nrank (* 2 nrank))  (getattr node :dtype) nil)))))

(defnode (:Buffer :LOAD) (BufferOps JITAble)
	 "Fills the first tensor in `read` with `value`, writing the result into the first write. The first read can be either of tensor or scalar.
```
x[...] = value;
out = x;
```

- value[symbol or number] initial value.
"
	 :slots ((value))
         :type-relay (make-type-relay 0))

(defnode (:Buffer :STORE) (BufferOps BinaryOps JITAble)
	 "Just like a :MOVE, moves the second tensor in read into the first tensor in read, writing the result to the first write.
(Note: :STORE can be removed in the future refactoring)
"
         :type-relay (make-type-relay 0))

(defnode (:Buffer :VIEW) (BufferOps JITAble)
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
                 (tr :initform nil))
         :type-relay #'(lambda (id->type node)
                         (assert-verify-tensor-relay id->type node :assert-scalar t :nthcdr 1)
                         (macrolet ((nsubseq (x y z) `(subseq ,x (1+ ,y) (1+ ,z))))
                           (let* ((args (merge-with-initial-value (node-reads node) (map 'list #'(lambda (x) (or (gethash x id->type) x)) (node-reads node))))
                                  (nrank (getattr node :nrank))
                                  (shape (nsubseq args 0 nrank))
                                  (upfrom (nsubseq args nrank (* 2 nrank)))
                                  (below (nsubseq args (* 2 nrank) (* 3 nrank)))
                                  (by (nsubseq args (* 3 nrank) (* 4 nrank)))
                                  (stride (nsubseq args (* 4 nrank) (* 5 nrank)))
                                  (bc (getattr node :broadcast))
                                  (base (gethash (car (node-reads node)) id->type)))
                             (assert base ())
                             (assert (= (length (node-reads node)) (+ 1 (* 5 nrank))) () "Failed to verify :VIEW~%Invaild number of node-reads (~a)" node)
                             (list
                              (make-tensor-relay shape stride (tensor-relay-dtype base) (loop for i upfrom 0 below (length shape) collect (list (nth i upfrom) (nth i below) (nth i by) (nth i bc)))
                                                 :permute (getattr node :permute) :orig-shape (copy-list (or (tensor-relay-orig-buffer-shape base) (tensor-relay-shape base)))))))))

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
"
         :type-relay (make-type-relay 0))

) ;; eval-when
