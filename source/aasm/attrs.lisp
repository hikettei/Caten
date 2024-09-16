(in-package :caten/aasm)
;; [Summary of ops in caten/aasm]
;; UnaryOps   | {NEG, RECIP, SIN, EXP2, SQRT, NOT}             | 6 Ops
;; BinaryOps  | {ADD, MUL, IDIV, AND, OR, XOR, MOVE, MAX, GCD} | 9 Ops
;; TernaryOps | {!=, <, WHERE}                                 | 3 Ops
;; Buffer     | {ALLOCATE, LOAD, STORE, VIEW}                  | 4 Ops
;; Indexing   | {INDEX-COMPONENTS}                             |
;; +)__________________________________________________________________
;;                                                             | 22 Ops

;; (defclass JITAble) (_reads_old) ...)
(defclass UnaryOps ()
  nil
  (:documentation "
UnaryOps applies an operaton to the first given read value and overwrites the result to write.
```
out <- f(x)
```"))

(defattr (:UnaryOps :NEG) (UnaryOps)
	 "The node :NEG flips the sign of the first read tensor, writing the result to the first write.
```
out = (-x);
```
")

(defattr (:UnaryOps :RECIP) (UnaryOps)
	 "The node :RECIP computes the reciprocal of the first read tensor, writing the result to the first write.
```
out = (1/x);
```
")

(defattr (:UnaryOps :SIN) (UnaryOps)
	 "The node :SIN computes sine of the first read tensor, writing the result to the first write.
```
out = sin(x);
```
")

(defattr (:UnaryOps :EXP2) (UnaryOps)
	 "The node :EXP2 computes `exp2` of the first read tensor, writing the result to the first write.
```
out = exp2(x);
```
")

(defattr (:UnaryOps :SQRT) (UnaryOps)
	 "The node :SQRT computes square-root of the first read tensor, writing the result to the first write.
```
out = sqrt(x);
```
")

(defattr (:UnaryOps :NOT) (UnaryOps)
	 "The node :NOT computes the logical-not of the given tensor if the input is a boolean, otherwise (integer) computes a bitwise-not.

```
out = not(x) (if boolean)
out = lognot(x) (if integer) 
")

(defattr (:UnaryOps :CAST) (UnaryOps)
	 "The node :CAST casts the first read tensor into `:dtype`, writing the result into the first write."
	 :slots ((dtype :type dtype-t)))

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

(defattr (:BinaryOps :Add) (BinaryOps)
	 "The node :ADD adds the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```
Unlike other BinaryOps, :ADD is allowed to have more than two values in Read. This is only used when aggregating three or more gradients. i.e.:
```
out <- x + y + z + ...
```")	

(defattr (:BinaryOps :MUL) (BinaryOps)
	 "The node :MUL multiplies the two tensors in `read` and writes the result to the first `write`.
```
out <- x + y
```")

(defattr (:BinaryOps :IDIV) (BinaryOps)
	 "The node :IDIV divides the first tensor in `read` by the second tensor in `read`, writing the result to the first `write`.
Unlike other BinaryOps, :IDIV assumes two tensors to be an integer typed tensor.
```
out <- x / y
```")

(defattr (:BinaryOps :AND) (BinaryOps)
	 "The node :AND computes the bit-wise and of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-and.
```
out <- x && y (if boolean)
out <- x & y (if integer)
```")

(defattr (:BinaryOps :OR) (BinaryOps)
	 "The node :OR computes the bit-wise or of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-or.
```
out <- x || y (if boolean)
out <- x | y (if integer)
```")

(defattr (:BinaryOps :XOR) (BinaryOps)
	 "The node :XOR computes the bit-wise xor of two tensors in `read` if they are integer, otherwise (boolean) computes the logical-xor.
```
out <- x ^ y (if boolean)
out <- x ^ y (if integer)
```")

(defattr (:BinaryOps :MOVE) (BinaryOps)
	 "Moves the second read into the first read, setting the result to first write.
```
out <- move(x, y)
where move(x, y) is x = y
```")

(defattr (:BinaryOps :MAX) (BinaryOps)
	 "Computes the maximum value of two tensors in read, writing the result to the first write.
```
out <- max(x, y)
```")

(defattr (:BinaryOps :GCD) (BinaryOps)
	 "Computes the greatest common divisor of two integer tensors in read, writing the result to the first write.

(Note: This computation should only applied to scalar tensors, and used for only computing the dynamic shaped tensor indexing.)

```
out <- gcd(x, y)
```
")

(defclass TernaryOps ()
  nil
  (:documentation "
TernaryOps applies an operation to the first three read tensor, writing the result to the first write.
```
out <- f(x, y, z)
```
"))

(defattr (:TernaryOps :!=) (TernaryOps)
	 "Compares the second and third tensors in read with `not-equal`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y != z;
out = x;
```
")

(defattr (:TernaryOps :<) (TernaryOps)
	 "Compares the second and third tensors in read with `<`, writing the result to the first write. The first read tensor is an placeholder for the first write tensor and is always boolean.
```
x = y < z;
out = x;
```
")

(defattr (:TernaryOps :WHERE) (TernaryOps)
	 "If the result of the first read (boolean) is true, the second read is selected, and if false, the third read is selected and written to the first write. When optimizing in-place, note that the value of the second read is used as a placeholder since choosing the first read would result in a data type mismatch with write.
```
in_dtype = dtype_of(y);
out[in_dtype] = x[boolean] ? y[in_dtype] : z[in_dtype];
```
")

(defclass BufferOps ()
  nil
  (:documentation "BufferOps performs an operaton related to the buffer."))

(defattr (:Buffer :Allocate) (BufferOps)
	 "Allocates a new matrix of scalar value in the VM.
```
out = allocate(*shape, *stride)
```

:Allocate is defined as described above. The first through `nrank`-th read scalar tensors represent the size of the Tensor, and from the `nrank`-th read to the last, they represent the stride of the allocated Tensor. (they can be sliced using `subseq`)

- dtype[dtype-t] dtype to allocate.
- nrank[(unsigned-byte 32)] a rank of tensor. If set to 0, allocates a scalar.
- from[symbol or buffer or null] If specified, instead of allocating, an already allocated Buffer is used. If a symbol is specified, a buffer is already defined in the variable table of AVM. If buffer is specified, use the buffer directly.
"
	 :slots ((nrank :type (unsigned-byte 32))
		 (dtype :type dtype-t)
		 (from)))

(defattr (:Buffer :LOAD) (BufferOps)
	 "Fills the first tensor in `read` with `value`, writing the result into the first write. The first read can be either of tensor or scalar.
```
x[...] = value;
out = x;
```

- value[symbol or number] initial value.
"
	 :slots ((value)))

(defattr (:Buffer :STORE) (BufferOps BinaryOps)
	 "Just like a :MOVE, moves the second tensor in read into the first tensor in read, writing the result to the first write.
(Note: :STORE can be removed in the future refactoring)
")

(defattr (:Buffer :VIEW) (BufferOps)
	 "Creates a view object of the tensor in a first read.
`View object` can modify the multi-dimensional offset of tensors, strides, shapes, and strides without copying.
```
out = view(x, *upfrom, *below, *by, *shape-new, *stride-new)
```
upfrom and below describes the multi-dimensional offset of the tensor. Caten applies an operation to out in the range of `[upfrom, below)`. by indicates the step of stride. the out tensor is reinitialized with `shape-new` and stride-new`.
View has an attribute `broadcast[list]`, this indicates the stride of thecorresponding axis is recognised as 0 if set to T.

- nrank[(unsigned-byte 32)] the rank of viewed tensor.
- broadcast[list] broadcasting order.
- permute[list] is an optional parameter and does nothing in VM, but requires to apply Polyhedral Compiler. If the view was created in `caten/apis:!permute`, set the argument to this attribute.
"
	 :slots ((nrank :type (unsigned-byte 32))
		 (broadcast :type list)
		 (permute :type list :initform nil)))

(defclass Indexing () nil)
(defattr (:Indexing :Index-Components) (Indexing)
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
