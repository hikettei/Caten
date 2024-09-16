(in-package :caten/aasm)

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
