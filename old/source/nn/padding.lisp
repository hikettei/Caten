(in-package :caten/nn)

(defun padding2d-shape (padding dims)
  (declare (type (or integer list) padding)
	   (type fixnum dims))
  (if (integerp padding)
      (loop repeat (* 2 dims) collect padding)
      (if (= (length padding) (* 2 dims))
	  padding
	  (reverse
	   (loop for p in padding append (loop repeat 2 collect p))))))

(defclass Padding (Func) nil
  (:documentation "The Func Padding pads the input_tensor with the specified padding. The argument is given in the following format.
```
(output_tensor, input_tensor, *index_components, *padding_starts, *padding_ends)
```
where each (length index_components), (length padding_starts), and (length padding_ends) == ndim(input_tensor) == ndim(output_tensor).

With JIT=1, everything is expected to be fused in a single EXPR like:
```
out = where(start_0 < index_components[0] < end_0 and start_1 < index_components[1] < end_1 and ..., input_tensor[i=i'-start_0, j=j'_start_1, ...], value)
```
"))

(defmethod padding-args ((op Padding) list)
  (let ((nrank (ndim (car (func-variables op)))))
    (values (first list) (second list) (subseq list 2 (+ 2 nrank)) (subseq list (+ 2 nrank) (+ 2 (* 2 nrank))) (subseq list (+ 2 (* 2 nrank))))))

(defmethod forward ((op Padding) &rest inputs) (st "A[~] -> A[~]" ((car inputs))))

(defmethod backward ((op Padding) &optional prev-dout)
  (multiple-value-bind (_ __ ___ pad-starts pad-ends) (padding-args op (func-variables op))
    (declare (ignore _ __ ___))
    (values nil (apply #'!view prev-dout (loop for start in pad-starts for end in pad-ends collect (list start end))))))

(defmethod lower ((op Padding) &rest inputs)
  (multiple-value-bind (padded-tensor* x* index-components padding-starts padding-ends) (padding-args op inputs)
    (let* ((padded-tensor (car (func-variables op)))
           (x (second (func-variables op)))
           (shape-graph (map 'list #'tensor-graph (tr-shape (tensor-tr padded-tensor))))
           (stride-graph (map 'list #'tensor-graph (tr-stride (tensor-tr x)))))
      (labels ((expand (x)
                 (let ((shape (map 'list #'graph->id shape-graph))) ;; needs to feed an explicit shape for logical ops
                   (%view x shape (make-list (length shape) :initial-element 0) shape (make-list (length shape) :initial-element 1) (make-list (length shape)) (make-list (length shape) :initial-element 0))))
               (A<=B<C (a b c)
                 (let ((shape (map 'list #'graph->id shape-graph)) ;; needs to feed an explicit shape for logical ops
                       (order (tensor-order (car (func-variables op)))))
                   ;; Assumes a/b/c is an integer: A<=B is equivalent to A < B+1 to simplify the graph.
                   (%and (%< shape order (expand (%sub a (%iconst 1))) b) (%< shape order b (expand c)))))
               (graph->id (graph)
                 (if (graph-p graph)
                     (progn
                       (assert (= 1 (length (graph-outputs graph))))
                       (id->value graph (car (graph-outputs graph))))
                     graph)))
        (with-context-from-parents
            (shape-graph stride-graph)
            (conditions (map 'list #'A<=B<C padding-starts index-components padding-ends))
            (x* (%view x* (map 'list #'graph->id shape-graph)
                       (map 'list #'%neg padding-starts) (map 'list #'%sub padding-ends padding-starts) (loop repeat (length padding-starts) collect 1)
                       (loop repeat (length padding-starts) collect nil)
                       (map 'list #'graph->id stride-graph)))
            (output (%where (%not (reduce #'%and conditions)) padded-tensor* x*))))))) ;; padded-tensor* is a first args because it is a returned value and has a larger size than x*.
(defun !padding (x padding &key (value 0.0))
  "
```
(!padding x padding &key (value 0.0))
```
Pads the tensor `x` with the specified padding and value.

`Padding` is specified as: `((pad_0_1 pad_0_2) (pad_1_0 pad_1_0) ...)`. If `T` is provided instead of `(pad_0_0 pad_0_1)`, that means no padding is applied to the axis. (that is, `T` = `(0 0)`)

Each start_0 and pad_0 is expected as a positive integer (caten will not check this!), or a scalar int32/int64 tensor.
"
  (declare (type tensor x) (type list padding))
  (assert (= (ndim x) (length padding)) () "!padding: padding must have the same length as the number of dimensions of the input tensor=~a." (ndim x))
  (assert (every #'(lambda (x) (or (eql x t) (and (listp x) (= 2 (length x))))) padding) () "!padding: padding must be a list of pairs (start_pos end_pos), or t.")
  (let* ((x (!contiguous x)) ;; x must be a contiguous tensor because Padding overwrites the view.
         (new-shape (loop for pad in padding
                          for shape in (tr-shape (tensor-tr x))
                          if (eql pad t)
                            collect shape
                          else
                            collect (!+ (caten/api::->iconst shape) (caten/api::->iconst (first pad)) (caten/api::->iconst (second pad)))))
         (pad-slice (loop for pad in padding ;; normalize to new-shape2
                          for shape in new-shape
                          for base-shape in (tr-shape (tensor-tr x))
                          if (eql pad t) collect (list (iconst 0) (caten/api::->iconst shape)) ;; all inputs must be a tensor to pass the forward
                            else collect (list (caten/api::->iconst (first pad)) (!+ (caten/api::->iconst (first pad)) (caten/api::->iconst base-shape)))))
         (padded-tensor (make-tensor new-shape :dtype (dtype-of x) :initial-element value))
         (index-components (map 'list #'(lambda (n) (!gid padded-tensor n :dtype *default-int*)) (range 0 (ndim x)))))
    (apply #'forward (make-instance 'Padding) (flatten (list padded-tensor x index-components (map 'list #'first pad-slice) (map 'list #'second pad-slice))))))

(defun !padding2d (x padding &key (value 0.0))
  "
```
(!padding2d x padding &key (value 0.0))
```
Pads the last two dimension of the tensor `x` with the specified padding and value..

padding is specified as: (padding_left padding_right padding_top padding_bottom)."
  (declare (type Tensor x) (type list padding) (type number value))
  (assert (= 4 (length padding)) () "!padding2d expects 4 elements in padding: (padding_left, padding_rightm padding_top, padding_bottom), getting ~a" padding)
  (assert (>= (ndim x) 2) () "!padding2d: x must have at least 2 dimensions, getting ~a" x)
  ;; No Pads?
  (when (every #'(lambda (x) (and (numberp x) (= 0 x))) padding) (return-from !padding2d x))
  (let* ((pad-args (list (list (first padding) (second padding)) (list (third padding) (fourth padding))))
         (noops (make-list (- (ndim x) 2) :initial-element t)))
    (!padding x (append noops pad-args) :value value)))
