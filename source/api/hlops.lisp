(in-package :caten/api)

(defclass HLOps () nil)
;; (defnode (:HLOps :SIGMOID))
;; [TODO] Cleanup!

(defun !sum (x &key (axis t) (keepdims nil))
  (multiple-value-bind (new-shape new-view dims) (parse-reduce-axes x axis)
    (let* ((out (make-tensor new-shape :dtype (tensor-dtype x) :initial-element 0.0))
	   (out (apply #'!view out new-view))
	   (out (!add out x :reduction t))
           (out (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view))))
      (if keepdims
          out
          (!squeeze out dims)))))

(defun !matmul (x y)
  (declare (type Tensor x y))
  (multiple-value-bind (n1 n2) (values (tensor-nrank x) (tensor-nrank y))
    (assert (= n1 n2) () "Cannot multiply matrices with different dimensions. Are they properly broadcasted?~%A: ~a~%B: ~a" x y)
    (let* ((mid (loop for i upfrom 0 below (min (- n1 1) (- n2 1) 1) collect 1))
	   (x (!reshape x `(,@(butlast (tensor-shape x) 1) ,@mid ,(car (last (tensor-shape x))))))
	   (y (!reshape y `(,@(butlast (tensor-shape y) 2) ,@mid ,@(last (tensor-shape y) (min n2 2))))))
      (!sum (!mul x (!transpose y -1 (- (min n2 2)))) :axis -1))))

(defun conv-out-size (in padding dilation kernel-size stride &key (ceiling #'floor))
  ;; TODO: Support Symbolic (needs !floor function)
  (funcall ceiling (+ 1 (/ (+ in (* 2 padding) (* (- dilation) (- kernel-size 1)) -1) stride))))

(defun maybe-list (value kernel-size)
  (if (numberp value)
      (if (numberp kernel-size)
	  value
	  (loop repeat (length kernel-size) collect value))
      (progn
	(assert (and (listp value) (listp kernel-size) (= (length value) (length kernel-size))))
	value)))

(defun compute-filter-size (in dilation kernel stride &key (ceiling #'ceiling))
   (funcall ceiling (/ (- in (* dilation (- kernel 1))) stride)))

(defmacro slice (list upfrom &optional (below `(length ,list)) (by 1))
  (alexandria:with-gensyms (upfrom1 below1)
    `(let* ((redirect (signum ,by))
	    (,upfrom1 (if (>= ,upfrom 0) ,upfrom (+ (length ,list) ,upfrom)))
	    (,below1  (when ,below (if (>= ,below 0) ,below (+ (length ,list) ,below))))
	    (out
	      (loop for i upfrom ,upfrom1 below (or ,below1 (length ,list)) by (abs ,by) collect (nth i ,list))))
       (if (= 1 redirect)
	   out
	   (reverse out)))))

(defun !as-strided (x shape stride &aux (x (!contiguous x)))
  (declare (type Tensor x) (type list shape) (type list stride))
  (assert (= (length shape) (length stride)))
  (flet ((ids (lst) (map 'list #'tensor->id lst)))
    (apply-tir (x shape stride)
        (out)
        (out (%view (tensor-node x) (ids shape)
                    (loop repeat (length shape) collect (%iconst 0 :dtype *default-indexing-dtype*))
                    (loop repeat (length shape) collect (%iconst 1 :dtype *default-indexing-dtype*))
                    (ids stride))))))

(defun compute-unfold-shape (shape window-shape ceiling strides dilation)
  (append
   (map 'list #'->size (slice shape 0 (- (length window-shape))))
   (loop for s in (slice shape (- (length window-shape)))
         for w in window-shape
         for d in dilation
         for stride in strides
         collect (->size (compute-filter-size s d w stride :ceiling ceiling)))
   (map 'list #'->size window-shape))))

(defun compute-unfold-stride (stride strides dilation n)
  (append
   (map 'list #'->size (slice stride 0 (- n)))
   (loop for s1 in (slice stride (- n)) for s2 in strides
         collect (!mul (->size s1) (->size s2)))
   (loop for s1 in (slice stride (- n)) for s2 in dilation
         collect (!mul (->size s1) (->size s2)))))

(defun !unfold (x kernel-size &key (dilation 1) (stride 1) (ceiling #'ceiling))
  "
Extracts sliding local blocks from a batched input tensor.

```
(!unfold x kernel-size &key (dilation 1) (stride 1) (ceiling #'ceiling))
```
"
  (declare (type Tensor x) (type list kernel-size))
  (let ((dilation (maybe-list dilation kernel-size)) (stride (maybe-list stride kernel-size)))
    (assert (every #'numberp (slice (tensor-shape x) (- (length kernel-size)))) () "!unfold: the shape intersecting with kernel_size must be static, getting ~a with kernel-size=~a" (tensor-shape x) kernel-size)
    (let ((shape (compute-unfold-shape (tensor-shape x) kernel-size ceiling stride dilation))
          (stride (compute-unfold-stride (tensor-stride x) stride dilation (length kernel-size))))
      (!as-strided x shape stride))))

(defun !convnd (x weight &key (bias nil) (groups 1) (stride 1) (dilation 1) (padding 0))
  (let ((out-channels (car (tensor-shape weight)))
        (hw (subseq (tensor-shape weight) 2)))
    (multiple-value-bind (bs cin_ cout cin)
	(apply #'values `(,@(subseq (tensor-shape x) 0 2) ,@(subseq (tensor-shape weight) 0 2)))
      ;; assert groups*cin == cin_ and len(self.shape) == len(weight.shape)
      (when (and (numberp groups) (numberp cin) (numberp cin_)))
      (assert (= cin_ (* groups cin))
	      ()
	      "Input Tensor shape ~a do not match the shape of the weights ~a. ~a vs ~a (= cin_ (* groups cin))"
	      (tensor-shape x) (tensor-shape weight) cin (* groups cin_))
      (assert (= (tensor-nrank weight) (tensor-nrank x)) () "Input Tensor Shape ~a do not match the shape of the weights ~a" (tensor-shape x) (tensor-shape weight))
      ;; x = [bs, groups*cin, oy, ox, H, W]
      ;; [TODO] Padding
      (let* (;;(x (!unfold (!padding2d x (padding2d-shape padding (length hw))) hw :dilation dilation :stride stride))
             (x (!unfold x hw :dilation dilation :stride stride))
             (rcout (floor (/ cout groups)))
	     (oyx   (slice (tensor-shape x) 2 (- (length hw)))))
	;; TODO: use winograd when fails to satisfy (or (not (some #'(lambda (x) (= x 3)) hw)) (not (eql stride 1)) (not (eql dilation 1)))
	;; x = x.reshape(bs, groups, cin, 1, *oyx, *HW).expand(bs, groups, cin, rcout, *oyx, *HW)
	;; x = x.permute(0,1,3,*[4+i for i in range(len(oyx))],2,*[4+len(oyx)+i for i in range(len(HW))])
	(let* ((x (!reshape x (alexandria:flatten (list bs groups cin 1 oyx hw))))
               (x (!expand x (alexandria:flatten (list bs groups cin cout oyx hw))))
	       (x (!permute x (append (list 0 1 3) (map 'list #'(lambda (x) (+ 4 x)) (range 0 (length oyx))) (list 2) (map 'list #'(lambda (x) (+ 4 (length oyx) x)) (range 0 (length hw))))))
	       ;; x = (x * weight.reshape(1, groups, rcout, *[1] * len(oyx), cin, *HW))
	       (x (!mul (!reshape weight (append (list 1 groups rcout) (loop repeat (length oyx) collect 1) (list cin) hw)) x))
	       ;; x = x.sum([-1-i for i in range(1+len(oyx))], keepdim=True, acc_dtype=acc_dtype)
	       (x (!sum x :axis (loop for i in (range 0 (+ 1 (length oyx))) collect (+ -1 (- i))) :keepdims t))
	       ;; x = x.reshape(bs, cout, *oyx)
	       (x (!reshape x (append (list bs cout) oyx))))
	  (if bias
              (!add x (!reshape bias (append (list 1) (list out-channels) (loop repeat (length hw) collect 1))))
              x))))))
