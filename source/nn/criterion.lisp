(in-package :caten/nn)

(deftype reduce-specifier-t ()  `(member :mean :sum))

(defmodel (L1NormLoss (&key (reduction :mean)) :documentation "
```
(L1NormLoss &key (reduction :mean))
```

Implements L1Norm Loss:

```math
l(x, y) = L = {l_1, ..., l_n}^\\intercal, l_n = abs(x_n - y_n)
```

reduction is one of :mean or :sum.
")
    ((reduction reduction :type reduce-specifier-t)))

(defcall (loss L1NormLoss) (A[~] B[~])
  (let ((out (!sub a b)))
    (ecase (slot-value loss 'reduction)
      (:mean (!mean (!abs out)))
      (:sum (!sum (!abs out))))))

(defun !l1norm (a b &key (reduction :mean))
  "
```
(!l1norm a b &key (reduction :mean))
```

Computes L1Norm
"
  (forward (L1NormLoss :reduction reduction) a b))

(defmodel (MSELoss (&key (reduction :mean)) :documentation "
```
(MSELoss &key (reduction :mean))
```

Implements MSE Loss:

```math
l(x, y) = L = {l_1, ..., l_n}^\\intercal, l_n = (x_n - y_n)^2
```

reduction is one of :mean or :sum.
")
    ((reduction reduction :type reduce-specifier-t)))

(defcall (loss MSELoss) (A[~] B[~])
  (let ((out (!square (!sub a b))))
    (ecase (slot-value loss 'reduction)
      (:mean (!mean out))
      (:sum  (!sum out)))))

(defun !mse (a b &key (reduction :mean))
  "
```
(!mse a b &key (reduction :mean))
```

Computes MSE Loss between a and b.
"
  (forward (MSELoss :reduction reduction) a b))

(defmodel (CrossEntropyLoss (&key (reduction :mean) (delta 1e-5)) :documentation "
```
(CrossEntropyLoss &key (reduction :mean) (delta 1e-5))
```

Returns a tensor that measures the Cross-Entropy-Error between each element in the x and labels. labels are one-hot encoded.

```math
L_i = -p_ilog(x_i + delta)
```

```math
\\begin{equation}
  out_i=
  \\begin{cases}
    sum(L)  & \\text{reduction = sum} \\\\
    mean(L) & \\text{reduction = mean} \\\\
    L       & \\text{otherwise}
  \\end{cases}
\\end{equation}
```")
    ((reduction reduction :type reduce-specifier-t)
     (delta delta :type number)))

(defmethod call ((op CrossEntropyLoss) &rest inputs)
  (multiple-value-bind (x labels) (apply #'values inputs)
    (let ((z (!mul (!const x -1) (!mul labels (!log (!add x (!const x (slot-value op 'delta))))))))
      (ecase (slot-value op 'reduction)
        (:mean (!mean z))
        (:sum  (!sum z))))))

(defun !cross-entropy (x labels &key (reduction :mean) (delta 1e-5))
  "
```
(!cross-entropy x labels &key (reduction :mean) (delta 1e-5))
```
Computes Cross Entropy Loss between x and labels. labels are one-hot encoded.
"
  (forward (CrossEntropyLoss :reduction reduction :delta delta) x labels))
