(in-package :caten/apis)

(docs:define-page ("caten/apis" "packages/caten.apis.md")
  (docs:title "Overview")
  (docs:body "Welcome to Caten/APIs")
  ;; TODO: quick tutorial
  ;; ctx:with-contextvar
  ;; proceed
  ;; make tensor
  ;; How to debug?
  ;; JIT
  ;; JIT_DEBUG
  ;; DOT
  )

(docs:define-page ("Tensor" "packages/caten.apis.tensor.md")
  (docs:title "Tensor")
  (docs:doc/struct "Tensor" 'Tensor)
  (docs:doc/function "make-tensor" #'make-tensor)
  (docs:doc/function "make-scalar" #'make-scalar)
  (docs:doc/function "grad" #'grad)
  (docs:doc/function "shape" #'shape)
  (docs:doc/function "ndim" #'ndim)
  (docs:doc/function "dtype-of" #'dtype-of)
  (docs:doc/function "order" #'order)

  (docs:doc/function "caten" #'caten)
  (docs:body "
### [method] forward

```
(forward AVM &rest params)
```

Compute the forward pass of the compiled computational graph (AVM). The params are additional parameters that are used to initialize the AVM variable table, passed in the following format:


- `(symbol . number)` Loaded as a `*default-int*` scalar tensor, used to determine the shape of dynamic shaped tensors.
- `(symbol . buffer)` or `(symbol . tensor)` Used to assign the initial elements of the tensor specified by the name in `form` in `(make-tensor ... :from x)`

Here's an example.
")
  (docs:subsection "Examples")
  (docs:example-code "
(let ((model (caten (!randn `(a b)))))
  (print (forward model `(a . 10) `(b . 10))))"
                     :title "Example of forward with dynamic shaped")
  
  (docs:body "
### [method] backward
```
(forward AVM &optional prev-dout)
```

Compute the backward pass of the compiled computational graph (AVM). Note that the `prev-dout` is ignored. Forward pass must be computed first. Gradients are automatically reset to zero before the forward pass.
")
  (docs:doc/function "proceed" #'proceed)
  (docs:doc/macro "with-no-grad" 'with-no-grad)
  
  (docs:section "Examples")
  (docs:subsection "Tensor Creation")
  (docs:example-repl "(make-tensor `(30 30))")
  (docs:subsection "Realize")
  (docs:example-repl "(proceed (make-tensor `(30 30)))")
  (docs:subsection "Creating a computational graph (Lazy)")
  (docs:example-repl "(!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1))")
  (docs:subsection "Evaluating a computational graph.")
  (docs:example-repl "(proceed (!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1)))"))

(docs:define-page ("Differentiable Ops" "packages/caten.apis.differentiable_ops.md")
  (docs:title "Differentiable Ops")
  )

(docs:define-page ("Module" "packages/caten.apis.module.md")
  (docs:title "Module")

  )

(docs:define-page ("Models" "packages/caten.apis.models.md")
  (docs:title "Models")
  )

(docs:define-page ("AOT" "packages/caten.apis.aot.md")
  (docs:title "AOT")
  )

(docs:define-page ("Initializers" "packages/caten.apis.initializers.md")
  (docs:title "Initializers")
  )

(docs:define-page ("ShapeTracker" "packages/caten.apis.shapetracker.md")
  (docs:title "ShapeTracker")
  )
