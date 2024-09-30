(in-package :caten/apis)

(docs:define-page ("caten/apis" "packages/caten.apis.md")
  (docs:title "caten/apis")
  (docs:subtitle "Tensor")
  ;; Examples
  (docs:doc/struct "Tensor" 'Tensor)
  (docs:doc/function "grad" #'grad)
  (docs:doc/function "shape" #'shape)
  (docs:doc/function "ndim" #'ndim)
  (docs:doc/function "dtype-of" #'dtype-of)
  (docs:doc/function "order" #'order)
  
  (docs:example-repl "(make-tensor `(30 30))")
  (docs:example-repl "(proceed (make-tensor `(30 30)))")
  (docs:example-repl "(!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1))")
  (docs:example-repl "(proceed (!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1)))")
  (docs:subtitle "Differentiable Ops")
  (docs:subtitle "Modules")
  (docs:subtitle "Models")
  (docs:subtitle "Initializers")
  (docs:subtitle "ShapeTracker")
  
  )
