(in-package :caten/apis)

(docs:define-page ("caten/apis" "packages/caten.apis.md")
  (docs:title "caten/apis")
  (docs:body "welcome to apis section")
  ;; TODO: quick tutorial
  ;; Brief
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
  
  (docs:section "Examples")
  (docs:subsection "Tensor Creation")
  (docs:example-repl "(make-tensor `(30 30))")
  (docs:subsection "Realize")
  (docs:example-repl "(proceed (make-tensor `(30 30)))")
  (docs:subsection "Creating a computational graph (Lazy)")
  (docs:example-repl "(!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1))")
  (docs:subsection "Evaluating a computational graph.")
  (docs:example-repl "(proceed (!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1)))")

  ;; ADD:
  ;; Caten
  ;; Forward
  ;; Backward
  ;; Proceed
  )

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
