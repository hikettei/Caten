(defpackage :caten/api
  (:use :cl :trivia :cl-ppcre :caten/ir :caten/graph)
  ;; From tensor.lisp
  (:export
   #:Tensor
   #:tensor-graph
   #:tensor-id
   #:tensor-buffer

   #:%%make-tensor
   #:tensor-simplify
   #:tensor-verify
   #:tensor-node
   #:tensor-is-symbolic-p
   #:tensor->id
   #:tensor-type
   #:node->tensor
   #:ensure-node-is-tensor
   #:tensor-shape
   #:tensor-nrank
   #:tensor-stride
   #:tensor-dtype
   #:tensor-views

   #:tensor-from-graph
   #:apply-tensor-graph
   #:with-inlined-tir
   #:apply-tir

   #:make-tensor
   #:make-scalar

   #:!+ #:!- #:!* #:!/ #:!add #:!sub #:!mul #:!div
   #:!idiv #:!move #:!maximum #:!minimum

   #:!sin

   ;; Movements
   #:!contiguous
   #:!reshape
   #:!permute
   #:!t
   #:!transpose
   #:!uprank
   #:!flatten
   #:!repeat
   #:!expand
   #:!squeeze
   #:!unsqueeze
   #:!view

   ;; floating utils
   #:inf
   #:-inf
   #:nan
   #:float-infinity-p
   #:float-nan-p
   #:float-type-of
   )
  ;; from facets.lisp
  (:export
   #:get-global-runtime
   #:change-facet
   #:with-facet
   #:with-facets)
  ;; from hlops.lisp
  (:export
   #:!sum
   #:!matmul)
  ;; from shape.lisp
  (:export
   #:*restart-point*
   #:Restart-Point)
  )
