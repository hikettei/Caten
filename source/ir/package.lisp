(defpackage :caten/ir
  (:use :cl :trivia :caten/graph :alexandria :caten/utilities/documentation)
  (:import-from
   :caten/utilities/dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from attrs.lisp
  (:export
   #:JITAble)
  ;; from tensor-ir.lisp
  (:export
   #:*default-order*
   #:*default-float*
   #:*default-uint*
   #:*default-int*

   #:%uconst
   #:%iconst
   #:%fconst
   
   #:%alloc
   #:%salloc
   #:%load
   #:%stride
   #:%shape
   #:%make-tensor
   #:%index-components
   )
  ;; from ctx.lisp
  (:export
   #:with-context
   #:with-context-nodes
   #:with-context-from-parents
   #:with-asm
   #:*ctx*
   #:emit)
  ;; from ops.lisp
  (:export
   ;; Binary
   #:TensorGraph
   #:*wrap-around-mode*
   #:%add
   #:%sub
   #:%mul
   #:%div
   #:%idiv
   #:%mod
   #:%move
   #:%and
   #:%or
   #:%xor
   #:%max #:%min

   ;; Unary
   #:%sin
   #:%log2
   #:%exp2
   #:%neg
   #:%recip
   #:%sqrt
   #:%not
   #:%cast

   ;; CMP
   #:%!= #:%=
   #:%< #:%> #:%<= #:%>=
   )
  ;; from constant-folding.lisp
  (:export
   #:fold-constant)
  ;; from view.lisp
  (:export
   #:infer-tensor-info
   #:%view)
  ;; from logical.lisp
  (:export
   #:%where)
  ;; from optimizers.lisp
  (:export #:optimize-aasm #:minimize-duplicated-symbolic-path)
  ;; TensorRelay
  (:export
   #:TensorRelay #:tensor-relay-shape #:tensor-relay-stride #:tensor-relay-dtype #:tensor-relay-views #:tensor-relay-nrank
   #:tensor-relay-value #:tensor-relay-inferred-permute #:tensor-relay-orig-buffer-shape #:tensor-relay-depend-idx-list #:tensor-relay-iterspace #:tensor-relay-vectorize
   #:copy-tensor-relay #:make-tensor-relay)
  ;; AST
  (:export #:ASTGraph #:with-blueprint #:simplify-ast)
  ;; ASTOps
  (:export #:%range #:%dotimes #:%if #:%when #:%progn #:%global #:%barrier #:%bind #:%aref #:%polyaref #:%function #:%expr #:%setf #:%defsmem #:%function #:%lid)
  ;; RuntimeOps
  (:export #:RuntimeGraph #:$sink #:$kernel #:$sync)
  ;; ScheduleOps
  (:export #:%ast-band-tile #:ast-band-tile-gpu #:ast-apply-cse)
  (:Export #:ast-remove-extra-memloads #:ast-concrete-sequence #:ast-merge-expr-from-aref-subgraph)
  ;; schedule-ops
  (:export #:ScheduleGraph #:->schedule-graph #:$affine #:$nonaffine))
