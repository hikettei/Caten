(in-package :caten/ir)

(define-page ("caten/ir" "packages/caten.ir.md")
  (title "caten/ir")
  (body (node-build-documentation-by-class "UnaryOps" :UnaryOps))
  (body (node-build-documentation-by-class "BinaryOps" :BinaryOps))
  (body (node-build-documentation-by-class "TernaryOps" :TernaryOps))
  (body (node-build-documentation-by-class "Buffer" :Buffer))
  (body (node-build-documentation-by-class "Indexing" :INDEXING))
  (body (node-build-documentation-by-class "JIT Specific Ops" :JIT)))
