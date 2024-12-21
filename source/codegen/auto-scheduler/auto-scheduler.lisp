(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr)
  (:export #:auto-schedule))

(in-package :caten/codegen/auto-scheduler)
;; Scheduling commands
;; - [x] apply-tile
;; - [x] apply-parallel
;; - [ ] apply-collapse
;; - [ ] apply-fuse
;; - [ ] apply-interchange
;; - [x] apply-vectorize
;; - [ ] apply-unroll
;; Purpose: get a list of optimal scheduling commands
;; Note: 積極的にISL ASTとBlueprintを変換しながら変形を施していく
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (todo: search tiramisu)
;; BEAM Search

;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun has-reduction-p (node)
  "Returns a list of gids that have data reuse"
  (some #'node-writes-broadcasted-p (getattr node :items)))

(defun has-data-reuse-p (node)
  (flet ((node-has-data-reuse-p (item)
           (let ((broadcasts
                   (loop for r in (relay-read-iters (read-type-relay item))
                         for map = (and r (map 'list #'(lambda (x) (expr-equal-to x 0)) (iteration-space-strides r)))
                         when (and map (some #'identity map)) collect map)))
             ;; Reduce more than two ways
             (> (length (remove-duplicates broadcasts :test #'equal)) 1))))
    (and
     (has-reduction-p node)
     ;; Data Reuse = Broadcasted at multiple dimension
     (some #'node-has-data-reuse-p (getattr node :items)))))

(defun is-elementwise-p (node)
  "Returns T if the node is element-wise operation."
  (and
   (null (has-reduction-p node))
   (= 1 (count :FOR (getattr node :blueprint) :key #'node-type))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun optimize-elementwise-kernel (auto-scheduler node)
  "For elementwise operation, we apply the optimization based on the template, because there is nothing to search for."
  ;; [TODO]
  ;; - [ ] Vectorize -> Parallelize -> Unroll is the all thing we can do for elementwise operation
  ;; Unroll the loop in order to find the vectorize
  ;; [TODO] Optimize the unroll-by is automatically adjusted based on the simd register size (if vectorize is allowed or possible)
  (caten/codegen/unroll:apply-unroll node 4)
  ;; [TODO] Parallelize the outermost loop
  ;; [TODO] Unroll the loop again, if possible, remove the small loop
  (print "ELEMWISE")
  )

(defun optimize-reduction-kernel (auto-scheduler node)
  "Optimization for LayerNorm/Softmax/Argmax/Sum is here"
  (print "SOFTMAX")
  )

(defun optimize-data-reuse-kernel (auto-scheduler node)
  "Optimization for Conv/Gemm is here"
  (print "REUSE")
  )

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (cond
    ((is-elementwise-p node)
     (optimize-elementwise-kernel auto-scheduler node))
    ((has-data-reuse-p node)
     (optimize-data-reuse-kernel auto-scheduler node))
    ((has-reduction-p node)
     (optimize-reduction-kernel auto-scheduler node))
    (T
     (warn "Skipped the auto-scheduler for ~a, we have to add more sketch templates" node)))
  ;; Load blueprint from optimized polyhedral IR
  (setf (getattr node :blueprint)
        (caten/codegen/ast-parser:lower-into-bp-from-polyhedral
         (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank))
         node)))
