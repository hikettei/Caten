(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config)
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
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (TODO: Searching method like tiramisu)
;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun has-reduction-p (node)
  "Returns a list of gids that have data reuse"
  (some #'node-writes-broadcasted-p (getattr node :items)))

(defun has-data-reuse-p (node)
  ;; [TODO] There should be much better way to determine this
  ;; LayerNorm/Embedding has no data reuse
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
  (caten/codegen/coincidence:apply-parallel
   (getattr node :polyhedral)
   (auto-scheduler-n-global-loops auto-scheduler))
  ;; [TODO] Unroll the loop again, if possible, remove away for the small loop
  )

(defun optimize-reduction-kernel (auto-scheduler node)
  "Optimization for LayerNorm/Softmax/Argmax/Sum is here."
  (caten/codegen/unroll:apply-unroll node 2)
  (caten/codegen/coincidence:apply-parallel
   (getattr node :polyhedral)
   (auto-scheduler-n-global-loops auto-scheduler)))

(defun optimize-data-reuse-kernel (auto-scheduler node)
  "Optimization for Conv/Gemm is here. We assume kernels labelled here has a large impact on the end-to-end performance.
So we are going to apply an optiimzation method which takes a long time to search for the optimal schedule."
  ;; Data Reuse kernel has a chance to apply the tiling
  ;; And Tuning the tiling size tested by the measurer
  ;(caten/codegen/tiling::apply-tile (getattr node :polyhedral) 16) ;; Tile_Size = Optimal_Tile_Size * N_UNROLL
  (caten/codegen/unroll:apply-unroll node 4)
  ;; (caten/codegen/unroll::apply-unroll node 4)
  ;; [TODO] After Tiling, Unroll the outermost tile band.
  ;; [TODO] If the outermost loop is a tile -> #pragma omp parallel for collapse(2)
  (caten/codegen/coincidence:apply-parallel
   (getattr node :polyhedral)
   (auto-scheduler-n-global-loops auto-scheduler)))

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (>= OPTIMIZE 1)

      )
    (when (>= OPTIMIZE 2)

      )
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
