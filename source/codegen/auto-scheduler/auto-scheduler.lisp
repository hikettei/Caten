(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference)
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
(defun has-data-reuse-p (node)
  "Returns a list of gids that have data reuse"
  (and
   (some #'node-writes-broadcasted-p (getattr node :items))))

(defun is-elementwise-p (node)
  "Returns T if the node is element-wise operation."
  (and
   (null (has-data-reuse-p node))
   (= 1 (count :FOR (getattr node :blueprint) :key #'node-type))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun optimize-elementwise-kernel (auto-scheduler node)
  "For elementwise operation, we apply the optimization based on the template, because there is nothing to search for."
  ;; [TODO]
  ;; - [ ] Vectorize -> Parallelize -> Unroll is the all thing we can do for elementwise operation
  (caten/codegen/unroll:apply-unroll node 4))

(defun optimize-reduction-kernel (auto-scheduler node)
  ;; [TODO]
  (warn "WIP")
  )

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (cond
    ((is-elementwise-p node)
     (optimize-elementwise-kernel auto-scheduler node))
    ((has-data-reuse-p node)
     (optimize-reduction-kernel auto-scheduler node))
    (T
     (warn "Skipped the auto-scheduler for ~a, we have to add more sketch templates" node)))
  ;; Load blueprint from optimized polyhedral IR
  (setf (getattr node :blueprint)
        (caten/codegen/ast-parser:lower-into-bp-from-polyhedral
         (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank))
         node)))
