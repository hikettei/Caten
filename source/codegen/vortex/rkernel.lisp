(defpackage :caten/codegen/rkernel
  (:documentation "Low Level Kernel Abstraction Layer as proposed in vortex")
  (:use :cl :alexandria :caten/codegen/blueprint))

(in-package :caten/codegen/rkernel)
;; TODO
;; - [ ] remove caten/isl
;; - [ ] remove libyaml dependencies
  
;; Workload
;; :FORを三つに分ける:
;; - [ ] Parallel Loop Set: Batch Axis
;; - [ ] Temporal Spatial Loop Set: LOAD=0.0するとこ
;; - [ ] Temporal Reduction Loop Set: reduce=T
;; - [ ] 各 rKernelは，N-1のrKERNEL, LOAD, STOREを保有する
;; - [ ] 
;; - Goal is: Map into low level intrinsics
(deftype loop-type-t () `(member :pl :tsl :trl))

(defstruct rKernel
  "An implementation of rKernel proposed in [vortex](https://arxiv.org/pdf/2409.01075), Figure 10."
  (layer-depth 0 :type integer)
  ;; A hash table: axis -> loop-type-t
  (loop-type (make-hash-table) :type hash-table)
  (analyze-type (required-argument :analyze-type)
   :type
   (member :empirical :analytical))
  (load-func (required-argument :load-func)
   :type function)
  (store-func (required-argument :store-func)
   :type function)
  (compute-func (required-argument :compute-func)
   :type function))

(defun rKernel (L PL TSL TRL LF SF)
  "Implements Algorithm1( Unified Recursive Abstraction) proposed in the [vortex](https://arxiv.org/pdf/2409.01075) paper."
  (declare (type fixnum L) (type list PL TSL TRL))
  (dolist (p (nth L PL))
    (dolist (ts (nth L TSL))
      (dolist (tr (nth L TRL))
        ;; LOAD_FUNC
        ;; rKERNEL
        (rKernel (1- L) PL TSL TRL LF SF))
      ;; STORE_FUNC
      )))




