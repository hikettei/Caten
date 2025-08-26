(defpackage :caten/codegen/search/optimization-rule
  (:documentation "
Default Search Space (Hackable by users for different BYOC)
- [x] Reschedule  Solves ILP and then generate multiple candidates for an entry point
- [x] Interchange Change the partial schedule in the same band
- [x] Tile        TileBands
- [x] TileGPU     Coalesce+Tile+Mapping w/ block/threadIdx
- [x] Parallel    Coalesce+Tile+Parallel
- [x] Vectorize   Tile+Sink, later mapped w/ TensorCore
- [x] SplitReduce Tile+Sink, this is the optimization for reduction and it has two mode: :warp and :block
TODO:
- [ ] FUSE
")
  (:use :cl :caten/codegen/search/polyhedral :caten/codegen/search/schedule)
  (:export
   #:OptimizationRule
   #:optrule-generate-search-space
   #:optrule-apply-transform-on-polyhedral
   #:optrule-apply-transform-on-blueprint
   #:apply-optimization
   #:psi-get-first-unoptimized-sequence
   
   #:NoOpt
   ;; Fusuion/Parallelism
   #:RewriteTree
   #:Fuse
   ;; Optimization
   #:Reschedule
   #:Interchange
   ))

(in-package :caten/codegen/search/optimization-rule)
;; ~~ Definitions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptimizationRule ()
  ((band :initarg :band :accessor optrule-band :initform nil)))

(defmethod print-object ((obj OptimizationRule) stream)
  (print-unreadable-object (obj stream :type t)
    (dolist (slot-def (closer-mop:class-slots (class-of obj)))
      (let ((name  (closer-mop:slot-definition-name slot-def))
            (value (slot-value obj (closer-mop:slot-definition-name slot-def))))
        (when (null (find name `(band nth-kernel)))
          (format stream " :~a ~S" name value))))))

(defgeneric optrule-generate-search-space (polyhedral optrule-id)
  (:documentation "A callback method for generating next-generation optimization space"))
(defgeneric optrule-apply-transform-on-polyhedral (polyhedral optrule)
  (:documentation "A callback method for doing `θ_n+1 = apply_optimization(θ_n, optrule)`"))
(defgeneric optrule-apply-transform-on-blueprint (directive-id bands blueprint)
  (:documentation "A callback for making a transformation to blueprint named as directive-id"))

(defun apply-optimization (polyhedral optrule)
  (declare (type Polyhedral-Schedule-Item polyhedral) (type OptimizationRule optrule))
  (let ((polyhedral (psi-clone-for-next-generation polyhedral)))
    (push optrule (psi-opt-history polyhedral))
    (optrule-apply-transform-on-polyhedral polyhedral optrule)
    polyhedral))
;; ~~ NoOpt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass NoOpt (OptimizationRule) nil)
(defmethod optrule-generate-search-space (poly (id (eql :NoOpt))) (list (make-instance 'NoOpt)))
(defmethod optrule-apply-transform-on-polyhedral (poly (optrule NoOpt)) poly)
;; ~~ Simplifiers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass RewriteTree (OptimizationRule) ((rule :initarg :rule :reader rt-rule)))
(macrolet ((add-rule (name)
             `(defmethod optrule-generate-search-space (poly (id (eql ,name)))
                (list (make-instance 'RewriteTree :rule id)))))
  (add-rule :Maximize-Filter-Candidates)
  (add-rule :Maximize-Band-Depth))
(defmethod optrule-apply-transform-on-polyhedral (poly (optrule RewriteTree))
  (assert (rt-rule optrule))
  (setf (psi-theta poly)
        (ecase (rt-rule optrule)
          (:Maximize-Filter-Candidates (schedule-split-all-band (psi-theta poly)))
          (:Maximize-Band-Depth (schedule-fuse-all-band (schedule-compute-parallel (psi-theta poly) (psi-dependency-graph poly)))))))
;; ~~ Optimizations on schedule-node-sequence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun psi-get-first-unoptimized-sequence (poly) (car (schedule-get-non-marked-sequence/set (psi-theta poly))))

(defclass Fuse (OptimizationRule) ((at :initarg :at))
  (:documentation "Fuse all filters in the same sequence/set node whose child is band."))
(defclass Reorder (OptimizationRule) ((at :initarg :at) (order :initarg :order))
  (:documentation "Reorder all filters in the same sequence/set node whose child is band or else to create a new fusible sequence."))

(defmethod optrule-generate-search-space (poly (id (eql :Fuse)))
  (let ((seq (psi-get-first-unoptimized-sequence poly)))
    ;; [TODO] Return 'Saturated if seq is null (and succeed)
    (when seq
      (let* ((seqnode (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) seq))
             (status (schedule-node-sequence-check-fusible seqnode)))
        (case status
          (:valid (list (make-instance 'Fuse :at seq)))
          (:need-reorder
           (let ((order
                   (schedule-node-sequence-tpsort (schedule-node-sequence-splice-children seqnode))))
             (list (make-instance 'Reorder :at seq :order order))))
          (otherwise
           ;; TODO: (list (make-instance 'Fail))
           ))))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Fuse))
  (with-slots ((at at)) optrule
    (let ((theta-fused
            (schedule-node-sequence-full-fuse
             (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) at))))
      (setf (psi-theta poly) theta-fused))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Reorder))
  (with-slots ((at at) (order order)) optrule
    (let* ((seq (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) at))
           (seq (schedule-node-sequence-splice-children seq)))
      (setf
       (psi-theta poly)
       (isl:schedule-node-get-schedule
        (schedule-node-sequence-group-sequence
         (schedule-node-sequence-reorder seq order)))))))

(defclass Fission (OptimizationRule)
  ((at :initarg :at) (sizes :initarg :sizes))
  (:documentation "Fission <=> Coalesce"))

(defclass Flash (OptimizationRule)
  ((at :initarg :at) (size :initarg :size))
  (:documentation "Tile(Size)+Sink+FUSE"))

(defmethod optrule-generate-search-space (poly (id (eql :Reshape)))
  (let* ((pos (psi-get-first-unoptimized-sequence poly))
         (status
           (when pos
             (schedule-node-sequence-check-fusible
              (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) pos)))))
    (when (and pos)
      (when (not (eql status :valid))
        (return-from optrule-generate-search-space (list (make-instance 'NoOpt))))
      (let ((seq (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) pos)))
        (multiple-value-bind (sizes min max min-equals-to-max-p fuse-legal-p)
            (schedule-node-sequence-get-band-sizes (psi-domain poly) seq)
          (declare (ignore min max))
          (if fuse-legal-p ;; [todo] should try both of fission and flash
              (if min-equals-to-max-p
                  (list (make-instance 'NoOpt))
                  (list
                   (make-instance 'Fission :at pos :sizes sizes)))
              ;; (list (make-instance 'Flash :at pos :size (reduce #'isl:value-min sizes)))
              ))))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Fission))
  (with-slots ((at at) (sizes sizes)) optrule
    (setf (psi-theta poly)
          (isl:schedule-node-get-schedule
           (schedule-node-sequence-align-band-size
            (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) at)
            sizes)))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Flash))
  (with-slots ((at at) (size size)) optrule
    (setf (psi-theta poly)
          (isl:schedule-node-get-schedule
           (schedule-node-sequence-apply-flash
            (psi-domain poly)
            (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) at)
            size)))))

(defclass Transpose (OptimizationRule)
  ((at :initarg :at) (depth :initarg :depth))
  (:documentation "
Swaps top-level band and N-th band from a n-chain of schedule_node_band. That is, transpose(0, depth).
```
schedule: ... <-------|
  child:              |
    schedule: ... <---|
      child: ...
        xN
```
"))

(defmethod optrule-generate-search-space (poly (id (eql :Transpose)))
  (let* ((pos (psi-get-first-unoptimized-sequence poly))
         (status
           (when pos
             (schedule-node-sequence-check-fusible
              (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) pos)))))
    (when pos
      (let ((seq (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) pos)))
        (when (or
               (not (eql status :valid))
               (not (= 2 (isl::%isl-schedule-node-n-children (isl::schedule-node-handle seq)))))
          (return-from optrule-generate-search-space (list (make-instance 'NoOpt))))
        (let ((depth (schedule-node-band-get-n-chain (isl:schedule-node-first-child (isl:schedule-node-get-child seq 1)))))
          (append
           (list (make-instance 'NoOpt))
           (loop for i upfrom 1 to depth
                 collect (make-instance 'Transpose :at pos :depth i))))))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Transpose))
  (with-slots ((at at) (depth depth)) optrule
    (let* ((seq (schedule-node-at-path (isl:schedule-get-root (psi-theta poly)) at))
           (band (isl:schedule-node-first-child (isl:schedule-node-get-child seq 1))))
      (setf (psi-theta poly) (isl:schedule-node-get-schedule (schedule-node-band-scoop-up band depth))))))

(defclass Shift (OptimizationRule) nil) ;; Skewing
;; ~~ Reschedule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Reschedule (OptimizationRule)
  ((outer-coincidence :initarg :outer-coincidence :initform 0)
   (maximize-coincidence :initarg :maximize-coincidence :initform 0)
   (treat-coalescing :initarg :treat-coalescing :initform 0)
   (maximize-band-depth :initarg :maximize-band-depth :initform 0)
   (schedule-whole-component :initarg :schedule-whole-component :initform 0)
   (serialize-sccs :initarg :serialize-sccs :initform 0)
   (max-coefficient :initarg :max-coefficient :initform 1) ;; always set to 1 to keep simplicy!
   (max-constant-term :initarg :max-constant-term :initform 0)) ;; always set to 0 to keep simplicity!
  (:documentation "Polyhedral rescheduling rule. Sets ISL scheduling options and solves the
ILP-based scheduler over constraints C(D,Δ) to obtain a new schedule θ.
Parameters (0/1 unless stated):
  outer-coincidence, maximize-coincidence   — fusion/parallelism bias;
  treat-coalescing                          — favor stride/coalesced access;
  maximize-band-depth                       — prefer deeper bands;
  schedule-whole-component                  — schedule SCCs jointly;
  serialize-sccs                            — serialize SCCs (enables fission);
  max-coefficient, max-constant-term ∈ ℤ≥0  — bounds on affine coefficients.
Effect: computes θ := schedule-constraints-compute-schedule(C) under these
options; typically used to seed candidate schedules at the start of search."))
(defmethod optrule-generate-search-space (poly (id (eql :Serialize)))
  (list (make-instance 'Reschedule :serialize-sccs 1)))
;; [TODO]Remove
(defmethod optrule-generate-search-space (poly (id (eql :Reschedule)))
  ;; Reschedule can be placed on the top of scheduling commands.
  (list
   ;(make-instance 'Reschedule) ;; Keep Loop Fusion (Softmax, FlashAttention)
   (make-instance 'Reschedule :serialize-sccs 1) ;; Maximize Fusion Chance!
   ;; Locality Strategy
   (make-instance 'Reschedule :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 1 :treat-coalescing 1) ;; Full Fusion
   (make-instance 'Reschedule :outer-coincidence 1 :schedule-whole-component 1) ;; Keep Loop Fusion (Softmax, FlashAttention)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 0 :maximize-band-depth 1 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 1 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)
   ))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Reschedule))
  (macrolet ((set-option (name slot)
	       `(cffi:foreign-funcall
                 ,(format nil "isl_options_set_~(~a~)" name)
                 :pointer (isl::context-handle isl::*context*)
                 :int (slot-value optrule ',slot)
		 :void)))
    (set-option "schedule_serialize_sccs" serialize-sccs)
    (set-option "schedule_max_constant_term" max-constant-term)
    (set-option "schedule_max_coefficient" max-coefficient)
    (set-option "schedule_outer_coincidence" outer-coincidence)
    (set-option "schedule_maximize_coincidence" maximize-coincidence)
    (set-option "schedule_treat_coalescing" treat-coalescing)
    (set-option "schedule_maximize_band_depth" maximize-band-depth)
    (set-option "schedule_whole_component" schedule-whole-component))
  (let ((new-schedule
          (isl:schedule-constraints-compute-schedule
           (compute-schedule-constraints
            (psi-domain poly)
            (compute-dependence-relation
             (psi-read-union-map poly)
             (psi-write-union-map poly)
             (psi-theta poly))))))
    (setf (psi-theta poly) new-schedule)))
;; ~~ Exploration Space ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~ Interchange ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Interchange (OptimizationRule) ((order :initarg :order :accessor interchange-order :type list)))

(defmethod optrule-generate-search-space (poly (id (eql :Interchange)))
  ;; where each permute has band_depth length list
;  LoopInterchangeについて整理すると:
;- HasDataReuse=0 Bands | ==> SIMDのために，Stride=1となるアクセスを最も下にする
;- HasDataReuse=1  Bands | ==>どのInterchangeが最適かわからない (Device Specific)
  (let ((top (isl:schedule-node-get-child (isl:schedule-get-root (psi-theta poly)) 0)))
    ;; If body is a sequence => split to multiple kernel
    (print top)
    (print poly)
    ;; Parallelで256x256x256Tile作る->InnerTileをInterchange
    ;; i.e.: InterchangeはBEAMでずっとやるのが吉？
    nil
    ))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Interchange))
  (setf (psi-theta poly) (isl:schedule-node-get-schedule (schedule-node-band-permute (optrule-band opt) (interchange-order opt)))))
;; ~~ Tile ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Delete?
;; [TODO] Make size @parametric?
(defclass Tile (OptimizationRule)
  ((size :initarg :size :accessor tile-size)))

(defmethod optrule-generate-search-space (poly (id (eql :Tile)))
  ;; TODO
  )

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Tile))
  (setf
   (psi-theta poly)
   (isl:schedule-node-get-schedule
    (schedule-node-band-tile*
     (optrule-band opt) (tiling-size (optrule-band opt) (tile-size opt))))))
;; ~~ TileGPU ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass TileGPU (OptimizationRule)
  ((local-size :initarg :local-size :accessor tile-gpu-local-size)))

(defmethod optrule-generate-search-space (poly (id (eql :TileGPU)))
  ;; [TODO]
  ;; - MultiKernelの扱い
  ;; - TileGPUがTreeの上の方に存在しないか。
  ;; - 全てのKernelに対して存在するべき
  )

(defmethod optrule-apply-transform-on-polyhedral (poly (opt TileGPU))
  (when (= 1 (slot-value (psi-strategy poly) 'caten/codegen/byoc::ptile-max-rank) 1)
    (loop for root in (schedule-get-roots (psi-theta poly))
          do (print root))
    nil))

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :TileGPU)) bands blueprint)
  
  )
;; ~~ Parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Parallel (OptimizationRule) nil)

(defmethod optrule-generate-search-space (poly (id (eql :Parallel)))
  (when (> (slot-value (psi-strategy poly) 'caten/codegen/byoc::ptile-max-rank) 1)
    (loop for root in (schedule-get-roots (psi-theta poly)) do
      (print root)
          )
    nil))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Parallel))

  )

(defmethod optrule-apply-transform-on-blueprint ((id (eql :PARALLEL)) bands blueprint)

  )
;; ~~ Vectorize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Vectorize (OptimizationRule) ((width :initarg :width :accessor vectorize-width))
  (:documentation "Vectorize = Tile+Sink"))

(defmethod optrule-generate-search-space (poly (id (eql :Vectorize)))

  )

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Vectorize))

  )

(defmethod optrule-apply-transform-on-blueprint ((id (eql :Vectorize)) bands blueprint)

  )
;; ~~ SplitReduce ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass SplitReduce (OptimizationRule)
  ((size :initarg :size :accessor splitreduce-size)
   (mode :initarg :mode :type (member :warp :block) :initform :warp :accessor splitreduce-mode)))

(defmethod optrule-generate-search-space (poly (id (eql :SplitReduce)))

  )

(defmethod optrule-apply-transform-on-polyhedral (poly (opt SplitReduce))

  )

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :WarpReduce)) bands blueprint)
  blueprint)

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :BlockReduce)) bands blueprint)
  blueprint)

;; [TODO]
;; - [ ] Coalesce on ScheduleTree Level
;; - [ ] Fuse/Shift/Stencil(Skewing)
;; - [ ] Transpose
;;   - [ ] stride=1であることのCostFunction
;; - [ ] FuseWithParent
;; - [ ] Beautiful MultiKernel Separation, Directive Exploration,
;; - [ ] NoOptなBand --> check-legality-parallelで自動でcoincident付与する？
