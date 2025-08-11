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
;; ~~ Reschedule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(defmethod optrule-generate-search-space (poly (id (eql :Reschedule)))
  ;; Reschedule can be placed on the top of scheduling commands.
  (list
   (make-instance 'Reschedule :serialize-sccs 1) ;; Loop Fission (GEMM)
   (make-instance 'Reschedule :outer-coincidence 1) ;; Keep Loop Fusion (Softmax, FlashAttention)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 0 :maximize-band-depth 1 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 1 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)))

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
            (psi-dependency-graph poly)))))
    (setf (psi-theta poly) new-schedule)))
;; ~~ Interchange ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Interchange (OptimizationRule)
  ((order :initarg :order :accessor interchange-order :type list)))

(defmethod optrule-generate-search-space (poly (id (eql :Interchange)))
  ;; where each permute has band_depth length list
  (let ((top (isl:schedule-node-get-child (isl:schedule-get-root (psi-theta poly)) 0)))
    ;; If body is a sequence => split to multiple kernel

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
  )

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :TileGPU)) bands blueprint)
  )
;; ~~ Parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Parallel (OptimizationRule)
  ((depth :initarg :depth :accessor parallel-depth)
   (tile-size :initarg :tile-size :accessor parallel-tile-size :initform 1)
   (nth-kernel :initarg :nth-kernel :accessor parallel-nth-kernel)))

(defmethod optrule-generate-search-space (poly (id (eql :Parallel)))

  )

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
