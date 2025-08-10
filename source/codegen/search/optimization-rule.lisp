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
  (:use :cl :caten/codegen/search/polyhedral)
  (:export

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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Reschedule (OptimizationRule)
  ((outer-coincidence :initarg :outer-coincidence :initform 0)
   (maximize-coincidence :initarg :maximize-coincidence :initform 0)
   (treat-coalescing :initarg :treat-coalescing :initform 0)
   (maximize-band-depth :initarg :maximize-band-depth :initform 0)
   (schedule-whole-component :initarg :schedule-whole-component :initform 0)
   (serialize-sccs :initarg :serialize-sccs :initform 0)
   (max-coefficient :initarg :max-coefficient :initform 1) ;; always set to 1 to keep simplicy!
   (max-constant-term :initarg :max-constant-term :initform 0)) ;; always set to 0 to keep simplicity!
  (:documentation "Reschedule: Solves ILP Problem with various cost functions to generate multiple schedule candidates to start with."))

(defmethod optrule-generate-search-space (poly (id (eql :Reschedule)))
  ;; Reschedule can be placed on the top of scheduling commands.
  (list
   (make-instance 'Reschedule :serialize-sccs 1) ;; Loop Fission (GEMM)
   (make-instance 'Reschedule :outer-coincidence 1) ;; Keep Loop Fusion (Softmax, FlashAttention)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 0 :maximize-band-depth 1 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)
   (make-instance 'Reschedule :outer-coincidence 1 :maximize-coincidence 1 :maximize-band-depth 0 :schedule-whole-component 0)))
