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
")
  (:use :cl)
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
