(defpackage :caten/codegen/search/autotune
  (:use :cl :caten/air :caten/codegen/search/polyhedral :caten/codegen/byoc :caten/codegen/search/evaluator
        :caten/codegen/search/optimization-rule)
  (:export
   #:online-autotune-kernel
   #:ILP/SolveProximity
   ))

(in-package :caten/codegen/search/autotune)
;; ~~ ScheduleGenerationTree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Schedule-Generation-Tree ()
  ((depth :initform 0 :accessor sgt-depth :initarg :depth)
   (items :initform nil :accessor sgt-items :initarg :items)
   (best-item :initform nil :accessor sgt-best-item)
   (best-score :initform nil :accessor sgt-best-score))
  (:documentation
"Schedule-Generation-Tree collects Polyhedral IR candidates that belong to the
same search generation (beam frontier). Conceptually, for a parent schedule θ₀,
its children {θ₁,θ₂,…} formed by one-step transformations constitute one level
(generation) of this tree. The class stores the current frontier to be scored,
pruned (Top-k), and expanded to produce the next generation."))

(defun sgt-add-evaluations (sgt evaluator blueprint)
  "Attach evaluation scores to the current generation."
  (declare (type Schedule-Generation-Tree sgt))
  (mapc
   #'(lambda (x) (setf (psi-evaluation x) (evaluate-polyhedral x evaluator blueprint)))
   (sgt-items sgt))
  (let ((items (sort (sgt-items sgt) #'< :key #'psi-evaluation)))
    (setf (sgt-best-item sgt) (car items)
          (sgt-best-score sgt) (psi-evaluation (car items)))))

(defun sgt-prune-topk (sgt topk)
  "Prune the current frontier by keeping the Top-k candidates under the active"
  (declare (type Schedule-Generation-Tree sgt) (type fixnum topk))
  (let ((best-items (sort (sgt-items sgt) #'< :key #'psi-evaluation)))
    (setf (sgt-items sgt) (subseq best-items 0 (min topk (length best-items))))))

(defun sgt-make-nextgen (sgt)
  "Expand the pruned frontier to form the next generation."
  (declare (type Schedule-Generation-Tree sgt))
  (make-instance 'Schedule-Generation-Tree :items (copy-list (sgt-items sgt)) :depth (1+ (sgt-depth sgt))))

(defun psi-apply-optimization-space (item optrule-id)
  (let ((optrule-id (etypecase optrule-id (keyword (list optrule-id)) (list optrule-id)))
        (items (list item)))
    (dolist (id optrule-id)
      (setf items
            (loop for item in items
                  append
                  (loop for space in (optrule-generate-search-space item id)
                        for transformed = (apply-optimization item space)
                        if (psi-verify-legality transformed)
                          collect transformed))))
    items))

(defun sgt-apply-transformations (sgt &rest optrule-ids)
  (declare (type Schedule-Generation-Tree sgt))
  (setf (sgt-items sgt)
        (loop for optrule-id in optrule-ids
              append
              (loop for item in (sgt-items sgt)
                    append
                    (psi-apply-optimization-space item optrule-id)))))
;; is it used?
(defun sgt-find-legal-transformation (sgt &rest optrule-ids)
  (declare (type Schedule-Generation-Tree sgt))
  (loop for optrule-id in optrule-ids
        append
        (loop for item in (sgt-items sgt)
              append
              (loop for space in (optrule-generate-search-space item optrule-id)
                    for transformed = (apply-optimization item space)
                    if (psi-verify-legality transformed)
                      do (setf (sgt-items sgt) (list transformed))
                         (return-from sgt-find-legal-transformation (list transformed)))))
  (setf (sgt-items sgt) nil))
;; is it used?
(defun sgt-apply-until-saturated (sgt &rest optrule-ids)
  (labels ((n (sgt) (apply #'sgt-find-legal-transformation sgt optrule-ids) sgt))
    (let ((curr-items (sgt-items sgt))
          (next-gen (n sgt)))
      (if (sgt-items next-gen)
          (apply #'sgt-apply-until-saturated next-gen optrule-ids)
          (progn
            (setf (sgt-items sgt) curr-items)
            sgt)))))

(defun sgt-improvements (old-sgt new-sgt)
  (assert (and (sgt-best-score old-sgt) (sgt-best-score new-sgt)))
  (/ (sgt-best-score new-sgt) (sgt-best-score old-sgt)))
;; ~~ Exploration Stages/Spaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun sgt-prepare-for-sketch-generation (sgt)
  (sgt-apply-transformations sgt :Serialize)
  (sgt-apply-transformations sgt :Maximize-Filter-Candidates))

(defun sgt-finalize-sketch (sgt)
  (sgt-apply-transformations sgt :Coincidence)
  (sgt-apply-transformations sgt :Maximize-Band-Depth))

(defun sgt-prepare-for-device-optimization (sgt)
  (sgt-apply-transformations sgt :Interchange :Tile :Vectorize :SplitReduce))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - TILE Parameter Space?
(defun setup-autotune (runtime blueprint)
  (values
   (ctx:getenv :BEAM)
   (+ (ctx:getenv :BEAM_THRESHOLD) 100.0)
   (make-instance 'DeviceMeasurer :runtime runtime :blueprint blueprint :version (ctx:getenv :BACKEND))
   ;; evaluator2
   ))

(defun online-autotune-kernel (runtime node)
  "
BEAM Search Workflow:
                [Input Blueprint]
                        |
    [Template Generation with Early Pruning] Mapping Reschedule, Interchange, TileGPU/PARALLEL at this stage
                        |
                   [BEAM Search] Optimizing TILE/VECTORIZE/SPLITREDUCE
"
  (when (getattr node :optimized-p) (return-from online-autotune-kernel node))
  (let ((blueprint (kernel-blueprint (getattr node :kernel-info))))
    (multiple-value-bind (beam-width threshold cost1 cost2) (setup-autotune runtime blueprint)
      (caten/isl::with-isl-context
        ;; - BEAM Search With Early Pruning
        ;; - 最初にInterchange, Parallel, Rescheduleから50個くらいの空間を生成
        ;; - 古典的なPolyhedral Compilerとしてできないか，top@5ができればいい
        ;; [TODO] No Ondevice Profiling Mode
        (let* ((root (make-polyhedral-schedule-item blueprint))
               (gen0 (make-instance 'Schedule-Generation-Tree :items (list root))))
          ;; [Template Construction] (Which is the best?)
          ;; - Option1: BEAM Search + LightWeight Cost Function
          ;; - Option2: ISL Reschedule
          ;; - Option3: No Template Search
;;          (print (car (sgt-items gen0)))
;;          (sgt-apply-transformations gen0 :Tile :Interchange :Vectorize :SplitReduce)
;;
;;          (sgt-apply-transformations gen0 :Interchange)
;;          (sgt-apply-transformations gen0 :Parallel :TileGPU)
;;          (sgt-add-evaluations gen0 cost1 blueprint)
;;          (sgt-prune-topk gen0 3)
;;          (print (sgt-make-nextgen gen0))
          ;; 次やること(ちょっとむずい)
          ;; - online-autotune-kernel終了時点で，正しい計算結果をReturnする(Bring Back Replayer)
          ;;  - うまくArrayをCopy
          ;; - First Kernel Generation
          ;; - Vectorize/Tile etc generation and finish implementing beam search
          (error "STOP")
          t
          )))))
;; [TODO] Prevent Non-beneficial fusion (e.g.: Matmul+Matmul)
;; ==> CostFunction Design
;; ==> CmdHistoryから求める？(Less Transpose/Reshape The Better)
;; [TODO] Faster Exploration Time (Call ISL APIs Directly?)
;; [TODO] ↓をSCCsのみで実行するようにして，End2EndでILP Based Polyhedral Compiler
;; [TODO]
;; - Fast ILP Solver (Build Conv+ReLU+Pool < 1e-2)
;;   - Restrict the exploration space
;;   - Optimize ISL ops
;;   - Transpose ==> How to pickup just "relavant" dim?
;; - FlashAttention => Avoid FullFuse
;; - Restrict The Exploration Space for Transpose
;; - Finish Reorder? Should we search it?
;; - [TODO] Finalize Fusion
;;   - [ ] Reorder
;;   - [ ] Flash
;;   - [ ] Optimize ISL
;;   - [ ] CostModel
;;   - [ ] If it works well ==> apply this function algo end2end
(defun ILP/SolveProximity (polyhedral &key (cost-model))
  "Generates a maximum fused graph"
  (declare (type Polyhedral-Schedule-Item polyhedral))
  (let ((gen0 (make-instance 'Schedule-Generation-Tree :items (list polyhedral))))
    (labels ((generate (&aux (prev-items (sgt-items gen0)))
               ;; Search Valid Permutation, Reshape, and Fusion
               (sgt-apply-transformations
                gen0
                '(:Transpose :Reshape :Fuse))
               ;(assert (<= (length (sgt-items gen0)) 1))
               ;; [TODO] Sort TopK
               (when (null (sgt-items gen0))
                 (let ((last-item (car prev-items)))
                   (return-from
                    ILP/SolveProximity
                     (if (psi-get-first-unoptimized-sequence last-item) ;; is everything fused?
                         nil
                         (progn
                           (setf (psi-theta last-item) (caten/codegen/search/schedule:schedule-remove-all-marks (psi-theta last-item)))
                           last-item)))))))
      (loop while t do (generate)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] BlockLevel Fusion (e.g.: Group multiple sequence of EXPR into a single group)
;; [Note]
;; - Assume the compiler gives a multiple section of tensors enclosured by two VIEWS
;; - G1: [VIEW] -> Add -> Sub -> [VIEW]
;; - G2: [VIEW] -> Mul -> Exp -> [VIEW]
;; The function (will be responsible for) fusion G1 and G2 correctly
;; - [ ] Move byoc.lisp ==> runtime or byoc
;; - [ ] Move renderer.lisp ==> byoc or runtime
;; - [ ] Move codegen
;; - [ ] Remove realize
;; - [x] Create ScheduleGraph (each node is Polyhedral w/ Lexiographical Order)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
