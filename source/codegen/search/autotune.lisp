(defpackage :caten/codegen/search/autotune
  (:use :cl :caten/air :caten/codegen/search/polyhedral :caten/codegen/byoc :caten/codegen/search/evaluator
        :caten/codegen/search/optimization-rule :caten/codegen/search/schedule)
  (:export
   #:ILP/Search
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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - TILE Parameter Space?
(defun setup-autotune (cost-model)
  (values
   (ctx:getenv :BEAM)
   (+ (ctx:getenv :BEAM_THRESHOLD) 100.0)
   cost-model
;;   (make-instance 'DeviceMeasurer :runtime runtime :blueprint blueprint :version (ctx:getenv :BACKEND))
   ;; evaluator2
   ))

(defun ILP/Search (affine &key (cost-model nil))
  "
BEAM Search Workflow:
                [Input Blueprint]
                        |
    [Template Generation with Early Pruning] Mapping Reschedule, Interchange, TileGPU/PARALLEL at this stage
                        |
                   [BEAM Search] Optimizing TILE/VECTORIZE/SPLITREDUCE
"
  (declare (type node affine))
  (assert (eql (node-type affine) :Affine))
  (let ((polyhedral (getattr affine :polyhedron)))
    (multiple-value-bind (beam-width threshold cost1 cost2) (setup-autotune cost-model)
      ;; [TODO]
      ;; - [ ] exp2 recomputation
      ;; - [ ] opt-history ==> Extend
      ;; - [ ] smoll exploration space
      (let ((gen0 (make-instance 'Schedule-Generation-Tree :items (list polyhedral)))
            (minimized nil))
        ;; Stage1: ScheduleTree Preprocessing (MaximizeBandDepth, ComputeParallel)
        ;; - [ ] todo: compute permutable
        (sgt-apply-transformations gen0 :Maximize-Band-Depth)
        (print (car (sgt-items gen0)))
        (error "STOP (BEAM Search)")
        ;; Step1. Mapping then w/ Parallel
        (sgt-apply-transformations gen0 :Interchange)
        (sgt-apply-transformations gen0 :Parallel :TileGPU)
        ;; CPUだと無条件でParallel
        ;; itemsが1の時はevalしない
        ;; TILEGPU ==> GLOBAL/LOCALで分ける
        ;; LOCALはGLOBALとMarkされたLoopをParallelizeできる。
        ;; (select_best)
        ;; Step2. Profile based tuning
        ;; - [ ] microkernel: create 256x256x256 tile (tileall+sink)
        ;; - [ ] (!matmul (make-tensor `(n 512 512 512)) (make-tensor `(n 512 512 512)))
        ;;       ^ 2回目以降BEAMする意味ある？
        (labels ((search1 ()
                   ;; [TODO] ここで全てのRecompute可能なbufferだけデータをProfileする
                   (sgt-apply-transformations gen0 :Recompute))
                 (next (&aux (prev-items (sgt-items gen0)))
                   (sgt-apply-transformations
                    gen0
                    :MicroKernel ;; これはどうやって4dim から 3dimをselectするかが難しい
                    :Tile :Vectorize
                    :Local :Interchange :SplitReduce)
                   ;; (select_best_topk) (sgt-prune-topk gen0 3)
                   (when (null (sgt-items gen0))
                     (setf minimized t))))
          ;; (loop while (null minimized) do (next))
          )
        t))))

;; MCFusion w/ DB Like Approach
;; Reduction ==> MemoryIntensive

;; [TODO] Make it general beam search function
;; S1(i, j)    => S(j, i)
;; S2(i, j, k) => S(j, i, k)
;; Restrict Search Space by doing:
;; Many vs One Fusion only
;; Ref: https://arxiv.org/pdf/2505.07829
;;(let ((tg (tensor-lowered-graph (!matmul (make-tensor `(256 512)) (!matmul (make-tensor `(512 1024)) (make-tensor `(1024 2048)))))))
;;              (time (caten/codegen/lowerer::codegen tg)))

(defun ILP/Preprocess (parent child &key (fuse-into :parent))
  (declare (type Polyhedral-Schedule-Item parent child))
  (let ((root (psi. parent child)))
    ;; Pre-transformations 1: Detect Coalesce/Create tile to maximize fusion chance
    (multiple-value-bind (new-sched new-child-rmap new-child-wmap) (schedule-detect-coalesce (psi-theta root) (psi-read-union-map child) (psi-write-union-map parent) (psi-write-union-map child))
      ;; Pre-transformations 2: Compute valid permutations in advance.
      (progn
        ;; [TODO] Union of domains?
        (setf (psi-theta root) new-sched)
              ;(psi-read-union-map root) new-read
              ;(psi-write-union-map root) new-write
              ;(psi-dependency-graph root) new-deps)
        root))))

(defun ILP/SolveProximity (parent child &key (fuse-into :parent))
  (print "Searching ...")
  (print parent)
  (print child)
  (let ((gen0 (make-instance 'Schedule-Generation-Tree :items (list (ILP/Preprocess parent child)))))
    (print "Fusion ...")
    (print (sgt-items gen0))
    (labels ((beam (&aux (prev-items (sgt-items gen0)))
               (sgt-apply-transformations
                gen0
                :Fuse)
               (when (null (sgt-items gen0))
                 (let ((seq (psi-get-first-unoptimized-sequence (car prev-items))))
                   (if seq
                       (return-from ILP/SolveProximity nil)
                       (progn
                         (print "FusionCompleted")
                         (setf (psi-theta (car prev-items)) (schedule-remove-all-marks (psi-theta (car prev-items))))
                         (return-from ILP/SolveProximity (car prev-items))))))))
      (loop while t do (beam)))))
