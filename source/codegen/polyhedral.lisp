(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl :caten/codegen/byoc)
  (:import-from :caten/codegen/renderer #:render-expr #:Default-Renderer)
  (:export
   #:realize-node-with-autotuning
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)
;; ~~ Polyhedral ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule :initarg :schedule)
   (domain   :accessor poly-domain :initarg :domain)
   (dependencies :accessor poly-dependencies :initarg :dependencies)
   (cmd-history :accessor poly-cmd-history :initform nil :initarg :history)
   (blueprint :accessor poly-blueprint :initarg :blueprint)))

(defun make-polyhedral-ir (blueprint domain read write schedule)
  (let ((pg (make-instance 'Polyhedral-IR)))
    (setf (poly-schedule pg) schedule (poly-domain pg) domain (poly-blueprint pg) blueprint)
    (let* ((access (union-access-info-from-sink read))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-schedule access schedule))
           (flow (union-access-info-compute-flow access))
           (RaW (union-flow-get-must-dependence flow))
           (access (union-access-info-from-sink write))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-may-source access read))
           (access (union-access-info-set-schedule access schedule))
           (flow   (union-access-info-compute-flow access))
           (WaW    (union-flow-get-must-dependence flow))
           (WaR    (union-flow-get-may-dependence flow))
           (dependencies (union-map-union (union-map-union WaR RaW) WaW)))
      (setf (poly-dependencies pg) dependencies)
      pg)))

(defmethod poly-clone-for-next-generation ((pg Polyhedral-IR))
  (make-instance 'Polyhedral-IR :schedule (copy (poly-schedule pg)) :history (copy-list (poly-cmd-history pg)) :dependencies (poly-dependencies pg) :domain (poly-domain pg) :blueprint (poly-blueprint pg)))

(defmethod poly-make-schedule-constraints ((pg Polyhedral-IR))
  (let* ((sc (schedule-constraints-on-domain (poly-domain pg)))
         (sc (schedule-constraints-set-coincidence sc (poly-dependencies pg)))
         (sc (schedule-constraints-set-validity sc (poly-dependencies pg)))
         (sc (schedule-constraints-set-proximity sc (poly-dependencies pg))))
    sc))

(defun gid (n) (intern (format nil "_gid~a" n)))
(defun ->ast (schedule rank)
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_atomic_upper_bound" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:copy schedule))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))

(defmethod pg-dump-into-str ((pg Polyhedral-IR))
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (->ast (poly-schedule pg) 0))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))

(defmethod print-object ((pg Polyhedral-IR) stream)
  (print-unreadable-object (pg stream :type t :identity t)
    (format stream "~%~a~%  :history ~a" (pg-dump-into-str pg) (poly-cmd-history pg))))
;; ~~ SCoP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "Context for tracking loop structure during traversal"
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list)
  (exprs nil :type list))

(defun make-scop-ctx-from-blueprint (graph)
  "Traverse the blueprint graph to extract loop structure"
  (let ((ctx (make-ctx)) (visited (make-hash-table)))
    (labels ((traverse (node)
               (when (or (null node) (gethash (node-id node) visited)) (return-from traverse))
               (setf (gethash (node-id node) visited) t)
               (ecase (node-type node)
                 (:FOR
                  ;; Extract loop info from the FOR node FOR(RANGE(upfrom, below), BODY)
                  (let* ((range-id (car (node-reads node)))
                         (range-node (id->value graph range-id))
                         (idx (when range-node (getattr range-node :idx)))
                         (size (when range-node (car (node-reads range-node))))
                         (step (when range-node (cadr (node-reads range-node))))
                         (mark (getattr node :mark :allow-undefined t)))
                    (when idx
                      (let ((loop-info (list :idx idx :size size :step step :mark (or mark :noopt) :for-node node :range-node range-node)))
                        (push loop-info (ctx-all-loops ctx))
                        (push loop-info (ctx-stack ctx))))
                    ;; Traverse body
                    (traverse (id->value graph (second (node-reads node))))
                    ;; Pop loop from stack after processing body
                    (when idx (pop (ctx-stack ctx)))))
                 (:PROGN (dolist (child-id (node-reads node)) (traverse (id->value graph child-id))))
                 (:IF (when (>= (length (node-reads node)) 2) (traverse (id->value graph (second (node-reads node))))))
                 (:EXPR (push node (ctx-exprs ctx)) (setf (gethash (node-id node) (ctx-node-to-loops ctx)) (copy-list (ctx-stack ctx)))))))
      ;; Start traversal from output nodes or all nodes
      (assert (= 1 (length (graph-outputs graph))))
      (traverse (id->value graph (car (graph-outputs graph))))
      ctx)))

(defun render-expr-for-isl (id graph &aux (node (id->value graph id)))
  "Render an expression in ISL-compatible format"
  (cond
    ((numberp id) (format nil "~a" id))
    (node
     (let ((id (if (eql (node-type node) :EXPR) (car (node-reads node)) id)))
       (render-node (make-instance 'Default-Renderer :graph graph) id)))
    (t (error "The variable ~a is not defined. ~a" id node))))

(defun render-domain-for-node (blueprint node loop-info)
  "Render ISL domain string for a single node"
  (declare (type Graph blueprint) (type Node node) (type hash-table loop-info))
  (flet ((r (id) (render-expr-for-isl id blueprint)))
    (let ((loops (gethash (node-id node) loop-info)))
      (let ((constraints
              (loop for l in (reverse loops)
                    for step = (getf l :step)
                    if (= step 1)
                      collect (format nil "0 <= ~(~a~) < ~a" (getf l :idx) (r (getf l :size)))
                    else ;; [NOTE] Not Tested!!
                      collect (format nil "exists e : ~(~a~) = ~a*e and 0 <= ~(~a~) < ~a" (getf l :idx) (r step) (getf l :idx) (r (getf l :size))))))
        (format nil "~a[~{~a~^, ~}] ~a ~{~a~^ and ~}" (node-id node) (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)) (if constraints ":" "") constraints)))))

(defun render-domains (ctx blueprint)
  "Create ISL domain representation from blueprint"
  (format nil "{ ~{~a~^; ~} }" (reverse (map 'list #'(lambda (x) (render-domain-for-node blueprint x (ctx-node-to-loops ctx))) (ctx-exprs ctx)))))

(defun extract-buffer-access-info (id blueprint &aux (visited (make-hash-table)) (found))
  (labels ((explore (id &aux (node (id->value blueprint id)))
             (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
             (when (eql (node-type node) :EXPR) (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (when (eql (node-type node) :AREF)
               (let* ((p (id->value blueprint (car (node-reads node))))
                      (p (if (and p (eql (node-type p) :BIND)) (getattr p :value) (car (node-reads node)))))
                 (push (cons p (second (node-reads node))) found)))
             (mapc #'explore (node-reads node))))
    (explore id)
    found))

(defun render-access-for-node (node loops buffer index blueprint)
  "Render access relation for a single node"
  (format nil "~a[~{~a~^, ~}] -> ~a[~a]"
          (node-id node)
          (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops))
          buffer
          (if index (render-expr-for-isl index blueprint) "0")))

(defun extract-accesses (ctx blueprint &aux (reads) (writes))
  "Extract read and write access relations from blueprint"
  (with-slots ((node-to-loops node-to-loops) (exprs exprs)) ctx
    (loop for expr in exprs
          for expr-domain = (gethash (node-id expr) node-to-loops)
          for expr-entry-point = (id->value blueprint (car (node-reads expr))) do
            (assert expr-entry-point)
            (case (node-type expr-entry-point)
              (:SETF
               ;; SETF(AREF, EXPR)
               ;;       ^W    ^R
               (let ((write-region (extract-buffer-access-info (car (node-reads expr-entry-point)) blueprint))
                     (read-region  (extract-buffer-access-info (second (node-reads expr-entry-point)) blueprint)))
                 (dolist (w write-region)
                   (push (render-access-for-node expr expr-domain (car w) (cdr w) blueprint) writes))
                 (dolist (r read-region)
                   (push (render-access-for-node expr expr-domain (car r) (cdr r) blueprint) reads))))
               (otherwise
                (let ((read-region (extract-buffer-access-info (car (node-reads expr)) blueprint)))
                  (push (render-access-for-node expr expr-domain (car (node-writes expr)) nil blueprint) writes)
                  (dolist (r read-region)
                    (push (render-access-for-node expr expr-domain (car r) (cdr r) blueprint) reads))))))
    (cons
     (format nil "{ ~{~a~^; ~} }" (reverse reads))
     (format nil "{ ~{~a~^; ~} }" (reverse writes)))))

(defun render-band-node-in-domain (range-node related-nodes loop-info &aux (idx (getattr range-node :idx)))
  (declare (type node range-node) (type list related-nodes) (type hash-table loop-info))
  (with-output-to-string (out)
    (format out "[{")
    (loop for filter in related-nodes for nth upfrom 0
          for idxs = (map 'list #'(lambda (x) (format nil "~(~a~)" (getf x :idx))) (reverse (or (gethash (node-id filter) loop-info) (error ""))))
          do (assert (eql (node-type filter) :EXPR))
          if (not (= nth 0)) do (format out "; ")
            do (format out "~a[~{~a~^, ~}] -> [~(~a~)]" (node-id filter) idxs idx))
    (format out "}]")))

(defun rewrite-blueprint-tree->schedule-tree (ctx blueprint &aux (visited (make-hash-table)))
  "Build ISL Schedule Tree directly from blueprint structure following analyze-scop pattern"
  (declare (type Graph blueprint))
  ;; ISL Schedule starts w/ domain
  (with-slots ((loops loops) (node-to-loops node-to-loops)) ctx
    (labels ((rewrite-node (id &key (region nil) &aux (node (id->value blueprint id)))
               (declare (type symbol id))
               (when (or (null node) (gethash (node-id node) visited)) (error "Rendering for multiple times, should we allow it?"))
               (setf (gethash (node-id node) visited) t)
               (values
                (case (node-type node)
                  (:FOR
                   ;; FOR(RANGE(UPFROM, BELOW), BODY)
                   (multiple-value-bind (body-sched exprs-in-body) (rewrite-node (second (node-reads node)) :region region)
                     (let* ((range (id->value blueprint (car (node-reads node))))
                            (band (render-band-node-in-domain range exprs-in-body node-to-loops)))
                       (setf region (append region exprs-in-body))
                       (if (string= band "[{}]")
                           body-sched
                           (schedule-insert-partial-schedule body-sched (multi-union-pw-aff-from-str band))))))
                  (:IF
                   ;; [Not] How to express :IF?
                   (error "not ready"))
                  ;; EXPR ==> Rewrite as a filter, and is a leaf of graph.
                  (:EXPR
                   (setf region (append region (list node)))
                   (schedule-from-domain (union-set-from-str (format nil "{ ~a }" (render-domain-for-node blueprint node node-to-loops)))))
                  (:PROGN
                    ;; [todo] you can use reduce
                    (let ((tmp-schedule :nothing))
                      (loop for item in (node-reads node) do
                        (multiple-value-bind (sched reg) (rewrite-node item)
                          (setf region (append region reg))
                          (if (eql tmp-schedule :nothing)
                              (setf tmp-schedule sched)
                              (setf tmp-schedule (schedule-sequence tmp-schedule sched)))))
                      (assert (not (eql tmp-schedule :nothing)))
                      tmp-schedule))
                  (otherwise (error "No handling case for ~a" (node-type node))))
                region)))
      (assert (= 1 (length (graph-outputs blueprint))))
      (rewrite-node (car (graph-outputs blueprint))))))

(defun make-polyhedral-from-blueprint (blueprint)
  "Constructs Polyhedral IR from blueprint which is a static graph.
   
   The blueprint should be a FastGraph containing nodes with the following types:
   - :RANGE - defines loop bounds
   - :FOR - marks loop entry with :mark attribute (:coincident, :reduction, :noopt)
   - :AREF - memory load operations
   - :SETF - memory store operations
   - :PROGN - sequence of operations
   
   Returns a Polyhedral-IR object."
  (declare (type Graph blueprint))
  ;; Extract domain, reads, writes
  (let* ((ctx (make-scop-ctx-from-blueprint blueprint))
         (domain (union-set-from-str (render-domains ctx blueprint)))
         (schedule (rewrite-blueprint-tree->schedule-tree ctx blueprint))
         (reads/writes (extract-accesses ctx blueprint)))
    (print reads/writes)
    (make-polyhedral-ir blueprint domain (union-map-from-str (car reads/writes)) (union-map-from-str (cdr reads/writes)) schedule)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun get-blueprint-from-polyhedral (polyhedral)
  "Convert ISL polyhedral representation back to blueprint graph"
  ;; Entry point for:
  ;; - @directive parsing, getting blueprint from Polyhedral.
  ;; - 
  ;;
  ;; This would require parsing the ISL AST and reconstructing the graph
  ;; For now, this is a placeholder that returns the input for compatibility
  (declare (ignore polyhedral))
  (warn "get-blueprint-from-polyhedral: Not fully implemented yet")
  nil);; [TODO] ↓のAccess Relations, ScalarはFissionできるように記述したい
;; ~~ OptimizeRule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptimizationRule () nil)

(defgeneric optrule-generate-search-space (polyhedral optrule-trigger))
(defgeneric optrule-apply-transform-on-polyhedral (polyhedral optrule)) ;; Insert Directive
(defgeneric optrule-apply-transform-on-blueprint (polyhedral optrule))  ;; Directive Parse

(defun apply-optimization (polyhedral optrule)
  (declare (type Polyhedral-IR polyhedral) (type OptimizationRule optrule))
  (let ((polyhedral (poly-clone-for-next-generation polyhedral)))
    (push optrule (poly-cmd-history polyhedral))
    (optrule-apply-transform-on-polyhedral polyhedral optrule)
    polyhedral))
;; ~~ Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Note: This is hackable by users (as intended)
(defclass NoOpt (OptimizationRule) nil)
(defmethod optrule-generate-search-space (poly (id (eql :NoOpt))) (list (make-instance 'NoOpt)))
(defmethod optrule-apply-transform-on-polyhedral (poly (optrule NoOpt)) poly)
(defmethod optrule-apply-transform-on-blueprint (poly (optrule NoOpt)) nil)

(defclass Reschedule (OptimizationRule)
  ((outer-coincidence :initarg :outer-coincidence :initform 0)
   (maximize-coincidence :initarg :maximize-coincidence :initform 0)
   (treat-coalescing :initarg :treat-coalescing :initform 0)
   (maximize-band-depth :initarg :maximize-band-depth :initform 0)
   (schedule-whole-component :initarg :schedule-whole-component :initform 0)))

(defmethod optrule-generate-search-space (poly (id (eql :Reschedule)))
  ;; Reschedule can be placed on the top of commands.
  (when (null (some #'(lambda (x) (typep x 'Reschedule)) (poly-cmd-history poly)))
    (list
     ;; [TODO] Isn't there more to search configurations?
     ;; [TODO] proximity/validity/coincidence, what is constraints?
     ;; [TODO] More Patterns!
     (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 1 :treat-coalescing 0 :maximize-band-depth 0 :schedule-whole-component 0)
     (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 0 :treat-coalescing 0 :maximize-band-depth 1 :schedule-whole-component 0)
     (make-instance 'Reschedule :outer-coincidence 1 :maximize-coincidence 1 :treat-coalescing 1 :maximize-band-depth 0 :schedule-whole-component 0))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Reschedule))
  (macrolet ((set-option (name slot)
	       `(cffi:foreign-funcall
                 ,(format nil "isl_options_set_~(~a~)" name)
                 :pointer (isl::context-handle isl::*context*)
                 :int (slot-value optrule ',slot)
		 :void)))
    (set-option "schedule_outer_coincidence" outer-coincidence)
    (set-option "schedule_maximize_coincidence" maximize-coincidence)
    (set-option "schedule_treat_coalescing" treat-coalescing)
    (set-option "schedule_maximize_band_depth" maximize-band-depth)
    (set-option "schedule_whole_component" schedule-whole-component))
  (setf (poly-schedule poly) (schedule-constraints-compute-schedule (poly-make-schedule-constraints poly))))

(defmethod optrule-apply-transform-on-blueprint (poly (optrule Reschedule))
  nil)

(defclass FuseWithParent (OptimizationRule)
  nil)

(defclass TensorCore (OptimizationRule)
  nil)

(defclass Reorder (OptimizationRule)
  nil)

(defclass Tile (OptimizationRule)
  nil)

(defclass ParallelTile (OptimizationRule) ;; CPU will use this!
  nil)

(defclass SplitReduce (OptimizationRule)
  ;; TODO: Mode = :warp :block
  nil)


;; RootがReschedule->Reorderなら...的な話かも
;; うまく言語化できないけど，最初にReorder -> Tileとかで，求めるOptimalに到達する可能性があるから，やっぱり木構造で順番に
;; Apply Optsしていく探索空間をイメージするのでうまくいくんじゃないかな
;; PPRINTを充実させるか，とっととParser作ってもろて
;; - poly-ir-schedule-node: これを追加するべきか？
;; - [TODO] Reductionのval_2 = ...のScalar, Write, これをMatrixにする
;; ~~ AutoScheduler Implementation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *search-space*
  '((0 . (:NoOpt :Reschedule)) ;; (n-generation . Candidates)
    (1 . (:NoOpt :Reorder))
    (t . nil)))

(defmethod get-next-optimization-rules ((polyhedral Polyhedral-IR))
  (let ((n-generation (length (poly-cmd-history polyhedral))))
    (loop for space in (cdr (or (find n-generation *search-space* :key #'car) (find t *search-space* :key #'car) (error "No *search-space* configuration for t")))
          append (optrule-generate-search-space polyhedral space))))

(defmethod polyhedral-ir-mutate-for-children ((polyhedral Polyhedral-IR))
  (let ((space (get-next-optimization-rules polyhedral)))
    (remove-duplicates
     (loop for opt in space collect (apply-optimization polyhedral opt))
     :test #'string= :key #'pg-dump-into-str)))

(defun realize-node-with-autotuning (runtime node args &aux (searched))
  (labels ((evaluate-kernel (kernel &key (n 10) &aux (total 0.0))
             (dotimes (i n)
               (incf total (caten/codegen/byoc:kernel-call kernel runtime node args)))
             total)
           (register-kernel-as-candidate (kernel)
             (push (cons (evaluate-kernel kernel) kernel) searched)))
    (register-kernel-as-candidate (caten/air:getattr node :kernel-info))
    (let ((origin (caten/codegen/polyhedral:make-polyhedral-from-blueprint (kernel-blueprint (caten/air:getattr node :kernel-info)))))
      (print "==== Generation 1 =========")
      (print (polyhedral-ir-mutate-for-children origin))
      (print searched)
      ;; [TODO] Apply BEAM Search
      (setf (caten/air:getattr node :kernel-info) (cdr (sort searched #'< :key #'car)))
      ;; [TODO] Copy the initial results? to avoid overflow? or for sparse optimizations?
      (apply #'values (subseq args 0 (length (caten/air:node-writes node)))))))

;; Paper: https://arxiv.org/pdf/2410.03210
;; [TODO] Implement Polyhedral-Guided, Customizable AutoScheduler Engine
;; https://chatgpt.com/c/6870e59d-2970-8005-abda-1b62c5808111?model=o3-pro
;; o3-pro proposed the following:
"
| 名前                             | 意味・対象                          | 主な効果 (GPU 観点)                                     |
| ------------------------------ | ------------------------------ | ------------------------------------------------- |
| **reorder\_ikj**               | ループ順序交換                        | メモリアクセス連続化，依存を壊さずに L/S 帯域↑。                       |
| **tile\_block\_ij{M×N}**       | CTA (block) タイル化               | グローバル→SMEM 転送の削減。CTA 数で並列度を制御。                    |
| **tile\_warp\_ij{m×n}**        | Warp タイル化                      | Tensor Core／simdgroup 演算単位と一致させ，LD/ST を coalesce。 |
| **tile\_lane\_vec{w}**         | Lane 内 SIMD (vec2/4/8)         | ld.v4/st.v4 等のベクトル命令で帯域効率↑。                       |
| **split\_k\_reduce{p}**        | k 軸並列 + 反復還元                   | CTA/SM 並列度拡大。`p` は分割数。                            |
| **cache\_read\_shared\_{A/B}** | 行列タイルの SMEM 読み込み               | L2⇔SMEM 帯域と再利用回数の最適化。                             |
| **cache\_write\_shared**       | 出力タイルを SMEM 経由で書き戻し            | 書き込みコアレッシング・衝突回避。                                 |
| **sw\_pipeline\_stage{n}**     | 二重/三重バッファリング                   | データ転送と演算をオーバラップし隠蔽。                               |
| **tensorcore\_mma**            | 専用 FMA (16×16×16 等)            | FLOPs/clk を劇的に向上。Metal は `simdgroup_mad`.         |
| **vector\_fma**                | 汎用 SIMD FMA                    | Tensor Core が無い GPU で使用。                          |
| **unroll\_k{u}**               | 内部ループ展開                        | ループ制御除去と ILP 向上。                                  |
| **async\_copy**                | 非同期 cp.async / simdgroup\_copy | 転送レイテンシ隠蔽と帯域最大化。                                  |
| **rfactor\_axis{k}**           | 還元因子分割                         | prefix‑sum, attention‐score reduce 等で並列度↑。        |
| **fuse\_{i,j}**                | ループ／演算子融合                      | 中間テンソル排除，DRAM 往復削減。                               |
| **recompute\_small**           | 小演算の再計算でメモリ節約                  | O(N²)→O(N) メモリモデルで有効。                             |
| **auto\_vector\_width**        | 動的 SIMD 幅決定                    | GPU 世代差・データ型差を隠蔽。                                 |


def softmax(input[1280, 1280], output[1280, 1280]):
  allocate temp[1280, 1280]
  for n in parallel(1280):
    val_11 = 0.0
    val_2 = -Inf
    for m in range(1280):
      val_2 = max(val_2, input[1280*n+m]
    for m in range(1280):
      temp[_gid0*n+m] = exp2((input[1280*n+m]-val_2)*1.442695)
      val_11 = val_11 + temp[_gid0*n+m]
    for m in range(1280):
      output[n*1280+m] = val_9[n*1280+m]/val_11
//=============================================================================
// softmax_stored_locally_multi_dim の “warpOnly” 完全インライン化版
//=============================================================================
def softmax_stored_locally_multi_dim(input, output, m, n):
    num_packs = ceil((n/4) / blockDim.x)
    parallel for block_x in range(gridDim.x):
      parallel for thread_y in range(blockDim.y):
        parallel for tid in range(blockDim.x):
          for row in range(block_x*blockDim.y + thread_y, m, gridDim.x*blockDim.y):
            row_offset = row * (n>>2)
            row_x = input  + row_offset
            row_y = output + row_offset

            //── ローカル読み込み＋最大値
            buf = allocate float4[num_packs]
            local_max = -Inf
            for pack_id in range(num_packs):
              col_base = pack_id*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  h4 = row_x[col_base]
                  buf[pack_id][lane] = half2float(h4.component[lane])
                else:
                  buf[pack_id][lane] = -Inf
              local_max = max(local_max,
                              max(max(buf[pack_id].x, buf[pack_id].y),
                                  max(buf[pack_id].z, buf[pack_id].w)))

            //── ワープ内最大値還元
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_max = max(local_max,
                              __shfl_xor_sync(0xffffffff,
                                              local_max,
                                              mask,
                                              32))

            //── exp＋ワープ内和還元
            local_sum = 0.0
            for i in range(num_packs):
              vectorize(4):
                buf[i][lane] = exp(buf[i][lane] - local_max)
                local_sum += buf[i][lane]
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_sum += __shfl_xor_sync(0xffffffff,
                                           local_sum,
                                           mask,
                                           32)

            //── 書き戻し
            for i in range(num_packs):
              col_base = i*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  row_y[col_base].component[lane] = buf[i][lane] / local_sum

def matmul(X[128, 128], Y[128, 128], OUT[128, 128]):
  for i in range(128):
    for j in range(128):
      acc = 0.0f
      for k in range(128):
          acc = acc + X[128*i+k] * Y[128*k + j]
      OUT[128*i+j] = acc

// CTA タイル = 64×64，Warp タイル = 16×16，lane = vec4
for B_i in blockIdx.y parallel tile_block_ij:64  // 0..1
  for B_j in blockIdx.x parallel tile_block_ij:64
    // 共有メモリキャッシュ
    shared Xs[64][64] @cache_read_shared_X
    shared Ys[64][64] @cache_read_shared_Y

    // Warp グリッド (=4×4 Warps per CTA)
    for W_i in threadIdx.y parallel tile_warp_ij:16  // 0..3
      for W_j in threadIdx.x/32 parallel tile_warp_ij:16  // 0..3
        // レジスタ蓄積
        reg_C[16][16] = 0

        //--------- k 軸分割（split_k_reduce=2）-----------
        for k_outer in range(0, 128, 64):
          // 二重バッファで Xs, Ys を先読み (sw_pipeline_stage2 + async_copy)
          prefetch X, Y tiles → Xs, Ys

          //---------------- 主演算 ------------------------
          for k_inner in unroll_k8 range(0, 64, 8):
            // lane = vec4 で 4 要素ロード
            vectorize(4):
              reg_A = ld4(Xs, W_i*16 + lane, k_inner + s)    // s=0..7
              reg_B = ld4(Ys, k_inner + s, W_j*16 + lane)
            // Tensor Core (16×16×8 FMA) または simdgroup_mad
            tensorcore_mma(reg_C, reg_A, reg_B)
          //------------------------------------------------

        //--------- 出力 (vec4 store) ---------------------
        vectorize(4):
          st4(OUT,
              (B_i*64 + W_i*16 + lane),
              (B_j*64 + W_j*16 + vec4-id),
              reg_C[lane][:])


"

;; - コストモデル, 測定方法はどうでもいい。
;; ASTに対する
;;   - 探索方法(DFS)
;;   - How to transform the loop?
;; We already have a naive and fused FlashAttention Kernel.
;; We need:
;; - Loop Tiling               (Unroll, Vectorize, )
;; - Evaluate the cacheability (The number of alloc reuse)
;;  - 早い指標でTop Kを決めて, 最後にまとめてCompileしてTop1を決める，はできるかもしれない
;; これかが決定すれば自動スケジューラーが完成する
;; - Load from VRAM -> SRAM -> Register. Polyhedralでどう表現しますか
;; - optimal_kernel = apply(base_kernel, [] in SUBSET)
;;   - What is SUBSET?
;; - Ops.TRANSFER的なのを実装する (n-layered memory caching arch)
;;   - [ ] SIMD/Warp
;;   - [ ] 

;; Optimization is applied into:
;; - 最適なループ形状 + :AREF={CACHE, NOOPT}の空間を探索？
;; - :FOR Level (Everything is ND TILING)
;;   - Loop Tiling
;;   - Loop Unrolling (i.e.: VECTORIZE, PARALLELIZE)
;; - :AREF Level
;;  - (ast-apply-cache的なsomethingが必要) VRAM -> DRAM -> SRAM
;;  -
;; The ft of Tensor Compiler is POLYHEDRAL Compiler.
;; - Believe Polyhedral Compiler, implement something like polyhedral compiler.
;; e.g.: Caten can create both of:
;;   - LU Decomposing
;;   - 

;; TODO
;; - 1. Introduce Ops.FUNCTION
;;   - 2. Hand-writtern Kernelを記述するMacro, 構文を実装する
;; - 2. Thinking the minimal Softmax Transformation
;; - 3.

;; [SearchSpace] => {Action} => [SearchSpace]

;; (defkernel xxx (...) (:policy `(d . 128))) <- specify OptimizationPolicy

;; Policy:
;; - GraphRewriteでRuntimeGraphが常にStaticであることを保証して，進めていく
;; - いずれにせよグラフの形状がわからないと進められない。
;; ScheduleCommands:
;; 1. new_graph, hi, ho = apply_tile(graph, ...)
;;
;; Workload
;; LoopInterchange is actually what we need:
;; - Implement Polyhedral
