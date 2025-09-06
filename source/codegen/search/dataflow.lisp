(defpackage :caten/codegen/dataflow
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl :caten/air :caten/aasm :caten/codegen/search/schedule :caten/codegen/search/ast)
  (:export))
(in-package :caten/codegen/dataflow)


;; ScheduleGraph => AbstractHardware
;;; dataflow-ops.lisp
;;; Domain
;;; Band
;;; Filter
;;; - Rename dataflow => DFG

(defclass DataFlowGraph (FastGraph)
  nil)

(defnode (:DFG :Memory) ()
         ""
         :slots nil)

(defnode (:DFG :LoopIn) ()
         ""
         :slots nil)

(defnode (:DFG :LoopOut) ()
         ""
         :slots nil)

(defnode (:DFG :ALU) ()
         ""
         :slots nil)

(defun %memory (&optional (from))
  (emit (make-node :DFG :MEMORY (list (gensym)) (if from (list from) nil))))

(defstruct Global-Context
  (var->memory (make-hash-table)))

(defstruct Scope ;; ASTBuild的な
  (domain))

(defun union-map-intersect-name (umap name &aux (results))
  (declare (type string name))
  (%foreach-map
   umap
   #'(lambda (map)
       (when (string= (set-get-tuple-name (map-domain map)) name)
         (push map results))))
  (nreverse results))

(defun union-map-from-map-list-lisp (map-list)
  (let ((map-list (map 'list #'(lambda (x) (map-union-map (! x))) map-list)))
    (reduce #'(lambda (x y) (union-map-union (! x) (! y))) map-list)))

(defun restrict-map-to-set (umap uset)
  "Return UMAP ∩ (USET × Range(UMAP)) — i.e., restrict map's domain to USET."
  (let* ((model (space-align-params
                 (union-map-get-space umap)
                 (union-set-get-space uset)))
         (umap* (align-params/umap umap model))
         (uset* (align-params/uset uset model))
         (res   (union-map-intersect-domain umap* uset*)))
    (union-map-coalesce (union-map-detect-equalities res))))

(defun lift-access-to-schedule (schedule read)
  "Given S: Dom->Time and R: Dom->Mem, return Time->Mem = S^{-1} ∘ R."
  (declare (type isl::union-map schedule read))
  (let* ((model (space-align-params
                 (union-map-get-space schedule)
                 (union-map-get-space read)))
         (S*    (align-params/umap schedule model)) ; Dom -> Time
         (R*    (align-params/umap read     model)) ; Dom -> Mem
         (DomS  (union-map-domain S*))
         (R-dom (union-map-intersect-domain R* DomS))
         ;; S^{-1}: Time -> Dom
         (S-inv (union-map-reverse S*))
         ;; Time -> Mem = (Time -> Dom) ∘ (Dom -> Mem)
         (T->V  (union-map-apply-range S-inv R-dom)))
    (union-map-coalesce (union-map-detect-equalities T->V))))

(defun lift-access-to-schedule-chain (schedule read)
  "Return Dom -> [Time -> Mem] from S: Dom->Time and R: Dom->Mem."
  (declare (type isl::union-map schedule read))
  (let* ((model (space-align-params
                 (union-map-get-space schedule)
                 (union-map-get-space read)))
         (S*    (align-params/umap schedule model)) ; Dom -> Time
         (R*    (align-params/umap read     model)) ; Dom -> Mem
         (DomS  (union-map-domain S*))
         (R-dom (union-map-intersect-domain R* DomS))
         (T->M  (union-map-apply-range (union-map-reverse S*) R-dom)) ;; Time -> Mem = S^{-1} ∘ R
         (Wrapped (union-map-wrap T->M)) ;; [Time->Mem]
         (D2Wrapped (union-map-from-domain-and-range DomS Wrapped))) ;; Dom -> [Time->Mem]
    (union-map-coalesce (union-map-detect-equalities D2Wrapped))))

(defun %make-dataflow-graph (schedule read write)
  (declare (type isl::schedule schedule) (type isl::union-map read write))
  (let ((ctx (make-global-context))
        (ast-build (create-ast-build)))
    (labels ((explore (node scope)
               ;; https://github.com/Meinersbur/isl/blob/433e17b9bccf4417725744316dff8a44caedcbcc/isl_ast_codegen.c#L5749
               (case (schedule-node-get-type node)
                 (:schedule-node-domain
                  ;; https://github.com/Meinersbur/isl/blob/433e17b9bccf4417725744316dff8a44caedcbcc/isl_ast_codegen.c#L5828
                  (assert (null (scope-domain scope)) () "Domain should be root")
                  (let ((D (union-set-coalesce (schedule-node-domain-get-domain node))))
                    (explore (schedule-node-first-child node) (make-scope :domain D))))
                 (:schedule-node-filter
                  ;; https://github.com/Meinersbur/isl/blob/433e17b9bccf4417725744316dff8a44caedcbcc/isl_ast_codegen.c#L5504
                  ;; filter intersects the range of domain
                  (let* ((filter (schedule-node-filter-get-filter node))
                         (filter   (align-params/uset filter (union-set-get-space (scope-domain scope))))
                         (dom*     (union-set-intersect (scope-domain scope) filter)))
                    (explore (schedule-node-first-child node) (make-scope :domain dom*))))
                 ((:schedule-node-sequence :schedule-node-set)
                  (dotimes (i (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
                    (explore (schedule-node-get-child node i) scope)))
                 ((:schedule-node-leaf)
                  ;; https://github.com/Meinersbur/isl/blob/master/isl_ast_codegen.c#L5165
                  (let* ((S (restrict-map-to-set
                             (schedule-node-get-prefix-schedule-union-map node)
                             (scope-domain scope)))
                         (model-space (union-map-get-space S))
                         (S  (align-params/umap S model-space)))
                    (print "LEAF")
                    (print S)
                    (%foreach-set
                     (scope-domain scope)
                     #'(lambda (stmt)
                         (let* ((name (set-get-tuple-name stmt))
                                (read-maps (union-map-intersect-name read name))
                                (write-maps (union-map-intersect-name write name)))
                           (dolist (var (append write-maps read-maps))
                             (let* ((A (lift-access-to-schedule S (restrict-map-to-set (map-union-map (map-align-params var model-space)) (scope-domain scope)))))
                               (print A)
                               ;; [TODO]
                               ;; ↑のAのMapからTileSize, およびCacheMissを評価できる
                               ;; TileSizeの結果については，Graphにして共通化する。
                               ;; - [ ] Read/WriteがTile内簡潔かどうか
                               ))
                           ;; 単純にSpaceの階層=メモリの階層という解釈でいいのか？
                           ;; 次元ごとに捜査する。
                           
                           ;; 1. CacheLineを評価したい
                           ;; 2. Memoryをどこから読んでるかを評価したい
                           ;;  - これにALUとなんかの係数をかけて再利用を評価したい
                           )))))
                 (:schedule-node-band
                  ;; https://github.com/Meinersbur/isl/blob/433e17b9bccf4417725744316dff8a44caedcbcc/isl_ast_codegen.c#L5219
                  (explore (schedule-node-first-child node) (make-scope :domain (scope-domain scope)))
                  ;; LoopOut
                  )
                 (otherwise (error "No case for ~a" (schedule-node-get-type node))))))
      (print "PARSING ...")
      (explore (print (schedule-get-root schedule)) (make-scope)))))

(defun make-dataflow-graph (schedule read write)
  (setf schedule (schedule-fuse-all-band schedule))
  (let ((band (schedule-node-first-child (schedule-get-root schedule))))
    (when (eql :schedule-node-band (schedule-node-get-type band))
      (setf schedule
            (schedule-node-get-schedule
             (schedule-node-band-tile
              (schedule-node-band-tile
               band
               (tiling-size band 4))
              (tiling-size band 32))))))
  (time (%make-dataflow-graph schedule read write)))
;; DataFlowGraph Specs
;; - BAND
;; - LoopIn
;; - LoopOut
;; - Graph Output is same as base kernel output

;; [TODO] Requirements for Loop Fusion
;; - [ ] Statement単位でのFusion Algorithm
;; - [ ] CostFunction


;; - [ ] Mark @TileEffective Directive.
;; - [ ] MEMORY (GMEM, SMEM, L1, L2) <-- BANDがこれを作成する。Writeの書き込む先でもある。
;; - [ ] Loop In
;; - [ ] Loop Out
;; - [ ] Tileする次元も決めれそう。(But the size is N)
;; - [ ] MP/DPもPolyhedral Compiler LVLで？
;; - [ ] BANDってMupaを合成しちゃえば頑張って2vs3次元とかでFusionできるのでは？
  ;; - [ ] e.g.: split w/ smaller size

;; Matmul+Matmul, TileでOutputの想像をする
;; Input:
;; for i in range(0, 64)
;;   for j in range(0, 64)
;;     acc = 0.0
;;     for k in range(0, 64)
;;       acc += A*B
;;     out[i, j] = acc
;; for i in range(0, 64)
;;   for j in range(0, 64)
;;     acc = 0.0
;;     for k in range(0, 64)
;;       acc += A*out[i, k]
;;     out1[i, j] = acc
;; ==========================
;; for k in range(0, 64)
;;   for i in range(0, 64)
;;     for j in range(0, 64)
;;       out1[i,j] += A*B
;;     for j in range(0, 64)
;;       out[i,j] += A*out2[i, j]
