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

(defstruct Scope
  (domain)
  (schedule))

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

(defun %make-dataflow-graph (schedule read write)
  (declare (type isl::schedule schedule) (type isl::union-map read write))
  (let ((ctx (make-global-context))
        (ast-build (create-ast-build)))
    (labels ((explore (node scope)
               (case (schedule-node-get-type node)
                 (:schedule-node-domain
                  (assert (null (scope-domain scope)) () "Domain should be root")
                  (let ((domain-set (schedule-node-domain-get-domain node))
                        (sched (scope-schedule scope)))
                    (explore (schedule-node-first-child node) (make-scope :domain domain-set :schedule sched))))
                 (:schedule-node-filter
                  ;; filter intersects the range of domain
                  (let* ((filter (schedule-node-filter-get-filter node))
                         (dom* (union-set-intersect (scope-domain scope) filter))
                         (sched (if (scope-schedule scope)
                                    (multi-union-pw-aff-intersect-domain (scope-schedule scope) dom*)
                                    (scope-schedule scope))))
                    (explore (schedule-node-first-child node) (make-scope :domain dom* :schedule sched))))
                 ((:schedule-node-sequence :schedule-node-set)
                  (dotimes (i (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
                    (explore (schedule-node-get-child node i) scope)))
                 ((:schedule-node-leaf)
                 ; (print "LEAF")
                 ; (print scope)
                  ;; A list of domain is inserted
                  ;; [TODO] Writeは，現在のレベルのBandに値を書き込む，後続のメモリはそのBandを参照。
                  ;; [TODO] Readは ...
                  ;; [TODO]
                  )
                 (:schedule-node-band
                  ;; こういう形になるはず
                  ;; BAND(i, 512, 64) -->  MEMORY(X, 0~512 by 64) -> LoopIn(i, 512, 64)
                  ;;   BAND(ii, 64, 1) --> MEMORY(X, i:i+64)      -> LoopIn(ii, 64, 1) -> (Filterが読むとCost発生)
                  ;;
                  ;;          LoopIn(i,512, 64) 0, 64, 128, ...
                  ;;               |     |
                  ;;    MEMORY(X,i:i+64) MEMORY(Y, i:i:64) // 64ずつ読んで一個下のメモリへ移動 <- まずはこのMEMORYを作成したい。
                  ;; ここでは，
                  ;; 1. 現在のDomainが保守する変数の一覧
                  ;; 2. 現在のScheduleが各変数のどのエリアを読むかを整数集合演算で取得する
                  ;; ができる必要がある
                  (let* ((mupa   (schedule-node-band-get-partial-schedule node))
                         (prefix (scope-schedule scope))
                         (theta  (if prefix
                                     (multi-union-pw-aff-flat-range-product prefix mupa)
                                     mupa))
                         (s0     (union-map-from-multi-union-pw-aff theta))
                         (dom0   (scope-domain scope))
                         (model  (union-map-get-space
                                  (schedule-get-map (schedule-node-get-schedule node))))
                         (dom1   (align-params/uset dom0 model))
                         (s1     (align-params/umap s0   model))
                         (s      (union-map-intersect-domain s1 dom1))
                         (maps   nil))
                    (%foreach-set
                     dom1
                     (lambda (set)
                       (let ((nm (set-get-tuple-name set)))
                         (setf maps
                               (append maps
                                       (union-map-intersect-name read  nm)
                                       (union-map-intersect-name write nm))))))
                    (let* ((A0  (union-map-from-map-list-lisp maps))
                           (A1  (align-params/umap A0 model))
                           (A1  (union-map-intersect-domain A1 Dom1))
                           (F   (union-map-apply-range (union-map-reverse S) A1))
                           (Ainv (union-map-reverse A1))        
                           (F-iter (union-map-apply-range F Ainv))   
                           (F-iter (union-map-intersect-range F-iter Dom1))
                           (F-iter (union-map-coalesce F-iter))
                           (F-iter (union-map-detect-equalities F-iter))
                           (F-iter (union-map-gist-range F-iter Dom1)))
                      ;; 各変数のメモリアクセス一次元だった！
                      (format t "~&[Band] T->D F_iter=~a~%" F-iter))
                    (explore (schedule-node-first-child node)
                             (make-scope :domain (scope-domain scope) :schedule theta))))
                 (otherwise (error "No case for ~a" (schedule-node-get-type node))))))
      (print "PARSING ...")
      (explore (print (schedule-get-root schedule)) (make-scope)))))

(defun make-dataflow-graph (schedule read write)
  (setf schedule (schedule-fuse-all-band schedule))
  (let ((band (schedule-node-first-child (schedule-get-root schedule))))
    (setf schedule
          (schedule-node-get-schedule
           (schedule-node-band-tile
            band
            (tiling-size band 4)))))
  (print (time (caten/codegen/search/ast::compute-ast-from-schedule schedule)))
  (time (%make-dataflow-graph schedule read write)))
;; DataFlowGraph Specs
;; - BAND
;; - LoopIn
;; - LoopOut
;; - Graph Output is same as base kernel output

;; - [ ] MEMORY (GMEM, SMEM, L1, L2) <-- BANDがこれを作成する。Writeの書き込む先でもある。
;; - [ ] Loop In
;; - [ ] Loop Out
