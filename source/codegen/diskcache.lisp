(defpackage :caten/codegen/diskcache
  (:use :cl :caten/graph :caten/runtime/renderer :caten/codegen/polyhedral :caten/codegen/schedule)
  (:export
   #:DBEntry
   #:*db-connection*
   #:db-connection
   #:make-kernel-description
   #:make-diskcache-entry
   ))

(in-package :caten/codegen/diskcache)
;; ~~ CreateIdentity ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun sha256-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (babel:string-to-octets string :encoding :utf-8))))

(defun make-kernel-description (graph &key (version) (cache-polyhedral nil) (getraw nil) (glo) &aux (seen))
  (let ((renderer (make-instance 'JSONStyle-Renderer :graph graph))
        (reads/writes (when cache-polyhedral (extract-accesses (make-scop-ctx-from-blueprint graph :glo glo) graph :scal->array nil :getlisp t)))
        (reads) (writes))
    (when reads/writes (setf reads (car reads/writes) writes (cdr reads/writes)))
    (funcall
     (if getraw #'identity #'sha256-hex)
     (with-output-to-string (out)
       (format out "{\"version\": ~a," version)
       (format out "\"globals\":[")
       (loop for node in (graph-nodes graph)
             if (eql (node-type node) :DEFINE-GLOBAL) do
               (format out "{\"arg\":\"~a\",\"dtype\":~a_~a_~a},"
                       (jr-gensym renderer (car (node-writes node)))
                       (if (getattr node :pointer-p) "*" "")
                       (getattr node :mode)
                       (getattr node :dtype)))
       (format out "{\"op\":\"end\"}],")
       (labels ((r (s &aux (val (id->value graph s)))
                  (when (and val (null (find (node-id val) seen)))
                    (f val) (push (node-id val) seen))
                  s)
                (e (id) (render-node renderer id))
                (emit-array (items emit-fn)
                  (format out "[")
                  (loop for it in items
                        for i from 0 do
                          (when (> i 0) (format out ","))
                          (funcall emit-fn it))
                  (format out "]"))
                (f (node)
                  (case (node-type node)
                    (:PROGN
                      (format out "{\"progn\":")
                      (emit-array (node-reads node) #'r)
                      (format out "}"))
                    (:EXPR
                     (if cache-polyhedral
                         (let* ((name (node-id node))
                                (name-unique (jr-gensym renderer name)))
                           (flet ((get-related (lst)
                                    (loop for l in lst
                                          if (find name lst :key #'car)
                                            collect l))
                                  (to-json (lst)
                                    (format nil "{~a, {\"time\": \"~a\"}}" (jr-gensym renderer (second lst)) (third lst))))
                             (let ((reads (map 'list #'to-json (get-related reads))) (writes (map 'list #'to-json (get-related writes))))
                               (format out "{\"stmt\": \"name\": \"~a\", \"writes\": ~{~a~^, ~}, \"reads\": ~{~a~^, ~}}" name-unique (or writes (list "\"\"")) (or reads (list "\"\""))))))
                         (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                             (format out "{\"expr_store\":~a}" (e (car (node-reads node))))
                             (let ((type (car (relay-writes (read-type-relay node)))))
                               (format out "{\"expr\":{\"id\":~a,\"sym\":~a,\"value\":"
                                       (caten/codegen/renderer::->cdtype (caten/aasm:tensor-relay-dtype type))
                                       (jr-gensym renderer (car (node-writes node))))
                               (format out "~a" (e (car (node-reads node))))
                               (format out "}}")))))
                    (:FOR
                     (multiple-value-bind (range body) (apply #'values (node-reads node))
                       (setf range (id->value graph range))
                       (assert (and range (eql (node-type range) :RANGE)) () "The first argument of :FOR should be :RANGE, getting ~a" range)
                       (multiple-value-bind (bind size step) (values (jr-gensym renderer (getattr range :idx)) (first (node-reads range)) (second (node-reads range)))
                         (when (symbolp size)
                           (let ((val (id->value graph size)))
                             (assert (and val (eql (node-type val) :EXPR)) () "Range: The size must be specified as EXPR or fixnum, getting ~a" val)
                             (setf size (car (node-reads val)))))
                         (when (symbolp step)
                           (let ((val (id->value graph step)))
                             (assert (and val (eql (node-type val) :EXPR)) () "Range: The step must be specified as EXPR or fixnum, getting ~a" val)
                             (setf step (car (node-reads val)))))
                         (format out "{\"for\":{\"idx\":\"~(~a~)\",\"lower\":0,\"upper\":" bind)
                         (format out "~a" (e size))
                         (format out ",\"step\":~a,\"body\":" (e step))
                         (r body)
                         (format out "}}"))))
                    (:IF
                     (multiple-value-bind (cond body) (apply #'values (node-reads node))
                       (setf cond (id->value graph cond))
                       (assert (and cond (eql (node-type cond) :EXPR)) () "IF: the conditon must be EXPR.")
                       (format out "{\"if\":{\"cond\":~a,\"then\":" (e (car (node-reads cond))))
                       (r body)
                       (format out "}}")))
                    (otherwise
                     (warn "JSONStyleRenderer: Unknown op type ~a" (node-type node))
                     (format out "{\"unknown_op\":~a}" (node-type node))))))
         (format out ",\"body\":")
         (f (id->value graph (car (graph-outputs graph))))
         (format out "}"))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Workload]
;; - [ ] 1. $Affine ==> FusionKeyのMappingを実装
;; - [ ] 2. ILP Basedで計算量が大きいFusionを実施するか，Valueを (Item Relocate Basedで実装)
;; - [ ] 3. DB Backend or HashTable Backend
;; - [ ] dashboard.lispで累積したカーネルの情報を簡易的に可視化する
;; - [ ] dashboard.lispに同型のグラフの情報を蓄積させる ==> ２回目以降Cache
;; dashboard.lispで累積したグラフを可視化させる
;; 検索エントリは$Affine
;; DB
;; - Entry: 入力ドメインのBytes, データ型, 
;; - ScheduleNode YAML
;; - UnionMap YAML
;; -
;; ShapeTrackerCache: UNION(PREV_CACHE, POST_CACHE) = RESULT
(defparameter *db-connection* nil)

(defclass DBEntry ()
  ((device :initarg :device :accessor dbentry-device :col-type (:varchar 32))
   (st :initarg :sha256/sched :col-type (:varchar 64) :accessor dbentry-st-id)
   (graph-id :initarg :sha256/graph :col-type (:varchar 64) :accessor dbentry-graph-id))
  (:metaclass mito:dao-table-class))

(mito:deftable ShapeTraker ()
  ((parent :col-type (:varchar 64))
   (child  :col-type (:varchar 64))
   (result :col-type :text)))

(defmethod print-object ((db DBEntry) stream)
  (flet ((cutoff (obj) (format nil "~a..." (subseq obj 0 (min (length obj) 7)))))
    (format stream "device=~a/st=~a/bp=~a" (dbentry-device db) (cutoff (dbentry-st-id db)) (cutoff (dbentry-graph-id db)))))

(defun db-connection (&key (path (ctx:getenv :DB_PATH)))
  (when (null *db-connection*)
    (setf *db-connection* (dbi:connect :sqlite3 :database-name (pathname path))))
  *db-connection*)

(defun diskcache-clean ())

(defun diskcache-get (entry &key (keys '(:device :st :graph-id)))
  (declare (type DBEntry entry))
  
  )

(defun diskcache-set ()

  )

(defun make-diskcache-entry (blueprint polyhedron)
  (declare (type FastGraph blueprint) (type Polyhedral-Schedule-Item polyhedron))
  (let ((glo (psi-global-lex-order polyhedron)))
    (assert glo () "make-diskcache-entry: Global-Lex-Order must be provided to create a diskcache")
    (make-instance
     'DBEntry
     :device (princ-to-string (ctx:getenv :BACKEND))
     :sha256/sched (make-kernel-description blueprint :getraw nil :cache-polyhedral t :glo glo) ;; Cache ShapeTracker
     :sha256/graph (make-kernel-description blueprint :getraw nil)))) ;; Cache Kernel (Same ID = Same Computation)
