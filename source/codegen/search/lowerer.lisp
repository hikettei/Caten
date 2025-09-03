(defpackage :caten/codegen/lowerer
  (:documentation "TensorGraph => ScheduleGraph Lowerer")
  (:use :cl :caten/air :caten/aasm :caten/aasm/expr :caten/codegen/helpers
   :caten/codegen/search/polyhedral :caten/codegen/search/autotune
   :caten/codegen/search/ast :caten/runtime)
  (:export
   #:make-schedule-graph
   #:schedule-graph-fuse))

(in-package :caten/codegen/lowerer)
;; ~~ Scheduling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Grids
  "A `Grids` is the minimum scheduling unit. It corresponds to zero or one `VIEW`, and the list of nodes contained in items must satisfy the following conditions:
- All nodes can be executed within the same for loop
- The entire subgraph produces a single output"
  (is-affine t :type boolean)      ;; Optimize graph as polyhedral model?
  (is-zero-cost nil :Type boolean) ;; Allowed to clone Grids for simplicity?
  (id 0 :type fixnum)              ;; Unique ID
  (items nil :type list)           ;; A list of nodes grouped to this grids
  (writes nil :type list)          ;; grid writes (list (cons name relay) ...)
  (reads nil :type list))          ;; grid reads  (list (cons name relay) ...)

(defun make-grids-from-node (graph node id->grids id->users)
  "The function `make-grids-from`node` creates the next generation node of Grids. However if the following
conditions are satisfied, it will be fused into the predecessor Grids:
- When the predecessor and the node are connected one-to-one and both are affine.
- If reduction=T, then it cannot participate in any predecssor Grids.
Throughout the entire scheduling process, it must be ensured that when items in the Grids are converted into the Polyhedral-Schedule-Item, its theta should not containt any sequence (i.e.: only a single STMT exists in every Grids.)"
  (declare (type node node) (type hash-table id->grids id->users))
  (flet ((node-is-singleton-p (id &aux (node (id->value graph id)))
           (and
            node
            (null (getattr node :reduction :allow-undefined t))
            (= 1 (length (gethash (car (node-writes node)) id->users))))))
    (let ((next-id (hash-table-count id->grids)))
      (case (node-type node)
        (:Allocate
         (if (node-reads node)
             (make-grids :id next-id :is-affine nil :items (list node))
             (make-grids :id next-id :is-affine t :items (list node))))
        (:View (make-grids :id next-id :is-affine t :items (list node)))
        (otherwise
         (if (typep (node-attr node) 'JITAble)
             (let* ((is-reduce-p (getattr node :reduction :allow-undefined t))
                    (parent-grids
                      (loop for r in (node-reads node)
                            for g = (gethash r id->grids)
                            for nth upfrom 0
                            if (and g (grids-is-affine g) (node-is-singleton-p r)
                                    (if is-reduce-p (not (= nth 0)) t)) ;; if reduction, force realize at the first argument.
                              collect g))
                    (items (append (reduce #'append (map 'list #'grids-items parent-grids)) (list node)))
                    (new-grids
                      (make-grids :id next-id :items items)))
               (dolist (n items)
                 (dolist (w (node-writes n))
                   (setf (gethash w id->grids) new-grids)))
               new-grids)
             (make-grids :id next-id :is-affine nil :id 0 :items (list node))))))))

(defun grids-ensure-affine (grids)
  "Converts grids into NonAffine if the given Affine grids is no worth to jit-compile.
- If the grid view isn't paired w/ any jitable nodes.
- grids is composed of scalar computations."
  (declare (type grids grids))
  ;; Affine composed of a single VIEW = NonAffine
  (when (and (= 1 (length (grids-items grids))) (eql :VIEW (node-type (car (grids-items grids)))))
    (setf (grids-is-affine grids) nil
          (grids-is-zero-cost grids) t))
  ;; Scalar Graph = NonAffine
  (when (and
         (grids-is-affine grids)
         (every
          #'(lambda (node)
              (and
               (every #'(lambda (x) (or (null x) (= 0 (the fixnum (tensor-relay-nrank x))))) (relay-writes (read-type-relay node)))
               (every #'(lambda (x) (or (null x) (= 0 (the fixnum (tensor-relay-nrank x))))) (relay-reads (read-type-relay node)))))
          (grids-items grids)))
    (setf (grids-is-affine grids) nil
          (grids-is-zero-cost grids)
          (every #'(lambda (node) (find (node-type node) `(:ALLOCATE :LOAD))) (grids-items grids)))))

(defun grids-init-edges (grids id->grids id->users graph-outputs)
  "Finalizes grids.reads and grids.writes, also determines the types of them."
  (declare (type Grids grids) (type hash-table id->grids id->users) (type list graph-outputs) (optimize (speed 3)))
  (labels ((node-is-output-p (node)
             (or
              (some
               #'(lambda (usr)
                   (let ((usr (gethash (car (node-writes usr)) id->grids)))
                     (not (= (grids-id grids) (grids-id usr)))))
               (gethash (car (node-writes node)) id->users))
              (find (the symbol (car (node-writes node))) graph-outputs)))
           (node-reads-from-another-grids (node)
             (loop for r in (node-reads node)
                   for typ in (relay-reads (read-type-relay node))
                   for g = (gethash r id->grids)
                   if (and (symbolp r) g (not (= (grids-id grids) (grids-id g)))) ;; collect when definition is not self
                     collect (cons r typ))))
    (let ((grid-writes*
            (loop for item in (grids-items grids)
                  if (node-is-output-p item)
                    collect (cons (car (node-writes item)) (car (relay-writes (read-type-relay item))))))
          (grid-reads*
            (loop for item in (grids-items grids)
                  append (node-reads-from-another-grids item))))
      (setf (grids-writes grids) grid-writes*
            (grids-reads grids) grid-reads*))))

(defun extract-access (sp list dom-str constraints bp)
  (let ((accesses))
    (loop for item in (graph-nodes bp)
          if (and (eql (node-type item) :PolyAref) (find (car (node-reads item)) list :key (alexandria:compose #'car #'node-reads)))
            do (let ((dg (id->value bp (car (node-reads item)))))
                 (assert (eql (node-type dg) :DEFINE-GLOBAL))
                 (push (format nil "~a -> ~(~a~)[~a]" dom-str (getattr dg :name) (polyaref-on-global-lex-order sp item bp)) accesses)))
    (isl::union-map-from-str (format nil "~a -> { ~{~a~^; ~} }" constraints accesses))))

(declaim (ftype (function (Graph) (values ScheduleGraph)) make-schedule-graph))
(defun make-schedule-graph (graph)
  "
```
(make-schedule-graph graph)
```
Creates a ScheduleGraph from the given grpah.
"
  (declare (type Graph graph) (optimize (speed 3)))
  (graph-infer-type-relay graph)
  (assert (null (graph-seen graph)) () "tensor-graph->schedule-graph: Scheduling partial graph is not allowed! Set graph-seen = nil")
  (let ((id->grids (make-hash-table)) (id->users (make-hash-table)) (queue)
        (in-degrees (make-hash-table)) (out-degrees (make-hash-table)))
    (flet ((butseen (list) (loop for l in list for v = (id->value graph l) if (and v (symbolp l)) collect v)))
      (loop for node in (graph-nodes graph) do
        (assert (= 1 (length (the list (node-writes node)))))
        (setf (gethash (node-id node) in-degrees) (butseen (node-reads node)))
        (dolist (r (butseen (node-reads node)))
          (let ((node-id (car (node-writes r))))
            (when (null (find (node-id node) (the list (gethash node-id id->users)) :key #'node-id))
              (push node (gethash node-id id->users))))
          (when (null (find (the symbol (node-id node)) (the list (gethash (node-id r) out-degrees)) :key #'node-id))
            (push node (gethash (node-id r) out-degrees))))))
    ;; [TODO]
    ;; Insert (car (node-writes backward)) to node-reads of all backward nodes
    (loop for node in (graph-nodes graph) if (null (gethash (node-id node) in-degrees)) do (push node queue))
    (loop while queue
          for node = (pop queue)
          for new-grid = (make-grids-from-node graph node id->grids id->users) do
            (dolist (w (node-writes node)) (setf (gethash w id->grids) new-grid))
            (dolist (adj (gethash (node-id node) out-degrees))
              (setf (gethash (node-id adj) in-degrees) (remove (node-id node) (gethash (node-id adj) in-degrees) :key #'node-id))
              (when (null (gethash (node-id adj) in-degrees))
                (push adj queue)))
            (remhash (node-id node) out-degrees))
    (assert (= 0 (hash-table-count out-degrees)) ()
            "The following nodes are not scheduled. circular dependencies?~%~a" (alexandria:hash-table-values out-degrees))
    ;; Construct Graph
    (let ((all-grids (make-hash-table)) (n-scheduled 0) (val->grids (make-hash-table)))
      (declare (type fixnum n-scheduled))
      ;; circular dependency of schedule graph? will it happen?
      (maphash
       #'(lambda (id grids)
           (declare (ignore id))
           (setf (gethash (grids-id grids) all-grids) grids))
       id->grids)
      ;; [todo] parallelize grids-init w/ lparallel!
      (mapc #'grids-ensure-affine (alexandria:hash-table-values id->grids))
      (mapc
       #'(lambda (x)
           (grids-init-edges x id->grids id->users (graph-outputs graph))
           (dolist (w (grids-writes x))
             (setf (gethash (car w) val->grids) x)))
       (alexandria:hash-table-values id->grids))
      (let* ((g
               (loop with id->bind = (make-hash-table)
                     for key in (sort (the list (alexandria:hash-table-keys all-grids)) #'<)
                     for grids = (gethash key all-grids) do (incf n-scheduled (length (the list (grids-items grids))))
                     collect (grids->schedule-item id->bind graph grids val->grids)))
             (schedule-space (create-lexicographical-ctx g))
             (g
               (loop for item in g
                     if (listp item)
                       collect ($affine (getf item :grid-writes) (getf item :grid-reads)
                                        :polyhedron (%make-polyhedral-schedule-item
                                                     (getf item :domain) (getf item :schedule)
                                                     (extract-access schedule-space (getf item :read-arefs) (getf item :domain-str) (getf item :constraints) (getf item :blueprint))
                                                     (extract-access schedule-space (getf item :write-arefs) (getf item :domain-str) (getf item :constraints) (getf item :blueprint))
                                                     :global-lex-order schedule-space)
                                        :blueprint (getf item :blueprint)
                                        :reduction (getf item :reduction)
                                        :storage-map (getf item :storage-map))
                     else
                       collect item)))
        (when (>= (the fixnum (ctx:getenv :JIT_DEBUG)) 1)
          (let ((dims (the list (alexandria:hash-table-keys (global-lex-order-dict schedule-space)))))
            (caten/common.logger:print-info "Constructed ~a-Dimensional Polyhedral Model: edges=~A" (length dims) dims)))
        (assert (= n-scheduled (length (the list (graph-nodes graph)))))
        (setf g (apply #'make-graph g)
              (graph-outputs g) (copy-list (graph-outputs graph)))
        (setf g (->schedule-graph g))
        (verify-graph g)
        g))))
;; ~~ Lowering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun create-lexicographical-ctx (items &key (warn-threshold 30))
  "Collect all the strides used within the compilation session and construct a hash table.
Attention: this assumes that CSE has already detected all redundant stride computations.
Otherwise, an excessive dictionary will be created, leading to a significant increase in compilation time!"
  (declare (type list items))
  (let* ((lex (make-global-lex-order :session-id (gensym "SESSION")))
         (dict (global-lex-order-dict lex)))
    (loop for item in items
          if (listp item) do
            (dolist (item (graph-nodes (getf item :blueprint)))
              (when (eql :PolyAref (node-type item))
                (loop for i upfrom 0 below (getattr item :nrank)
                      do (setf (gethash (nth (1+ i) (node-reads item)) dict) t)))))
    (loop for key being the hash-key of (global-lex-order-dict lex)
          for nth upfrom 0 do
            (setf (gethash key (global-lex-order-dict lex)) nth))
    (when (> (length (alexandria:hash-table-keys (global-lex-order-dict lex))) warn-threshold)
      (warn "create-lexicographical-ctx: Detected an excessive lexicographical space creation (n_keys > ~a)
This may cause a significant increase in compilation time. Please check the following:
- There is a possibility that the TensorGraph Simplifier failed to detect Common Subexpression Elimination (CSE) for complex stride computations.
- If you believe this behavior is correct, please partition the TensorGraph partially and compile it to improve compilation speed." warn-threshold))
    lex))

;; TODO
;; - [ ] !view from INDEX-COMPONENTS? (e.g.: mask is applied?)
;; TODO: Test (!exp (!add (!sin (make-tensor `(3 3))) (make-tensor `(3 3)) :reduce t))
(defun lower-into-blueprint (storage-map id->bind iterspace gids grid-id items base-graph writes reads write-types read-types
                             &aux (domain-out) (schedule-out) (constants-out) (domain-str) (read-arefs) (write-arefs))
  "Creates blueprint from storage-map:
- storage-map
- id->bind
"
  (declare (type hash-table id->bind) (type list gids items iterspace) (type Graph base-graph)
           (type fixnum grid-id) (type list writes reads write-types read-types))
  (assert (= 1 (length writes)))
  ;; Ensure no sequence is introduced.
  ;; TODO
  ;; - [x] Polyhedral Model Construction At Here
  ;; - [x] Reduction
  ;; - [x] Single Domain
  ;; - [x] Aref can introduce extra args
  ;; - [x] SYMBOLIC
  ;; - [x] EXPRifyもこの段階で挿入してあげる
  ;; - [ ] _gid conflict発生しない？ during fusion
  ;; - [ ] SCoPを簡略化する？
  ;; - [ ] define-global ==> rename
  (values
   (with-blueprint (:noopt t)
     (let ((binds) (root) (const-loads) (expr-write-to (intern (format nil "E_~a" grid-id))))
       (labels ((guard-scalar (v &key (allow-raw))
                  (etypecase v
                    (number (if allow-raw v (%load (%salloc :dtype :int64) v)))
                    (symbol
                     (let ((definition (id->value base-graph v)))
                       (assert definition () "lower-into-blueprint: Cannot introduce symbolic shape ~a because it is not in the given tensor-graph. Is the type-relay up-to-date?" v)
                       (let ((type (car (relay-writes (read-type-relay definition)))))
                         (when (null (find v const-loads :key #'car))
                           (push (cons v (tensor-relay-dtype type)) const-loads))
                         v)))))
                (ensure-expr (id)
                  (etypecase id
                    (number id)
                    (symbol (%expr id))))
                (relay->aref (name relay)
                  (declare (type TensorRelay relay))
                  ;; Note
                  ;; - [ ] (fconst 'a) どのようにLowerされる？
                  (%PolyAref
                   name
                   (map 'list #'(lambda (x) (guard-scalar x :allow-raw t)) (tensor-relay-stride relay))
                   (loop for nth upfrom 0 below (tensor-relay-nrank relay)
                         for v = (nth nth (tensor-relay-views relay))
                         for gid = (nth nth gids) ;; (upfrom below by broadcast)
                         if (fourth v) ;; broadcasted
                           collect (guard-scalar 0)
                         else
                           collect
                           (if (null v)
                               gid
                               (%add (guard-scalar (car v)) (%mul (guard-scalar (third v)) gid))))))
                (%insert-item (item)
                  ;; Output/Reduction
                  (loop for w in (node-writes item)
                        for wt in (relay-writes (read-type-relay item))
                        for nth upfrom 0
                        if (find w writes) do
                          (let ((waypoint (gensym "WP"))
                                (write-to
                                  (if (getattr item :reduction :allow-undefined t)
                                      (progn
                                        (setf (gethash (car (node-writes item)) id->bind)
                                              (make-node :JIT :BIND (list (car (node-writes item))) (list expr-write-to) :value (car (node-reads item))))
                                        (relay->aref (car (node-reads item)) (car (relay-reads (read-type-relay item)))))
                                      (relay->aref w wt))))
                            (when (getattr item :reduction :allow-undefined t)
                              (push write-to read-arefs))
                            (push write-to write-arefs)
                            (assert (null root) () "lower-into-blueprint: Single grids should provide single root (given items are invalid)")
                            (setf (nth nth (node-writes item)) waypoint)
                            (setf root (%setf write-to waypoint))))
                  
                  (loop for r in (node-reads item)
                        for rt in (relay-reads (read-type-relay item))
                        for nth upfrom 0
                        if (find r reads)
                          do (let ((raref (relay->aref r rt)))
                               (push raref read-arefs)
                               (setf (nth nth (node-reads item)) (car (node-writes raref)))))
                  (assert (= 1 (length (node-writes item))) () "lower-into-blueprint: JITAble nodes must have a single output.")
                  (emit item))
                (lower-item (item)
                  (case (node-type item)
                    (:INDEX-COMPONENTS
                     (assert (= (length gids) (length (cdr (node-reads item)))) () "lower-into-blueprint: Cannot lower index-components because iteration spaces does not match.")
                     (let ((node
                             (reduce
                              #'%add
                              (loop for stride in (cdr (node-reads item))
                                    for gid in gids
                                    if (eql stride 1)
                                      collect (%load (%salloc :dtype :int64) 0)
                                    else
                                      collect (%mul (guard-scalar stride) gid)))))
                       (setf (node-writes node) (node-writes item))
                       (emit node)))
                    (otherwise (%insert-item item)))))
         (mapc #'lower-item items)
         (assert root () "lower-into-blueprint: the root was not found")
         ;; Blueprint construction
         (let* ((stmt (%expr (node->id root)))
                (constraints) (quasiaffine-params)
                (body expr-write-to)) ;; Can introduce only single stmt
           (setf (node-id stmt) (intern (format nil "STMT_~a" grid-id)) ;; Rename unique but more readable NID for stmt
                 (car (node-writes stmt)) expr-write-to)
           (loop for gid in (reverse gids) for space in (reverse iterspace)
                 do (push (format nil "0 <= ~(~a~) <= ~a" gid space) constraints)
                    (when (symbolp space) (push space quasiaffine-params))
                    (setf body (%range gid (ensure-expr (guard-scalar space :allow-raw t)) body :rid gid)))
           ;; Finalize graph input/outputs, and scalar outputs gathered by (guard-scalar)
           (flet ((e (id)
                    (if (gethash id id->bind)
                        (progn
                          (setf (gethash id storage-map) (getattr (gethash id id->bind) :value))
                          (getattr (gethash id id->bind) :value))
                        id)))
             (loop for w in writes for wt in write-types do
               (%global w (e w) (tensor-relay-dtype wt) (not (= 0 (tensor-relay-nrank wt)))))
             (loop for r in reads for rt in read-types do
               (%global r (e r) (tensor-relay-dtype rt) (not (= 0 (tensor-relay-nrank rt))))))
           ;; Constant Arguments
           (loop for (name . dtype) in const-loads do
             (%global name name dtype nil :mode :read))
           (dolist (b binds) (emit b))
           ;; Polyhedral Model Initialization
           (let* ((domain
                    (isl:union-set-from-str
                     (format nil "[~{~a~^, ~}] -> { ~a[~{~(~a~)~^, ~}] : ~{~a~^ and ~} }"
                             (remove-duplicates quasiaffine-params) (node-id stmt) gids constraints)))
                  (theta (isl:schedule-get-root (isl:schedule-from-domain domain))))
             (loop for gid in gids
                   for mupa = (isl:multi-union-pw-aff-from-str (format nil "[{~a[~{~(~a~)~^, ~}] -> [(~(~a~))]}]" (node-id stmt) gids gid))
                   do (setf theta (isl:schedule-node-insert-partial-schedule (isl:schedule-node-first-child theta) mupa)))
             (setf domain-out domain
                   schedule-out (isl:schedule-node-get-schedule theta)
                   constants-out (format nil "[~{~a~^, ~}]" (map 'list #'car const-loads))
                   domain-str (format nil "~a[~{~(~a~)~^, ~}]" (node-id stmt) gids))
             (%progn body))))))
   domain-out
   schedule-out
   constants-out
   domain-str
   read-arefs write-arefs))

(defun copy-item (item &aux (item (copy-node item)))
  (setf (node-id item) (gensym "NID"))
  item)

(defun items/fold-and-verify-toplevel-views (items reads writes)
  "Removes `VIEW` from items w/ keeping the graph consistency."
  (declare (optimize (speed 3)) (type list items reads writes) (ignore reads))
  (let ((w->r (make-hash-table)))
    (loop for i in items
          if (eql (node-type i) :VIEW) do
            ;; TODO: Detect illegal scheduling like:
            ;; A -> [VIEW] -> B -> [VIEW]
            (assert (null (find (the symbol (car (node-writes i))) writes))
                    ()
                    "Detected illegal scheduling group: Affine groups should not return VIEW.")
            (setf (gethash (car (node-writes i)) w->r) (car (node-reads i))))
    (flet ((n (id) (gethash id w->r id)))
      (loop for i in items
            if (not (eql (node-type i) :VIEW))
              collect
              (progn
                (setf (node-reads i) (map 'list #'n (node-reads i)))
                i)))))

(defun get-grouped-dims (items)
  "Determines the common iteration space among items"
  (declare (type list items))
  (let ((kernel-rank
          (loop for node in items
                for type = (read-type-relay node)
                maximize
                (loop for r in (append (relay-reads type) (relay-writes type))
                      when r maximize (length (tensor-relay-shape r)))))
        (rank2space (make-hash-table)))
    (labels ((check (relay)
               (when (and relay (> (tensor-relay-nrank relay) 0))
                 (assert (= kernel-rank (tensor-relay-nrank relay)) () "get-grouped-dims: Co-grouped items must have the same rank by VIEW. kernel-rank=~a vs tensor-relay-nrank=~a" kernel-rank (tensor-relay-nrank relay))
                 (loop for s in (tensor-relay-shape relay)
                       for rank upfrom 0 below kernel-rank
                       do (setf (gethash rank rank2space)
                                (if (eql s 1)
                                    (or (gethash rank rank2space) s)
                                    s)))))
             (explore (node)
               (mapc #'check (relay-reads (read-type-relay node)))
               (mapc #'check (relay-writes (read-type-relay node)))))
      (mapc #'explore items))
    (loop for rank upfrom 0 below kernel-rank
          collect
          (or (gethash rank rank2space) (error "get-grouped-dims: The size for rank ~a is not determined." rank)))))

(defun grids->schedule-item (id->bind graph grids val->grids)
  "Converts Grids ==> $Affine"
  (assert (grids-writes grids))
  (let ((grid-reads (map 'list #'car (grids-reads grids)))
        (grid-writes (map 'list #'car (grids-writes grids)))
        (grid-read-types (map 'list #'cdr (grids-reads grids)))
        (grid-write-types (map 'list #'cdr (grids-writes grids))))
    (when (null (grids-is-affine grids))
      (return-from grids->schedule-item ($nonaffine grid-writes grid-reads :items (grids-items grids))))
    (let ((extra-items
            (loop for r in grid-reads for nth upfrom 0
                  for g = (gethash r val->grids)
                  if (and g (grids-is-zero-cost g))
                    do (setf (nth nth grid-reads) (map 'list #'car (grids-reads g))
                             (nth nth grid-read-types) (map 'list #'cdr (grids-reads g)))
                    and append (grids-items g))))
      (setf grid-reads (alexandria:flatten grid-reads)
            grid-read-types (alexandria:flatten grid-read-types)
            (grids-items grids)
            (items/fold-and-verify-toplevel-views (map 'list #'copy-item (append extra-items (grids-items grids))) grid-reads grid-writes))
      ;; Grids are affine
      (let* ((iterspace (get-grouped-dims (grids-items grids)))
             (gids      (map 'list #'gid (range 0 (length (the list iterspace))))) ;; gid0 gid1 ...
             (is-memory-intensive-p (some #'(lambda (x) (getattr x :reduction :allow-undefined t)) (grids-items grids)))
             (storage-map (make-hash-table)))
        (multiple-value-bind (bp domain schedule constraints domain-str rarefs warefs)
            (lower-into-blueprint  storage-map id->bind iterspace gids (grids-id grids) (grids-items grids) graph grid-writes grid-reads grid-write-types grid-read-types)
          ;;        (caten/codegen/blueprint:print-blueprint bp t)
          (setf bp (caten/aasm::%simplify-ast bp))
          (let ((args (loop for item in (graph-nodes bp) if (eql (node-type item) :DEFINE-GLOBAL) collect (car (node-writes item)))))            (caten/codegen/blueprint:print-blueprint bp t)
            (list :grid-writes grid-writes :grid-reads (loop for a in args if (null (find a grid-writes)) collect a)
                  :domain domain :schedule schedule :constraints constraints :domain-str domain-str
                  :storage-map storage-map :reduction is-memory-intensive-p
                  :blueprint bp :read-arefs rarefs :write-arefs warefs)))))))
;; ~~~ Entry Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - [x] Rename: make-schedule-graph
;; - [ ] Schedule involving scalars
;; - [ ] Schedule threefry (Reduce)
;; - [ ] KVCache Scheduling
;; - [x] SETF Bind failing case w/ Softmax CSE
;; ~~ TopLevel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun schedule-item-apply-schedule (graph item &key (allow-fission nil) (keep-scop t))
  (declare (type Node item))
  (assert (eql :Affine (node-type item)))
  (let ((kernels (apply-schedule (psi-theta (getattr item :polyhedron)) (getattr item :blueprint))))
    (assert (= 1 (length kernels)) () "schedule-graph-apply-schedule: Cannot schedule multiple kernels for a single affine object at this level.")
    (let* ((singletons
             (loop for r in (node-writes item)
                   if (and (= 0 (length (id->users graph r))) ;; todo:optimize id->users
                           (null (find r (graph-outputs graph))))
                     collect r))
           (kernel
             (if keep-scop
                 (caten/aasm::%simplify-ast
                  (ast-merge-expr-from-aref-subgraph
                   (ast-remove-extra-memloads (car kernels) singletons)))
                 (car kernels)))
           (args (loop for item in (graph-nodes kernel)
                       if (eql (node-type item) :DEFINE-GLOBAL)
                         collect (car (node-writes item)))))
      ;; (caten/codegen/blueprint:print-blueprint kernel t)
      (setf
       (node-writes item) (loop for w in (node-writes item) if (find w args) collect w)
       (node-reads item) (loop for r in (node-reads item) if (find r args) collect r)
       (getattr item :blueprint) kernel
       (getattr item :polyhedron)
       (make-polyhedral-schedule-item
        (getattr item :blueprint) :scal->array allow-fission
                                  :opt-history (psi-opt-history (getattr item :polyhedron))))))
  item)

(defun schedule-graph-apply-schedule (graph &key (allow-fission nil) (keep-scop t))
  "Recompute (out-of-date) blueprint w/ updated schedule."
  (declare (type ScheduleGraph graph))
  ;; [TODO] use lparallel:pdotimes, this can be parallelized.
  (dolist (item (graph-nodes graph))
    (when (eql (node-type item) :Affine)
      (schedule-item-apply-schedule graph item :allow-fission allow-fission :keep-scop t)))
  (verify-graph graph)
  graph)
;; ~~ Fusion Utilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun blueprint-sequence (x y)
  (with-blueprint (:noopt t)
    (dolist (e (graph-nodes x)) (emit e))
    (dolist (e (graph-nodes y)) (emit e))
    (%progn (graph-outputs x) (graph-outputs y))))

(defun merge-affine (a1 a2 new-poly)
  (let ((r (loop for r in (append (node-reads a1) (node-reads a2))
                 if (and (null (find r (node-writes a1))) (null (find r (node-writes a2))))
                   collect r)))
    ($affine (append (node-writes a1) (node-writes a2)) (remove-duplicates r)
             :polyhedron new-poly
             :blueprint (blueprint-sequence (getattr a1 :blueprint) (getattr a2 :blueprint))
             :reduction (or (getattr a1 :reduction) (getattr a2 :reduction)))))

(defun affine/solve-ilp-fusion-pair (parent child &key (mode :full)) ;; assuming parent.order < child.order
  (declare (type Node parent child))
  (assert (eql (node-type parent) :Affine)) (assert (eql (node-type child) :Affine))
  (flet ((m (x) (getattr x :polyhedron)))
    ;; [TODO] childに含まれている各Domainをどこに挿入するか，という問題に変える
    ;; もっと言えば順序付けしない
    ;; AffineにPrintされてる情報でCacheを作成できる
    ;; - [x] 1. Cacheを実装する
    ;; - [ ] 2. 少しHeavyなILPベースでFusionを実施する
    ;; - [ ] 3. ある程度データが集まったら，検索ベースでFusionを実施するアルゴリズムを作る
    ;; Parent/Childは同一RankのTensor操作だと仮定する
    (let ((fused (ILP/SolveProximity (m parent) (m child))))
      (when fused
        (merge-affine parent child fused)))))

(defun affine/solve-ilp-fusion (unfused-ops parent children block &key (mode :full))
  (declare (type Node parent) (type list children))
  (when (not (= 1 (length children)))
    (flet ((p (i) (or (position (node-writes i) unfused-ops :key #'node-writes :test #'intersection) 0)))
      ;; [TODO] Is this sorting method valid? or needed?
      (setf children (sort children #'< :key #'p))))
  (dolist (c children)
    (when (null (find (node-id c) block :key #'node-id))
      (let ((fused (affine/solve-ilp-fusion-pair parent c :mode mode)))
        (when (null fused) (return-from affine/solve-ilp-fusion))
        (setf parent fused))))
  parent)

(defun generate-seed (ops)
  (declare (type list ops))
  (find-if #'(lambda (node) (and (eql (node-type node) :Affine) (getattr node :reduction))) ops))

(defun fuse-successor (unfused-ops graph sp sucs block &key (mode :full))
  (declare (type ScheduleGraph graph) (type list block sucs unfused-ops) (type node sp))
  ;; Only Affine is mergeable
  (when (or (= 0 (length sucs)) (some #'(lambda (x) (eql (node-type x) :NonAffine)) sucs))
    (return-from fuse-successor sp))
  (let ((fused (affine/solve-ilp-fusion unfused-ops sp sucs block :mode mode)))
    ;; Stop when no fusable pairs, or fusion is not beneficial.
    (when (null fused) (return-from fuse-successor sp))
    (dolist (w (node-writes fused)) (remnode graph w))
    (insert-nodes graph (list fused))
    (verify-graph graph)
    (setf block (nconc block sucs))
    (dolist (w (node-writes fused))
      (setf fused (fuse-successor unfused-ops graph fused (id->users graph w) block :mode mode)))
    fused))

(defun schedule-graph-fuse (graph)
  "Solve ILP to minimize proximity while maximizing benefit."
  (declare (type ScheduleGraph graph))
  (let ((unfused-ops (tpsort-graph graph))) ;; explore from leaves to roots.
    ;; Step1. Explore top-down.
    ;; - Maximize Reduce+Reduce Fusion (may change the memory order signifcantly)
    (loop for sp = (generate-seed unfused-ops)
          while sp for block = (list sp) do
            ;; pop first reduction from unfused-ops, and pair them w/ another reduction who lives in descendants.
            ;; Head to successor
            (dolist (w (node-writes sp))
              ;; [todo] optimize id->users which is O(N)
              (fuse-successor unfused-ops graph sp (id->users graph w) block :mode :full))
            ;; Update unfused-ops
            (loop for b in block do
              (setf unfused-ops (remove (node-id b) unfused-ops :key #'node-id))))
    ;; Step2. Fuse predecessors.
    ;; - Fuse remained elwise ops if not required by its children.
    (when nil ;; [TODO] Ascending Orderで探索する(many vs oneじゃないといけない)
    (loop for sp = (pop unfused-ops)
          while sp for block = (list sp)
          if (eql (node-type sp) :Affine) do
            (dolist (w (node-writes sp))
              (fuse-successor unfused-ops graph sp (id->users graph w) block :mode :partial))
            (loop for b in block do
              (setf unfused-ops (remove (node-id b) unfused-ops :key #'node-id))))
    (assert (null unfused-ops)))
    ;; [todo] this will break graph
    (schedule-graph-apply-schedule graph :allow-fission nil) ;; [TODO] is it slow? 
    graph))

;; [TODO] Runtime is a subclass of FastGraph
;; [TODO] Introduce LocalGensym
;; - CostModelを切り替えて，OfflineでBEAM Search, OnlineでBEAM Search, 両方可能にする
;; - LLM => BatchSizeをIterateしてBEAM Search...
;; - remove marks
;; [TODO] Reimplement api
;; - Node is always singleton (Cache)
;; - Faster compilation time
;; [TODO]
;; - BEAM Search: ScheduleGraphの状態のまま解く
;; - Symbolicも最適化できるようにする
;; - Nonaffineも普通に実行すればいい
;; - TensorID -> (cons speed kernel) mitaini cache sitai
;; - symbolic graph fusion?
;; - quasi affine?
;; - AccessMapさえ作れればいい。gidの係数ごとにlexiographical order?
;;   - w/ assuming each coefficient is constant.
;;   - 次元数も関係ない。
;;   | M*N | M | 1 |
;;   --------------|
;; S |  0  | 0 | 1 | = index
;; Affine/NonAffineのまま動かすために
;;
(defmethod realize-node ((node-type (eql :NonAffine)) runtime node args)
  (flet ((v (id) (if (symbolp id) (runtime-getvar runtime id) id)))
    (dolist (item (getattr node :items))
      (loop for w in (node-writes node)
            for v in (multiple-value-list (realize-node (node-type item) runtime item (map 'list #'v (node-reads item))))
            do (runtime-setvar runtime w v)))
    (apply #'values (map 'list #'v (node-writes node)))))

(defmethod realize-node ((node-type (eql :Affine)) runtime node args)
  (ILP/Search node) ;; :cost-model (make-instance runtime args)
  (error "STOP"))

(defun schedule-graph-search (graph)
  "BEAM Search for Affine Schedule Items"
  (declare (type ScheduleGraph graph))
  (let ((runtime
          (make-runtime
           (make-graph)
           :runtime (caten/codegen/byoc:get-runtime-type)
           :buffer-type (caten/codegen/byoc:get-buffer-type))))
    (flet ((v (id) (if (symbolp id) (runtime-getvar runtime id) id)))
      (dolist (item (tpsort-graph graph))
        (ecase (node-type item)
          ((:Affine :NonAffine)
           (loop for w in (node-writes item)
                 for v in (multiple-value-list (realize-node (node-type item) runtime item (map 'list #'v (node-reads item))))
                 do (runtime-setvar runtime w v)))))
      (free-runtime runtime))))

(defun schedule-graph-solve-memory-planner (graph)
  "Solve ILP to minimize the number of temporary buffer allocation."
  (declare (type ScheduleGraph graph))
  ;; Default:
  ;; - (*) write_id = get_from_memory_pool(id)
  ;; id is subject to optimize.
  )

(defun schedule-graph-finalize (graph)
  "Finalize ScheduleGraph+Construct a runtime graph."
  (declare (type ScheduleGraph graph))
  (dolist (item (tpsort-graph graph))
    (when (eql (node-type item) :Affine)
;;      (print (isl:schedule-get-root (psi-theta (getattr item :polyhedron))))
;      (print (caten/codegen/dataflow::make-dataflow-graph (psi-theta (getattr item :polyhedron))
;                                                          (psi-read-union-map (getattr item :polyhedron))
;                                                          (psi-write-union-map (getattr item :polyhedron))))
      ;(print (psi-read-union-map  (getattr item :polyhedron)))
      ;(print (psi-write-union-map  (getattr item :polyhedron)))
      (caten/codegen/blueprint:print-blueprint (getattr item :blueprint) t)
      )))
;; Memo:
;; - [ ] ISL = Schedule and Memory Access Separation
;; - [ ] DataFlowGraph = MemoryAccessRelation+Schedule
;; - [ ] ScheduleGraph <==> DataFlowGraph Constructionを実装する
;;   - [ ] いや，BANDもしかしてBandがDEFINE-GLOBALなどに対応するのでは？
;;   - [ ] ScheduleNodeBand: LV(BAND_DEPTH), AREA=512x512
;; - [ ] DataFlowGraphから，FusionのStrategyを生成する方法の提案をしたい
;; - [ ] やることを一般化し範囲を絞って言語化すると，サイズが大きいが速度が遅いN段階のメモリがある。これがブラックボックスだとして，自動でマッピングするモデルの構築
;; - [ ] あと2週間あれば完成しそうな目処がたった！
;; - [ ] Schedule作り直し+差し替え+Refactor (2 or 3 days)
;;   - [ ] RuntimeGraph, TensorGraph, ...
;; - [ ] caten/api作り直す, reimpl autodiff (maybe 2 or 3 days)
;; - [ ] BEAM Search/BYOC作り直す           (2 days)
;; - [ ] テスト作り直す                     (2 days)

;; [TODO]
;; - [ ] Fusionをもう少し賢く実施したい。
;; - [ ] Memory Access Map => 依存違反で使うのではなく，最初からこれベースでもっと賢く実施
;; - [ ] ReSCoP, CSEを最初と最後の一回のみにしたい。
;; - [ ] UnionMapとScheduleからどこに融合するかって一発で作れそうなもんに見える
;; - [ ] DataFlowGraphを作成
;; - [ ] DEFINE-GLOBALを経由した数，VRAM <-> SRAMの読み込みなどを表現, これをCost Functionにする？
;; - [ ] 各変数について，iPadに記したみたいなDataFlowGraphを作成する
;;  - [ ] Visualize!
;; - [ ] うまくいけば途中でCSEせずにFusion続行？
;; - [ ] Read/WriteUnionMapのアクセス ==> Sort(UnionMap)ができる性質があればOK
;; - [ ] ScheduleTreeとDataFlowGraphだけで判定して軽量化...はできる
;; - [ ] ScheduleTree/Read/WriteUMap ==> DataFlowGraph
;; - [ ] Simplify(DataFlowGraph)
;;  - [ ] TILEしたらL1(DEFINE-GLOBAL) -> L2(more fater but small mem) -> L3(more ...) を明示的に作る？
;; (***) ManySchedule vs OneFilterScheduleで，FusionはOneFilterScheduleを適切な場所でInsertする操作だと考える。
;;  - [ ] i.e.: SCC同士でFusionをする。Oneの方のPlaceableな地点のリストを列挙, あるいはManyの方に何らかの操作をして
;;   - こうすればReSCOPifyが必要なくなる！！
;;  - [ ] TILEすると，AREF(GLOBAL_MEM_VAL, idx)が，AREF(GLOBAL_MEM_VAL, 0 <= idx <= TILE_SIZE)になる。
;;   - 0 <= idx-parent_loop_idx <= 0+TILE_SIZE
;;  - [ ] DataFlowGraphを実装する。これはSchedule, AST, どっちに対して実装するといい？
;;    - [ ] => 多分ScheduleTree, ただしScheduleTreeをもう少し理解しないといけない。。。
;; [DataFlowGraph]
;; - [ ] ScheduleNodeBand, 各Bandの深さ=メモリ階層のLVL
;; - [ ] ASTUserがどこに常に生成されるのか確認しないといけない。
;; - [ ] Fusion実装できたら，codegenの必要ないコード全部消す。
;; - [ ] caten aasm -> caten/ir
;; - [ ] ScheduleTree ==> DataFlowGraphを作成する。
;;   - [ ] これはTile探索のVislizeも兼ねる
;; ↓これがFusionできないといけない。
;; - (fconst 'a)
;; - (!add gemm gemm)
;; (let ((tg (tensor-lowered-graph (!sin (!t (!relu (!matmul (make-tensor `(1024 1024)) (make-tensor `(1024 1024)))))))))
;;              (time (caten/codegen/lowerer::codegen tg)))
(defun schedule-item-to-optrules ()

  )
;; Visualizeが大事？
;; [TODO]
;; - [ ] codegen再実装をやっちゃおう
;; - [ ] make-schedule-graph再実装が終わったら，
;; - [ ] specs, aasm, codegen周りの大掃除やる
;; - [ ] renderer, etcに使ってない関数多すぎ
;; - [ ] 一回のScheduleGraphで使われたStride全てでBroadcastを実施する
;; - [ ] Fusionについて，途中のCSEが必要なくなるのでは？
;; - [ ] Polyhedral: SCOP
;; - [ ] IDを見やすくするRewriting Ruleを導入する。
;;   - [ ] N Load => N_1とかにする
;;   - [ ] Fix DB
;; - [ ] Start w/ Simple Fusion Rule
(defun codegen (graph)
  (declare (type Graph graph))
  (let ((sched (make-schedule-graph graph)))
    (schedule-graph-fuse sched) ; Minimize Proximity
    (schedule-graph-solve-memory-planner sched)
    (schedule-graph-finalize sched)
    sched))
