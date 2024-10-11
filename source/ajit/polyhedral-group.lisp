(in-package :caten/ajit)

;; Implements Reconfigurable Polyhedral Compiler
;; https://arxiv.org/abs/2401.06665

;; Before PG -> Optimizations are common across architectures
;; PG -> Architecture specific optimizations

;; Takes Scheduled Group as an input
;; -> Polyhedral
;; Optimized Group
;; [TODO]
;; - [Here] Tiling, Loop Collapse, Vectorize
;; - Fix Transformer Scheduling
;; - ISeq Lowring is slow
;; - Transpose+Matmul Fusion
;; - Deprecate ./polyhedral.lisp and existing polyhedral compiler, replace with this!!!
;;   - If they can fuse the Embedding < 1 w/o using old compiler, then it's a success and replace!
;; WIP
(defclass Polyhedral-Group ()
  ((base :initarg :base :type Group :accessor polyhedral-group-base)
   (kr   :initarg :kr :type Kernel-Renderer :accessor polyhedral-kr))
  (:documentation "
A Polyhedral form of the fused schedule group.
"))

(defclass Polyhedral-Auto-Scheduler (Polyhedral-Group)
  ((schedule :accessor pg-schedule)
   (domain   :accessor pg-domain)
   (dependencies :accessor pg-dependencies))
  (:documentation "groups which is subject to jit"))

(defmethod pprint-schedule ((schedule schedule))
  (let ((schedule (yaml:parse (schedule-to-str schedule))))
    (with-output-to-string (out)
      (format out "~%")
      (labels ((indent (n)
                 (make-string n :initial-element #\space))
               (separate-screen (indent &key (n 120))
                 (format out "~%~a~a~%" (indent indent) (make-string n :initial-element #\-)))
               (explore (schedule key &key (indent 0))
                 (cond
                   ((string= key "domain")
                    (format out "~adomain(~%" (indent indent))
                    (let ((domains (cl-ppcre:split
                                    ";"
                                    (cl-ppcre:regex-replace-all
                                     "{|}"
                                     (gethash key schedule)
                                     ""))))
                      (format out "~a"
                              (apply
                               #'concatenate
                               'string
                               (butlast
                                (loop for dom in domains
                                      collect (format nil "~a~a" (indent (+ indent 2)) dom)
                                      collect (format nil "~%"))))))
                    (format out "~a)" (indent indent)))
                   ((string= key "child")
                    (format out "~%~achild()" (indent indent))
                    (separate-screen indent)
                    (mapc
                     #'(lambda (x)
                         (explore (gethash key schedule) x :indent (+ indent 2)))
                     (reverse (hash-table-keys (gethash key schedule)))))
                   ((string= key "schedule")
                    (let ((schedules (cl-ppcre:split
                                      " , "
                                      (cl-ppcre:regex-replace-all
                                       "{|}"
                                       (subseq (gethash key schedule) 1 (1- (length (gethash key schedule))))
                                       ""))))
                      (format out "~aschedule()" (indent indent))
                      (when schedules (format out "~%"))
                      (format out "~a"
                              (apply
                               #'concatenate
                               'string
                               (butlast
                                (loop for s in schedules
                                      for nth upfrom 0
                                      for separator = (if (zerop nth) "┏" (if (= (length schedules) (1+ nth)) "┗" "┃"))
                                      collect (format nil "~a  ~a~a" (indent indent) separator s)
                                      collect (format nil "~%")))))))
                   ((or (string= key "sequence") (string= key "set"))
                    (format out "~a~a()" (indent indent) key)
                    (mapc
                     #'(lambda (x)
                         (mapc
                          #'(lambda (k)
                              (explore x k :indent (+ 2 indent)))
                          (hash-table-keys x)))
                     (gethash key schedule)))
                   ((string= key "filter")
                    (format out "~%~afilter(~%" (indent indent))
                    (let ((domains (cl-ppcre:split
                                    ";"
                                    (cl-ppcre:regex-replace-all
                                     "{|}"
                                     (gethash key schedule)
                                     ""))))
                      (format
                       out
                       "~a"
                       (apply
                        #'concatenate
                        'string
                        (butlast
                         (loop for dom in domains
                           collect (format nil "~a~a" (indent (+ indent 2)) dom)
                           collect (format nil "~%")))))
                      (format out ")")))
                   ((or (string= key "permutable") (string= key "coincident"))
                    (format out "~%~a~a(~a)" (indent indent) key (gethash key schedule)))                 
                   (t (warn "pprint: the key ~a is not implemented." key)))))
        (mapc #'(lambda (x) (explore schedule x)) (reverse (hash-table-keys schedule)))))))

(defmethod initialize-instance :after ((pg Polyhedral-Auto-Scheduler) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (domain read write schedule) (scop (polyhedral-group-base pg) (polyhedral-kr pg))
    (setf (pg-schedule pg) schedule
          (pg-domain pg) domain)
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
           (dependencies
             (union-map-union
              (union-map-union WaR RaW)
              WaW)))
      (setf (pg-dependencies pg) dependencies)))
  (setf (pg-schedule pg) (schedule pg)))

(defmethod schedule ((pg Polyhedral-Auto-Scheduler))
  (let ((serialize-sccs 0)
        (outer-coincidence 0)
        (maximize-coincidence 1)
        (treat-coalescing 1)
        (maximize-band-depth 1)
        ;; Only schedule the scc. (not to change the structure of kernel)
        (schedule-whole-component 0))
    (macrolet ((set-option (name level)
	         `(progn
		    (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				     :pointer (isl::context-handle isl::*context*)
				     :int ,level
				     :void))))
      (flet ((configure ()
               (set-option "schedule_serialize_sccs" serialize-sccs)
               (set-option "schedule_outer_coincidence" outer-coincidence)
               (set-option "schedule_maximize_coincidence" maximize-coincidence)
               (set-option "schedule_treat_coalescing" treat-coalescing)
               (set-option "schedule_maximize_band_depth" maximize-band-depth)
               (set-option "schedule_whole_component" schedule-whole-component)))
        (configure))))
  ;; [TODO] Find best configuration for it
  ;; Finding optimal solution for both
  ;; - (!add (!add (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))
  ;;                     (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))))
  ;; - Embedding
  ;; [TODO] Create Reconfigurable Polyhedral Compiler
  (schedule-constraints-compute-schedule
   (schedule-constraints-set-coincidence
    (schedule-constraints-set-proximity
     (schedule-constraints-set-validity
      (schedule-constraints-on-domain (pg-domain pg))
      (pg-dependencies pg))
     (pg-dependencies pg))
    (pg-dependencies pg))))

;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/scop.cc#L47
;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/schedule_isl_conversion.cc
(defmethod render-domain-body-from-group ((group Group) (kr kernel-renderer) &aux (idx2domain (make-hash-table)))
  (values
   (with-output-to-string (out)
     (format out "{~%")
     (loop with domains = nil
           with conditions = nil
           for node in (kernel-renderer-nodes kr) do
             (ecase (node-type node)
               (:FOR
                (let ((by (getattr node :by)))
                  (assert (and (eql (expr-op by) :Const) (eql 1 (expr-x by)))
                          ()
                          "The group is not a scop. (TODO: Fix)"))
                (push node domains))
               (:ENDFOR (setf domains (remove (getattr node :idx) domains :key #'(lambda (x) (getattr x :idx)) :test #'equalp)))
               (:FUNCALL
                (let ((domains (reverse domains))
                      (isl     (default-device :isl-expr)))
                  (setf (gethash (getattr node :idx) idx2domain)
                        (if domains
                            (format nil "T~a[~(~a~)]" (getattr node :idx) (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains)))
                            (format nil "T~a[]" (getattr node :idx))))
                  (if domains
                      (format out "  T~a[~(~a~)] : ~(~a~);~%"
                              (getattr node :idx)
                              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
                              (render-expr
                               isl
                               (reduce
                                #'(lambda (x y) (make-expr :AND x y))
                                (append
                                 (loop for dom in domains
                                       collect (make-expr :>= (make-const (getattr dom :idx) nil) (getattr dom :upfrom))
                                       collect (getattr dom :below))
                                 (loop for ds in (poly-dynamic-shapes (group-polyhedron group))
                                       collect (make-expr :>= (make-const ds nil) (make-const 1 nil)))
                                 conditions))))
                      (if conditions
                          (format out "  T~a[] : ~a~%" (getattr node :idx) (render-expr isl (reduce #'(lambda (x y) (make-expr :AND x y)) conditions)))
                          (format out "  T~a[];~%" (getattr node :idx))))))
               (:IF
                (push (getattr node :condition) conditions))
               (:ELSE
                (error "ELSE IS NOT IMPLEMENTED"))
               (:ENDIF
                (assert conditions () ":ENDIF without :IF")
                (pop conditions))))
     (format out "}"))
   idx2domain))

(defun render-domain-from-loops (node domains &aux (isl (default-device :isl-expr)))
  (if domains
      (format nil "  T~a[~(~a~)] : ~(~a~);~%"
              (getattr node :idx)
              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
              (render-expr
               isl
               (reduce
                #'(lambda (x y) (make-expr :AND x y))
                (loop for dom in domains
                      collect (make-expr :>= (make-const (getattr dom :idx) nil) (getattr dom :upfrom))
                      collect (getattr dom :below)))))
      (format nil "  T~a[];~%" (getattr node :idx))))

(defmethod render-band-node-in-domain ((group group) area related-domains idx2domain)
  "Creates a band node(mupa):
```
for (i=0; i<10; i++)
  for (j=0; j<10; j++)
    S1[i, j] // If you were here:
    for (k=0; k<10; k++)
      S[i, j, k]
```
-> then the partial schedule is:
```
[] -> {
  S1[i, j] -> [i, j];
  S2[i, j, k] -> [i, j];
}
```
Corresponds to the position of the subgraph in the parent schedule.
"
  (let ((related-functions
          (loop for node in area
                if (eql (node-type node) :FUNCALL)
                  collect node))
        (declared-ids (map 'list #'(lambda (x) (getattr x :idx)) related-domains)))
    (when (null declared-ids) (return-from render-band-node-in-domain "[]"))
    (with-output-to-string (out)
      (format out "[~%")
      (loop for idx in declared-ids
            for nth upfrom 0
            if (not (= nth 0))
              do (format out ",~%")
            do (format out "~% { ")
               (loop for funcall in related-functions
                     for nth upfrom 0
                     if (not (= nth 0))
                       do (format out "; ")
                     do (format out "~a -> [~a]"
                                (or (gethash (getattr funcall :idx) idx2domain) (error ""))
                                idx))
               (format out " } "))
      (format out "~%]"))))

(defun render-access-rep (reader type-reader group idx2domain kr)
  (union-map-from-str
   (with-output-to-string (out)
     (format out "[~(~a~)] -> {~%" (render-list (poly-dynamic-shapes (group-polyhedron group))))
     (maphash
      #'(lambda (idx dom)
          (when (find idx kr :key #'(lambda (x) (and (eql (node-type x) :FUNCALL) (getattr x :idx))))
            (let ((graph-in-funcall (gethash idx (poly-pipeline (group-polyhedron group)))))
              (assert graph-in-funcall)
              (dolist (node (graph-nodes graph-in-funcall))
                (loop for var in (funcall reader node)
                      for typ in (funcall type-reader (read-type-relay node))
                      do (format out "  ~a -> ~(~a~)[~(~a~)];~%"
                                 dom
                                 var
                                 (render-isl-aref typ :indexing #'isl-access-expr-no-stride :mutate-scalar t :flatten t :use-permute nil)))))))
      idx2domain)
     (format out "}"))))

(defmethod scop ((group group) (kr kernel-renderer))
  "Formulates the Polyhedral Model from scop/
Reference: https://www.researchgate.net/publication/347152973_PET-to-MLIR_A_polyhedral_front-end_for_MLIR

1. Schedule Construction
```2
     [DOMAIN_NODE]
           |
      [BAND_NODE] (= corresponds to a loop in short)
           |
     [SEQUENCE_NODE]
       /       \
  [FILTER]  [FILTER] (= corresponds to an operation in short)
      |         |
        ...
```
"
  (let* ((deps (render-list (poly-dynamic-shapes (group-polyhedron group))))
         (render-nodes (kernel-renderer-nodes kr)))
    (multiple-value-bind (domain idx2domain) (render-domain-body-from-group group kr)
      (labels ((explore-schedule-tree (from to
                                       &key
                                         (parent-loops)
                                       &aux
                                         (schedule :nothing)
                                         (region (subseq render-nodes from to)))
                 (loop with count = from while (< count to)
                       for node = (nth count render-nodes) do
                         (ecase (node-type node)
                           (:FUNCALL
                            ;; -> Filter Node
                            (let* ((inst-schedule
                                     (schedule-from-domain
                                      (union-set-from-str
                                       (format nil "[~(~a~)] -> { ~a }" deps (render-domain-from-loops node parent-loops))))))
                              (setf
                               schedule
                               (if (eql schedule :nothing)
                                   inst-schedule
                                   (schedule-sequence schedule inst-schedule)))
                              (incf count)))
                           (:FOR
                            ;; -> Band Node
                            (let* ((endfor
                                     (find (getattr node :idx) (nthcdr count render-nodes)
                                           :key #'(lambda (x) (and (eql (node-type x) :ENDFOR) (getattr x :idx)))
                                           :test #'equalp))
                                   (_ (when (null endfor) (error "scop: malformed rendering graph ~a" render-nodes)))
                                   (endfor-abs-position
                                     (position
                                      (node-id endfor)
                                      render-nodes
                                      :key #'node-id)))
                              (declare (ignore _))
                              (assert (>= endfor-abs-position count))
                              ;; for (...) {
                              ;;  T0[]       }
                              ;;  ...        } dom-schedule = schedule of this area
                              ;; }
                              (let ((dom-schedule
                                      (explore-schedule-tree
                                       (1+ count) endfor-abs-position
                                       :parent-loops `(,@parent-loops ,node))))
                                (setf
                                 schedule
                                 (if (eql schedule :nothing)
                                     dom-schedule
                                     (schedule-sequence schedule dom-schedule)))
                                (setf count endfor-abs-position)
                                ;; Move next to endfor
                                (incf count))))
                           (:IF
                            ;; -> Conditioned body
                            (let* ((endif (loop with if-count = 0 ;; Finding corresponding :ENDIF (pay attention for nested IF)
                                                named search-for-id
                                                for node in (nthcdr count render-nodes)
                                                if (eql (node-type node) :IF) do (incf if-count)
                                                else if (eql (node-type node) :ENDIF) do (decf if-count)
                                                if (= if-count 0) do (return-from search-for-id node)))
                                   (_ (when (null endif) (error "scop: malformed rendering graph when making a pair of :IF and :ENDIF:~%~a" render-nodes)))
                                   (endif-pos-abs (position (node-id endif) render-nodes :key #'node-id)))
                              (declare (ignore _))
                              (assert (>= endif-pos-abs count))
                              ;; Condition belongs to the domain, let skip it
                              (let ((if-schedule
                                      (explore-schedule-tree
                                       (1+ count) endif-pos-abs
                                       :parent-loops parent-loops)))
                                (setf
                                 schedule
                                 (if (eql schedule :nothing)
                                     if-schedule
                                     (schedule-sequence schedule if-schedule))))
                              (setf count endif-pos-abs)
                              (incf count)))))
                 (when (eql schedule :nothing) (error "nothing was scheduled?"))
                 (let* ((partial-schedule
                          ;; (last parent-loops) or just parent-loops?
                          (render-band-node-in-domain group region (last parent-loops) idx2domain)))
                   (when (not (string= "[]" partial-schedule))
                     (setf schedule (schedule-insert-partial-schedule schedule (multi-union-pw-aff-from-str partial-schedule)))))
                 schedule))
        (values
         (union-set-from-str
          (format
           nil
           "[~(~a~)] -> ~a"
           (render-list (poly-dynamic-shapes (group-polyhedron group))) domain))
         (render-access-rep #'node-reads #'relay-reads group idx2domain (kernel-renderer-nodes kr))
         (render-access-rep #'node-writes #'relay-writes group idx2domain (kernel-renderer-nodes kr))
         (explore-schedule-tree 0 (length render-nodes)))))))
;; ~~ Creation/Conversion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod group->polyhedral-group ((group Group) (kr kernel-renderer))
  (unless (group-realize-on-vm group)
    (assert (null (find :ELSE (kernel-renderer-nodes kr) :key #'node-type))
            ()
            "ELSE is not allowed in the Polyhedral Model."))
  (make-instance
   (if (or
        (group-realize-on-vm group)
        (null (find :FOR (kernel-renderer-nodes kr) :key #'node-type)))
       'Polyhedral-Group
       'Polyhedral-Auto-Scheduler)
   :base group
   :kr kr))

(defmethod polyhedral-group->group ((polyhedral-group Polyhedral-Group))
  ;; [TODO] Ignores for ElementWise Kernels e.g.
  ;; the goal is to apply the heavy and hardware-specific optimization
  ;; like complicated kernels, (gemm, convnd)
  ;; these parameters are reconfigurable by providing the Polyhedral-Config
  (polyhedral-group-base polyhedral-group))
;; ~~ Scheduling Language ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Polyhedral-Config ()
  ((parallelizable-loops))
  (:documentation ""))
;; [Design] Only effects on Render-Graph and Pipelining
;; - Matmul+TransposeをFusionしないといけない
;; - Loop Fusionと別で物事を考えた方がいい
;; - Step1
;;   - Matmul+Transpose, ConvND Fusion in transform.lisp (Supported by ISL)
;; - Step2
;;   - Proper Kernel Partition for larger graph like Transforer
;; - Step3
;;   - Apply Loop Collapse/Tile to the final kernel
;; - そうすれば ConvND < 1 Kernelsができるはず
;; - Assume ^がPrepreq, Embedding/Gemm, Tile, Loop Collapse, Vectorize

(defmethod polyhedral-auto-schedule ((pg Polyhedral-Auto-Scheduler))
  ;; [TODO] TileOuterBand
  ;; Working in progress...
  )

(defmethod polyhedral-auto-schedule ((pg Polyhedral-Group)))

(defmethod loop-reorder ((pg Polyhedral-Auto-Scheduler) order)
  
  )

(defmethod loop-interchange ((pg Polyhedral-Auto-Scheduler) axis)
  
  )

(defmethod tile-bands ((polyhedral-group Polyhedral-Auto-Scheduler) config)
  "
For example, consider the following loop:
```
for (int i=0; i<M; i++)
  for (int j=0; j<N; j++)
    T0[i, j]
```
=> After tile-bands
```
for (int ii=0; ii<M; ii+=ITILE)
  for (int ji=0; jj<N; jj+=JTILE)
    for (int ii=0; ii<min(M, ii+ITILE); ii++)
      for (int jj=0; jj<min(N, jj+JTILE); jj++)
        T0[i+ii, j+jj];
```
"
  
  )

(defmethod loop-permute ((pg Polyhedral-Auto-Scheduler) config)
  
  )

(defmethod unroll-bands ((polyhedral-group Polyhedral-Auto-Scheduler) unroll-factors)
  
  )

(defmethod build ((pg Polyhedral-Auto-Scheduler))
  (let* ((schedule (schedule-set-options (copy (pg-schedule pg)) :atomic))
         (build (ast-build-from-context (set-from-str "{:}")))
         (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (ast-build-node-from-schedule build schedule))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))
