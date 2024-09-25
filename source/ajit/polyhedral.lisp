(in-package :caten/ajit)

(defstruct (Polyhedral
	    (:conc-name poly-)
	    (:constructor make-polyhedral (avm pipeline domain read write initial-schedule vm-inputs vm-outputs lex-table &key (ast-option :atomic))))
  (avm avm :type avm)
  (vm-inputs vm-inputs :type list)
  (vm-outputs vm-outputs :type list)
  (vm-io-types (infer-vm-io-types avm `(,@vm-inputs ,@vm-outputs)) :type hash-table)
  (pipeline pipeline :type hash-table)
  ;; constraints
  (domain domain :type string)
  (domain-ptr (union-set-from-str domain) :type union-set)
  (read read :type string)
  (read-ptr (union-map-from-str read) :type union-map)
  (write write :type string)
  (write-ptr (union-map-from-str write) :type union-map)
  (initial-schedule initial-schedule :type union-map)
  (schedule nil :type (or null Schedule))
  (lex-table lex-table :type hash-table)
  (ast-option ast-option :type (member :separate :atomic)))

(defun poly/io-scalar-p (poly x)
  (let ((type (gethash x (poly-vm-io-types poly))))
    (when (null type) (error "~a is not input/output" type))
    (= (buffer-nrank (car (relay-writes type))) 0)))

(defun poly/schedule-metadata (polyhedral)
  (declare (type polyhedral polyhedral))
  (when (poly-schedule polyhedral)
    (yaml:parse (schedule-to-str (poly-schedule polyhedral)))))

(defstruct (Band)
  (domain (error "") :type union-set)
  (permutable nil :type boolean)
  (coincident nil :type list))

(defun collect-bandnode (top &aux (out) (depth 0))
  (declare (type polyhedral top))
  (labels ((explore (schedule-node)
	     ;; isl_bool isl_schedule_node_has_children(__isl_keep isl_schedule_node *node);
	     (let ((c (isl::%isl-schedule-node-has-children (isl::schedule-node-handle schedule-node))))
	       (when (eql c :bool-true)
		 ;;isl_size isl_schedule_node_n_children(__isl_keep isl_schedule_node *node);
		 (loop for n upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle schedule-node))
		       for node = (schedule-node-get-child schedule-node n)
		       do (explore node)))
	       (ecase (isl::%isl-schedule-node-get-type (isl::schedule-node-handle schedule-node))
		 (:Schedule-Node-Leaf)
		 (:Schedule-Node-Filter)
		 (:Schedule-Node-Sequence)
		 (:Schedule-Node-Band
		  (let ((dom (schedule-node-get-domain schedule-node))
			(n (isl::%isl-schedule-node-band-n-member (isl::schedule-node-handle schedule-node)))
			(shuffle-p (eql :bool-true (isl::%isl-schedule-node-band-get-permutable (isl::schedule-node-handle schedule-node)))))
		    (incf depth)
		    (push
		     (make-band
		      :domain dom
		      :permutable shuffle-p
		      :coincident
		      (loop for i upfrom 0 below n
			    collect (eql :bool-true (isl::%isl-schedule-node-band-member-get-coincident (isl::schedule-node-handle schedule-node) i))))
		     out)))
		 (:Schedule-Node-Domain)
		 (:Schedule-Node-Expansion)
		 (:Schedule-Node-Extension)
		 (:Schedule-Node-Mark)
		 (:Schedule-Node-Set)
		 (:Schedule-Node-Context)
		 (:Schedule-Node-Guard)))))
    (explore (schedule-get-root (poly-schedule top)))
    (values out depth)))

(defun finalize-polyhedral (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (declare (type polyhedral polyhedral))
  (macrolet ((set-option (name level)
	       `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (schedule-set-options schedule (poly-ast-option polyhedral)))
	 (bands (multiple-value-list (collect-bandnode polyhedral)))
	 ;; [TODO] Better way to determine the depth (currently, 2 x {band_count})
	 (depth (* 2 (second bands)))
	 (bands (car bands))
	 (ast-build (ast-build-from-context (set-from-str "{:}")))
	 (ast-build (ast-build-set-iterators ast-build (apply #'make-id-list (map 'list #'gid (range 0 (1+ depth))))))
	 (ast-build-node (ast-build-node-from-schedule ast-build schedule)))
    (values ast-build-node bands)))

(defmethod print-polyhedral ((poly Polyhedral) stream)
  (format stream "
= [Polyhedral] ========================================================
Domain:
~a
Read:
~a
Write:
~a
Schedule:
~a
Expected Output (Scalar ops are temporarily excluded):
~a
======================================================================"
	  (poly-domain poly)
	  (poly-read poly)
	  (poly-write poly)
	  (when (poly-schedule poly)
	    (schedule-get-root (schedule-set-options (poly-schedule poly) (poly-ast-option poly))))
	  (when (poly-schedule poly)
	    (debug/render-c poly))))

(defun debug/render-c (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (let* ((schedule (schedule-set-options schedule (poly-ast-option polyhedral)))
	 (build (ast-build-from-context (set-from-str "{:}")))
	 (ast   (ast-build-node-from-schedule build schedule))
	 (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
	 (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
	 (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
	 (str   (isl::%isl-printer-get-str q)))
    str))
3
(defun create-dependency-graph (polyhedral)
  (with-slots ((domain domain-ptr) (initial-schedule initial-schedule) (read-access read-ptr) (write-access write-ptr)) polyhedral
    ;; References https://github.com/zhen8838/isl_learn/blob/main/12_schedule_program.ipynb
    (let* ((raw (union-map-intersect
		 (union-map-apply-range
		  write-access
		  (union-map-reverse read-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	   (war (union-map-intersect
		 (union-map-apply-range
		  read-access
		  (union-map-reverse write-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	   (waw (union-map-intersect
		 (union-map-apply-range
		  write-access
		  (union-map-reverse write-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule))))
      (values raw waw war))))

(defun poly/make-constraints (polyhedral)
  "(2) Validty/Legality Constraints"
  (declare (type polyhedral polyhedral))
  (with-slots ((domain domain-ptr)) polyhedral
    (multiple-value-bind (raw-deps waw-deps war-deps)
	(create-dependency-graph polyhedral)
      (let* ((all-deps (union-map-union waw-deps war-deps))
	     (all-deps (union-map-union all-deps raw-deps))
	     (schedule-constraints (schedule-constraints-on-domain domain))
	     (schedule-constraints (schedule-constraints-set-coincidence schedule-constraints all-deps))
	     (schedule-constraints (schedule-constraints-set-validity schedule-constraints all-deps))
	     ;; proximity constraints (keeps loops nested based on dependencies)
	     (schedule-constraints (schedule-constraints-set-proximity schedule-constraints all-deps)))
	schedule-constraints))))

(defun poly/schedule (polyhedral &key (serialize nil))
  "
[Scheduler]
This function analyzes the read/write dependencies on the polyhedron space,
trying to apply the operator fusion as many as possible
```
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T3(c0, c1, 0);
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    for (int c2 = 0; c2 < b; c2 += 1)
      T4(c0, c1, c2);
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T5(c0, c1);
=>    
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T3(c0, c1, 0);
    for (int c2 = 0; c2 < b; c2 += 1)
      T4(c0, c1, c2);
    T5(c0, c1);
```
"
  (declare (type polyhedral polyhedral))
  (macrolet ((set-option (name level)
	       `(progn
		  (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				   :pointer (isl::context-handle isl::*context*)
				   :int ,level
				   :void))))
    (when serialize (set-option "schedule_serialize_sccs" 1))
    (let ((n 0))
      (loop for g in (hash-table-values (poly-pipeline polyhedral))
	    do (incf n (length (graph-nodes g))))
      (set-option "schedule_outer_coincidence" 1)
      ;;(set-option "schedule_maximize_band_depth" 1)
      (set-option "schedule_treat_coalescing" 1)
      ))
  (with-slots ((domain-ptr domain-ptr) (read-ptr read-ptr) (write-ptr write-ptr)) polyhedral
    (let* ((constraints (poly/make-constraints polyhedral))
	   (schedule (schedule-constraints-compute-schedule constraints)))
      (setf (poly-schedule polyhedral) schedule)
      polyhedral)))
