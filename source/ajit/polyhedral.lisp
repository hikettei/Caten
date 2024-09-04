(in-package :caten/ajit)
;; This paper is good to read first:
;; - https://arxiv.org/pdf/2401.06665
;; - https://www.researchgate.net/publication/320992060_Consecutivity_in_the_isl_Polyhedral_Scheduler
;; TODO: Making all isl pointers gc-reachable
;; TODO: Symbolic Model Scheduling
(defstruct (Polyhedral
	    (:conc-name poly-)
	    (:constructor make-polyhedral (avm pipeline domain read write initial-schedule vm-inputs vm-outputs lex-table)))
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
  (lex-table lex-table :type hash-table))

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

(defun collect-bandnode (top &aux (out))
  (declare (type polyhedral top))
  (labels ((explore (schedule-node)
	     (let ((c (isl::%isl-schedule-node-has-children schedule-node)))
	       (when (eql c :bool-true)
		 (loop for n upfrom 0 below (isl::%isl-schedule-node-n-children schedule-node)
		       for node = (isl::%isl-schedule-node-get-child schedule-node n)
		       do (explore node)))
	       (ecase (isl::%isl-schedule-node-get-type schedule-node)
		 (:Schedule-Node-Leaf)
		 (:Schedule-Node-Filter)
		 (:Schedule-Node-Sequence)
		 (:Schedule-Node-Band
		  (let ((dom (isl::%make-union-set (isl::%isl-schedule-node-get-domain schedule-node)))
			(n (isl::%isl-schedule-node-band-n-member schedule-node))
			(shuffle-p (eql :bool-true (isl::%isl-schedule-node-band-get-permutable schedule-node))))
		    ;;(print (isl::%make-union-set (isl::%isl-schedule-node-band-get-ast-build-options schedule-node)))
		    ;;(print dom)
		    ;;(print (isl::%make-schedule-node (isl::%isl-schedule-node-band-set-ast-build-options schedule-node (isl::union-set-handle (union-set-from-str "[_gid0, _gid1] -> { atomic[t] : 0 <= t <= 2 }")))))
		    ;;(print (isl::%make-union-set (isl::%isl-schedule-node-band-get-ast-build-options schedule-node)))
		    (push
		     (make-band
		      :domain dom
		      :permutable shuffle-p
		      :coincident
		      (loop for i upfrom 0 below n
			    collect (eql :bool-true (isl::%isl-schedule-node-band-member-get-coincident schedule-node i))))
		     out)))
		 (:Schedule-Node-Domain)
		 (:Schedule-Node-Expansion)
		 (:Schedule-Node-Extension)
		 (:Schedule-Node-Mark)
		 (:Schedule-Node-Set)
		 (:Schedule-Node-Context)
		 (:Schedule-Node-Guard)))))
    (explore (isl::%isl-schedule-get-root (isl::schedule-handle (poly-schedule top))))
    out))

(defun finalize-polyhedral (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (declare (type polyhedral polyhedral))
  (macrolet ((set-option (name level)
	       `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_separation_bounds" 1)
    (set-option "ast_build_scale_strides" 1))
  (let* ((schedule (schedule-set-options schedule :separate))
	 (bands (collect-bandnode polyhedral))
	 (ast-build (ast-build-from-context (set-from-str "{:}")))
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
	    (schedule-get-root (poly-schedule poly)))
	  (when (poly-schedule poly)
	    (debug/render-c poly))))

(defun debug/render-c (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (let* ((build (ast-build-from-context (set-from-str "{:}")))
	 (ast   (ast-build-node-from-schedule build schedule))
	 (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
	 (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
	 (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
	 (str   (isl::%isl-printer-get-str q)))
    str))

(defun create-dependency-graph (polyhedral)
  (with-slots ((domain domain-ptr) (initial-schedule initial-schedule) (read-access read-ptr) (write-access write-ptr)) polyhedral
    (let* ((before-map (union-map-lex-lt-union-map initial-schedule initial-schedule))
           (read-access (union-map-intersect-domain read-access domain))
           (write-access (union-map-intersect-domain write-access domain))
           (RaW (union-map-intersect
		 (union-map-apply-range write-access (union-map-reverse read-access))
		 before-map))
           (WaW (union-map-intersect
		 (union-map-apply-range write-access (union-map-reverse write-access))
		 before-map))
           (WaR (union-map-intersect
		 (union-map-apply-range read-access (union-map-reverse write-access))
		 before-map)))
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
	     (schedule-constraints (schedule-constraints-set-validity schedule-constraints all-deps))
	     ;; proximity constraints (keeps loops nested based on dependencies)
	     (schedule-constraints (schedule-constraints-set-proximity schedule-constraints all-deps)))
	schedule-constraints))))

(defun poly/reschedule (polyhedral &key (serialize nil))
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
		  ;;(format t "~a = ~a~%" ,name (foreign-funcall ,(format nil "isl_options_get_~(~a~)" name) :pointer (isl-ctx-ptr *isl-context*) :int))
		  (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void))))
    (when serialize (set-option "schedule_serialize_sccs" 1))
    (let ((n 0))
      (loop for g in (hash-table-values (poly-pipeline polyhedral))
	    do (incf n (length (graph-nodes g))))
      (set-option "schedule_outer_coincidence" 1)
      (set-option "schedule_maximize_band_depth" 1)
      ;;(set-option "schedule_whole_component" 1)
      (set-option "schedule_treat_coalescing" 1)
      ))
  (with-slots ((domain-ptr domain-ptr) (read-ptr read-ptr) (write-ptr write-ptr) (schedule schedule)) polyhedral
    (let* ((constraints (poly/make-constraints polyhedral))
	   (schedule (schedule-constraints-compute-schedule constraints)))
      (setf (poly-schedule polyhedral) schedule)
      polyhedral)))
