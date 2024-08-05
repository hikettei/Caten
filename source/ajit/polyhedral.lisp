(in-package :caten/ajit)
;; This paper is good to read first:
;; - https://arxiv.org/pdf/2401.06665
;; TODO: Making all isl pointers gc-reachable
;; TODO: Symbolic Model Scheduling
(defstruct (Polyhedral
	    (:conc-name poly-)
	    (:constructor make-polyhedral (avm pipeline domain read write schedule)))
  (avm avm :type avm)
  (pipeline pipeline :type hash-table)
  ;; constraints
  (domain domain :type string)
  (domain-ptr (isl-union-set-read-from-str domain) :type isl-obj)
  (read read :type string)
  (read-ptr (isl-union-map-read-from-str read) :type isl-obj)
  (write write :type string)
  (write-ptr (isl-union-map-read-from-str write) :type isl-obj)
  (schedule schedule :type isl-obj))

(defmethod print-object ((poly Polyhedral) stream)
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
Expected Output:
~a
======================================================================"
	  (poly-domain poly)
	  (poly-read poly)
	  (poly-write poly)
	  (debug/render-schedule (poly-schedule poly))
	  (debug/render-c poly)))

(defun debug/render-schedule (schedule &aux (schedule (isl-obj-ptr schedule)))
  (foreign-funcall "isl_schedule_to_str" :pointer schedule :string))

(defun debug/render-c (polyhedral &aux (schedule (isl-obj-ptr (poly-schedule polyhedral))))
  (let* ((space (isl-set-read-from-str "{:}"))
	 (build (isl-ast-build-from-context space))
	 (cp (foreign-funcall "isl_schedule_copy" :pointer schedule :pointer))
	 (ast   (isl-ast-build-node-from-schedule build	(make-isl-obj :ptr cp)))
	 (c     (isl-ast-node-get-ctx ast))
	 (p     (isl-printer-to-str c))
	 (p     (isl-printer-set-output-format p 4)) ;; 4 indicates C
	 (q     (isl-printer-print-ast-node p ast))
	 (str   (isl-printer-get-str q)))
    ;; (foreign-funcall "isl_schedule_free" :pointer cp :void)
    str))

(defun create-dependency-graph (polyhedral &aux (copied1) (copied2))
  (declare (type polyhedral polyhedral))
  (with-slots ((schedule schedule) (may-read read-ptr) (must-write write-ptr)) polyhedral
    (flet ((isl-schedule-copy (x)
	     (let ((val (foreign-funcall "isl_schedule_copy" :pointer (isl-obj-ptr x) :pointer)))
	       (push val copied1)
	       (make-isl-obj :ptr val)))
	   (isl-union-map-copy (x)
	     (let ((val (foreign-funcall "isl_union_map_copy" :pointer (isl-obj-ptr x) :pointer)))
	       (push val copied2)
	       (make-isl-obj :ptr val))))
      (let* (;; 1. RAW (Read After Write), a=1 then b=a
	     (access
	       (isl-union-access-info-from-sink
		(isl-union-map-copy may-read)))
	     (access
	       (isl-union-access-info-set-must-source
		access
		(isl-union-map-copy must-write)))
	     (access
	       (isl-union-access-info-set-schedule
		access
		(isl-schedule-copy schedule)))
	     (flow
	       (isl-union-access-info-compute-flow
		access))
	     (raw-deps
	       (isl-union-flow-get-must-dependence
		flow))
	     ;; 2. WAR (Write After Read) deps
	     (access
	       (isl-union-access-info-from-sink
		(isl-union-map-copy must-write)))
	     (access
	       (isl-union-access-info-set-must-source
		access
		must-write))
	     (access
	       (isl-union-access-info-set-may-source
		access
		may-read))
	     (access
	       (isl-union-access-info-set-schedule
		access
		schedule))
	     (flow
	       (isl-union-access-info-compute-flow
		access))
	     (waw-deps
	       (isl-union-flow-get-must-dependence
		flow))
	     (war-deps
	       (isl-union-flow-get-may-dependence
		flow)))
	;;(dolist (c copied1) (foreign-funcall "isl_schedule_free" :pointer c :void))
	;;(dolist (c copied2) (foreign-funcall "isl_union_map_free" :pointer c :void))
	(values raw-deps waw-deps war-deps)))))

(defun poly/make-constraints (polyhedral)
  "(2) Validty/Legality Constraints"
  (declare (type polyhedral polyhedral))
  (with-slots ((domain domain-ptr)) polyhedral
    (multiple-value-bind (raw-deps waw-deps war-deps)
	(create-dependency-graph polyhedral)
      (let* ((all-deps
	       (isl-union-map-union waw-deps war-deps))
	     (all-deps
	       (isl-union-map-union all-deps raw-deps))
	     (schedule-constraints
	       (isl-schedule-constraints-on-domain
		(isl-union-set-copy domain)))
	     (schedule-constraints
	       (isl-schedule-constraints-set-validity
		schedule-constraints
		(isl-union-map-copy all-deps)))
	     (schedule-constraints
	       (isl-schedule-constraints-set-proximity
		schedule-constraints
		(isl-union-map-copy all-deps))))
	schedule-constraints))))

(defun poly/fuse-schedules (polyhedral)
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
Reference: https://dl.acm.org/doi/fullHtml/10.1145/3416510
"
  (declare (type polyhedral polyhedral))
  (with-slots ((domain-ptr domain-ptr) (read-ptr read-ptr) (write-ptr write-ptr) (schedule schedule)) polyhedral
    (let* ((constraints (poly/make-constraints polyhedral))
	   (schedule (foreign-funcall "isl_schedule_constraints_compute_schedule" :pointer (isl-obj-ptr constraints) :pointer)))
      (setf (poly-schedule polyhedral) (make-isl-obj :ptr schedule))
      ;; Symbolに対してread-ptr/write-ptrがError
      ;;(print domain-ptr)
      ;;(print read-ptr)
      ;;(print write-ptr)
      (print polyhedral)
      nil
      )))

(defun poly/affine (polyhedral)
  "
[Scheduler]
Try to apply the affine transformation is the iteration is contiguous in the polyhedral space
"
  (declare (type polyhedral polyhedral))
  
  )
(defun poly/parallel (polyhedral)
  "[Scheduler]
Reading the RAW/WAW/WAR dependencies, determines the parallelizable axis.
If possible, attempts to reorder the iteration to enable outer-loop parallelism
"
  (declare (type polyhedral polyhedral))

  )

(defun poly/locality (polyhedral)
  ""
  (declare (type polyhedral polyhedral))
  )

(defun poly/tile (polyhedral)
  ""
  (declare (type polyhedral polyhedral))
  )

(defun poly/vectorize (polyhedral)
  ""
  (declare (type polyhedral polyhedral))
  )

(defun optimize-polyhedral (domain read-deps write-deps schedule &key (verbose nil))
  "Run the polyhedral model to minimize the following goal:
- 1. Fuse more ops as many as possible
- 2. Loop Transformation
- 3. Analyze which axes are parallelizable
- 4. Optimize the memory-locality
"
  (declare (type string domain read-deps write-deps))
  (multiple-value-bind (domain read-deps write-deps)
      (values
       (isl-union-set-read-from-str domain)
       (isl-union-map-read-from-str read-deps)
       (isl-union-map-read-from-str write-deps))
    (when verbose
      (isl-schedule-dump schedule)
      (format t "== [Initial Schedule in Clang] =====~%")
      (format t "~%~a~%" (debug/render-c schedule)))

    (multiple-value-bind (raw-deps waw-deps war-deps)
	(create-dependency-graph schedule read-deps write-deps)
      (flet ((dump (x)
	       (foreign-funcall "isl_union_map_dump" :pointer (isl-obj-ptr x) :void)))
	(when verbose
	  (dump raw-deps) (dump waw-deps) (dump war-deps)))
      
      (macrolet ((set-option (name level)
		   `(foreign-funcall ,name
				     :pointer (isl-ctx-ptr *isl-context*)
				     :int ,level
				     :void)))
	(set-option "isl_options_set_schedule_maximize_band_depth" 1)
	(set-option "isl_options_set_schedule_whole_component" 1)
	(set-option "isl_options_set_schedule_treat_coalescing" 1)
	(set-option "isl_options_set_tile_scale_tile_loops" 1)
	;; (set-option "isl_options_set_schedule_split_scaled" 1)
	(set-option "isl_options_set_schedule_serialize_sccs" 1)
	;; More ...
	)
      (let* ((all-deps
	       (isl-union-map-union waw-deps war-deps))
	     (all-deps
	       (isl-union-map-union all-deps raw-deps))
	     (schedule-constraints
	       (isl-schedule-constraints-on-domain
		(isl-union-set-copy domain)))
	     (schedule-constraints
	       (isl-schedule-constraints-set-validity
		schedule-constraints
		(isl-union-map-copy all-deps)))
	     (schedule-constraints
	       (isl-schedule-constraints-set-proximity
		schedule-constraints
		(isl-union-map-copy all-deps)))
	     (schedule
	       (make-isl-obj
		:ptr
		(foreign-funcall
		 "isl_schedule_constraints_compute_schedule"
		 :pointer (isl-obj-ptr schedule-constraints)
		 :pointer)))
	     ;;(loop-orders
	     ;;  `((0 1) (0 1) (1 0)))
	     )
	;;(apply-reorder-schedule-loops! (isl-obj-ptr schedule) loop-orders)
	(print (debug/render-c schedule))
	))))

	

