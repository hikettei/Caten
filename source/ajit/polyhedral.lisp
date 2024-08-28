(in-package :caten/ajit)
;; This paper is good to read first:
;; - https://arxiv.org/pdf/2401.06665
;; - https://www.researchgate.net/publication/320992060_Consecutivity_in_the_isl_Polyhedral_Scheduler
;; TODO: Making all isl pointers gc-reachable
;; TODO: Symbolic Model Scheduling
(defstruct (Polyhedral
	    (:conc-name poly-)
	    (:constructor make-polyhedral (avm pipeline domain read write schedule vm-inputs vm-outputs)))
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
  (schedule schedule :type Schedule))

(defun poly/io-scalar-p (poly x)
  (let ((type (gethash x (poly-vm-io-types poly))))
    (when (null type) (error "~a is not input/output" type))
    (= (buffer-nrank (car (relay-writes type))) 0)))

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
  (ast-build-node-from-schedule (ast-build-from-context (set-from-str "{:}")) schedule))

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
	  (poly-schedule poly)
	  (debug/render-c poly)))

(defun debug/render-c (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (let* ((build (ast-build-from-context (set-from-str "{:}")))
	 (ast   (ast-build-node-from-schedule build schedule))
	 (p     (isl-printer-to-str))
	 (p     (isl::%isl-printer-set-output-format (isl::isl-printer-handle p) 4)) ;; 4 == Clang
	 (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
	 (str   (isl::%isl-printer-get-str q)))
    str))

(defun create-dependency-graph (polyhedral)
  (declare (type polyhedral polyhedral))
  (with-slots ((schedule schedule) (may-read read-ptr) (must-write write-ptr)) polyhedral
    (let* (;; 1. RAW (Read After Write), a=1 then b=a
	   (access (union-access-info-from-sink (copy may-read)))
	   (access (union-access-info-set-must-source access must-write))
	   (access (union-access-info-set-schedule access schedule))
	   (flow (union-access-info-compute-flow access))
	   (raw-deps (union-flow-get-must-dependence flow))
	   ;; 2. WAR (Write After Read) deps
	   (access (union-access-info-from-sink must-write))
	   (access (union-access-info-set-must-source access must-write))
	   (access (union-access-info-set-may-source access may-read))
	   (access (union-access-info-set-schedule access schedule))
	   (flow (union-access-info-compute-flow access))
	   (waw-deps (union-flow-get-must-dependence flow))
	   (war-deps (union-flow-get-may-dependence flow)))
      (values raw-deps waw-deps war-deps))))

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
    ;;(set-option "schedule_maximize_band_depth" 1)
    ;;(set-option "schedule_whole_component" 1)
    ;;(set-option "schedule_treat_coalescing" 1)
    )
  (with-slots ((domain-ptr domain-ptr) (read-ptr read-ptr) (write-ptr write-ptr) (schedule schedule)) polyhedral
    (let* ((constraints (poly/make-constraints polyhedral))
	   (schedule (schedule-constraints-compute-schedule constraints)))
      (setf (poly-schedule polyhedral) schedule)
      polyhedral)))
;; Work in progress ...
(defun poly/loop-collapse (polyhedral)
  "
[Scheduler]
Try to apply the affine transformation is the iteration is contiguous in the polyhedral space.
- Refernece: https://www.researchgate.net/publication/320992060_Consecutivity_in_the_isl_Polyhedral_Scheduler
"
  (declare (type polyhedral polyhedral))
  
  )

(defun poly/vectorize (polyhedral)
  ""
  (declare (type polyhedral polyhedral))
  (poly/loop-collapse polyhedral)
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
