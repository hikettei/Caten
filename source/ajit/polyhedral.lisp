(in-package :caten/ajit)

(defun debug/render-c (schedule &aux (schedule (isl-obj-ptr schedule)))
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

(defun create-dependency-graph (schedule may-read must-write &aux (copied1) (copied2))
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
      (values raw-deps waw-deps war-deps))))

(defun optimize-polyhedral (domain read-deps write-deps &key (verbose nil))
  "Run the polyhedral model to minimize the following goal:
- 1. Fuse more ops as many as possible
- 2. Loop Transformation
- 3. Analyze which axes are parallelizable
- 4. Optimize the memory-locality
"
  (declare (type string domain read-deps write-deps))
  (multiple-value-bind (domain read-deps write-deps schedule)
      (values
       (isl-union-set-read-from-str domain)
       (isl-union-map-read-from-str read-deps)
       (isl-union-map-read-from-str write-deps)
       (isl-schedule-from-domain
	(isl-union-set-read-from-str domain)))
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
      
      )))

