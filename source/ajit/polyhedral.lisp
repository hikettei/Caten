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

(defun reorder-band! (band loop-orders &aux (ctx *isl-context*))
  (declare (type list loop-orders))
  (macrolet ((% (&rest args) `(foreign-funcall ,@args)))
    (let* ((sched
	     (%"isl_schedule_node_band_get_partial_schedule" :pointer band :pointer))
	   (sched-str
	     (%"isl_multi_union_pw_aff_to_str" :pointer sched :string))
	   (sched-copy
	     (%"isl_multi_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string sched-str :pointer))
	   (n
	     (%"isl_multi_union_pw_aff_dim"
	       :pointer sched-copy
	       :int 3
	       :int))
	   (count 0)
	   (upas (loop for i upfrom 0 below n
		       collect
		       (cons nil (%"isl_multi_union_pw_aff_get_union_pw_aff" :string sched-copy :int i :pointer))))
	   (upa-str (%"isl_union_pw_aff_to_str" :pointer (cdr (car upas)) :string))
	   (new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string upa-str :pointer)))
      (%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int 0 :pointer new-upa :void)

      (loop named top-loop for ordering in loop-orders do
	(loop for iname in ordering do
	  (loop for i upfrom 0
		for (used . upa) in upas do
		  (when used (return-from top-loop))
		  (let* ((upa-str (%"isl_union_pw_aff_to_str" :pointer upa :string))
			 (upa-iname-str (car (last (cl-ppcre:split "->" upa-str))))
			 (upa-iname-str (car (cl-ppcre:split ":" upa-iname-str))))
		    (when (cl-ppcre:scan (format nil "~(~a~)" iname) upa-iname-str)
		      (let ((new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string upa-str :pointer)))
			(%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int count :pointer new-upa :void)
			(incf count)
			(setf (nth i upas) (cons t upa))))))))

      (loop for (used . upa) in upas do
	(when used
	  (return-from reorder-band!))
	(let* ((upa-str (%"isl_union_pw_aff_to_str" :pointer upa :string))
	       (new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :pointer upa-str :pointer)))
	  (%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int count :pointer new-upa :void)
	  (incf count))))))

(defun apply-reorder-schedule-loops! (schedule loop-orders)
  (declare (type list loop-orders))
  (let* ((root
	   (foreign-funcall "isl_schedule_get_root"
			    :pointer schedule :pointer))
	 (node root)
	 (next-nodes nil))
    
    (flet ((isl-schedule-node-get-child (schedule i)
	     (foreign-funcall "isl_schedule_node_get_child"
			      :pointer schedule
			      :int i
			      :pointer))
	   (isl-schedule-node-get-type (obj)
	     (foreign-funcall "isl_schedule_node_get_type"
			      :pointer obj
			      :int)))
      (loop named find-node
	    while (> (the fixnum (%isl-schedule-node-n-children node)) 0) do
	      (loop named band-loop
		    for i fixnum upfrom 0 below (the fixnum (%isl-schedule-node-n-children node))
		    for band = (isl-schedule-node-get-child node i)
		    if (eql (isl-schedule-node-get-type band) 0)
		      do (reorder-band! band loop-orders)
			 (setf next-nodes (if next-nodes
					      (append next-nodes (list (isl-schedule-node-get-child band 0)))
					      (list (isl-schedule-node-get-child band 0))))
			 (return-from band-loop)
		    else
		      do (setf next-nodes (if next-nodes (append next-nodes (list band)) (list band))))
	      (when (= (length next-nodes) 0)
		(return-from find-node))
	      (setf node (car (last next-nodes)))
	      (setf next-nodes (butlast next-nodes)))))
  nil)

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

	

