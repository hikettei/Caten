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
    (foreign-funcall "isl_schedule_free" :pointer cp :void)
    str))

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
    
    ))

