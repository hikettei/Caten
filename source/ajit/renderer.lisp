(in-package :caten/ajit)
;; temporary
(defun render-c-style (graph &aux (indent 0))
  (with-output-to-string (out)
    (flet ((indent () (dotimes (i (* 4 indent)) (princ " " out))))
      (dolist (node (graph-nodes graph))
	(case (node-type node)
	  (:FOR
	   (indent)
	   (incf indent)
	   (format out "for (int ~(~a~)=~a;i<~a;i+=~a){~%"
		   (car (node-writes node))
		   (nth 0 (node-reads node))
		   (nth 1 (node-reads node))
		   (nth 2 (node-reads node))))
	  (:WMMA
	   (indent)
	   (format out "~a = ~a + ~a * ~a;~%"
		   (if (getattr node :reduction)
		       (nth 2 (node-reads node))
		       (car (node-writes node)))
		   (nth 0 (node-reads node))
		   (nth 1 (node-reads node))
		   (nth 2 (node-reads node))))
	  (:LOAD
	   (indent)
	   (format out "~a = ~a;~%" (car (node-writes node)) (getattr node :value)))
	  (:STORE
	   (indent)
	   (let ((x (first (node-reads node)))
		 (y (second (node-reads node)))
		 (z (car (node-writes node))))
	     (when (not (eql x y))
	       (format out "~a = ~a;~%" x y))
	     (format out "~a = ~a;~%" z x)))
	  (:ENDFOR
	   (decf indent)
	   (indent)
	   (format out "}~%"))
	  (otherwise
	   (indent)
	   (format out "~a;~%" node)))))))

;; WIP

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))

(defun graph->domain (graph)
  (declare (type graph graph))
  (with-output-to-string (out)
    (let ((loop-factors))
      (loop for node in (graph-nodes graph)
	    if (eql (node-type node) :FOR) do
	      (multiple-value-bind (gid from to by)
		  (values
		   (car (node-writes node))
		   (nth 0 (node-reads node))
		   (nth 1 (node-reads node))
		   (nth 2 (node-reads node)))
		(assert (and (numberp by) (= by 1)) () "Do not change the value of by; should be modified by polyhedral optimizer")
		(push `(,gid ,from ,to ,by) loop-factors)))
      (if (null loop-factors)
	  (format out " { } ")
	  (progn
	    (format out "{ [~(~a~)] : " (render-list (map 'list #'car loop-factors)))
	    (loop with last = (1- (length loop-factors))
		  for factor in loop-factors
		  for id = (nth 0 factor)
		  for upfrom = (nth 1 factor)
		  for below = (nth 2 factor)
		  for n upfrom 0 do
		    (format out "~(~a~) <= ~(~a~) < ~(~a~)" upfrom id below)
		    (if (= n last) (format out " } ") (format out " and "))))))))

;; CL-Autowrapしないで，手動でCFFIした方が良くない？
;; uiop:getenv suru
;; Step1 Grouping: RenderされるForの形が完全に同じものをGroupingし，Submoduleとして定義する
;; Step2 ISL: Submoduleに対するConstraints/Domainを定式化する
;; Step3 Further Fusion: 二つのGroupingがFuseできるかをISLで判定
;; Step4 Scheduling: ISLのConstraintsの条件下の元でどの軸が並列化・UnrollできるかなどをSchedule
(defun node->raw-deps (node type-map)
  (declare (type node node))
  
  )
(defun graph->raw-deps (graph type-map)
  (declare (type graph graph))
  (with-output-to-string (out)
    
    ))
    
(defun run-poly (graph type-map)
  (declare (type graph graph))
  (with-inlined-foreign-funcall-mode
    (with-isl-ctx ctx
      (%"isl_options_set_on_error" :void :pointer (isl-ctx-ptr ctx) :int 1)

      (print "DOMAIN")
      (isl-union-map-read-from-str ctx (print (graph->domain graph)))

      )))
