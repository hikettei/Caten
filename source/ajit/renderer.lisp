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

(defgeneric render-jit-graph (lang jit-graph polyhedral indent)
  (:documentation
   "IRs used in the jit-graph:
(TODO: Docs)
- FOR
- ENDFOR
- SCHEDULE
- IF
- ELSE
- ENDIF
"))
;;(defpattern NodeAttrs(type &rest attrs
(defmethod render-jit-graph ((lang (eql :clang)) jit-graph polyhedral indent)
  (with-output-to-string (out)
    (macrolet ((line (designator &rest args)
		 `(progn
		    (dotimes (i indent) (princ " " out))
		    (format out ,designator ,@args)
		    (format out "~%"))))
      (loop for node in (graph-nodes jit-graph)
	    for type = (node-type node) do
	      (assert (eql :Render (node-class node)))
	      (ecase type
		(:FOR
		 )
		(:ENDFOR

		 ))))))
