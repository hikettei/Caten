(in-package :caten/avm)

(define-condition avm-runtime-error ()
  ((avm :initarg :avm)
   (cond :initarg :cond))
  (:report
   (lambda (c s)
     (with-slots ((avm avm) (cond cond)) c
       (format s "AVM-Runtime-Error: Encountered the runtime error at ~ath instruction.
condition:
  ~a
disassemble:
~a"
	       (avm-pc avm)
	       cond
	       (with-output-to-string (out)
		 (loop for nth upfrom 0
		       for node in (graph-nodes (avm-graph avm))
		       do (format out "~a| ~a~%"
				  (if (= nth (avm-pc avm))
				      (format nil "*~a " nth)
				      (format nil " ~a " nth))
				  node))))))))

