(in-package :caten/api)
;; == Context =======
;; - only used in api
;; - wrapped by more higher level apis in the future
;;    - e.g.: Aten -> tpsort -> obtain iseq
;; ==================
(defparameter *ctx* nil)
(defmacro with-context (&rest ssa-forms)
  "ssa-forms: (bind-to form)"
  `(let ((*ctx* (make-graph)))
     (let* (,@ssa-forms)
       (declare (ignorable ,@(map 'list #'car ssa-forms))))
     (setf (graph-nodes *ctx*) (reverse (graph-nodes *ctx*)))
     (verify-graph *ctx*)
     *ctx*))
(defmacro emit (form)
  `(if *ctx*
       (let ((out ,form))
	 (push out (graph-nodes *ctx*))
	 out)
       ,form))
