(in-package :caten/api)
;; == Context =======
;; - only used in api
;; - wrapped by more higher level apis in the future
;;    - e.g.: Aten -> tpsort -> obtain iseq
;; ==================
(defparameter *ctx* nil)
(defmacro with-context (&body body)
  `(let ((*ctx* (make-graph)))
     (progn ,@body)
     (setf (graph-nodes *ctx*) (reverse (graph-nodes *ctx*)))
     (verify-graph *ctx*)
     *ctx*))
(defmacro emit (form)
  `(if *ctx*
       (let ((out ,form))
	 (push out (graph-nodes *ctx*))
	 out)
       ,form))
