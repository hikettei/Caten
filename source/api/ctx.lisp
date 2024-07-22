(in-package :caten/api)
;; == Context =======
;; - only used in api
;; - wrapped by more higher level apis in the future
;;    - e.g.: Aten -> tpsort -> obtain iseq
;; ==================
(defparameter *ctx* nil)
(defmacro with-context (&rest ssa-forms)
  `(let ((*ctx* (make-graph)))
     (with-ssa ,@ssa-forms)
     (setf (graph-nodes *ctx*) (reverse (graph-nodes *ctx*)))
     (verify-graph *ctx*)
     *ctx*))
(defmacro with-ssa (&rest ssa-forms)
  "ssa-forms: (bind-to form)"
  `(let* (,@ssa-forms) (declare (ignorable ,@(map 'list #'car ssa-forms)))))
(defmacro emit (form)
  `(if *ctx*
       (let ((out ,form))
	 (push out (graph-nodes *ctx*))
	 out)
       ,form))
