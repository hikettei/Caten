(in-package :caten/aasm)
;; == Context =======
;; - only used in api
;; - wrapped by more higher level apis in the future
;;    - e.g.: Aten -> tpsort -> obtain iseq
;; ==================
(defparameter *ctx* nil)
(defmacro with-context (&rest ssa-forms)
  `(let ((*ctx* (make-graph)))
     (with-asm ,@ssa-forms)
     (setf (graph-nodes *ctx*) (reverse (graph-nodes *ctx*)))
     *ctx*))
(defmacro with-context-nodes (&rest ssa-forms)
  `(graph-nodes (with-context ,@ssa-forms)))
(defmacro with-asm (&rest ssa-forms)
  "ssa-forms: (bind-to form)"
  `(let* (,@ssa-forms) (declare (ignorable ,@(map 'list #'car ssa-forms))) ,(caar (last ssa-forms))))
(defmacro emit (form)
  `(if *ctx*
       (let ((out ,form))
	 (push out (graph-nodes *ctx*))
	 out)
       ,form))
(defmacro with-context-from-parents ((&rest parents) &rest ssa-forms)
  "Basically the same as with-context, but it is useful when merging another graph into the returned graph."
  `(let ((g (with-context ,@ssa-forms)))
     (setf (graph-nodes g)
           (append 
            (loop for g in (flatten (list ,@parents))
                  if (graph-p g) append (graph-nodes g))
            (graph-nodes g)))
     g))
