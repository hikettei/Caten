(defpackage :caten/polyhedral/parallelize
  (:use :cl :caten/polyhedral/search :caten/polyhedral/ir :caten/polyhedral/config :caten/polyhedral/ast)
  (:import-from :caten/polyhedral/transforms #:polyir-loop-permutable-bands #:polyir-set-coincident #:polyir-loop-interchange)
  (:export
   ))

(in-package :caten/polyhedral/parallelize)

(defclass Interchange-Search ()
  ((n-global-loops :initarg :n-global-loops :accessor search-n-global-loops)))

(defstruct Range from to by)

(defun get-outermost-loops (ast)
  (let ((loops))
    (labels ((lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (trivia:ematch object
	         ((ASTBlock :body body) (map 'list #'lower body))
	         ((AstFor :body body :from from :to to :by by)
                  (assert (eql (expr-op from) :Const))
                  (assert (eql (expr-op to) :<))
                  (assert (eql (expr-op by) :const))
                  (push (make-range :from (car (expr-args from)) :to (car (expr-args (second (expr-args to)))) :by (car (expr-args by))) loops)
                  (lower body))
	         ((AstIf :then-node then)
		  (lower then))
	         (_))))
      (lower ast))
    (nreverse loops)))

(defmethod metric ((op Interchange-Search) poly)
  (let* ((ast (get-optimized-ast poly))
         (area (1+ (search-n-global-loops op)))
         (loops (get-outermost-loops ast))
         (loops (subseq loops 0 (max 0 (min (length loops) area)))))
    (apply
     #'*
     (loop for l in loops
           if (numberp (range-to l))
             collect (range-to l)
           else
             collect 1))))

(defclass Interchange (Sketch)
  ((band-idx :initarg :band-idx :type fixnum :accessor interchange-band-idx)))

(defun make-interchange (idx) (make-instance 'Interchange :band-idx idx))

(defmethod apply-sketch ((sketch Interchange) poly)
  (declare (type Polyhedral-IR poly))
  (polyir-loop-interchange poly (interchange-band-idx sketch))
  (poly-schedule poly))

(defun apply-parallelize (scheduler poly)
  (declare (type Auto-Scheduler-Config scheduler) (type Polyhedral-IR poly))
  (search-best-interchange poly scheduler)
  (caten/polyhedral/transforms:polyir-set-coincident poly (auto-scheduler-n-global-loops scheduler)))

(defun search-best-interchange (poly config)
  (let ((sketch-list
          (map 'list #'make-interchange (polyir-loop-permutable-bands poly)))
        (method
          (make-instance 'Interchange-Search :n-global-loops (auto-scheduler-n-global-loops config))))
    (run-search poly method sketch-list)))
