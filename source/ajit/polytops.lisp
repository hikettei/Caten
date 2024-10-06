(in-package :caten/ajit)


(defclass Polyhedral-Configure ()
  ((new-variables :initarg :new-variables :reader pc-new-variables)
   (ilp-constructions :initarg :ilp-constructions :reader pc-ilp-constructions)
   (custom-constraints :initarg :custom-constraints :reader pc-custom-constraints)
   (directives :initarg :directives :reader pc-directives))
  (:documentation "PolyTOPS Configuration
   Local Configurations
   1) Cost Functions Control
     2)  New Vars
     3)  proximity , feautrier, contiguity fig (5), bigLoopFirst
   2) Custrom Constraints
   3) Fusion Control (not going to implement this)
   Global Configurations
   AutoVectorize, Tiling, etc
  ;; [TODO] Generate Vectorize Loop (Later, Transformed into packed-funcall)
"))

(defstruct (ILP-Construction-Config
            (:constructor ILP-Construction (cost-functions &key (scheduling-dimension -1))))
  "
ILP-Construction-Config
cost-functions: 
scheduling-dimension: -1 to default.
"
  (scheduling-dimension scheduling-dimension :type (integer -1 512))
  (cost-functions cost-functions :type list))

(defstruct (Custom-Constraints-Config
            (:constructor Custom-Constraint (constraints &key (scheduling-dimension -1))))
  "TODO: Docs"
  (scheduling-dimension scheduling-dimension :type (integer -1 512))
  (constraints constraints :type list))

(defstruct (Directives-Config
            (:constructor Directive (type stmts iterator)))
  "TODO: Docs"
  (type type)
  (stmts stmts)
  (iterator iterator))

(defstruct (Statement
            (:constructor Statement (stmt var-type idx-var)))
  "S[stmt]_[var_type]_[idx_var]
T to sum (for stmt and idx_var)"
  (stmt stmt :type (or (member t) (unsigned-byte 32)))
  (var-type var-type :type (member :it :par :cst))
  (idx-var idx-var :type (or (member t) (unsigned-byte 32))))

(defmethod statement-union-set ((s statement) constraint)
  (assert constraint () "Constraint is a function")
  (loop for stmt in (if (eql (statement-stmt s) t)
                        (range 0 10)
                        (list (statement-stmt s)))
        append
        (loop for idx-var in (if (eql (statement-idx-var s) t)
                                 (range 0 10)
                                 (list (statement-idx-var s)))
              collect 0)))
              
(defun make-polyhedral-configure (&key
                                    (new-variables)
                                    (ilp-constructions)
                                    (custom-constraints)
                                    (directives))
  (make-instance 'Polyhedral-Configure
                 :new-variables new-variables
                 :ilp-constructions ilp-constructions
                 :custom-constraints custom-constraints
                 :directives directives))

(defmethod create-constraints ((config Polyhedral-Configure) (pg Polyhedral-Auto-Scheduler))
  (let ((sc (schedule-constraints-on-domain (pg-domain pg))))
    (setf sc (schedule-constraints-set-coincidence sc (pg-dependencies pg)))
    (setf sc (schedule-constraints-set-validity sc (pg-dependencies pg)))
    (setf sc (schedule-constraints-set-proximity sc (pg-dependencies pg)))
    sc))

(defmethod constraints-on-dimension ((sc Schedule-Constraints) (config Polyhedral-Configure) (dimension fixnum))
  (dolist (constraint (pc-custom-constraints config))
    (with-slots ((sd scheduling-dimension) (formula constraints)) constraint
      (when (or (= -1 sd) (= dimension sd))
        ;; i to sum
        
        )))
  sc)
;; https://github.com/mindspore-ai/akg/blob/master/src/poly/polytops.h
(defmethod polytops-schedule ((pg Polyhedral-Auto-Scheduler) (config Polyhedral-Configure))
  "
Implements Algorithm 1: PolyTOPS Scheduler
Paper: https://arxiv.org/pdf/2401.06665

https://ieeexplore.ieee.org/document/9188233
"
  ;; Inputs: Deps(PG), Statements(PG), Config
  ;; Outputs, Schedule, Tiliability, Parallelism Info
  (let ((constraints (create-constraints config (pg-dependencies pg)))
        (dimension 0)
        (band 0))
    ;; Loop Distribution is not implemented.
    ;; config.Distribute(dimension) is not implemented.
    ;; The termination criteria of the algorithm are to check if the iteration space is completely covered and if all the dependencies are fulfilled (line 42).
    (flet ((satisfied-p ()
             t))
      ;; pc -> tiling
      (loop until (satisfied-p) do
        (let ((constraints (constraints-on-dimension constraints config dimension)))

          )))))
