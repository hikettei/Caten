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

(defmethod create-constraints ((config Polyhedral-Configure) deps)

  
     (schedule-constraints-set-proximity
    (schedule-constraints-set-validity
     (schedule-constraints-set-coincidence
      (schedule-constraints-on-domain (pg-domain pg))
      (pg-dependencies pg))
     (pg-dependencies pg))
    (pg-dependencies pg))
  )
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


    ;; -> Proceed to Tiling
    ))
