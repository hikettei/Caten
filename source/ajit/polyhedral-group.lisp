(in-package :caten/ajit)

;; Implements Reconfigurable Polyhedral Compiler
;; https://arxiv.org/abs/2401.06665

;; Takes Scheduled Group as an input
;; -> Polyhedral
;; Optimized Group

(defclass Polyhedral-Group ()
  nil
  (:documentation "
A Polyhedral form of the fused schedule group.
"))

(defmethod group->polyhedral ((group Group))
  
  )
