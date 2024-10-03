(in-package :caten/ajit)

;; Implements Reconfigurable Polyhedral Compiler
;; https://arxiv.org/abs/2401.06665

;; Before PG -> Optimizations are common across architectures
;; PG -> Architecture specific optimizations

;; Takes Scheduled Group as an input
;; -> Polyhedral
;; Optimized Group
;; [TODO]
;; - [Here] Tiling, Loop Collapse, Vectorize
;; - Fix Transformer Scheduling
;; - ISeq Lowring is slow
;; - Transpose+Matmul Fusion
(defclass Polyhedral-Group ()
  ((base :initarg :base :type Group :accessor polyhedral-group-base))
  (:documentation "
A Polyhedral form of the fused schedule group.
"))

(defclass Polyhedral-Auto-Scheduler (Polyhedral-Group)
  ((schedule))
  (:documentation "groups which is subject to jit"))
;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/scop.cc#L47
;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/schedule_isl_conversion.cc
(defmethod group-domains ((group group))
  (let ((domains (make-hash-table)))
    ))

(defmethod scop ((group group))
  (let ((domains ))
    (loop for render-node in (graph-nodes (group-render-graph group))
          
          )))

;; ~~ Creation/Conversion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod group->polyhedral-group ((group Group))
  (make-instance
   (if (group-realize-on-vm group)
       'Polyhedral-Group
       'Polyhedral-Auto-Scheduler)
   :base group))

(defmethod polyhedral-group->group ((polyhedral-group Polyhedral-Group))
  (polyhedral-group-base polyhedral-group))
;; ~~ Auto Scheduler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod tile-bands ((polyhedral-group Polyhedral-Auto-Scheduler) tile-sizes)
  
  )

(defmethod unroll-bands ((polyhedral-group Polyhedral-Auto-Scheduler) unroll-factors)

  )

#|
for(int _gid0=0;(_gid0<=4);_gid0+=1) {
  for(int _gid1=0;(_gid1<=4);_gid1+=1) {
    for(int _gid2=0;(_gid2<=9);_gid2+=1) {
      float val_54 = 0.0; // T0[_gid0, _gid1, _gid2]
      for(int _gid3=0;(_gid3<=9);_gid3+=1) {
        float val_41 = _gid2; // T1[_gid0, _gid1, _gid2, _gid3]
        boolean val_48 = !(val_41!=val_36[((5*_gid0)+_gid1)]); // T2[_gid0, _gid1, _gid2, _gid3]
        float val_34 = (val_48 ? val_30[((10*_gid2)+_gid3)] : 0.0); // T3[_gid0, _gid1, _gid2, _gid3]
        val_54 = (val_54+val_34); // T4[_gid0, _gid1, _gid2]
      }
      val_58[(((50*_gid1)+(10*_gid0))+_gid2)] = val_54; // T5[_gid0, _gid1, _gid2]
    }
  }
}
|#
