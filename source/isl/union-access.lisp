(in-package :caten/isl)

(define-isl-object union-access-info :free %isl-union-access-info-free :copy %isl-union-access-info-copy)
(define-isl-object union-flow :free %isl-union-flow-free :copy %isl-union-flow-copy)

(define-isl-function union-access-info-from-sink %isl-union-access-info-from-sink
  (:give union-access-info)
  (:take union-map))

(define-isl-function union-access-info-set-must-source %isl-union-access-info-set-must-source
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

(define-isl-function union-access-info-set-may-source %isl-union-access-info-set-may-source
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

(define-isl-function union-access-info-set-schedule %isl-union-access-info-set-schedule
  (:give union-access-info)
  (:take union-access-info)
  (:take schedule))

(define-isl-function union-access-info-set-schedule-map %isl-union-access-info-set-schedule-map
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

(define-isl-function union-access-info-compute-flow %isl-union-access-info-compute-flow
  (:give union-flow)
  (:take union-access-info))

(define-isl-function union-flow-get-must-dependence %isl-union-flow-get-must-dependence
  (:give union-map)
  (:take union-flow))

(define-isl-function union-flow-get-may-dependence %isl-union-flow-get-may-dependence
  (:give union-map)
  (:take union-flow))
