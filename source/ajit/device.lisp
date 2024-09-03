(in-package :caten/ajit)

(defclass Device () nil)
(defgeneric default-device (device-prefix) (:documentation "Returns a default device class dispatched by the device-prefix."))
(defgeneric device-parallel-depth (device-prefix) (:documentation "Return a fixnum indicating n outermost loops are parallelized."))
