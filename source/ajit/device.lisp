(in-package :caten/ajit)

(defclass Device () nil)
(defgeneric default-device (device-prefix) (:documentation "Returns a default device class dispatched by the device-prefix."))
