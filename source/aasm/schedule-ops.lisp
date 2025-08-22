(in-package :caten/aasm)

(defun $affine (writes reads)
  (declare (type list writes reads))
  (emit (make-node :Schedule :Affine writes reads)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

