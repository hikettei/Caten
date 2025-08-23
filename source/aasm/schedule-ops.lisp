(in-package :caten/aasm)

(defun $affine (writes reads)
  (declare (type list writes reads))
  (emit (make-node :Schedule :Affine writes reads)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

(defmethod print-node ((node Node) (id (eql :Affine)))
  ;; [TODO] Moduleと考えて内部のItemsをpprintする
  (with-output-to-string (out)
    (format out "<Affine : ~a <- (~a)" (render-list (node-writes node)) (render-list (node-reads node)))
    (format out "~%>")))

(defmethod print-node ((node Node) (id (eql :NonAffine)))
  (format nil "<NonAffine : ~a <- (~a)>" (render-list (node-writes node)) (render-list (node-reads node))))
