(in-package :caten/aasm)

(defun $affine (writes reads)
  (declare (type list writes reads))
  (emit (make-node :Schedule :Affine writes reads)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

(defmethod print-node ((node Node) (id (eql :Affine)))
  ;; [TODO] Moduleと考えて内部のItemsをpprintする
  (format nil "<Affine : ~a <- (~a)>" (render-list (node-writes node)) (render-list (node-reads node))))

(defmethod print-node ((node Node) (id (eql :NonAffine)))
  (format nil "<NonAffine : ~a <- (~a)>" (render-list (node-writes node)) (render-list (node-reads node))))
