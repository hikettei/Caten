(in-package :caten/aasm)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass RuntimeOps () nil)

;; [TODO] Remove ./api/attrs.lisp after switching to use :SINK
(defnode (:SPECIAL/VM :SINK) (JITAble RuntimeOps)
	 "During VM execution, forward computation is paused at the point where this node exists."
	 :placeholder -1
         ;; :slots nil (TODO: Add :forward/:backward)
         :type-relay #'(lambda (id->type node) (list (gethash (car (node-reads node)) id->type))))

)
