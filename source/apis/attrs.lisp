(in-package :caten/apis)

(defnode (:Special/VM :Pause/Backward) (JITAble) "During VM execution, the forward computation is paused at the point where this node exists."
	 :placeholder -1)
