(in-package :caten/api)

(defnode (:Special/VM :Pause/Backward) (JITAble)
	 "During VM execution, forward computation is paused at the point where this node exists."
	 :placeholder -1)
