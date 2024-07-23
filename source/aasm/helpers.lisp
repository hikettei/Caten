(in-package :caten/aasm)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))
