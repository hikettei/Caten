(in-package :caten/apis)

(defparameter *conditioned* nil)
(defmacro !if (condition then &optional else))

(!if (> n 1)
     nil
     )

(defmacro !when (condition &rest forms) `(!if ,condition (progn ,@forms)))

(defparameter *iterate-over* nil)
;; TODO: Implement RNN
;; TODO: Implement KV Cache
(defmacro !loop (&rest keyword-and-forms)
  )
