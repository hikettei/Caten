(in-package :caten/ajit)

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((fname :type string)
		 (jit-info :type JIT-Info)))

(defnode (:Render :FOR) () "

"
	 :slots ((_scalar_p))) ;; _scalar_p ;; scope
(defnode (:Render :ENDFOR) () "")
(defnode (:Render :FUNCALL) () "")
(defnode (:Render :IF) () "")
(defnode (:Render :ELSE) () "")
(defnode (:Render :ENDIF) () "")
