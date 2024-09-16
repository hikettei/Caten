(in-package :caten/ajit)

;; [TODO] Rename :IR -> JIT
(defnode (:JIT :JIT_KERNEL) () "Loads a jit kernel (TODO: Docs)"
	 :slots ()) ;; TODO

;; [TODO] Reanem :EXPR -> :JIT
(defnode (:JIT :EXPR) () "TODO")
(defnode (:Render :FOR) () "TODO"
	 :slots ((_scalar_p))) ;; _scalar_p
(defnode (:Render :ENDFOR) () "")
(defnode (:Render :FUNCALL) () "")
(defnode (:Render :IF) () "")
(defnode (:Render :ELSE) () "")
(defnode (:Render :ENDIF) () "")
