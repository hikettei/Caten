(in-package :caten/ajit)
;; ~~ Fused Kernel Objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defstruct (JIT-Info)
  (fname :nil :type keyword)
  (caller #'(lambda ()) :type (or null function))
  (caller-body nil :type list)
  (argtypes nil :type list)
  (lang :nil :type keyword)
  (code "" :type string)
  (n-kernels 0 :type fixnum)
  (load-p t :type boolean)) ;; the count of outermost loops
(defmethod make-load-form ((jit Jit-Info) &optional env)
  (declare (ignore env))
  `(make-jit-info :argtypes ,(jit-info-argtypes jit) :fname ,(jit-info-fname jit) :caller ,(jit-info-caller-body jit) :caller-body nil :lang ,(jit-info-lang jit) :code ,(jit-info-code jit) :n-kernels ,(jit-info-n-kernels jit) :load-p nil))
(defmethod print-object ((s jit-info) stream) (format stream "<~a[~a] Code [~a kernels]>" (jit-info-lang s) (jit-info-fname s) (jit-info-n-kernels s)))
(defun make-fused-kernel-caller (fname args lambda fcaller-body code lang n-kernels)
  (make-node :IR :JIT_KERNEL
	     (map 'list #'argument-name args)
	     (map 'list #'argument-name args)
	     :fname fname :jit-info (make-jit-info :caller lambda :caller-body fcaller-body :lang lang :code code :n-kernels n-kernels
						   :argtypes (map 'list #'argument-dtype args))))
(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((jit (getattr node :jit-info)))
    (assert (jit-info-p jit) () "~a is not a jit kernel. :jit-info=~a" node jit)
    (apply (jit-info-caller jit) args)
    (apply #'values
	   (loop for a in args
		 for type in (jit-info-argtypes jit)
		 if (buffer-p a) collect a
		   else
		     collect
		     (let ((x (make-buffer 0 nil nil type nil)))
		       (setf (buffer-value x) a)
		       x)))))

(defun count-n-kernels (rendering-graph &aux (count 0) (level 0))
  "Counts the number of the outermost loops (= n-kernels)"
  (loop for node in (graph-nodes rendering-graph)
	if (eql (node-type node) :FOR)
	  do (when (= 0 level) (incf count))
	     (incf level)
	else if (eql (node-type node) :ENDFOR) do
	  (decf level))
  count)
