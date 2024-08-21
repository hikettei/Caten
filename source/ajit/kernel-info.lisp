(in-package :caten/ajit)
;; ~~ Fused Kernel Objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defstruct (JIT-Info)
  (fname :nil :type keyword)
  (caller #'(lambda ()) :type (or null function))
  (caller-body nil :type list)
  (lang :nil :type keyword)
  (code "" :type string)
  (n-kernels 0 :type fixnum)
  (load-p t :type boolean)) ;; the count of outermost loops
(defmethod make-load-form ((jit Jit-Info) &optional env)
  (declare (ignore env))
  `(make-jit-info :fname ,(jit-info-fname jit) :caller ,(jit-info-caller-body jit) :caller-body nil :lang ,(jit-info-lang jit) :code ,(jit-info-code jit) :n-kernels ,(jit-info-n-kernels jit) :load-p nil))
(defmethod print-object ((s jit-info) stream) (format stream "<~a[~a] Code [~a kernels]>" (jit-info-lang s) (jit-info-fname s) (jit-info-n-kernels s)))
(defun make-fused-kernel-caller (fname allocs lambda fcaller-body code lang n-kernels)
  (make-node :IR :JIT_KERNEL
	     (apply #'append (map 'list #'node-writes allocs))
	     (apply #'append (map 'list #'node-writes allocs))
	     :fname fname :jit-info (make-jit-info :caller lambda :caller-body fcaller-body :lang lang :code code :n-kernels n-kernels)))
(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((jit (getattr node :jit-info)))
    (assert (jit-info-p jit) () "~a is not a jit kernel. :jit-info=~a" node jit)
    (apply (jit-info-caller jit) args))
  (apply #'values args))

(defun count-n-kernels (rendering-graph &aux (count 0) (level 0))
  "Counts the number of the outermost loops (= n-kernels)"
  (loop for node in (graph-nodes rendering-graph)
	if (eql (node-type node) :FOR)
	  do (when (= 0 level) (incf count))
	     (incf level)
	else if (eql (node-type node) :ENDFOR) do
	  (decf level))
  count)
