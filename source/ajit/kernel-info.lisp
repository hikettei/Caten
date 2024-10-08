(in-package :caten/ajit)
;; kernel-info.lisp
;; A lisp-dumpable object for the compiled kernel, which is handled by AJIT.
;; Compiled kernels are finally represented as a node ":JIT_KERNEL"
;; Node[JIT_KERNEL] out1 out2 out3 <- jit_caller(out1 out2 out3)
;;   where fname = function_name
;;         jit-info = jit-info
;;             ...
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
  `(make-jit-info :argtypes ',(jit-info-argtypes jit) :fname ,(jit-info-fname jit) :caller ,(jit-info-caller-body jit) :caller-body nil :lang ,(jit-info-lang jit) :code ,(jit-info-code jit) :n-kernels ,(jit-info-n-kernels jit) :load-p nil))
(defmethod print-object ((s jit-info) stream) (format stream "<~a[~a] Code [~a kernels]>" (jit-info-lang s) (jit-info-fname s) (jit-info-n-kernels s)))
(defun make-fused-kernel-caller (fname args lambda fcaller-body code lang n-kernels)
  (declare (type device lang))
  (let ((lang (intern (symbol-name (class-name (class-of lang))) "KEYWORD")))
    (make-node :JIT :JIT_KERNEL
	       (map 'list #'argument-name args)
	       (map 'list #'argument-name args)
	       :fname fname :jit-info (make-jit-info :caller lambda :caller-body fcaller-body :lang lang :code code :n-kernels n-kernels
						     :argtypes (map 'list #'argument-dtype args)))))
(defun maybe-scal-buffer (arg type)
  (if (buffer-p arg)
      (if (= (buffer-nrank arg) 0)
	  (progn
	    (setf (buffer-value arg) (caten/common.dtype:dtype/cast (buffer-value arg) type))
	    arg)
	  arg)
      (let ((x (make-buffer 0 nil nil type nil)))
	(setf (buffer-value x) (caten/common.dtype:dtype/cast arg type))
	x)))

(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((jit (getattr node :jit-info)))
    (assert (jit-info-p jit) () "~a is not a jit kernel. :jit-info=~a" node jit)
    (let ((args (map 'list #'maybe-scal-buffer args (jit-info-argtypes jit))))
      (apply (jit-info-caller jit) args)
      (apply #'values args))))

(defun count-n-kernels (rendering-graph &aux (count 0) (level 0))
  "Counts the number of the outermost loops (= n-kernels)"
  (loop for node in (graph-nodes rendering-graph)
	if (eql (node-type node) :FOR)
	  do (when (= 0 level) (incf count))
	     (incf level)
	else if (eql (node-type node) :ENDFOR) do
	  (decf level))
  count)
