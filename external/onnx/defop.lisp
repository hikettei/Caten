(in-package :caten/onnx)

(defparameter *converter-features* (make-hash-table :test #'equal))

(defmacro defop ((opset-name min-opset-version) ((gph inputs attrs &rest more) &body body))
  "Defines a converter"
  (let ((tmp (gensym)))
    `(let ((,tmp (alexandria:named-lambda
                     ,(intern (format nil "~a_~a" opset-name min-opset-version))
                     (,gph ,inputs ,attrs ,@more) (declare (ignorable ,gph)) ,@body)))
       (if (gethash ,opset-name *converter-features*)
	   (let ((values (gethash ,opset-name *converter-features*)))
	     (let ((position (position ,min-opset-version values :test #'= :key #'cdr)))
	       (if position
		   (setf (nth position (gethash ,opset-name *converter-features*)) (cons ,tmp ,min-opset-version))
		   (push (cons ,tmp ,min-opset-version) (gethash ,opset-name *converter-features*)))))
	   (setf (gethash ,opset-name *converter-features*) (list (cons ,tmp ,min-opset-version)))))))

(defun get-converter (op-type opset-version)
  "Tries to find a converter for the given op-type and opset-version. If there's no converter for the given opset-version, it will try to find the closest one."
  (declare (type string op-type)
	   (type fixnum opset-version))
  (restart-case
      (progn
	(let ((candidates (gethash op-type *converter-features*)))
	  (when (null candidates)
	    (error "get-converter: Convertion pattern for ~a(version=~a) is not defined yet." op-type opset-version))

	  (let ((candidates (sort candidates #'> :key #'cdr)))
	    (loop for (impl . version) in candidates
		  if (<= version opset-version)
		    do (return-from get-converter impl)))
	  (error "get-converter: there's no implementation for ~a satisfying opset=~a.~%Candidates:~a"
		 op-type
		 opset-version
		 candidates)))
    (reload-and-retry-defop ()
      :report "Reload the (expected to be updated on REPL) converter and restart from the point in the error."
      (get-converter op-type opset-version))))
