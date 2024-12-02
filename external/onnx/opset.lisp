(in-package :caten/onnx)

(macrolet ((def-elwise (name version op)
	     `(defop (,name ,version)
		  ((gph inputs attrs)
		    (declare (ignore attrs))
		    (assert (= 2 (length inputs)) () "Assertion Failed: ~a expects binary ops but got ~a" ,name inputs)
                    (reduce #',op inputs)))))
  (def-elwise "Add" 1 caten:!add)
  (def-elwise "Sub" 1 caten:!sub)
  (def-elwise "Mul" 1 caten:!mul)
  (def-elwise "Div" 1 caten:!div))

