(in-package :caten/onnx)

(defun from-model-proto (model-proto &key (opset) &aux (cl-onnx::*visualize* nil))
  "
```
(from-model-proto model-proto &key opset)
```

Converts a cl-onnx model proto into an equivalent caten IR.

- opset[fixnum or null] specifies the opset version to use. If not specified, uses the opset version in the model proto.
"
  (declare (type Model-Proto model-proto))
  (let ((opset-in-model 1))
    (block detect-opsets
      (dolist (opset-id (model-proto-opset-import model-proto))
	(with-slots ((version cl-onnx::version) (domain cl-onnx::domain)) opset-id
	  ;; As per https://github.com/onnx/onnx/blob/main/docs/IR.md
	  ;; All operator sets except the default one must specify the operator version
	  (when (or (string= domain "") (string= domain "ai.onnx"))
	    (setf opset-in-model version)
	    (return-from detect-opsets)))))
    (when (null opset) (setf opset opset-in-model))
    
    (when (< opset opset-in-model)
      (warn "You are overwritting the original opset ver = ~a with lower version = ~a.~%This might cause model conversion errors." opset-in-model opset))
    (graph-proto-helper->onnx (make-graph-proto-helper (model-proto-graph model-proto) opset))))

(defun from-onnx (path &key (opset))
  "
```
(from-onnx path &key opset)
```
Constructs a caten ir from an onnx model file at `path`. opset specifies the opset version to use. If not specified, uses the opset version in the file."
  (from-model-proto (cl-onnx:load-model path) :opset opset))
