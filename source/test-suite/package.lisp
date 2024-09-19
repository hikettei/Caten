(in-package :cl-user)

(defpackage :caten/test-suite
  (:use :cl :rove
	:caten :caten/nn :caten/air :caten/aasm
	:caten/avm :caten/common.dtype :alexandria
	:caten/llm :py4cl))

(in-package :caten/test-suite)

(python-exec "import torch")
(import-module "numpy" :as "np")
(import-module "torch.nn.functional" :as "f")
(import-function "torch.from_numpy")
(import-function "list" :as "py.list")

(defun ->numpy (tensor &key (dtype "float32"))
  (declare (type tensor tensor))
  (assert (buffer-value (tensor-buffer tensor)) () "The tensor ~a is not realized yet!." tensor)
  (np:reshape
   (np:array
    (buffer-value (tensor-buffer tensor))
    :dtype
    dtype)
   (buffer-shape (tensor-buffer tensor))))

(defun ->torch (tensor &key (dtype "float32")) (remote-objects (torch.from_numpy (->numpy tensor :dtype dtype))))

(defun torch-shape (tensor) (remote-objects* (py.list (chain tensor (size)))))

(defun ->caten (tensor &key (dtype :float32))
  (let* ((size (coerce (torch-shape tensor) 'list))
 	 (buffer (remote-objects* (chain tensor (detach) (cpu) (flatten) (numpy))))
	 (buffer-out (make-buffer (length size) size (caten/apis::row-major-calc-strides size) dtype nil)))
    (setf (buffer-value buffer-out) buffer)
    (let ((new (make-tensor (coerce size 'list) :dtype dtype :order :row)))
      (setf (tensor-buffer new) buffer-out)
      (make-param #'(lambda (_) _ new) size :dtype dtype :order :row))))
      
