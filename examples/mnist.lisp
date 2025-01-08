;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; An end-to-end MNIST Example

(unless (find-package :caten)
  (ql:quickload :caten))

(unless (find-package :numpy-file-format)
  (ql:quickload :numpy-file-format))

(defpackage :mnist-example
  (:use :cl :caten :caten/nn :numpy-file-format))

(in-package :mnist-example)
;; Note: we assume that we have already created a dataset from MNIST using the following scripts:
;; - ./mnist_data/train_data.py
;; Set BACKEND=CLANG in order to enable JIT, and JIT_DEBUG=2 to see what's going on.
(setf (ctx:getenv :BACKEND) "CLANG" (ctx:getenv :JIT_DEBUG) 2)

(defun load-npy (path) (change-facet (load-array path) :tensor))
(defun scale (tensor) (proceed (!reshape (!div tensor (fconst 255.0)) (nth 0 (shape tensor)) (* 28 28))))

;; Loading and preprocessing the dataset. We assume M-x slime-cd to be the root of the project.
(defparameter *train-data* (scale (load-npy "./examples/mnist_data/train_data.npy")))
(defparameter *train-label* (load-npy "./examples/mnist_data/train_label.npy"))
(defparameter *test-data* (scale (load-npy "./examples/mnist_data/test_data.npy")))
(defparameter *test-label* (load-npy "./examples/mnist_data/test_label.npy"))
;; Defines the model to train
(defsequence MLP (in-features hidden-dim out-features &key (activation #'!relu))
	     (Linear in-features hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim out-features))

(defun window (tensor &aux (from (iconst 'from)) (to (iconst 'to)))
  "Creates a zero-cost offset in the batch-size dimension of a tensor.
Tensor: [==XXXXXXXXXXX==================]
           ^ from    ^ to
X: Visible Elements, =: Invisible Elements"
  (!view tensor `(,from ,to) t))

(defun build-mlp-trainer (train-data train-label)
  (let* ((model (MLP 784 512 10))
         (outputs (call model (window train-data)))
         (loss (!cross-entropy (!softmax outputs) (window train-label)))
         (runner (caten loss)))
    (values model runner (hook-optimizers runner (SGD :lr 1e-3)) loss)))

(defun build-mlp-validator (model test-data test-label)
  (let* ((predicted (!argmax (call model (window test-data))))
         (expected  (!argmax test-label)))
    (!mean (!where (!eq predicted expected) (fconst 1.0) (fconst 0.0)))))

(multiple-value-bind (model runner optimizer loss)
    (build-mlp-trainer *train-data* *train-label*)
  (defparameter *model* model) ;; Model to train
  (defparameter *runner* runner) ;; A graph runner for the compiled model
  (defparameter *optimizer* optimizer) ;; A list of optimizers
  (defparameter *loss* loss)) ;; the last tensor

(caten/air:pprint-graph (tensor-graph *loss*))         ;; To see the network architecture in the REPL
(caten/air:pprint-graph (tensor-lowered-graph *loss*)) ;; To see the lowered graph in the REPL
(caten/air:->dot (tensor-graph *loss*))                ;; To see the network architecture in the REPL (graphviz is required)

(defun run-epoch (runner optimizers &key (batch-size 100) (data-size (nth 0 (shape *train-data*))))
  (loop for from upfrom 0 below data-size by batch-size
        for to = (min data-size (+ from batch-size)) do
          (format t "from=~a, to=~a~%" from to)
          (forward runner 'from from 'to to) ;; Without AutoScheduler it is ridiculously slow. Currently not very usable
          (backward runner)
          (mapc #'step-optimizer optimizers)
          (mapc #'zero-grad optimizers)))

;; (run-epoch *runner* *optimizer*)
