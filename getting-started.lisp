;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :getting-started
  (:use :cl :caten/air :caten/aasm :caten/api :caten/runtime))

(in-package :getting-started)

;;; [Introduction]
;;; Welcome to Caten.
;;; Caten is a tensor library written in Common Lisp.
;;; Our ultimate goal is to generate a fast deep learning model inference runtime across a wide range of devices with minimal cost.
;;;
;;; We need contributors. This document was created to help new contributors to quickly understand the design of Caten.
;;; To learn how Caten works, you first need to understand the following 4 main components:

;;; - 1. caten/api     | High-Level Graph Interface (API)
;;; - 2. caten/air     | Low-Level  Graph Interface
;;; - 3. caten/codegen | AIR Graph => Kernel Generator
;;; - 4. caten/runtime | AIR Graph Runner + Buffer managements
;;; **All other systems are built on top of these packages.**


;;; ~~~[1. High Level Interface (caten/api)]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; The main role of `caten/api` is to provide a matrix operation API **with the same interface as Numpy/PyTorch**.
;;; Like Petalisp/tinygrad, Caten uses lazy evaluation.

;; For example, creating a 3x3 matrix initialized with 1.0 using make-tensor does not trigger any computation, that is what we call lazy evaluation.
(defparameter *tensor1* (make-tensor `(3 3) :initial-element 1.0))

(print *tensor1*) ;; Getting nothing! The tensor is not evaluated. This is called lazy evaluation.

;; If you want to see the result, you have to compile the kernel using the function `proceed`.
(print (proceed  *tensor1*))

;; Let's define another tensor for the next experiments

(defparameter *tensor2* (make-tensor `(3 3) :initial-element 1.0))

;; To execute a previously compiled graph without recompiling, create a `GraphRuntime` using the `caten` function, then execute it with forward.
;; The graph created consists of a matrix multiplication, passing two tensors
;; The caten function creates the low-level graph, that is a compiled (low level) version of the matmul as an AST
(print (caten (!matmul *tensor1* *tensor2*)))

;; this is being executed with forward
(print (forward (caten (!matmul *tensor1* *tensor2*))))

;; Proceed evaluates by computing the lower level AST and executes it with forward
(print (proceed (!matmul *tensor1* *tensor2*)))

;; Of course, Caten is designed so that all graphs can be compiled with dynamic shapes. There's no need to recompile every time the batch_size changes.

;;; The goal of `caten/api` is to prevent bugs by wrapping the low-level interface commands (described later) in a high-level API.
;;; You can use various optimizations by lowering the AST in `caten/api` into `caten/air`!
;;; => In the next section, we will learn about `caten/air`.


;;; ~~~[Low Level Interface (caten/air)]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; In general, a deep learning compiler is a program that lowers transformations and operations on a DAG (Directed Acyclic Graph).
;;; Caten implements a general-purpose DAG processing library called `caten/air`.
;;; Any data structure with a DAG structure in Caten's source code should be defined as a `caten/air:Graph`.

(print (make-node :BinaryOps :ADD (list 'a) (list 'b 'c) :reduction t))
(print (make-graph (make-node :BinaryOps :ADD (list 'a) (list 'b 'c))))

;;; Any function starting with `%` represents low level operations for the air node creation.
;;; By wrapping such graph constructions with the with-context macro, Caten automatically organizes them into an executable graph.
;;; - caten/runtime:realize-graph to run the graph in GraphRuntime
;;; - caten/aasm:->dot to open the graph in your browser

;; Example:
;; The graph defines a low level graph that performs addition, two constants are defined and added together
;; .dot requires graphdotviz!
(let ((graph
        (with-context
          (x (%fconst 1.0))
          (y (%fconst 2.0))
          (out (%add x y)))))
  (print graph)
  (print (realize-graph graph :buffer-type 'caten/byoc/lisp:LispBuffer))
  ;;(->dot graph)
  )

;;; - Nodes can be defined using defnode.
;;; - Pattern Matcher can be defined using `defsimplifier`
;;; - `caten/aasm` provides a set of instruction used in GraphRuntime


;;; ~~~[A Bridge between AIR and API]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; In this section, we will explain the mechanism of lowering the high-level interface(API) to the low-level interface(AIR Graph).

;; utility function for presentation
(defun present (&rest tensors)
  "Present compiles and executes the given tensor, then prints the result."
  (format t "~{~& =>~% ~A~}" (multiple-value-list (apply #'proceed tensors))))

;;; To lower `caten/api` to a Graph, implement the lower method in the computation graph `Func` of `Caten/api` and describe the lowered computation graph there.
;;; Let’s try defining a `Func` to compute `Sin(Cos(x))` as an example.
(defclass SinCos (Func) nil
  (:documentation "The func SinCos computes sin(cos(x))"))

;; Forward creates a lazy tensor for the next computation.
;; You can skip this process by using the `st` macro.
(defmethod forward ((op SinCos) &rest tensors) (st "A[~] -> A[~]" (tensors)))
;; Backward is optional (skipped this time)
(defmethod backward ((op SinCos) &optional prev-grad) (declare (ignore prev-grad)) nil)
;; Lower describes the lowered expression of `SinCos`
(defmethod lower ((op SinCos) &rest inputs)
  (let ((x (car inputs)))
    (with-context
      (a (%sin (%add x (%fconst (/ pi 2)))))
      (b (%sin a)))))

;; You can create the next lazy tensor using the forward method.
;; Let's define a function doing this as a utility named !sincos.
(defun !sincos (tensor)
  (forward (make-instance 'SinCos) tensor))

;; Calling !SinCos (Make sure that :op = SINCOS)
(print (!sincos (make-tensor `(3 3))))
;; Running SinCos
(present
 (!sincos (make-tensor `(3 3) :initial-element 1.0))) ;; = 0.51439524

;;; ~~~~[caten/codegen]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpackage :codegen-example
  (:use :cl :caten/air :caten/aasm :caten/api :caten/runtime :caten/codegen/expr-cache)
  ;; Import some low-level APIs
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules
   #:schedule-item-write-define-global)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/renderer
   #:CStyle-Renderer
   #:%render-kernel
   #:%compile-kernel)
  (:import-from
   :caten/codegen/jit
   #:schedule-graph->avm-graph))

(in-package :codegen-example)

;; Defining some utils ...
(defun run-shape-inference (runtime)
  (run-type-infer runtime)
  (apply-rewriting-rules runtime))

(defparameter *width* 120)

(defun saying (number title object)
  (dotimes (i *width*) (princ "="))
  (fresh-line)
  (format t "~a. ~a~%~a~%" number title object)
  object)

(defun try-codegen! (graph outputs)
  "Utility for visualizing the process of compiling the `graph`."
  (optimize-aasm graph)
  (let ((vm (make-runtime graph :fw-outputs outputs)))
    (fresh-line)
    (saying 1 "Compiling the following initial computation graph:" graph)
    (saying 2 "Created GraphRuntime with the computation graph:" vm)
    (run-shape-inference vm)
    (let ((schedule-graph (graph-schedule (runtime-graph vm)))
          (*expr-cache* (make-expr-cache))
          (renderer (make-instance 'CStyle-Renderer)))
      (saying 3 "Generated schedule-graph with the computation graph" schedule-graph)
      (dolist (item (graph-nodes schedule-graph))
        ;; If schedule-item was labelled as jitable, you can lower this
        (when (getattr item :jitable)
          (lower-schedule-item item (runtime-graph vm) schedule-graph)
          (saying 4 "Lowered schedule item to a blueprint suitable for code generation:" (print-blueprint (getattr item :blueprint) nil))
          (schedule-item-write-define-global item)
          (let ((c-kernel (%render-kernel renderer item)))
            (saying 5 "Generated C code from the blueprint:" c-kernel)
            (setf (getattr item :rendered-object) c-kernel))))
      ;; Invoking gcc ...
      (%compile-kernel renderer (graph-nodes schedule-graph) nil)
      ;; Overwrite the base graph with the compiled graph
      (let ((optimized-graph (schedule-graph->avm-graph (runtime-graph vm) schedule-graph)))
        (make-runtime optimized-graph :fw-outputs (graph-outputs schedule-graph) :buffer-type 'caten/byoc/lisp:LispBuffer)))))

;; Example 1. Lowering two matrices' addition into the C kernel.
(let* ((graph
         ;; This is a graph to compile: Z(3 3) = X(3 3) + Y(3 3)
         (with-context
           (x (%make-tensor `(3 3) :dtype :float32 :from 'x))
           (y (%make-tensor `(3 3) :dtype :float32 :from 'y))
           (c (%add x y :id 'z))))
       ;; Simplifying the input graph
       (_ (optimize-aasm graph))
       ;; Wrap the graph as an instance of GraphRuntime to manage allocations
       (vm (make-runtime graph :fw-outputs (list 'z))))
  (declare (ignore _))
  (fresh-line)
  (saying 1 "Compiling the following initial
 computation graph:" graph)
  (saying 2 "Created GraphRunner with the computation graph:" vm)
  ;; caten/codegen requires the shape of all computation nodes to be known!
  (run-shape-inference vm)
  ;; Ready for running the scheduler. `graph-schedule` to partition the input graph.
  ;; Pass the wrapped runtime graph into the `graph-schedule` which divides the graph into smaller units called `schedule-items`
  (let ((schedule-graph (graph-schedule (runtime-graph vm)))
        (*expr-cache* (make-expr-cache))
        (renderer (make-instance 'CStyle-Renderer)))
    (saying 3 "Generated schedule-graph with the computation graph" schedule-graph)
    ;; After the `schedule-items` are created, the items are lowered into blueprint suitable code for code-generation
    (dolist (item (graph-nodes schedule-graph))
      ;; If schedule-item was labelled as jitable, you can lower this
      (when (getattr item :jitable)
        ;;Each item from the AST is lowered using the lower-schedule-item function
        (lower-schedule-item item (runtime-graph vm) schedule-graph)
        (saying 4 "Lowered schedule item to a blueprint suitable for code generation:" (print-blueprint (getattr item :blueprint) nil))
        (schedule-item-write-define-global item)
        ;; With the blueprint code, a C kernel is generated
        (let ((c-kernel (%render-kernel renderer item)))
          (saying 5 "Generated C code from the blueprint:" c-kernel)
          (setf (getattr item :rendered-object) c-kernel))))
    ;; Invoking gcc ...
    (%compile-kernel renderer (graph-nodes schedule-graph) nil)
    ;; Overwrite the base graph with compiled graph
    (let* ((optimized-graph (schedule-graph->avm-graph (runtime-graph vm) schedule-graph))
           (vm (make-runtime optimized-graph :fw-outputs (graph-outputs schedule-graph) :buffer-type 'caten/byoc/lisp:LispBuffer)))
      ;; Try axpy!
      ;; Finally, the C code is executed
      (saying
       6 "Running the computation X(3x3) + Y(3x3), the result is:"
       (%run vm `(x . ,(linspace `(3 3) 1 0)) `(y . ,(linspace `(3 3) 1 0)))))))

;; Example 2. Lowering two squared matrix multiplying into the C kernel.
(defun %make-squared-gemm (N out)
  "a @ b.T"
  (optimize-aasm
   (with-context
     (a  (%make-tensor `(,n ,n)))
     (b  (%make-tensor `(,n ,n)))
     (a1 (%view a `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil t nil) (%stride `(,n 1 ,n) :row)))
     (b1 (%view b `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(t nil nil) (%stride `(1 ,n ,n) :row)))
     (o  (%mul a1 b1))
     (c  (%load (%make-tensor `(,n ,n 1)) 0.0))
     (c  (%view c `(,n ,n 1) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil nil t) (%stride `(,n ,n 1) :row)))
     (c  (%add c o :reduction t))
     (c  (%reshape c `(,n ,n) :id out)))))
;; Try matmul
(try-codegen!
 (%make-squared-gemm 128 'out)
 (list 'out))

#+(or nil)
(try-codegen!
 (make-graph
  ;; your code follows ...
  )
 (list ... ))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Caten provides APIs that use the aforementioned JIT by default.
;;; - Set JIT=1 to enable it.
;;; - By setting JIT_DEBUG>=2, you can access a debugger similar to the previous one.
(defun present-beautiful (tensor)
  (ctx:with-contextvar (:BACKEND "clang" :JIT_DEBUG 3)
    (with-no-grad (caten tensor))))

;; auto-scheduler is WIP as of this writing!
(present-beautiful
 (!matmul (make-tensor `(128 512)) (make-tensor `(512 1024))))

(present-beautiful
 (!matmul (make-tensor `(a b)) (make-tensor `(b c))))

(present-beautiful
 (forward (caten/nn:Embedding 10 20) (make-tensor `(b 20))))
