
;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(ql:quickload :caten)

(defpackage :end-to-end
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm :caten/codegen/expr-cache)
  ;; Import some low-level apis
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
(in-package :end-to-end)


;;; 1. High level api

;;; In this example, we will show an end-to-end example of how the overall process of codegen works, starting from Caten's
;;; High Level Interface (caten/apis)

;;; Let's first define a class (similar to torch's module) for the function sin(cos(x))
;;; It will be the same class used in getting-started.lisp
(defclass SinCos (Func) nil
  (:documentation "The func SinCos computes sin(cos(x))"))

(defmethod forward ((op SinCos) &rest tensors) (st "A[~] -> A[~]" (tensors)))

(defmethod backward ((op SinCos) &optional prev-grad) (declare (ignore prev-grad)) nil)

(defmethod lower ((op SinCos) &rest inputs)
  (let ((x (car inputs)))
    (with-context
        (a (%sin (%add x (%fconst (/ pi 2)))))
      (b (%sin a)))))

;; You can create the next lazy tensor using forward method.
;; Let's define a function doing this as a utility named !sincos.
(defun !sincos (tensor)
  (forward (make-instance 'SinCos) tensor))

;;; Utils definition

(defparameter *width* 120)

(defun saying (number title object)
  (dotimes (i *width*) (princ "="))
  (fresh-line)
  (format t "~a. ~a~%~a~%" number title object)
  object)

;; First of all, let's intialize the module we just defined !sincos

(defparameter mytensor (make-tensor '(1) :initial-element 1.0))

(defparameter sincos (!sincos mytensor))

;; If we print, we will see what the contents of sincos are.

(print sincos)

;; It might surprise you that there is nothing, the reason is that Caten is lazy, we have to compile the expression into
;; An abstract syntax tree. In order to compile, we can use the caten function as follows:

;; 2. Intermediate Representation

(defparameter graph (caten sincos))

;;; This will generate an AVM graph as seen in the REPL output.
;;; The graph generated is an intermediate representation.
(print graph)

;;; We defined SinCos lower as:
;;;(a (%sin (%add x (%fconst (/ pi 2)))))
;;; (b (%sin a))))
;; and in the intermediate representation we can map the operations to:
;;; 
;;; <ALLOCATE : TID1421 <- (shape=(1), stride=(1)) ...> -> will prepare the space to load the value of 1.0
;;; <Node[BUFFER] LOAD(NID1436) : LID1435* <- (TID1421) where :value=1.0> -> the tensor value is loaded into the allocated space
;;; <Node[BUFFER] ALLOCATE(NID1439) : SID1438* <- () -> allocates space for the defined const / pi 2
;;; <Node[BUFFER] LOAD(NID1441) : LID1440* <- (SID1438) where :value=1.5707964> -> loads the constant into a buffer
;;; <Node[BINARYOPS] ADD(NID1443) : BID1442* <- (LID1435, LID1440)> -> performs the addition (%add x (%fconst (/ pi 2))
;;; <Node[UNARYOPS] SIN(NID1445) : UID1444* <- (BID1442)> -> applies the sin function to the previous operation (%sin (%add x (%fconst (/ pi 2))))
;;; <Node[UNARYOPS] SIN(NID1447) : UID1446* <- (UID1444)> -> applies sin again, now to a (b (%sin a)
;;; <Node[SPECIAL/VM] PAUSE/BACKWARD(NID1460) : STC1423_1* <- (UID1446)> -> synchronizes the graph, let's call it a sink node, the output node b


;;; This is it as far as the Intermediate Representation goes.

;;; 3. code/gen

;;; The goal of run-type-infer is to propagate the type and shape information through the graph
;;; Executes vm/forward over the graph to propagate the information forward.
;;; It simulates running the graph forward and backward to infer the types of the nodes.

(print (run-type-infer graph))

;;; The goal is to transform and rename parts of the graph to improve readability, consistency or prepare it for code gen and optimizations.
;;; It takes the raw avm graph, rewrites certain references, applies stable naming conventions and returns a more uniform and redable graph representation.

(print (apply-rewriting-rules graph))

;;  The resulting graph after run-type-infer and apply-rewriting-rules is the following:
;;  <ALLOCATE : val_0 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE ((1)) <- NIL> :_read_views=(NIL NIL) :_output_type=NIL>
;;   <Node[BUFFER] LOAD(NID1588) : val_1* <- (val_0) where :value=1.0 :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;   <Node[BUFFER] ALLOCATE(NID1591) : val_2* <- () where :nrank=0 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE (NIL) <- NIL>>
;;   <Node[BUFFER] LOAD(NID1593) : val_3* <- (val_2) where :value=1.5707964 :_type_relay=#<INFERRED-TYPE (NIL) <- (NIL)> :_read_views=(NIL)>
;;   <Node[BINARYOPS] ADD(NID1595) : val_4* <- (val_1, val_3) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1) NIL)> :_read_views=(NIL NIL)>
;;   <Node[UNARYOPS] SIN(NID1597) : val_5* <- (val_4) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;   <Node[UNARYOPS] SIN(NID1599) : val_6* <- (val_5) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;   <Node[SPECIAL/VM] PAUSE/BACKWARD(NID1612) : val_7* <- (val_6) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>

;; Compared to the initial compiled graph, variables are renamed to val_X. It makes the graph more human
;; Readable and easier to debug. It also ensures type inference information (_type_relay, INFERRED-TYPE)
;; is attached to the nodes of the graph and clear.
;; It simplifies the IR for debugging and inspection. To make it easier in the code gen step.


;; graph-schedule generates a fast graph from the avm-graph, which is a wrapper around the graph we had earlier.
;; 1. first of all, we wrap the graph around a avm-graph.
;; then the graph schedule function is called over it.
;; graph schedule does:
;; 1. creates a schedule context and calls graph-breath-first-schedule
;;  1.1 it first traverses the graph in a bfs (breath search firt) fashion
;;  1.2 groups nodes together into larger "fused" kernels (optimizations)
;;  1.3 distributes certain operations like load that makes resulting kernels simpler and more uniform.
;; 2. then we try to merge groups of nodes into bigger, more optimal kernels.
;;  2.1 if two parts of a graph share operations, and they can be combined without violating constraints,
;;  they are merged
;;  2.2 extra rewriting is applied so data shapes, broadcasting and reduction dimensions align properly.
;; 3. Finally the fast graph is generated. A fast graph is a more optimized data structure for the final scheduler graph, it assumes a Directed Acyclic Graph structure and uses hash-table-based lookup for nodes to speed up further operations.
;;   3.1 The final FastGraph is a cleaner, more optimized representation of the computation.
;;   3.2 It contains fewer more meaninful nodes, for instance, SIN operations are combined into a single kernel. It's also easier and more efficient to traverse and manupulate, which makes it suitable for code generation.
(defparameter scheduled-graph (graph-schedule (avm-graph graph)))


(print scheduled-graph)


;; FastGraph[seen=NIL, outputs=(val_7)] {
;; { Allocate } : [ val_0 <- (1) where lowered-p=nil ]
;; { Allocate } : [ val_2 <- NIL where lowered-p=nil ]
;; {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD1669]
;; {   VMOP   } : [ val_7 <- val_6 where lowered-p=nil :name=FUSED_BACKWARD1665]
;; }

;; In the FastGraph, the operations we had previously such as add, sin, have been replaced by a fused kernel
;; We had a sequence LOAD -> ADD -> SIN -> SIN that is converted into { KERNEL } : [ val_6 <- val_2, val_0 ... :name=FUSED_SIN_SIN_ADD_LOAD_LOAD1669]
;; The identifiers we had in the graph were converted to val_X: TID1421 (original) -> val_0 (rewritten)


(defparameter renderer (make-instance 'CStyle-Renderer))

(print renderer)

