;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :end-to-end
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

(in-package :end-to-end)

;; configuration => backend = :lisp
(setf (ctx:getenv :BACKEND) "LISP") ;; Ensure you can see the raw input graph by disabling JIT

;;; 1. High-Level API

;;; In this example, we will show an end-to-end example of how the overall process of code generation works, starting from Caten's
;;; High-Level Interface (caten/api)

;;; Let's first define a class (similar to torch.autograd.Function) for the function sin(cos(x))
;;; It will be the same class used in getting-started.lisp
(defclass SinCos (Func) nil
  (:documentation "The func SinCos computes sin(cos(x))"))

(defmethod forward ((op SinCos) &rest tensors)
  ;; Defines the forward pass computation: sin(cos(x))
  (st "A[~] -> A[~]" (tensors)))

(defmethod backward ((op SinCos) &optional prev-grad)
  ;; Defines the backward pass (gradient computation), ignored in this example
  (declare (ignore prev-grad))
  nil)

(defmethod lower ((op SinCos) &rest inputs)
  ;; Lowers the high-level operation into the intermediate representation
  (let ((x (car inputs)))
    (with-context
      (a (%sin (%add x (%fconst (/ pi 2)))))
      (b (%sin a)))))

;;; You can create the next lazy tensor using the forward method.
;;; Let's define a function doing this as a utility named !sincos.
(defun !sincos (tensor)
  (forward (make-instance 'SinCos) tensor))

;;; First of all, let's initialize the module we just defined !sincos
(defparameter *mytensor* (make-tensor '(1) :initial-element 1.0))

(defparameter *sincos* (!sincos *mytensor*))

;;; If we print, we will see what the contents of sincos are.
(print *sincos*)
;;; It might surprise you that there is nothing, the reason is that Caten is lazy,
;;; i.e., using lazy evaluation. Therefore, we have to compile the expression into
;;; an abstract syntax tree. In order to compile, we can use the `caten` function:

;;; 2. Intermediate Representation
(defparameter *graph* (caten *sincos*))

;;; This will generate a runtime graph as seen in the REPL output.
;;; The graph generated is an intermediate representation.
(print *graph*)

;;; We defined SinCos lower as:
;;; (a (%sin (%add x (%fconst (/ pi 2)))))
;;; (b (%sin a))
;;; and in the intermediate representation we can map the operations to:
;;; 
;;; #<GRAPHRUNTIME {nil -> ((stc6914095_1), nil)}
;;;     tid6914093 = allocate(shape=(1), stride=(1));
;;;     lid6914106 = load(tid6914093);
;;;     sid6914109 = allocate(shape=(), stride=());
;;;     lid6914111 = load(sid6914109);
;;;     bid6914113 = add(lid6914106, lid6914111);
;;;     uid6914115 = sin(bid6914113);
;;;     uid6914117 = sin(uid6914115);
;;;     stc6914095_1 = pause/backward(uid6914117);
;;; > 
;;; This is it as far as the Intermediate Representation goes.

;;; 3. Code Generation

;;; The goal of run-type-infer is to propagate the type and shape information through the graph
;;; Executes vm/forward over the graph to propagate the information forward.
;;; It simulates running the graph forward and backward to infer the types of the nodes.
(print (run-type-infer *graph*))

;;; The goal is to transform and rename parts of the graph to improve readability, consistency or prepare it for code generation and optimizations.
;;; It takes the raw runtime graph, rewrites certain references, applies stable naming conventions and returns a more uniform and readable graph representation.
(print (apply-rewriting-rules *graph*))

;;; The resulting graph after run-type-infer and apply-rewriting-rules is the following:
;;; #<GRAPHRUNTIME {nil -> ((val_7), nil)}
;;;     val_0 = allocate(shape=(1), stride=(1));
;;;     val_1 = load(val_0);
;;;     val_2 = allocate(shape=(), stride=());
;;;     val_3 = load(val_2);
;;;     val_4 = add(val_1, val_3);
;;;     val_5 = sin(val_4);
;;;     val_6 = sin(val_5);
;;;     val_7 = pause/backward(val_6);
;;; >

;;; Compared to the initial compiled graph, variables are renamed to val_X. It makes the graph more human
;;; readable and easier to debug. It also ensures type inference information (_type_relay, INFERRED-TYPE)
(print (runtime-graph *graph*)) ;; To see full attributes
;;; is attached to the nodes of the graph and clear.
;;; It simplifies the IR for debugging and inspection. To make it easier in the code generation step.

;;; graph-schedule generates a fast graph from the runtime-graph, which is a wrapper around the graph we had earlier.
;;; 1. First of all, we wrap the graph around a runtime graph.
;;; 2. Then the graph-schedule function is called over it.
;;;    graph-schedule does:
;;;    1. Creates a schedule context and calls graph-breadth-first-schedule
;;;       1.1 It first traverses the graph in a BFS (breadth-first) fashion
;;;       1.2 Groups nodes together into larger "fused" kernels (optimizations)
;;;       1.3 Distributes certain operations like load that makes resulting kernels simpler and more uniform.
;;;    2. Then we try to merge groups of nodes into bigger, more optimal kernels.
;;;       2.1 If two parts of a graph share operations, and they can be combined without violating constraints,
;;;           they are merged
;;;       2.2 Extra rewriting is applied so data shapes, broadcasting and reduction dimensions align properly.
;;;    3. Finally, the fast graph is generated. A fast graph is a more optimized data structure for the final scheduler graph,
;;;       it assumes a Directed Acyclic Graph structure and uses hash-table-based lookup for nodes to speed up further operations.
;;;       3.1 The final FastGraph is a cleaner, more optimized representation of the computation.
;;;       3.2 It contains fewer, more meaningful nodes. For instance, SIN operations are combined into a single kernel.
;;;           It's also easier and more efficient to traverse and manipulate, which makes it suitable for code generation.

(defparameter *scheduled-graph* (graph-schedule (runtime-graph *graph*)))

(print *scheduled-graph*)
;;;FastGraph[seen=NIL, outputs=(val_7)] {
;;;     { Allocate } : [ val_0 <- (1) where lowered-p=nil ]
;;;     { Allocate } : [ val_2 <- NIL where lowered-p=nil ]
;;;     {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD6914144]
;;;     {   VMOP   } : [ val_7 <- val_6 where lowered-p=nil :name=FUSED_BACKWARD6914140]
;;; }

;;; In the FastGraph, the operations we had previously such as add, sin, have been replaced by a fused kernel
;;; We had a sequence LOAD -> ADD -> SIN -> SIN that is converted into { KERNEL } : [ val_6 <- val_2, val_0 ... :name=FUSED_SIN_SIN_ADD_LOAD_LOAD1669]
;;; The identifiers we had in the graph were converted to val_X: TID1421 (original) -> val_0 (rewritten)

;;; Now we can extract each graph node from the FastGraph, this is where it gets really interesting, and where the actual code generation happens

;;; First we extract the nodes from the FastGraph we had previously created
;;; graph-nodes is a method from FastGraph, it returns a list of unique nodes from FastGraph.
(defparameter *list-of-graph-nodes* (graph-nodes *scheduled-graph*))

(print *list-of-graph-nodes*)
;;; ({   VMOP   } : [ val_7 <- val_6 where lowered-p=nil :name=FUSED_BACKWARD6914140]
;;;  { Allocate } : [ val_2 <- NIL where lowered-p=nil ]
;;;  {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD6914144]
;;;  { Allocate } : [ val_0 <- (1) where lowered-p=nil ]) 

;;; Here we test whether an item of the FastGraph is jitable or not. Jitable items are the ones lowered.
(dolist (item *list-of-graph-nodes*)
  (print item)
  (format t "~A~%" (getattr item :jitable)))

;;; Jitable items (which at this point we can call them schedule-items) are items in the computation graph that can be just-in-time compiled.

;;; JIT compilation is the process of translating intermediate representations into lower-level optimized machine code (in this case blueprint code and later C kernels).

;;; A schedule-item that is jitable meets the criteria for being compiled into a kernel or blueprint suitable code.

;;; In this case, the only schedule-item that has jitable set to true is the fused kernel!
;;; {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD964] T

;;; Now, we can take the schedule-item and lower it
;;; Retrieve the second node in the list of graph nodes
(defparameter *item1* (nth 2 *list-of-graph-nodes*))

;;; Print the node to see what it is before lowering.
;;; At this stage, `item1` is a scheduled graph node—likely a kernel node 
;;; that can be lowered into a blueprint.
(print *item1*)

;;; What does lower-schedule-item do?
;;; ---------------------------------
;;; `lower-schedule-item` takes a Schedule-Item node and:
;;; 1. Checks if it is jitable (i.e., suitable for JIT compilation).
;;; 2. If yes, it performs a process called 'lowering':
;;;    - Extracts the computation graph within this schedule-item.
;;;    - Determines iteration spaces and common loop structures for the operations inside it.
;;;    - Converts the fused operations into a low-level blueprint representation that shows all loops,
;;;      array indices, arithmetic operations, and potential reductions.
;;;    - This blueprint is then suitable for final code generation (e.g., translating into C code).
;;; In other words, `lower-schedule-item` turns a high-level fused kernel node into a fully described,
;;; low-level intermediate form (the blueprint) that can be compiled into efficient machine code or C code.

;;; Here we lower `item1` using the `lower-schedule-item` function. The `base-graph` is given by `(runtime-graph graph)`,
;;; and `scheduled-graph` is the optimized graph after scheduling. This will mutate `item1` so that it
;;; now has a `:blueprint` attribute containing the lowered form.
(defparameter *lowered-item1* (lower-schedule-item *item1* (runtime-graph *graph*) *scheduled-graph*))

;;; After this, `lowered-item1` (which should be the same as `item1` but now lowered)
;;; will hold the blueprint. Printing it might show a node with `:expr` indicating
;;; a fully lowered representation of the computation (e.g., `sin(sin(x + pi/2))`).
(print *lowered-item1*)
;;; (<Node[JIT] EXPR(NID6914465) : val_6* <- (1.5707964, val_0) where :expr=#<EXPR sin(sin((1.0+1.5707964)))> :_type_relay=#<INFERRED-TYPE ((1)) <- (NIL (1))>>)
;;; The :expr attribute describes the blueprint code represented as a low-level computation
;;; print-blueprint allows us to see the blueprinted code before the code-gen step
(print (print-blueprint (getattr *item1* :blueprint) nil))
;;; Blueprint code generated by print-blueprint
;;; {
;;;   val_6[0] = sin(sin((1.0+1.5707964)));
;;; }

;;; Now after all these intermediate steps, it's time to generate the actual kernel!
;;; The function below makes sure parameters and other necessary variables are properly initialized and declared.
(schedule-item-write-define-global *item1*)

;;; Create a renderer instance
(defparameter *renderer* (make-instance 'CStyle-Renderer))

(print *renderer*)

;;; render-kernel is responsible for taking the previously generated C code for all jitable items, compiling it into a shared library or native code, and then making the resulting functions callable from Lisp.
(let ((c-kernel (%render-kernel *renderer* *item1*)))
  (format t "Generated C code:~%~a~%" c-kernel)
  (setf (getattr *item1* :rendered-object) c-kernel))

;;; We can visualize the generated kernel for our high-level function we defined previously, called sincos!
;;; Generated C code:
;;; void fused_sin_sin_add_load_load6914144(float* val_6);
;;; void fused_sin_sin_add_load_load6914144(float* val_6) {
;;;  val_6[0] = sin(sin((1.0+1.5707964)));
;;; }

;;; %compile-kernel transforms the high-level Lisp IR (intermediate representation) into optimized native code, loads that code, and provides a way for Lisp to call it directly—completing the JIT compilation pipeline

;;; We make a copy of the original graph without modifications
(defparameter *original-graph* (copy-graph (runtime-graph *graph*)))

(ctx:with-contextvar (:BACKEND "clang" :JIT_DEBUG 4)
  ;; Calling gcc to compile the generated kernel
  (%compile-kernel *renderer* (graph-nodes *scheduled-graph*) nil))
  
;;; Replace the compiled kernels in the graph
(defparameter *compiled-graph* (schedule-graph->avm-graph (runtime-graph *graph*) *scheduled-graph*))

(print *compiled-graph*)

;;; The schedule-graph->avm-graph function integrates the compiled kernels.

;;; Graph[seen=NIL, outputs=NIL] {
;;;    <ALLOCATE : val_0 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=NIL :_read_views=NIL :_output_type=NIL>
;;;    <ALLOCATE : val_6 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=NIL :_read_views=NIL :_output_type=NIL>
;;;    <Node[JIT] JIT_KERNEL(NID6914553) : val_6* <- (val_6) where :output-buffer-n=1 :kernel-info=<LISP[FUSED_SIN_SIN_ADD_LOAD_LOAD6914144]> :dtypes=(FLOAT32)>
;;;    <Node[SPECIAL/VM] PAUSE/BACKWARD(NID6914131) : val_7* <- (val_6)>
;;;    <Node[BUFFER] ALLOCATE(NID6914110) : val_2* <- () where :nrank=0 :dtype=FLOAT32>
;;; }

;;; Now we can create an optimized GraphRuntime for the *compiled-graph* and run it with the input tensor.
(defparameter *optimized-runtime* (make-runtime *compiled-graph* :fw-outputs (graph-outputs *scheduled-graph*) :buffer-type 'caten/byoc/lisp:LispBuffer))
(print *optimized-runtime*)
;;; #<GRAPHRUNTIME {nil -> ((val_7), nil)}
;;;    val_6 = allocate(shape=(1), stride=(1));
;;;    val_6 = fused_sin_sin_add_load_load6914144(val_6);
;;;    val_7 = pause/backward(val_6);
;;; > 

;;; Finally, we can run the lower-level graph with the replaced nodes!

;;; Run the computation using %run
(print (%run *optimized-runtime* (cons 'val_0 1.0)))
;;; Expected Output:
;;; The result of sin(sin(1.0 + π/2)) is printed, computed using the optimized, compiled kernel.

;;; 4. Execution Explanation

;;; The `%run` method orchestrates the execution of the compiled kernel within the runtime context. Here's how it works:


;;; (defmethod %run ((runtime GraphRuntime) &rest params)
;;;  (let ((params (loop for (key . val) in params collect (cons key (if (tensor-p val) (tensor-buffer val) val)))))
;;;    (runtime-set-params runtime params)
;;;    (runtime-forward runtime)
;;;    (runtime-sync-tensors runtime)
;;;    (flet ((ap (x) (gethash x (runtime-variables runtime))))
;;;      (apply #'values (map 'list #'ap (runtime-fw-outputs runtime))))))

;;;
;;; 5. Final Summary

;;; In this end-to-end tutorial, we've explored the complete pipeline of defining, compiling, and executing a computation graph using Caten's GraphRuntime. Here's what we've covered:

;;; 1. **High-Level Definition:**
;;;    - Defined a `SinCos` function that computes `sin(cos(x))` using a high-level class structure similar to modules in deep learning frameworks.

;;; 2. **Graph Construction:**
;;;    - Created a tensor and instantiated the `SinCos` function, resulting in a lazy tensor representation.
;;;    - Generated an intermediate representation (IR) graph using the `caten` function, which details each computation step.

;;; 3. **Type Inference and Rewriting:**
;;;    - Ran type inference to propagate type and shape information through the graph.
;;;    - Applied rewriting rules to rename variables and prepare the graph for optimization and code generation.

;;; 4. **Graph Scheduling and Optimization:**
;;;    - Scheduled the graph to identify and fuse operations into optimized kernels.
;;;    - Generated a FastGraph that consolidates operations like `ADD` and `SIN` into a single `JIT_KERNEL` node for efficiency.

;;; 5. **Kernel Lowering and Compilation:**
;;;    - Lowered the scheduled kernel into a blueprint, a low-level representation suitable for code generation.
;;;    - Rendered the blueprint into C code and compiled it into a native kernel using `%render-kernel` and `%compile-kernel`.
;;;    - Integrated the compiled kernel back into the runtime graph using `schedule-graph->avm-graph`, replacing high-level nodes with the optimized kernel.

;;; 6. **Execution with GraphRuntime:**
;;;    - Executed the computation graph using the `%run` method, which set input parameters, ran the forward pass, synchronized tensors, and retrieved the results.
;;;    - Leveraged the compiled kernel for efficient computation, resulting in the final output of `sin(sin(1.0 + π/2))`.
