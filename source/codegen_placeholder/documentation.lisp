(defpackage :caten/codegen/documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/codegen/documentation)

(define-page ("caten/codegen" "packages/caten.codegen.md")
  (title "caten/codegen")
  (subtitle "Hackable Kernel Generator")
  (body "
The `caten/codegen` is a system designed to JIT-compile AASM Graphs into other languages, such as C or Metal, to achieve faster computation.

This package has the following two objectives:

1. **Device Independence**: It imposes no constraints on the execution device, allowing users to extend it with only 200–300 lines of code.

2. **Performance**: It ensures computations run fairly fast on each target device.

Caten's code generation is divided into two stages based on the intended purpose:

1. **caten/codegen** Generates unoptimized code (e.g., Scheduler, Lowerer, op fusion, etc.).

2. **caten/polyhedral** __(Optional)__ Performs advanced kernel loop transformations and optimizations on the scheduled code, such as Auto Scheduler.

This document focuses on the first stage, `caten/codegen`, and provides related information and some examples.")

  (subtitle "How to enable JIT")
  (body "
To enable JIT, you need to feed the :BACKEND contextvar with the desired backend. (backends must be defined by `caten/codegen/backend:define-backend` and :JIT must be set to 1)

The following code snippet demonstrates how to enable JIT on repl:

```
;; Option1: Set globally
(setf (ctx:getenv :BACKEND) \"CLANG\")
;; Option2: Set locally
(ctx:with-contextvar (:BACKEND \"CLANG\")
  ;; ...
)
```
After setting the contextvar, you can run the function `caten` to JIT-compile the AASM Graphs, e.g.:
```
(caten (!rand `(3 3)))
```")
  (subtitle "How to debug JIT")
  (body "
You can set the `JIT_DEBUG` contextvar to debug the JIT compilation process. The debugging level ranges from 0 to 4, as shown below:
```
JIT_DEBUG | Effect
----------|-------
0         | None
1         | Nothing for now
2         | Displays blueprint and scop
3         | Displays schedule-graph
4         | Displays memory-planner and the compiled code
```
")
  (subtitle "How to learn JIT")
  (body "You can start by reading `./caten/docs/getting-started.lisp` to understand the basic concepts of `caten/codegen` through a running example.

Additionally, the following sections provide documentation and explanations for each JIT mechanism, along with simple executable examples.")
  (title "Shape Inference")
  (body "The JIT compilation starts by inferring the shape of each points in the graph. `caten/codegen/shape-inference` is responsible for this task.")
  (doc/function "run-type-infer" #'caten/codegen/shape-inference:run-type-infer)
  (doc/struct "Inferred-Type" 'caten/codegen/shape-inference:Inferred-Type)
  (doc/struct "Iteration-Space" 'caten/codegen/shape-inference:Iteration-Space)
  (title "Rewriting Rules")
  (body "TODO: apply-rewriting-rules doc")
  
  (subtitle "Optimization for gemm")
  (body "TODO: WMMA Rewriting Docs")
  
  (title "Scheduler")
  (doc/package 'caten/codegen/scheduler)
  (body (caten/air:node-build-documentation-by-class "Schedule-Item" :GRAPH))
  (doc/function "graph-schedule" #'caten/codegen/scheduler:graph-schedule)
  (subtitle "Example (Scheduler + Shape Inference + Rewriting Rules)")
  (body "This code snippet demonstrates how to create a schedule-graph from AASM Graph. AASM Graph is obtained by running caten with JIT=0.")
  (example-repl "(ctx:with-contextvar (:BACKEND \"LISP\") (pprint-graph (runtime-graph (caten (!relu (!matmul (make-tensor `(3 3)) (make-tensor `(3 3))))))))")
  (example-code "
(let* ((graph (ctx:with-contextvar (:BACKEND \"LISP\") (runtime-graph (caten (!relu (!matmul (make-tensor `(3 3)) (make-tensor `(3 3))))))))
       (vm (make-runtime graph :fw-outputs (graph-outputs graph))))
  (run-type-infer vm) ;; Running the shape inference
  (apply-rewriting-rules vm) ;; Running the rewriting rules
  ;; graph-schedule to finally run the scheduler
  (pprint-graph (graph-schedule (runtime-graph vm))))
")
  
  (title "Lowerer (Blueprint)")
  (doc/package 'caten/codegen/blueprint)
  (doc/function 'lower-schedule-item #'caten/codegen/blueprint:lower-schedule-item)

  (subtitle "Example (Scheduler + Lowerer)")
  (body "This code snippet demonstrates how to lower the schedule-graph into a blueprint created in the previous section.")
  (example-code "
(let* ((graph (ctx:with-contextvar (:BACKEND \"LISP\") (runtime-graph (caten (!relu (!matmul (make-tensor `(3 3)) (make-tensor `(3 3))))))))
       (vm (make-runtime graph :fw-outputs (graph-outputs graph))))
  (run-type-infer vm) ;; Running the shape inference
  (apply-rewriting-rules vm) ;; Running the rewriting rules
  ;; graph-schedule to finally run the scheduler
  (let ((schedule-graph (graph-schedule (runtime-graph vm))))
    (caten/codegen/expr-cache::with-expr-cache () ;; need this to forcibly keep the loop affine.
      (let ((bp (lower-schedule-item (nth 5 (graph-nodes schedule-graph)) graph schedule-graph)))
        (print-blueprint bp nil)))))")
  
  (title "Renderer")
  (body "TODO (Docs for Renderer/rendre-expr/extensible polyhedral compiler/etc)")
  (body "Here is a list of nodes used to render the kernel.")
  (body (caten/air:node-build-documentation-by-class "Render IRs" :Render))
  
  (title "EXPR")
  (body "TODO")
  
  (title "Memory Planner")
  (body "TODO: Docs (MIP Solver)")
  (title "Scop Analyzer")
  (body "TODO: Docs for scop"))
