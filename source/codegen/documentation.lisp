(defpackage :caten/codegen/documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/codegen/documentation)

(define-page ("caten/codegen" "packages/caten.codegen.md")
  (title "caten/codegen")
  (subtitle "Full Hackable Kernel Generator")
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
To enable JIT, you need to set the `JIT` contextvar before running the function `caten`. The following code snippet demonstrates how to enable JIT on repl:
```
;; Option1: Set globally
(setf (ctx:getenv :JIT) 1)
;; Option2: Set locally
(ctx:with-contextvar (:JIT 1)
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
JIT_DEBUG | Effect
------|-------
0     | None
1     | Nothing for now
2     | Displays blueprint and scop
3     | Displays schedule-graph
4     | Displays memory-planner and the compiled code
")
  (title "How to learn JIT")
  (body "You can start by reading `./caten/docs/getting-started.lisp` to understand the basic concepts of `caten/codegen` through a running example.

Additionally, the following sections provide documentation and explanations for each JIT mechanism, along with simple executable examples.")
  (title "Shape Inference")
  (body "The JIT compilation starts by inferring the shape of each points in the graph. `caten/codegen/shape-inference` is responsible for this task.")
  (doc/function "run-type-infer" #'caten/codegen/shape-inference:run-type-infer)
  (doc/struct "Inferred-Type" 'caten/codegen/shape-inference:Inferred-Type)
  (doc/struct "Iteration-Space" 'caten/codegen/shape-inference:Iteration-Space)
  ;; [TODO] Provide an example
  (title "Rewriting Rules")
  (body "TODO: Some patterns")

  (subtitle "Optimization for gemm")
  (body "TODO: WMMA Rewriting")
  
  (title "Scheduler")
  (doc/package 'caten/codegen/scheduler)
  (doc/function "graph-schedule" #'caten/codegen/scheduler:graph-schedule)
  (subtitle "Example (Scheduler + Shape Inference + Rewriting Rules")
  (body "This code snippet demonstrates how to create a schedule-graph from AASM Graph. AASM Graph is obtained by running caten with JIT=0.")
  (example-repl "(ctx:with-contextvar (:JIT 0) (pprint-graph (avm-graph (caten (!relu (!matmul (make-tensor `(3 3)) (make-tensor `(3 3))))))))")
  (example-code "
(let* ((graph (ctx:with-contextvar (:JIT 0) (avm-graph (caten (!relu (!matmul (make-tensor `(3 3)) (make-tensor `(3 3))))))))
       (vm (make-avm graph :example nil (graph-outputs graph) nil)))
  (run-type-infer vm) ;; Running the shape inference
  (apply-rewriting-rules vm) ;; Running the rewriting rules
  ;; graph-schedule to finally run the scheduler
  (pprint-graph (graph-schedule (avm-graph vm))))
")
  (title "Lowerer (Blueprint)")

  (title "Renderer")

  (title "EXPR")

  (title "Memory Planner")

  (title "Scop Analyzer")
  
  )
