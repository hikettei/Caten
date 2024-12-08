(in-package :caten-user)

(define-page ("Home" "index.md")
  (title "Home")
  (body "Welcome to the docs for Caten."))

(define-page ("Quickstart" "quickstart.md")
  (title "QuickStart")
  (subtitle "Install")
  (body "
Caten is not registered in quicklisp ...
```sh
$ git clone 
```
")
  )

(define-page ("Developer" "developer.md")
  (title "Intro")
  (subtitle "How Are We Attempting to Build DL Compiler Ecosystem in Common Lisp?")
  (body "We aim to create an ecosystem for a deep learning compiler called Caten on top of the mature and remarkable programming language Common Lisp (or Coalton!). Our ultimate goals include:

- Enabling interactive debugging interfaces (Such as Emacs+SLIME or LEM).

- Offering an inference runtime that operates with minimal dependencies across diverse environments (imagine `ros build ./roswell/caten.ros`, or generating pure C code).

- Keeping the overall compiler architecture simple enough for anyone to understand.

- Extending the compiler and implementing optimizations for various accelerators is straightforward.

- Achieving high performance for a wide range of models.

Modern deep learning compilers must meet numerous requirements. In particular, the following points currently do not align well with Common Lisp:

- The need for a mature, general-purpose scientific computing library (akin to NumPy).

- The requirement for this library to support various accelerators (Intel/AMD SIMD intrinsics, CUDA, Metal shaders, etc.).

Addressing these demands usually requires a large development team, which has not historically been a good fit for this language’s ecosystem.

However, we have a concept that can overturn these constraints: by defining a general-purpose IR (caten/air, caten/aasm) to represent the linear algebra operations used in deep learning, and then JIT-compiling this IR to various backends, we can solve these problems.

In conclusion, while we describe Caten as a deep learning compiler, the reality is that roughly 80% of its code is dedicated to this linear algebra compiler.

We envision several stages for this project:

- Stage 1: Mature the IR used for linear algebra (based on models like Llama3, ViT), generating kernels without optimization.
- Stage 2: Implement a polyhedral compiler (for automatic parallelization, vectorization, and hackable loop transformations).
- Stage 3: Expand to various backends (CUDA, Metal, Vulkan, etc.).
"
   )
  (subtitle "How it works?")
  (body "
Three phaeses

1. Tensor -> AASM

2. AASM -> Schedule Graph

3. Schedule Graph -> Kernel

```
(!relu (!matmul (make-tensor `(10 20)) (make-tensor `(20 30))))
```

->

```
(tensor-graph *)
```

->

or pasting (->dot *) image
```
[P=0, ID=0]:
   :GRAPH/RELU {0}
   └ :GRAPH/MATMUL {N1}
     ├ Allocate[:float32] (10 20)
     └ Allocate[:float32] (20 30)
```
"))
