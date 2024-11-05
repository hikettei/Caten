# Caten

- [ ] Short-term goal: Finish for ELS 2025

> **This repository is still in the early stages of development. Additionally, it includes many experimental approaches. Please consider this as a place to experiment with my ideas. Do not use it in a product under any circumstances.**

`Caten = Compile+AbstracTENsor`

something between tvm and tinygrad.

## Getting started

1. Install [Roswell](https://github.com/roswell/roswell) and suitable IDE. (If unsure, Emacs or [Lem](https://github.com/lem-project/lem) is recommended)
2. Install [ISL (Integer Set Library)](https://github.com/Meinersbur/isl) for the fast kernel generation.
3. Install [Qlot](https://github.com/fukamachi/qlot)
4. Check out [getting-started.lisp](./docs/getting-started.lisp)

```sh
$ git clone git@github.com:hikettei/Caten.git
$ cd Caten
$ qlot install
$ qlot exec ros run
> (ql:quickload :caten)
> (in-package :caten-user)
> (proceed (!randn `(3 3)))
```

## Overview

Caten is an extremely experimental deep learning compiler. The core design philosophy is centered around:

1. Minimal dependency libraries

2. Maintainable code with fewer lines (should be less than 10,000 lines excluding tests)

3. An easily extendable Renderer (like tinygrad) that can adapt to any device with minimal code

4. **Ahead-of-time** Compilation and Shape Inference

5. Usable as a command-line tool based on Roswell (of course, also available from Common Lisp, and Coalton in the future.)

6. A framework that significantly simplifies debugging (which is a full of pain for us) via a REPL-based approach.

Ultimately, Caten aims to generate runtime executables in C (based on our any-to-any renderer) and eliminate dependencies even on Common Lisp, thereby achieving high portability for inference. (This means that the implementation methods for `./source/apis/control-flow.lisp` elements like If and For need careful consideration.)

To achieve the above goals, Caten adopts a compiler-based approach. We do not write any kernels for matrix operations. Instead, we represent computations using a lazy evaluation approach and a fast, simple computational graph (Caten/AIR). We simplify the computational graph using a dedicated Pattern Matcher, and ultimately, optimized kernels for CPUs and GPUs are generated using a Polyhedral Compiler. At the same time, a VM for precision verification has been implemented, with JIT positioned as a subset for VM acceleration.

For the implementation of automatic differentiation and (in the future) quantization, it is crucial to facilitate easy rewriting of the computational graph. Caten's computation phase has multiple stages of Lowering, in which rewriting of the computational graph is possible at every stage using a Pattern Matcher. The smallest computational graph is a Func, declared in `./source/aasm/`. This graph is used ultimately for Rendering or VM execution, and the number of computations should be minimized through function composition. A Func can be obtained by Lowering a Module, which defines computations that can be composed (e.g., Matmul, Conv2D, Cos). For example, in cases where automatic differentiation cannot ensure computational accuracy, like with Sigmoid, it's possible to describe Backward at the Module level, as well as rewrite computations at the Module level.

The `Caten/AIR` graph used at runtime should be dumpable in a format compatible with Common Lisp. Ultimately, the computational graph used should be expressed solely as a list comprised of Number, Symbol, Keyword.

Nearly all of Caten's APIs support Symbolic Compilation, allowing for compilation with symbols instead of numbers where required, as in `(make-tensor '(a b c) :initial-element 'x)`. To implement this, Caten involves even the computation of Shape and Stride within the lazy evaluation graph (just like Numpy, Shape can be treated as fixed when the Pattern Matcher performs Constant Folding).

In the future, I would like to reimplement the frontend in Coalton.

## Project Structure (old)

Everything is a syntax sugar for ./source/air.

```
--- core ---------------------------------------------------------------------------
0 | ./source/air     | An optimized pattern matcher for DAG, the core of IR system.
1 | ./source/aasm    | Abstract Internal IR for aVM 
2 | ./source/avm     | Abstract+VM, an extensible simulator for aasm (but enough fast)
3 | ./source/ajit    | it lowers aasm into more lower irs, generating the kernel codes.
4 | ./source/meta    | Meta Programming Library (incl. Source Code Transformation)
--- frontend -----------------------------------------------------------------------
5 | ./source/lang    | a compiler from lisp to aIR (helping the complicated subscript notation like Conv/einsum) 
6 | ./source/apis    | Syntax sugar for core notations, including AbstractTensor, autodiff engine. (-> ./source/package.lisp?)
7 | ./source/nn      | a syntax sugar which implements nn ops and optimizers
--- external -----------------------------------------------------------------------
./external/avm/clang  | avm implementation using clang
./external/ajit/clang | ajit implementation using clang
...
------------------------------------------------------------------------------------
```

## Enable AOT Compilation

Please declare devices capable of AOT compilation in the environment variables AOT and AOT_VM.

```AOT_VM=LISP AOT_JIT=CLANG,LISP make test```

Afterwards, functions defined using the caten/defun macro will undergo AOT compilation. (Note that it does not analyze the source code, but traces the computational graph through Shape Tracker and lazy evaluation, so it does not behave exactly like defun).

```lisp
(caten/defun[float] (axpy! "axpy") (x y n froma toa bya fromb tob byb)
  (!add (!view (make-tensor `(,n) :from x) `(,froma ,toa ,bya)) (!view (make-tensor `(,n) :from y) `(,fromb ,tob ,byb)))) 
```

## Running the unittest

```sh
# either of
$ COVERAGE=1 rove caten.asd
$ make test
```

## Installing (WIP)

```sh
$ brew install isl
```

## Dependencies

- ISL
