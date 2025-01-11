# Caten

> **This repository is still in the early stages of development. Additionally, it includes many experimental approaches. Please consider this as a place to experiment with my ideas. Do not use it in a product under any circumstances.**

[![CI](https://github.com/hikettei/Caten/actions/workflows/tests_on_push.yml/badge.svg)](https://github.com/hikettei/Caten/actions/workflows/tests_on_push.yml) [![Benchmarks](https://github.com/hikettei/Caten/actions/workflows/benchmark.yml/badge.svg)](https://github.com/hikettei/Caten/actions/workflows/benchmark.yml) [![pages-build-deployment](https://github.com/hikettei/Caten/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/hikettei/Caten/actions/workflows/pages/pages-build-deployment) [![](https://dcbadge.limes.pink/api/server/tNawU7TN3s?style=flat)](https://discord.gg/tNawU7TN3s)

`Caten = Compile+AbstracTENsor`

Caten is an experimental deep learning compiler. Our goal is to create a solution that’s as simple as tinygrad yet as flexible as TVM—all while extending the possibilities of interactive programming into the realm of AI.

**We're looking for collaborators! Please join our Discord and let me know if you'd like to contribute!**

## Showcases

Caten is still under development, but it aims to support a wide range of models in the future—from image processing to text generation, and vision language models! Some models are already up and running.

### Examples

We have two doc files that explain how the Caten compilation pipeline works:

- [End-to-End Example](docs/end-to-end-example.lisp) Which how the end-to-end compilation pipeline works.
- [Getting Started](docs/getting-started.lisp) An intro to Caten.

### Running LLMs

```sh
$ BACKEND=CLANG PARALLEL=8 ./roswell/caten.ros llm-example --model "gpt2" --prompt "Hello" --max-length 100
```

Give the GPT2 demo a try! You can pass compilation settings through environment variables.

For example, setting `BACKEND=CLANG` enables JIT compilation, while `JIT_DEBUG >= 2` allows you to view the schedule and the generated kernels. Setting `PARALLEL=8` divides the ScheduleGraph and compiles it in parallel.

You may still find the token/ms rate slow, but we're not yet at the stage of implementing an AutoScheduler to accelerate kernel performance (as well as GPU support). Once our IR matures enough to handle a wide range of deep learning models, we plan to focus on speeding things up!

### Lazy Evaluation

Caten is capable of generating the necessary kernels independently!

Instead of relying on OpenBLAS bindings or hand-optimized CUDA kernels, Caten avoids abstractions that would restrict us to specific libraries.

Let’s take `Matmul+Activation` Fusion as an example to illustrate this approach:

```lisp
(in-package :caten-user)

(pprint-graph
  (tensor-graph (!relu (!matmul (make-tensor `(a b)) (make-tensor `(b c))))))
```

When you set `BACKEND=CLANG`, the graph is compiled to an external language. You can view the generated code by specifying `JIT_DEBUG >= 2`.

Give it a try in your REPL!

```lisp
(in-package :caten-user)
;; (setf (ctx:getenv :BACKEND) "CLANG") to set globally
(ctx:with-contextvar (:BACKEND "CLANG")
  (caten (!relu (!matmul (make-tensor `(a b)) (make-tensor `(b c))))))
```

We’ve adopted a RISC-style architecture. Ultimately, everything in Caten boils down to [just 26 composable primitive ops](https://github.com/hikettei/Caten/blob/main/source/aasm/attrs.lisp).

When you replace `tensor-graph` with `tensor-lowered-graph`, you’ll see exactly what we mean! And by using `->dot` instead of `pprint-graph`, you can visualize that graph right in your browser!

Finally, our lazy evaluation doesn’t make debugging any harder. If you want to check an intermediate result, just insert `proceed` at any point—it won’t break the computation graph!

```lisp
;; They are the equivalent
(proceed (!sin (!cos (ax+b `(3 3) 1 0))))
(proceed (!sin (proceed (!cos (ax+b `(3 3) 1 0)))))
```

### Training Models (Experimental)

```lisp
(in-package :caten-user)

(defsequence MLP (in-features hidden-dim out-features &key (activation #'!relu))
	     (Linear in-features hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim out-features))

(defun build-mlp-model ()
  (let* ((model (MLP 64 32 16))
         (outputs (call model (make-tensor `(b 64) :from :x)))
         (loss (!cross-entropy (!softmax outputs) (make-tensor `(b 16) :from :y)))
         (runner (caten loss)))
    (values runner (hook-optimizers runner (SGD :lr 1e-3)))))

(defun train ()
  (multiple-value-bind (runner optimizers) (build-mlp-model)
    (dotimes (i 10)
      (forward runner `(:x . ,(rand `(10 64))) `(:y . ,(rand `(10 16))) `(b . 10)) ;; replace with mnist dataloader
      (backward runner)
      (mapc #'step-optimizer optimizers)
      (mapc #'zero-grad optimizers))))
```

Though our focus is still on the inference, we will support training models. (Still Experimental, Unstable.) I am not sure our backward scheduler can be expanded into more large and complicated graphs. :(

## Getting Started

1. Install [Roswell](https://github.com/roswell/roswell) and a suitable IDE. (If unsure, Emacs or [Lem](https://github.com/lem-project/lem) is recommended)
2. Install [ISL (Integer Set Library)](https://libisl.sourceforge.io/) for the fast kernel generation.
3. If not already installed, then install [libyaml](https://github.com/yaml/libyaml) for YAML parsing and emitting.
4. Install [Qlot](https://github.com/fukamachi/qlot)
5. Check out [getting-started.lisp](./docs/getting-started.lisp)

```sh
$ git clone git@github.com:hikettei/Caten.git
$ cd Caten
$ qlot install
$ qlot exec ros run
> (ql:quickload :caten)
> (in-package :caten-user)
> (proceed (!randn `(3 3)))
```

## Get Involved

1. Join our [Discord Server](https://discord.gg/tNawU7TN3s).

2. Check out our [roadmap](https://github.com/users/hikettei/projects/2).

3. Create a PR

Caten is a project that started only a few months ago. We are currently in the stage of building a solid foundational library. Here’s what we’re looking for:

- Feature additions with tests (e.g., new activations, unimplemented matrix operations)

- Bug reports and additional tests.

- Refactoring of the core compiler components

- Improving the documentation

etc...

Before contributing, please note that there is no linter here. Make an effort to adhere to [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml). Changes that do not follow this should be rejected by the review.

## Roadmap

### Supported Models

- **Generative AI**
  - [x] GPT2
  - [ ] Llama3
  - [ ] TinyLLAMA
  - [ ] StableDiffusion
  - [ ] QwenVL2
- **Classification**
  - [x] MobileNetV2
  - [ ] MobileNetV3
  - [x] ResNet18/ResNet34/ResNet50
  - [ ] VIT_B_16
- **Segmentation**
  - [ ] CenterNet
- **Detection**
  - [ ] YoLOv3
  - [ ] YoLOv7

### Supported Formats

- [x] Common Lisp Frontend (caten/api)
- [x] ONNX (caten/onnx)
- [x] GGUF (caten/gguf)

### Quantization

- [x] Support Dequantization from GGUF
- [ ] Support QOPs

### Training

- [x] Autodiff
- [ ] Fast Autodiff
- [x] Support Training (But still limited)
- [ ] Distributed Training

### Accelerators (caten/byoc)

- [x] LISP VM (BACKEND=LISP)
- [x] LISP JIT (BACKEND=NATIVE)
- [x] CLANG JIT (BACKEND=CLANG)
- [x] METAL (BACKEND=METAL)
- [ ] WebGPU (BACKEND=WEBGPU)
- [ ] CUDA (BACKEND=CUDA)
- [ ] LLVM (BACKEND=LLVM)
- [ ] OpenCL (BACKEND=OPENCL)
- [ ] Finish AutoScheduler (Polyhedral Compiler + BEAM Search)

### Runtimes

- [x] LISP RUNTIME
- [ ] Exported Lisp Runtime (BACKEND=NATIVE)
- [ ] Exported to dylib (BACKEND=CLANG)
- [ ] JavaScript Runtime (BACKEND=WEBGPU)

## Running tests

You should install python, numpy, pytorch before running the test-suite by using `make install_extra`. If not specified, install the latest one. 

```sh
$ make install_extra # extra dependencies for running tests
$ make test
```

