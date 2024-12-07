# Caten

> **This repository is still in the early stages of development. Additionally, it includes many experimental approaches. Please consider this as a place to experiment with my ideas. Do not use it in a product under any circumstances.**

[![CI](https://github.com/hikettei/Caten/actions/workflows/tests_on_push.yml/badge.svg)](https://github.com/hikettei/Caten/actions/workflows/tests_on_push.yml)

`Caten = Compile+AbstracTENsor`

Caten is an experimental deep learning compiler. Our goal is to implement a compiler that is as simple as tinygrad, and as flexible as TVM.

## Getting Started

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

- **Transformer**
  - [x] GPT2 (but currently broken!)
  - [ ] Llama3 8B
  - [ ] TinyLLAMA
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

- [x] Common Lisp Frontend (caten/apis)
- [x] ONNX (caten/onnx)
- [x] GGUF (caten/gguf)

### Quantization

- [x] Support Dequantization from GGUF
- [ ] Support QOPs

### Training

- [x] Autodiff
- [ ] Fast Autodiff
- [ ] Support Training
- [ ] Distributed Training

### Accelerators

- [x] LISP VM
- [x] CLANG JIT
- [ ] CLANG with Auto Scheduler
- [ ] METAL
- [ ] CUDA
- [ ] Vulkan
- [ ] Auto Scheduler

## Running tests

You should install python, numpy, pytorch before running the test-suite by using `make install_extra`. If not specified, install the latest one. 

```sh
$ make install_extra # extra dependencies for running tests
$ make test
```

