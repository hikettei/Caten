(defpackage :caten/onnx.docs
  (:use :cl :caten/common.documentation))
(in-package :caten/onnx.docs)

(define-page ("caten/onnx" "packages/caten.external.onnx.md")
  (title "ONNX")
  (body "Looking to compile Caten models directly from ONNX files? The `caten/onnx` package has you covered. It supports dynamic shapes and weight loading, allowing for flexible and efficient model integration.

Currently, `caten/onnx` supports a limited set of operations. If you encounter any unsupported operations, we warmly welcome your contributions to expand its functionality. Please consider submitting a PR to add them! We'd like to support all opsets which is impossible!

Requirement: [cl-onnx](https://github.com/hikettei/cl-onnx/tree/main) requires [cl-protobufs](https://github.com/qitab/cl-protobufs), and cl-protobufs may require protocol-compiler to be installed on your system. (Follow the instruction in cl-protobufs first)

```
# Example
$ sudo apt-get install protobuf-compiler
```

")
  (doc/function "from-onnx" #'caten/onnx:from-onnx))
