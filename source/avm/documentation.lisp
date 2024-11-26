(in-package :caten/avm)

(docs:define-page ("caten/avm" "packages/caten.avm.md")
  (docs:title "caten/avm")
  (docs:body "
The package `caten/avm` provides a runtime environment for executing graphs obtained by lowering tensors or by further applying JIT compilation.

The main data structures provided are:

- AVM: A wrapper around a graph that adds memory management information.

- Buffer: Stores matrix data along with its shape, stride, data type, and other attributes.

")
  (docs:doc/struct "AVM" 'AVM)
  (docs:doc/struct "Buffer" 'Buffer)
  )
