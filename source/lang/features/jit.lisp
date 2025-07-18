(in-package :caten/lang)
;; @caten.jit creates blueprint graph from ANSI Common Lisp Program
;; Also by translating C code into Common Lisp (using with-c-syntax) ultimately it allows to embody C code in Common Lisp
;; and get optimized kernel for any language
(define-caten-feature (jit :docstring "
```
@caten.jit(:style style :n-profile) {

}
```")
    ((ctx &key (style :lisp))
      
      ))

(progn
  @caten.jit (:style :lisp) {
  
  })
