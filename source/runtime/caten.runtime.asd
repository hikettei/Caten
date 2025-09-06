(asdf:defsystem "caten.runtime"
  :description ""
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.graph" "caten.ir" "closer-mop")
  :serial t
  :components ((:file "buffer")
               (:file "profile")
               (:file "runtime")
               (:file "kernel")
               (:file "bring-your-own-backend")
               (:file "package")))
;; [TODO] Renderer, BYOC Design
;; - [ ] bring-your-own-backend.lisp (defbackend)
;; - [ ] runtime.lisp
;; - [ ] kernel.lisp
;; - [ ] renderer.lisp
;; - [ ] buffer.lisp
;; - [ ] export.lisp
