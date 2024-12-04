(ql:quickload :caten)
(use-package :caten)

(in-package :caten/gguf)

(setf (ctx:getenv :JIT) 1)
(setf (ctx:getenv :JIT_DEBUG) 2)


(caten/gguf)

(defpackage :codegen-example
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm :caten/codegen/expr-cache)
  ;; Import some low-level apis
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules
   #:schedule-item-write-define-global)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/renderer
   #:CStyle-Renderer
   #:%render-kernel
   #:%compile-kernel)
  (:import-from
   :caten/codegen/jit
   #:schedule-graph->avm-graph))
(in-package :codegen-example)

;; Defining Some utils ...
(defun run-shape-inference (avm)
  (run-type-infer avm)
  (apply-rewriting-rules avm))

(defparameter *width* 120)

(defun saying (number title object)
  (dotimes (i *width*) (princ "="))
  (fresh-line)
  (format t "~a. ~a~%~a~%" number title object)
  object)


(defun create-simple-graph ()
    (with-context
        (x (%make-tensor `(3 3) :dtype :float32 :from 'x))  ; Tensor X
      (y (%make-tensor `(3 3) :dtype :float32 :from 'y))  ; Tensor Y
      (x (%mul x (%fconst 1.0)))
      (y (%xor y (%fconst 0.0)))
      (z (%add x y :id 'z)))
)

(print (create-simple-xor-expression))







(defun create-and-run-simple-kernel ()
  (let* ((graph
           (create-simple-graph))
         ;; Step 1: Wrap the graph into an AVM
         (vm (make-avm graph :simple-kernel nil (list 'z) nil))
         (*expr-cache* (make-expr-cache)))                      ; Initialize expression cache
    ;; Step 2: Compile and optimize the graph
    ;(optimize-aasm graph)
    (run-shape-inference vm)

    ;; Step 3: Generate the code and compile it
    (let ((schedule-graph (graph-schedule (avm-graph vm)))
          (renderer (make-instance 'CStyle-Renderer)))
      (dolist (item (graph-nodes schedule-graph))
        ;; Lower the items to be compatible with code generation
        (when (getattr item :jitable)
          (lower-schedule-item item (avm-graph vm) schedule-graph)
          (schedule-item-write-define-global item)
          (let ((c-kernel (%render-kernel renderer item)))
            (setf (getattr item :rendered-object) c-kernel))))
      ;; Compile the generated kernel
      (%compile-kernel renderer (graph-nodes schedule-graph) nil)
      ;; Update the AVM graph with the compiled code
      (setf (avm-graph vm) (schedule-graph->avm-graph (avm-graph vm) schedule-graph))
      (avm-reset vm))

    ;; Step 4: Run the kernel and print the result
    (print (%run vm (cons 'x (linspace `(3 3) 1 0)) (cons 'y (linspace `(3 3) 1 0))))))


(create-and-run-simple-kernel)



(caten (!rand '(3 3)))