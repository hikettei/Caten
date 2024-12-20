(defpackage :caten/codegen/auto-scheduler/measurer
  (:documentation "Profiles the performance of the generated code.")
  (:use :cl :caten/avm :caten/air :caten/codegen/renderer :caten/aasm)
  (:export
   #:make-profiler-from-schedule-item))

(in-package :caten/codegen/auto-scheduler/measurer)

(defstruct Profiler
  (jit-kernel nil)
  (shapes nil)
  (buffers nil))

(defun device-tmp-buffer (exec-device buffer parameters)
  (let ((buffer (copy-buffer buffer)))
    (flet ((->id (sym) (if (symbolp sym) (or (gethash sym parameters) 

(defmethod profile-get-exectime ((profiler Profiler) exec-device parameters)
  (let ((args (loop for b in (profiler-buffers profiler)
                    if (consp b)
                      collect (or (gethash (car b) parameters) (error "profile-get-exectime: Missing parameter ~a" b))
                    else
                      collect (device-tmp-buffer exec-device b parameters))))

    ))

(defmethod make-profiler-from-schedule-item ((node Node) renderer)
  (assert (eql (node-type node) :Schedule-Item))
  (caten/codegen/rewriting-rules:schedule-item-write-define-global node)
  (setf (getattr node :rendered-object) (%render-kernel renderer node))
  (%compile-kernel renderer (list node) nil)
  ;; [TODO] Break jit.lisp into multiple files and remove symbol-call
  (make-profiler
   :jit-kernel (uiop:symbol-call :caten/codegen/jit :make-compiled-kernel-node node)
   :shapes (loop for s in (getattr node :dynamic-shapes)
                 if (find (cdr s) `(:int32 :uint32 :int64 :uint64)) ;; = Indexing
                   collect (car s))
   :buffers
   (append
    (loop for w in (getattr node :write-types)
          collect (copy-buffer w))
    (getattr node :dynamic-shape)
    (loop for r in (getattr node :read-types)
          collect (copy-buffer r)))))
;; TODO: GNUPLOT
