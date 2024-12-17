(defpackage :caten/metal
  (:use :cl :caten/air :cffi :caten/codegen/renderer :caten/codegen/helpers
   :caten/codegen/shape-inference :caten/avm :caten/codegen/expr :cl-metal)
  (:import-from
   :caten/polyhedral
   #:define-auto-scheduler
   #:make-schedule-options))

(in-package :caten/metal)

(defvar *indent*)
(defvar *depth*)
(defvar *global-idx-list*)

(defclass Metal-Renderer (CStyle-Renderer) ((device-id :initform 0 :initarg :id :accessor metal-device-id)))
(defmethod get-default-renderer ((id (eql :metal))) (make-instance 'Metal-Renderer))

(define-auto-scheduler
    (Metal-Auto-Scheduler ())
    :cost-functions '(:validity :proximity :coincidence)
    :n-global-loop 3)
(define-hook-auto-scheduler (Metal-Renderer Metal-Auto-Scheduler))
(defmethod initialize-instance :after ((metal Metal-Renderer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (use-device (metal-device-id metal)))
(defun Metal (&optional (gpu 0)) (make-instance 'Metal-Renderer :id gpu))

(defun dtype->mtype (dtype)
  (ecase dtype
    (:bool 'boolean)
    (:float64 'double)
    (:float32 'float)
    (:bfloat16 'bfloat)
    (:uint64 'uint64-t)
    (:int64 'int64-t)
    (:int32 'int32-t)
    (:uint32 'uint32-t)
    (:int16 'int16-t)
    (:uint16 'uint16-t)
    (:uint8 'uint8-t)
    (:int8 'int8-t)))

(defun io->metal (io) (ecase io (:input :in) (:output :out) (:shape :in)))
;; ~~~ Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod %render-kernel ((renderer Metal-Renderer) si)
  (let ((args (loop for item in (getattr si :blueprint)
                    if (eql (node-type item) :DEFINE-GLOBAL)
                      collect item)))
    `(define-kernel (,(intern (getattr si :name))
                     :threadgroup-position-in-grid gid :thread-position-in-threadgroup lid :style :metal :stream ,(>= (ctx:getenv :JIT_DEBUG) 4))
	 (void (,@(loop for arg in args
			collect `(,(intern (string-upcase (format nil "~a~a" (car (node-writes arg)) (if (getattr arg :pointer-p) "*" ""))))
                                  ,(dtype->mtype (getattr arg :dtype))
				  ,(io->metal (getattr arg :type))))))
         ,(with-output-to-string (out)
            (let ((*indent* 2) (*depth* 0) (*global-idx-list*))
              (dotimes (node (getattr si :items))
                (render-bp node out)))))))

(defun render-bp (bp stream)
  (flet ((indent () (make-string *indent* :initial-element #\space)))
    (ecase (node-type bp)
      (:FOR
       (if (eql (getattr bp :scope) :global)
           (progn
             (format stream "~auint ~(~a~) = lid.~a;" (indent) (getattr bp :idx) (case *depth* (0 "x") (1 "y") (2 "z") (otherwise (error "Exceecive loop depth"))))
             (push (getattr bp :idx) *global-idx-list*)
             (incf *depth*))
           (progn
             (format stream "~afor(int ~(~a~)=~a;~a;~a+=~a)" (indent)
                     (getattr bp :idx)
                     (render-expr 'CStyle-Renderer (getattr bp :upfrom))
                     (render-expr 'CStyle-Renderer (getattr bp :below))
                     (getattr bp :idx)
                     (render-expr 'CStyle-Renderer (getattr bp :by)))
             (incf *indent* 2))))
      (:ENDFOR
       (if (find (getattr bp :idx) *global-idx-list*)
           nil
           (progn (decf *indent* 2) (format stream "~a}" (indent)))))
      (:IF
       (format stream "~aif(~a){~%" (indent) (render-expr 'CStyle-Renderer (getattr bp :condition)))
       (incf *indent* 2))
      (:ENDIF
       (decf *indent* 2)
       (format stream "~a}~%" (indent)))
      (:EXPR
       (let ((pre-iterations (getattr bp :iterations)))
         (labels ((print-aref (name b is &key iterations)
                    (if (and is (not (= -1 (buffer-nrank b))) (> (length (iteration-space-shape is)) 0) (> (length iterations) 0))
                        (format nil "~(~a~)[~(~a~)]" name
                                (render-expr
                                 'CStyle-Renderer
                                 (apply
                                  #'expr-add
                                  (map
                                   'list
                                   #'(lambda (view stride i)
                                       (if view
                                           (expr-mul stride (expr-add (expr-const (car view) :int64) (expr-mul (expr-const (third view) :int64) i)))
                                           (expr-mul stride i)))
                                   (iteration-space-views is)
                                   (iteration-space-strides is)
                                   iterations))))
                        (format nil "~(~a~)" name))))
           (format stream "~a~a~a = ~a;~%"
                   (indent)
                   (if (car (getattr bp :declare-type))
                       (format nil "~a " (->cdtype (buffer-dtype (car (relay-writes (read-type-relay bp))))))
                       "")
                   (render-list
                    (map 'list #'(lambda (x y z) (print-aref x y z :iterations pre-iterations))
                         (node-writes bp) (relay-writes (read-type-relay bp)) (relay-write-iters (read-type-relay bp))))
                   (render-expr 'CStyle-Renderer (getattr bp :EXPR) :index-space pre-iterations)))))
      (:DEFINE-GLOBAL))))

(defmethod %compile-kernel ((renderer Metal-Renderer) items dir)
  (dolist (item items)
    (when (getattr item :rendered-object)
      (setf (getattr item :compiled-object)
            (compile nil (getattr item :rendered-object))))))
