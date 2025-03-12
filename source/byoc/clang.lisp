(defpackage :caten/byoc/clang
  (:use :cl :cffi :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
        :caten/codegen/backend :caten/codegen/renderer :caten/air :caten/codegen/runner
        :caten/ir/expr :caten/codegen/helpers :caten/codegen/type-relay)
  (:import-from
   :caten/codegen/search
   #:define-auto-scheduler)
  (:import-from :caten/byoc/lisp #:LispBuffer)
  (:export #:ClangBuffer #:ClangRuntime #:load-foreign-function))

(in-package :caten/byoc/clang)

(defclass ClangBuffer (LispBuffer) nil)
(defclass ClangRuntime (GraphRuntime) nil)
(defclass ClangKernel (AbstractKernel)
  ((program :accessor clang-program :type string)
   (caller :accessor clang-caller :type function)))
(define-auto-scheduler Clang-Auto-Scheduler :use-parallel 1)
(define-backend :clang ClangBuffer ClangRuntime CStyle-Renderer ClangKernel Clang-Auto-Scheduler t)

(defmethod %render-kernel ((renderer CStyle-Renderer) si)
  (let* ((kernel (getattr si :kernel))
         (bp (getattr (kernel-schedule-item kernel) :blueprint))
         (args (apply #'concatenate 'string
                      (butlast
                       (loop for arg in (kernel-args kernel)
                             append
                             (list
                              (format nil "~a~a~a~a ~(~a~)"
                                      (case (getattr arg :mode) (:read "const ") (otherwise ""))
                                      (->cdtype (getattr arg :dtype))
                                      (if (getattr arg :pointer-p) "*" "")
                                      (if (and (eql :read (getattr arg :mode)) (getattr arg :pointer-p))
                                          " restrict"
                                          "")
                                      (car (node-writes arg)))
                              ", "))))))
    (setf (clang-program kernel)
          (print
          (with-output-to-string (out)
            (format out "void ~(~a~)(~a);~%" (kernel-name kernel) args)
            (format out "void ~(~a~)(~a)~%~a" (kernel-name kernel) args (render-bp bp)))))))
;; OpenMP requires the brackets to be removed in the for loop.
(defun trim-brackets (str)
  (let ((len (length str)))
    (if (and (>= len 2) (char= #\( (aref str 0)) (char= #\) (aref str (1- len))))
        (subseq str 1 (1- len))
        str)))

(defun render-bp (graph &aux (indent 0) (seen))
  (with-output-to-string (out)
    (labels ((indent () (make-string indent :initial-element #\space))
             (fmt (desig &rest args) (apply #'format out (format nil "~a~a~%" (indent) desig) args))
             (r (s &aux (val (id->value graph s)))
               (when (and val (null (find (node-id val) seen)))
                 (f val) (push (node-id val) seen))
               s)
             (e (id)
               (let ((renderer (make-instance 'CStyle-Renderer :graph graph)))
                 (render-node renderer id)))
             (f (node)
               (case (node-type node)
                 (:PROGN
                   (fmt "{")
                   (incf indent 2) (mapc #'r (node-reads node)) (decf indent 2)
                   (fmt "}"))
                 (:EXPR
                  (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                      (fmt "~a;" (e (car (node-reads node))))
                      (let ((type (car (getattr node :dst-types))))
                        (assert type () "The node ~a must be shape inferred." node)
                        (fmt "~a ~(~a~) = ~a;" (->cdtype (caten/ir:typed-dtype type)) (car (node-writes node)) (e (car (node-reads node)))))))
                 (:DEFINE-GLOBAL) (:RANGE)
                 (:FOR
                  (multiple-value-bind (range body) (apply #'values (node-reads node))
                    (setf range (id->value graph range))
                    (assert (and range (eql (node-type range) :RANGE)) () "The first argument of :FOR should be :RANGE, getting ~a" range)
                    (multiple-value-bind (bind size step) (values (getattr range :idx) (first (node-reads range)) (second (node-reads range)))
                      (when (symbolp size)
                        (let ((val (id->value graph size)))
                          (assert (and val (eql (node-type val) :EXPR)) () "Range: The size must be specified as EXPR or fixnum, getting ~a" val)
                          (setf size (car (node-reads val)))))
                      (when (symbolp step)
                        (let ((val (id->value graph step)))
                          (assert (and val (eql (node-type val) :EXPR)) () "Range: The step must be specified as EXPR or fixnum, getting ~a" val)
                          (setf step (car (node-reads val)))))
                      (fmt "~afor (int ~(~a~)=0; ~(~a~)<~(~a~); ~(~a~)+=~a)"
                           (if (> (getattr node :parallel) 0)
                               (format nil "#pragma omp parallel for collapse(~a)~%~a" (getattr node :parallel) (indent))
                               "")
                           bind bind (trim-brackets (e size)) bind (trim-brackets (e step))))
                    (unless (eql (node-type (id->value graph body)) :PROGN) (incf indent 2))
                    (r body)
                    (unless (eql (node-type (id->value graph body)) :PROGN) (decf indent 2))))
                 (:IF
                  (multiple-value-bind (cond body) (apply #'values (node-reads node))
                    (setf cond (id->value graph cond))
                    (assert (and cond (eql (node-type cond) :EXPR)) () "IF: the conditon must be EXPR.")
                    (fmt "if (~(~a~)) {" (e (car (node-reads cond))))
                    (incf indent 2) (r body) (decf indent)
                    (fmt "}")))
                 (:BARRIER (error "thread barrier is not supported on clang"))
                 (:DEFINE-SHARED-MEMORY (error "shared memory is not supported on clang"))
                 (otherwise (error "The node ~a is not a supported renderop by clang" node)))))
      (f (id->value graph (car (graph-outputs graph)))))))

(defun header ()
  (format nil "~%#include <math.h>
#include <stdint.h>
~a
#define boolean _Bool
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))
"
	  (if (= 1 (ctx:getenv :OMP))
	      "#include <omp.h>"
	      "")))

(defun load-foreign-function (source &key (compiler "gcc") (lang "c") (compiler-flags) (dir nil))
  (declare (type string source compiler))
  (when (= 1 (ctx:getenv :OMP))
    (push "-fopenmp" compiler-flags))
  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t :directory dir)
    nil
    :close-stream
    (let* ((cmd
	     ;; gcc -shared -o sharedlib
	     (append
	      (list
	       compiler "-shared"
	       "-x" lang)
	      compiler-flags
	      (list "-o" (uiop:native-namestring sharedlib) "-")))
	   (process-info (uiop:launch-program
			  cmd
			  :input :stream
			  :error-output :stream))
	   (input (uiop:process-info-input process-info))
	   (error-output (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source input)
	(close input))
      (unless (zerop (uiop:wait-process process-info))
	(error "Caten[Clang]: Failed to compile a shared library:~%~a~%

Compiled with this command: ~a"
	       (alexandria:read-stream-content-into-string error-output)
	       (with-output-to-string (out)
		 (dolist (c cmd) (princ c out) (princ " " out))))))
    (cffi:load-foreign-library sharedlib)))

(defun disassemble-foreign-code (source &key (compiler "gcc") (lang "c") (compiler-flags))
  (declare (type string source compiler))
  (when (= 1 (ctx:getenv :OMP)) (push "-fopenmp" compiler-flags))
  (let* ((cmd (append (list compiler "-x" lang) compiler-flags (list "-" "-S" "-o" "-")))
	 (process-info (uiop:launch-program cmd :input :stream :error-output :stream :output :stream))
	 (input (uiop:process-info-input process-info))
	 (error-output (uiop:process-info-error-output process-info)))
    (unwind-protect (princ source input) (close input))
    (unless (zerop (uiop:wait-process process-info))
      (error "Caten[Clang]: Failed to compile a shared library:~%~a~%

Compiled with this command: ~a"
	     (alexandria:read-stream-content-into-string error-output)
	     (with-output-to-string (out)
	       (dolist (c cmd) (princ c out) (princ " " out)))))
    (alexandria:read-stream-content-into-string (uiop:process-info-output process-info))))

(defmacro with-kludge-if-needed-for-darwin-x86-64-with-invalid-float-traps-masked (form)
  #+(and :darwin :x86-64) `(float-features:with-float-traps-masked (:invalid) ,form)
  #-(and :darwin :x86-64) `(progn ,form))

(defun make-foreign-function-caller (name defglobals &aux (tmps))
  (labels ((expand (rest-forms body)
             (if rest-forms
                 (if (getattr (car rest-forms) :pointer-p)
                     ;; Vector
                     `(with-pointer-to-vector-data
			  (,(car (node-writes (car rest-forms))) (buffer-value ,(car (node-writes (car rest-forms)))))
			,(expand (cdr rest-forms) body))
                     ;; Scalar
                     (expand (cdr rest-forms) body))
		 `(progn
		    ,@body
		    ,@(loop for (buffer . node) in tmps
			    for cffi = (car (node-writes node))
			    for type = (->cffi-dtype (getattr node :dtype))
			    collect `(setf (buffer-value ,buffer) (mem-ref ,cffi ,type)))))))
    `(lambda (,@(map 'list #'(lambda (x) (car (node-writes x))) defglobals))
       ;; this is an unstable/experimental workaround for MacOS/X86-64/clang
       ;; we just ignore the "invalid" float trap raised by the called C code
       ;; Otherwise, e.g., running the test "caten/test-suite::threefry2x32",
       ;; (or: (ctx:with-contextvar (:BACKEND "CLANG") (caten:rand `(7))) )
       ;; causes an "arithmetic error FLOATING-POINT-INVALID-OPERATION".
       ;; This might be due to the (implicit and/or float/int) conversions
       ;; in the code generated for example, for threefry2x32.
       (caten/runtime/profile:with-real-time
         (with-kludge-if-needed-for-darwin-x86-64-with-invalid-float-traps-masked
	     ,(expand
	       defglobals
	       `((cffi:foreign-funcall
                  ,(format nil "~(~a~)" name)
                  ,@(loop for arg in defglobals
                          if (getattr arg :pointer-p)
                            append `(:pointer ,(car (node-writes arg)))
                          else
                            append `(,(->cffi-dtype (getattr arg :dtype)) ,(car (node-writes arg))))
                  :void))))))))

(defmethod %compile-kernel ((renderer CStyle-Renderer) items dir)
  (let ((code
          (apply #'concatenate 'string
                 (append
                  (list (header))
                  (loop for item in items
                        collect (clang-program (getattr item :kernel)))))))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[Final Code]:~%~a~%" code))
    ;; [Note] -ffast-math and CI fails?
    (load-foreign-function code :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3") :dir dir)
    (when (>= (ctx:getenv :DISASSEMBLE) 1)
      (format t "[DISASSEMBLE=1]:~%~a" (disassemble-foreign-code code :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3"))))
    (dolist (item items)
      (when (getattr item :kernel)
        (setf (clang-caller (getattr item :kernel))
              (make-foreign-function-caller
               (kernel-name (getattr item :kernel))
               (kernel-args (getattr item :kernel))))))
    nil))
