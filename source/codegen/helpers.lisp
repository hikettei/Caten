(defpackage :caten/codegen/helpers
  (:use :cl :caten/air)
  (:export
   #:gid
   #:range
   #:nodes-depends-on
   #:nodes-write-to
   #:render-list
   #:permute-list
   #:ensure-string-as-compilable
   #:simplify-arithmetic-code
   #:simplify-blueprint
   #:->cdtype
   #:float-type-of
   #:coerce-dtyped-buffer
   #:nodes-create-namespace))

(in-package :caten/codegen/helpers)

(defun nodes-depends-on (nodes)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type list nodes))
  (let ((seen) (depends-on))
    (loop for node in nodes do
      (dolist (r (node-reads node))
	(when (null (find r seen))
	  (when (symbolp r)
	    (push r depends-on))
	  (push r seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun nodes-write-to (nodes)
  (flet ((used-p (id) (find id nodes :key #'node-reads :test #'find)))
    (loop for node in nodes
	  append
	  (loop for w in (node-writes node)
		if (not (used-p w)) collect w))))

(defun gid (nth)
  (declare (type fixnum nth))
  (intern (format nil "_GID~d" nth)))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))
;; The same algorithm in function.lisp (class Permute)
(defun permute-list (order list)
  (assert (= (length order) (length list)) () "cannot shuffle ~a and ~a" order list)
  (loop for nth in order collect (nth nth list)))

(defun ensure-string-as-compilable (name)
  "Ensures that the string is a compilable to foreign library."
  (declare (type string name))
  (macrolet ((def (from to)
               `(setf name (cl-ppcre:regex-replace-all ,from name ,to))))
    (def "!=" "NEQ")
    (def "=" "EQ")
    (def "<" "LT")
    ;; Ensuring not containing any special characters. (e.g: a-b produces an error on clang)
    (def "[^a-zA-Z0-9_]" "_")
    name))

(defun simplify-arithmetic-code (code)
  "Removes extra brackets from the generated code (expecting C-Style)"
  (declare (type string code))
  (macrolet ((def (from to)
               `(let ((tmp (cl-ppcre:regex-replace-all ,from code ,to)))
                  (when (or (not (string= "" tmp)) (not (string= "()" tmp)))
                    (setf code tmp)))))
    (def "0\\+0" "0")
    (def "\\(0\\)\\+" "")
    (def "\\+\\(0\\)" "")
    (def "\\+0\\)" ")")
    (def "\\(0\\+" "(")
    code))

(defun remove-empty-loop (nodes &aux (removed nil))
  (loop for node in nodes
	for nth upfrom 0
	if (and (eql (node-type node) :FOR)
		(nth (1+ nth) nodes)
		(eql (node-type (nth (1+ nth) nodes)) :ENDFOR))
	  do (push (node-id (nth (1+ nth) nodes)) removed)
	else
          unless (find (node-id node) removed)
            collect node))

(defun simplify-blueprint (nodes)
  (let ((len (length nodes)))
    (setf nodes (remove-empty-loop nodes))
    (if (= (length nodes) len)
        nodes
        (simplify-blueprint nodes))))

(defun ->cdtype (dtype)
  (ecase dtype
    (:bool "boolean")
    (:float64 "double")
    (:float32 "float")
    (:uint64 "uint64_t")
    (:int64 "int64_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:uint8 "uint8_t")
    (:int8 "int8_t")))

(defun float-type-of (value)
  (uiop:symbol-call :caten/apis :float-type-of value))

(defun coerce-dtyped-buffer (arg type)
  "If buffer-nrank=0 -> the arg is passed by the value, not a buffer.
Otherwise -> they are passed as a buffer."
  (if (caten/avm:buffer-p arg)
      (if (= (caten/avm:buffer-nrank arg) 0)
          (caten/common.dtype:dtype/cast (caten/avm:buffer-value arg) type)
	  arg)
      (caten/common.dtype:dtype/cast arg type)))

(defun nodes-create-namespace (nodes)
  "This function returns a list of symbols used in the nodes."
  (declare (type list nodes))
  (loop for node in nodes
        append (node-writes node)
        append (node-reads node)))
