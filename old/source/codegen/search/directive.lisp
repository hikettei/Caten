(defpackage :caten/codegen/search/directive
  (:use :cl)
  (:export
   #:Directive
   #:directive-type #:directive-amount #:directive-depth #:directive-visible
   #:directive->id #:directive->str #:str->directive))

(in-package :caten/codegen/search/directive)

(defclass Directive ()
  ((type :initarg :type :accessor directive-type)
   (amount :initarg :amount :accessor directive-amount)
   (depth :initarg :depth :accessor directive-depth)
   (visible :initarg :visible :accessor directive-visible))
  (:documentation "
A `Directive` is a scheduling annotation that is placed directly above an ISL ScheduleNodeBand in the schedule tree.
```
@TYPE(AMOUNT=FIXNUM, DEPTH=FIXNUM, VISIBLE=BOOL)
```
AMOUNT: A parameter for directive
DEPTH: Indicates the depth of schedule_node_band
VISIBLE: If set to T, the band should not be further modified.
"))

(defun directive (type amount depth visible)
  (declare (type string type) (type fixnum amount) (type boolean visible))
  (make-instance 'Directive :type type :amount amount :depth depth :visible visible))

(defmethod print-object ((directive Directive) stream)
  (print-unreadable-object (directive stream)
    (format stream "~a" (directive->str directive))))

(defmethod directive->str ((directive Directive))
  (with-output-to-string (out)
    (format out "@DIRECTIVE(")
    (loop with slots = (c2mop:class-slots (class-of directive))
          for slot-def in slots
          for slot-name = (c2mop:slot-definition-name slot-def)
          for value     = (slot-value directive slot-name)
          for idx upfrom 0 do
            (format out "~a=~a" (string-upcase (princ-to-string slot-name)) value)
            (when (< idx (1- (length slots))) (format out ",")))
    (format out ")")))

(defmethod directive->id ((directive directive)) (isl::make-id-from-str (directive->str directive)))

(defun split-key-and-value (str)
  (let ((pos (position #\= str)))
    (assert pos)
    (let ((key (intern (subseq str 0 pos) "KEYWORD"))
          (value (subseq str (1+ pos))))
      (list
       key
       (case key
         (:TYPE value)
         ((:AMOUNT :DEPTH) (parse-integer value))
         (:VISIBLE (string= (string-upcase value) "T"))
         (otherwise value))))))

(defun split-directive-string (str)
  (let ((res '()) (start 0) (len (length str)))
    (loop for pos = (position #\, str :start start)
          do (cond
               ((null pos)
                (push (subseq str start len) res)
                (return-from split-directive-string (map 'list #'split-key-and-value (nreverse res))))
               (t
                (push (subseq str start pos) res)
                (setf start (1+ pos)))))))

(defmethod str->directive ((string string))
  ;; @DIRECTIVE(...) is a valid format.
  (unless (and (uiop:string-prefix-p "@DIRECTIVE(" string) (char= (char string (1- (length string))) #\))) (error "Invalid directive string: ~S" string))
  (let* ((content (subseq string #.(length "@DIRECTIVE(") (1- (length string)))))
    (apply #'make-instance 'Directive (apply #'append (split-directive-string content)))))
