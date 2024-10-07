(in-package :caten/ajit)
;;
;; Device. An abstraction class for the renderer.
;;

(defclass Device () nil)
;; Configurations
(defgeneric default-device (device-prefix) (:documentation "Returns a default device class dispatched by the device-prefix."))
(defgeneric device-parallel-depth (device-prefix) (:documentation "Return a fixnum indicating n outermost loops are parallelized."))
(defgeneric device-packed-by (device-prefix) (:documentation "Funcall is packed by the returned value of this method. Default: 1 (ignored)"))
(defmethod device-packed-by ((device-prefix t)) 1)

(defclass ISL-Expr (Device) nil)
(defmethod default-device ((device-prefix (eql :isl-expr))) (make-instance 'ISL-Expr))

(defmethod %render-expr ((lang ISL-Expr) op lhs rhs z)
  (assert (and lhs rhs) () "~a is not implemented?" op)
  (assert (null z))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:ADD :+) (:MUL :*) (:IDIV "/")
	    (:AND :and) (:OR :or) (:!= :!=) (:EQ :=)
	    (:XOR :xor)
	    (:% :%) (:equal :=) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

(defmethod %render-expr ((lang ISL-Expr) (op (eql :MAX)) lhs rhs z)
  (assert (and lhs rhs))
  (if z
      (format nil "max(~a, max(~a, ~a))" (render-expr lang lhs) (render-expr lang rhs) (render-expr lang z))
      (format nil "max(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs))))

(defmethod %render-expr ((lang ISL-Expr) (op (eql :MIN)) lhs rhs z)
  (assert (and lhs rhs))
  (if z
      (format nil "min(~a, min(~a, ~a))" (render-expr lang lhs) (render-expr lang rhs) (render-expr lang z))
      (format nil "min(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs))))

(defmethod %render-expr ((lang ISL-Expr) (op (eql :Const)) lhs rhs z)
  (format nil "~(~a~)" lhs))

(defclass Lisp-Backend (Device) nil)
(defmethod default-device ((device-prefix (eql :lisp))) (make-instance 'Lisp-Backend))

(defmethod %render-expr ((lang Lisp-Backend) op lhs rhs z)
  (format nil "(~a ~a ~a ~a)"
          (ecase op
            (:ADD :+) (:MUL :*) (:IDIV :/)
            (:AND :and) (:OR :or) (:!= :/=) (:== :=)
            (:XOR :xor)
            (:<= :<=) (:>= :>=) (:< :<) (:> :>)
            (:MAX :max) (:MIN :min)
            (:+ :+) (:- :-) (:* :*) (:/ :/)
            (:SIN :SIN) (:LOG2 :LOG2) (:EXP2 :EXP2) (:RECIP :/) (:SQRT :SQRT) (:NOT :NOT))
          (if lhs
              (render-expr lang lhs)
              "")
          (if rhs
              (render-expr lang rhs)
              "")
          (if z
              (render-expr lang z)
              "")))

(defmethod %render-expr ((lang Lisp-Backend) (op (eql :Const)) lhs rhs z) (format nil "~(~a~)" lhs))
;; For TC Compilation
(defmethod %render-expr ((lang Lisp-Backend) (op (eql :Take)) lhs rhs z)
  (labels ((explore (expr nth)
             (if (eql (expr-op expr) :CONS)
                 (format nil "(* (nth ~a (caten/avm:buffer-stride ~a)) ~a) ~a"
                         nth lhs (render-expr lang (expr-y expr)) (explore (expr-x expr) (1+ nth)))
                 (format nil "(* (nth ~a (caten/avm:buffer-stride ~a)) ~a)" nth lhs (render-expr lang expr)))))
    (assert (symbolp lhs))
    (assert (null z))
    (format nil "(aref (caten/avm:buffer-value ~a) (+ ~a))" lhs (explore rhs 0))))
