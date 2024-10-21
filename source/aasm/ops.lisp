(in-package :caten/aasm)
;; A subset of ops
;; +-/=
;; SIN, SQRT, LOG2, EXP2, RECIP

;; Notation: y <- f(x y)
;; x is always the destination of output buffers.

;; UnaryOps := [NEG, RECIP, SQRT, NOT, CAST]
(macrolet ((def (fname opname)
	     `(defun ,fname (x &key (id (gensym "UID")))
		(declare (type node x))
		(emit (make-node :UnaryOps ,opname (list id) (list (node->id x)))))))
  (def %neg   :NEG)
  (def %recip :RECIP)
  (def %sin   :SIN)
  (def %log2  :LOG2)
  (def %exp2  :EXP2)
  (def %sqrt  :SQRT)
  (def %not   :NOT))
;; x <- cast(y)
(defun %cast (x y dtype &key (id (gensym "CID")))
  (declare (type node x y) (type dtype-t dtype))
  (emit (make-node :UnaryOps :CAST (list id) (list (node->id x) (node->id y)) :dtype dtype)))
;; BinaryOps := [MOVE, Add, Mul, NEQ, LT, AND, OR, MAX, GCD]
;; reduction = nil -> | c = a + b
;; reduction = t   -> | a += b
(defparameter *wrap-around-mode* nil)
(macrolet ((def (fname opname &optional possibly-overflow)
	     `(defun ,fname (x y &key (id (gensym "BID")) (reduction nil) (wrap-around ,(if possibly-overflow '*wrap-around-mode* nil)))
		"If wrap-around=t -> (mod (op x y) (max_value_of (dtype x)))"
		(declare (type node x y))
		(when (and (null ,possibly-overflow) wrap-around)
		  (error "~a does not support the wrap-around option." ',fname))
		(emit (make-node :BinaryOps ,opname (list id) (list (node->id x) (node->id y)) :reduction reduction :wrap-around wrap-around)))))
  (def %add :ADD t)
  (def %mul :MUL t)
  (def %idiv :IDIV nil)
  (def %and :AND)
  (def %or :OR)
  (def %xor :XOR)
  (def %move :MOVE)
  (def %max :MAX)
  (def %gcd :GCD))
(defun %sub (x y &key (reduction nil) (id (gensym "BID"))) (%add x (%neg y) :reduction reduction :id id))
(defun %div (x y &key (reduction nil) (id (gensym "BID"))) (%mul x (%recip y) :reduction reduction :id id))

;; CompareOps: map <- [map{bool}, x, y]
(macrolet ((def (fname opname)
	     `(defun ,fname (shape order x y &key (id (gensym "BID")) (out nil))
		(declare (type node x y))
		(let ((out (or
			    out
			    (if shape
				(%make-tensor shape :dtype :bool :order order)
				(%salloc :dtype :bool)))))
		  (emit (make-node :TernaryOps ,opname (list id) (list (node->id out) (node->id x) (node->id y))))))))
  (def %!= :!=)
  (def %< :<))

(defun %= (shape order x y &key out (id (gensym "BID")))  (%not (%!= shape order x y :out out) :id id))
(defun %<= (shape order x y &key out (id (gensym "BID"))) (%or (%< shape order x y :out out) (%= shape order x y :out out) :id id))
(defun %>  (shape order x y &key out (id (gensym "BID"))) (%not (%<= shape order x y :out out) :id id))
(defun %>= (shape order x y &key out (id (gensym "BID"))) (%or (%> shape order x y :out out) (%= shape order x y :out out) :id id))
