(in-package :caten/aasm)
;; A subset of ops
;; +-/=
;; SIN, SQRT, LOG2, EXP2, RECIP

;; Notation: y <- f(x y)
;; x is always the destination of output buffers.

;; UnaryOps := [NEG, RECIP, SQRT, NOT]
(macrolet ((def (fname opname)
	     `(defun ,fname (x &key (id (gensym "UID")))
		(declare (type node x))
		(emit (make-node :UnaryOps ,opname (list id) (list (node->id x)))))))
  (def %neg   :NEG)
  (def %recip :RECIP)
  (def %sqrt  :SQRT)
  (def %not   :NOT))
;; BinaryOps := [Add, Mul, NEQ, LT, AND, OR]
;; reduction = nil -> | c = a + b
;; reduction = t   -> | a += b
(macrolet ((def (fname opname)
	     `(defun ,fname (x y &key (id (gensym "BID")) (reduction nil))
		(declare (type node x y))
		(emit (make-node :BinaryOps ,opname (list id) (list (node->id x) (node->id y)) :reduction reduction)))))
  (def %add :ADD)
  (def %mul :MUL)
  (def %and :AND)
  (def %or :OR))
(defun %sub (x y &key (reduction nil)) (%add x (%neg y) :reduction reduction))
(defun %div (x y &key (reduction nil)) (%mul x (%recip y) :reduction reduction))

;; CompareOps: map <- [map{bool}, x, y]
(macrolet ((def (fname opname)
	     `(defun ,fname (shape order x y &key (id (gensym "BID")))
		(declare (type node x y))
		(let ((out (if shape
			       (%make-tensor shape :dtype :bool :order order)
			       (%salloc :dtype :bool))))
		  (emit (make-node :TernaryOps ,opname (list id) (list (node->id out) (node->id x) (node->id y))))))))
  (def %!= :NEQ)
  (def %< :LT))
(defun %= (shape order x y)  (%not (%!= shape order x y)))
(defun %<= (shape order x y) (%or (%< shape order x y) (%= shape order x y)))
(defun %>  (shape order x y) (%not (%<= shape order x y)))
(defun %>= (shape order x y) (%or (%> shape order x y) (%= shape order x y)))
