(in-package :caten/aasm)
;; A subset of ops
;; +-/=
;; SIN, SQRT, LOG2, EXP2, RECIP
;;

;; UnaryOps := [NEG RECIP SQRT]
(macrolet ((def (fname opname)
	     `(defun ,fname (x &key (id (gensym "UID")))
		(declare (type node x))
		(emit (make-node :UnaryOps ,opname (list id) (list (node->id x)))))))
  (def %neg   :NEG)
  (def %recip :RECIP)
  (def %sqrt  :SQRT))
;; BinaryOps := [Add, Mul]
(macrolet ((def (fname opname)
	     `(defun ,fname (x y &key (id (gensym "BID")))
		(declare (type node x y))
		(emit (make-node :BinaryOps ,opname (list id) (list (node->id x) (node->id y)))))))
  (def %add :ADD)
  (def %mul :MUL))
(defun %sub (x y) (%add x (%neg y)))
(defun %div (x y) (%mul x (%recip y)))
