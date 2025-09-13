(in-package :caten/ir)

(defclass RuntimeGraph (FastGraph) nil)

(defgeneric realize (runtime))

;; [ ] Export RuntimeGraph into C

