(in-package :caten/air)

;; Difficult Point:
;; How to prepresent (A B) Tensor for example?
(defclass TypeRelay () nil)
;; The definition should be at aasm!
(defclass TensorRelay (TypeRelay )
  )
(defclass ASTRelay (TypeRelay) nil) ;; 例えばForのBodyは常にEXPRみたいなことが言えるはず

(defstruct (Typed
            (:constructor make-typed (dtype shape stride views)))
  (dtype dtype :type keyword)
  (shape shape :type list)
  (stride stride :type list)
  (views views :type list)
  (nrank (length shape) :type fixnum)
  (inferred-permute nil :type list)
  (orig-buffer-shape nil :type list)
  (depend-idx-list nil :type list)) ;; should not be used!

;; [TODO] Typedのような簡易的なものではなく，RelayBufferのような全ての地点のTrackingをするべき？
;; 進め方:
;; - 0から新しいTypedを作る (w/ shape, view etc)
;; - [ ] codegen/shape-inferece.lispをもとに，attrsのTypeRelayを書き直す
;;   - [ ] ASTもTypeRelayできるようにする。
;;   - 
;; - 既存のcodegen, Typed Basedで書き直す
;; - (なぜならTypeInferenceはASTにも適用したいから)
(defun graph-infer-type-relay (graph)
  (declare (type graph graph) (optimize (speed 3)))
  (let ((id->type-map (make-hash-table)) (nodes (tpsort-graph graph)))
    (declare (type list nodes))
    (loop for node in nodes
          for type-relay = (%node-get-type-relay (node-type node))
          for outs = (funcall (the function type-relay) id->type-map node) do
            (assert (listp outs) () "type-relay should return a list!")
            (assert (= (length outs) (length (node-writes node))) () "Mismatch in the number of outputs when inferecing the type of ~a" node)
            (print outs)
            (assert (every #'typed-p outs) () "The type-relay should return a list of Typed, getting ~a" outs)
            (loop for ot in outs
                  for w in (node-writes node) do
                    (setf (gethash w id->type-map) ot)))
    (loop for node in nodes do
      (print node)
      )
    id->type-map))
