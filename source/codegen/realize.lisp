(defpackage :caten/codegen/realize
  (:documentation "Infers which buffer in the graph needs to be realized or not. Takes a list of blueprint (which is consisted of TMPRange, TMPEndRange)")
  (:use :cl :caten/runtime :caten/air :caten/codegen/type-relay)
  (:export
   #:buffer-scalarify
   #:bp-finalize-realize
   ))

(in-package :caten/codegen/realize)

(defun buffer-scalarify (buffer)
  (let ((buffer (copy-buffer buffer)))
    (setf (buffer-nrank buffer) -1)
    buffer))

(defun memory-access-local-p (blueprint id)
  (declare (optimize (speed 3)) (type list blueprint) (type symbol id))
  (let ((search-key
          (loop for bp in blueprint
                for nth fixnum upfrom 0
                if (and (not (eql (node-class bp) :Tmp)) (or (find id (node-reads bp)) (find id (node-writes bp))))
                  collect nth)))
    (assert search-key () "The id ~a is not used in the bp." id)
    (loop with start fixnum = (apply #'min search-key)
          with end fixnum = (apply #'max search-key)
          with depth fixnum = 0
          for nth upfrom start to end
          for ir = (nth nth blueprint)
          if (find (node-type ir) `(:TmpRange)) do (incf depth)
          else if (find (node-type ir) `(:TmpEndRange)) do (decf depth)
          end
          if (< depth 0) do (return-from memory-access-local-p nil))
    t))

(defun bp-finalize-realize (blueprint node base-graph &aux (seen (make-hash-table)) (io (append (node-reads node) (node-writes node))))
  "Only the following buffers are needed to be realized (allocated on the device):
- appeared in either of (node-reads node) or (node-writes node)
- the access is not completed in the innermost loop"
  (flet ((local-p (id)
           (when (find id io) (return-from local-p nil))
           (when (not (symbolp id)) (return-from local-p nil))
           (if (gethash id seen)
               (eql (gethash id seen) :yes)
               (let ((result (memory-access-local-p blueprint id)))
                 (setf (gethash id seen) (if result :yes :no))
                 result))))
    (loop with idxs = nil
          for bp in blueprint
          if (eql (node-type bp) :TmpRange) do
            (push (getattr bp :idx) idxs)
          if (eql (node-type bp) :TmpEndRange) do
            (setf idxs (remove (getattr bp :idx) idxs))
          if (not (eql (node-class bp) :Tmp)) do
            (loop for r in (node-reads bp)
                  for rt in (relay-reads (read-type-relay bp))
                  for ri in (relay-read-iters (read-type-relay bp))
                  for parent = (find r blueprint :key #'node-writes :test #'find)
                  for parent-reduce-p = (and parent (getattr parent :reduction :allow-undefined t))
                  for definition = (id->value base-graph r)
                  for nth upfrom 0
                  if (and rt ri (local-p r) (or (null parent-reduce-p) (getattr parent :declare-type))) do
                    (setf (nth nth (relay-reads (read-type-relay bp))) (buffer-scalarify rt))
                  else if (and definition (= 0 (buffer-nrank (car (relay-writes (read-type-relay definition)))))) do
                    (let ((s (buffer-scalarify rt)))
                      (setf (buffer-nrank s) 0
                            (nth nth (relay-reads (read-type-relay bp))) s)))
            (when (null (getattr bp :reduction :allow-undefined t))
              (loop for w in (node-writes bp)
                    for wt in (relay-writes (read-type-relay bp))
                    for wi in (relay-write-iters (read-type-relay bp))
                    for nth upfrom 0
                    if (and wt wi (local-p w)) do
                      ;; [TODO] 戻ったら: ALlocationとdefine-globalの挿入をやる
                      ;;  ^ SimplifierがRealize判定と同値のことをやるはず
                      ;(print "+++DO ALLOC++++")
                      ;(print (id->value base-graph (get-output-to bp)))
                      (setf (nth nth (relay-writes (read-type-relay bp))) (buffer-scalarify wt)))))
    blueprint))
