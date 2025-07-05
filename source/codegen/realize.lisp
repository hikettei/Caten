(defpackage :caten/codegen/realize
  (:documentation "Infers which buffer in the graph needs to be realized or not. Takes a list of blueprint (which is consisted of TMPRange, TMPEndRange)")
  (:use :cl :caten/runtime :caten/air :caten/codegen/iteration :caten/aasm)
  (:export
   #:buffer-scalarify
   #:bp-finalize-realize
   #:schedule-item-sync-realize))

(in-package :caten/codegen/realize)
;; ~~ Scalarify ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun tensor-relay-scalarify (buffer)
  (declare (type TensorRelay buffer))
  (let ((buffer (copy-tensor-relay buffer)))
    (setf (tensor-relay-nrank buffer) -1)
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
  "Only the following tensor-relays are needed to be realized (allocated on the device):
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
                    (setf (nth nth (relay-reads (read-type-relay bp))) (tensor-relay-scalarify rt))
                  else if (and definition (= 0 (tensor-relay-nrank (car (relay-writes (read-type-relay definition)))))) do
                    (let ((s (tensor-relay-scalarify rt)))
                      (setf (tensor-relay-nrank s) 0
                            (nth nth (relay-reads (read-type-relay bp))) s)))
            (when (null (getattr bp :reduction :allow-undefined t))
              (loop for w in (node-writes bp)
                    for wt in (relay-writes (read-type-relay bp))
                    for wi in (relay-write-iters (read-type-relay bp))
                    for nth upfrom 0
                    if (and wt wi (local-p w)) do
                      (setf (nth nth (relay-writes (read-type-relay bp))) (tensor-relay-scalarify wt)))))
    blueprint))
;; ~~~ Realize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod schedule-item-sync-realize ((item Node))
  "Only the buffers appeared in the :DEFINE-GLOBAL, is needed to be allocated on the device."
  (assert (eql (node-type item) :schedule-item))
  (assert (eql (getattr item :type) :kernel))
  (let ((buffers
          (loop for node in (graph-nodes (getattr item :blueprint))
                if (eql (node-type node) :DEFINE-GLOBAL)
                  collect (car (node-writes node)))))
    (flet ((included-p (name) (member name buffers)))
      (setf (node-reads item) (loop for name in (node-reads item) if (included-p name) collect name)
            (node-writes item) (loop for name in (node-writes item) if (included-p name) collect name))
      item)))
