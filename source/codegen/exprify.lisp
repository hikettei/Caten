(defpackage :caten/codegen/exprify
  (:documentation "Implements various transforming patterns on the AST")
  (:use :cl :caten/air)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters)
  (:import-from
   :caten/avm
   #:Buffer
   #:Buffer-depend-idx-list
   #:buffer-nrank)
  (:export
   #:graph-scalarify
   #:graph-exprify
   ))

(in-package :caten/codegen/exprify)

(defun force-realize-p (node schedule-graph)
  (when (null (getattr node :reduction :allow-undefined t))
    (return-from force-realize-p nil))
  (let ((parent (id->value schedule-graph (car (node-reads node)))))
    (when (not (getattr parent :allocate-p))
      (return-from force-realize-p nil))
    (let ((allocate (car (getattr parent :Items))))
      (assert (eql (node-type allocate) :Allocate))
      (if (getattr allocate :from) t nil))))

(defun blueprint-tmp-buffers (blueprints schedule-graph &key (except-for nil))
  (let ((ids) (seen))
    (dolist (b blueprints)
      (let ((out (get-output-to b)))
        (when (and out (symbolp out))
          (push out ids)))
      (dolist (r (node-reads b))
        (when (symbolp r)
          (push r seen)))
      (dolist (w (node-writes b))
        (when (and (null (find w seen)) (not (force-realize-p b schedule-graph)))
          (push w ids))))
    (loop for e in (remove-duplicates ids)
          if (null (find e except-for))
            collect e)))

(defun memory-access-local-p (blueprint id)
  (declare (optimize (speed 3))
           (type list blueprint)
           (type symbol id))
  (let ((search-key
          (loop for bp in blueprint
                for nth fixnum upfrom 0
                if (and (not (eql (node-class bp) :Render)) (or (find id (node-reads bp)) (find id (node-writes bp))))
                  collect nth)))
    (assert search-key () "The id ~a is not used in the bp." id)
    (loop with start fixnum = (apply #'min search-key)
          with end fixnum = (apply #'max search-key)
          with depth fixnum = 0
          for nth upfrom start to end
          for ir = (nth nth blueprint)
          if (find (node-type ir) `(:FOR :IF))
            do (incf depth)
          else if (find (node-type ir) `(:ENDFOR :ENDIF))
                 do (decf depth)
          end
          if (< depth 0) do (return-from memory-access-local-p nil))
    t))

(defmethod rewrite-as-scalar ((buffer buffer) wi suffix)
  (setf (buffer-nrank buffer) -1))

(defun schedule-outputs (graph)
  (let ((outs))
    (loop for n in (graph-nodes graph)
          if (null (getattr n :allocate-p))
            do (setf outs (append outs (node-writes n))))
    (remove-duplicates outs)))

(defmethod graph-scalarify (blueprint (node Node) (schedule-graph Graph))
  "Rewrites the buffer as scalar as many as possible"
  (declare (type list blueprint))
  (let* ((ids (blueprint-tmp-buffers blueprint schedule-graph :except-for (schedule-outputs schedule-graph)))
         (replaceable (loop for i in ids if (memory-access-local-p blueprint i) collect i))
         (suffix))
    (loop for b in blueprint
          if (eql (node-type b) :FOR) do (push (getattr b :idx) suffix)
          if (eql (node-type b) :ENDFOR) do (setf suffix (remove (getattr b :idx) suffix))
          if (not (eql (node-class b) :Render)) do
            (loop for r in (node-reads b)
                  for rt in (relay-reads (read-type-relay b))
                  for ri in (relay-read-iters (read-type-relay b))
                  if (and (symbolp r) rt ri (find r replaceable))
                    do (rewrite-as-scalar rt ri (reverse suffix)))
            (assert (= (length (node-writes b)) 1) () "graph-scalarify excepts only one write node. (please update the loop below...)")
            (loop for w in (node-writes b)
                  for wt in (relay-writes (read-type-relay b))
                  for wi in (relay-write-iters (read-type-relay b))
                  if (and (symbolp w) wt wi (find w replaceable))
                    do (rewrite-as-scalar wt wi (reverse suffix))
                       (setf (getattr b :declare-type) (list t)
                             (node-reads node) (remove w (node-reads node)))))
    blueprint))

(defmethod graph-exprify (blueprint (node Node) (schedule-graph Graph))
  (declare (type list blueprint))
  (let* ((ids (blueprint-tmp-buffers blueprint schedule-graph :except-for (schedule-outputs schedule-graph)))
         (replaceable (loop for i in ids if (memory-access-local-p blueprint i) collect i)))
    (flet ((replace-p (id) (find id replaceable)))
      blueprint)))
