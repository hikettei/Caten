(defpackage :caten/codegen/fusion-rules
  (:documentation "`caten/coodegen/fusion-rules` is a package that supports for schedule.lisp, especially when merging compilcated views.")
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/avm)
  (:import-from
   :caten/codegen/helpers
   #:permute-list)
  (:export
   #:apply-fusion-rules))

(in-package :caten/codegen/fusion-rules)

(defun apply-merge-permute (write-type write-is tgt-type tgt-is)
  "Transforms tgt-type to fir in the write-type"
  (declare (ignore write-is))
  (when (not (= (buffer-nrank write-type) (buffer-nrank tgt-type)))
    (return-from apply-merge-permute nil))
  (values tgt-type tgt-is))

(defun apply-merge-broadcast (g write-type write-is tgt-type tgt-is &aux (tgt-type (copy-buffer tgt-type)))
  "
Transforms tgt-type to fit in the write-type
T=0 | ... = MOVE(..., tgt)
T=1 | ... = f(... read)
"
  (assert (>= (buffer-nrank write-type) (buffer-nrank tgt-type)))
  (when (and (not (= (buffer-nrank write-type) (length (buffer-views write-type))))
             (every #'null (buffer-views write-type)))
    (setf (buffer-views write-type) (loop repeat (buffer-nrank write-type) collect nil)))
  (labels ((merge-broadcast (views list1 &key (default 1) (size) &aux (list (copy-list list1)))
             (loop for v in views
                   for nth upfrom 0
                   for s = (nth nth size)
                   if (fourth v)
                     collect
                     (if (eql default :identity)
                         (progn (assert size) `(0 ,s ,1 t))
                         (if (eql default :identity-size)
                             (progn (assert size) s)
                             default))
                   else
                     collect (or (pop list)
                                 (progn
                                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                                     (warn "apply-merge-broadcast: Cannot merge views: ~a -> ~a, failing to merge views..." write-type tgt-type))
                                   (return-from apply-merge-broadcast nil)))))
           (fixup (base-type tgt-type)
             (assert (>= (buffer-nrank base-type) (buffer-nrank tgt-type)))
             (setf (buffer-shape tgt-type) (merge-broadcast (buffer-views base-type) (buffer-shape tgt-type)
                                                            :default :identity-size
                                                            :size (buffer-shape base-type))
                   (buffer-stride tgt-type) (merge-broadcast (buffer-views base-type) (buffer-stride tgt-type)
                                                             :default :identity-size
                                                             :size (buffer-stride base-type))
                   (buffer-views tgt-type) (merge-broadcast
                                            (buffer-views base-type) (buffer-views tgt-type)
                                            :size (buffer-shape base-type)
                                            :default :identity) ;; default (0 size 1 t)
                   (buffer-nrank tgt-type) (length (buffer-shape tgt-type)))))
    (fixup write-type tgt-type)
    (values tgt-type (buffer-merge-dims g tgt-type))))

(defun apply-fusion-rules (g tgt-view-types tgt-views read-view-types read-views
                           read-type read-is
                           write-type write-is
                           tgt-type tgt-is)
  "This function should only called in the `transform-and-mergeable-p` function."
  (declare (type Graph g)
           (type list tgt-views tgt-view-types)
           (type Buffer read-type tgt-type write-type)
           (type Iteration-Space read-is tgt-is write-is))
  ;; Reject unimplemented case
  ;; [TODO] If the views was :reshape, we have to rewrite the already grouped scheduled-item (e.g.: extending the bands 25 -> 5, 5)
  ;; or: as a result of applying apply-fusion-rules to the child, and the length of `procedure` is match, and merge.
  (when (or (find :reshape tgt-view-types) (find :reshape read-view-types))
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (warn "apply-fusion-rules: reshape fusion rule is not implemented: ~a ~a" tgt-view-types read-view-types))
    (return-from apply-fusion-rules nil))
  
  (when (or (not (= 1 (length tgt-views))) (not (= 1 (length read-views))))
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (warn "apply-fusion-rules: Cannot merge views ~a ~a" tgt-view-types read-view-types))
    (return-from apply-fusion-rules nil))

  (when (find :shrink tgt-view-types) (return-from apply-fusion-rules nil))
  (when (find :shrink read-view-types) (return-from apply-fusion-rules nil))
;;  (print "+Running+")
;;  (print tgt-view-types)
;;  (print "=>")
;;  (print read-view-types)
  (labels ((merge! (type prev-type prev-is prev-tgt-type prev-tgt-is)
             (case type
               (:Permute   (apply-merge-permute prev-type prev-is prev-tgt-type prev-tgt-is))
               (:Broadcast (apply-merge-broadcast g prev-type prev-is prev-tgt-type prev-tgt-is))
               (:Shrink    (error ":shrink shoud have been rejected?"))
               (:Reshape   (error "Not ready"))
               (otherwise  (error "The view ~a is not recognised?" type)))))
    ;; return nil if failed
    (multiple-value-bind (write-tmp-buf write-tmp-is) (merge! (car tgt-view-types) write-type write-is tgt-type tgt-is)
      (when (null write-tmp-buf) (return-from apply-fusion-rules nil))
      (multiple-value-bind (read-tmp-buf read-tmp-is) (merge! (car read-view-types) read-type read-is write-tmp-buf write-tmp-is)
        (when (null read-tmp-buf) (return-from apply-fusion-rules nil))
        (assert (= (buffer-nrank read-tmp-buf) (buffer-nrank read-type)))
        (values read-tmp-buf read-tmp-is read-type read-is)))))
