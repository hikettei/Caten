(in-package :caten/test-suite)

(defun compile-transformer (n-layers)
  (let ((model (Transformer 64 4 n-layers 1e-5 32 :max-seq-len 32)))
    (caten (forward model (make-tensor `(b 32)) (iconst 'n)))))

(defun count-compiled-kernels (graph)
  (length
   (remove-duplicates
    (loop for node in (graph-nodes graph)
          if (eql (node-type node) :JIT_KERNEL)
            collect (compiled-kernel-name (getattr node :kernel-info))))))

(defun compare-two-cache-nodes (node1 node2)
  (when (not (eql (node-type node1) (node-type node2)))
    (return-from compare-two-cache-nodes nil))
  (if (eql (node-type node1) :JIT_KERNEL)
      (and
        ;;(equal (compiled-kernel-name (getattr node1 :kernel-info))
       ;;        (compiled-kernel-name (getattr node2 :kernel-info)))
       (equal (node-reads node1) (node-reads node2))
       (equal (node-writes node1) (node-writes node2)))
      (and
       (equal (node-reads node1) (node-reads node2))
       (equal (node-writes node1) (node-writes node2)))))

(deftest schedule-cache-count-test
  (with-protect-jit
    (loop for i upfrom 1 below 6
          for tf = (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0) (compile-transformer i)))
          do (ok (= (+ 15 i) (count-compiled-kernels tf))
                 (format nil "Compiled ~a kernels (expecting ~a)" (count-compiled-kernels tf) (+ 15 i))))))
;; [TODO] cached-pはCacheを遡って元のカーネルと一致するか検証する
(deftest schedule-cache-consistency-test
  (with-protect-jit
    (let* ((n-layers 3)
           (tf1 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0) (compile-transformer n-layers))))
           (tf2 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 1) (compile-transformer n-layers)))))
      (ok (= (length (graph-nodes tf1)) (length (graph-nodes tf2)))
          (format nil "Scheduled ~a(no cache) and ~a(cached) kernels" (length (graph-nodes tf1)) (length (graph-nodes tf2))))
      (loop for node1 in (graph-nodes tf1)
            for node2 in (graph-nodes tf2)
            for nth upfrom 0
            for ok = (compare-two-cache-nodes node1 node2)
            if (not ok)
              do (ok nil (format nil "Found a discrepancy point at the ~ath node.
  CACHED   | ~a
 NO CACHED |~a
" nth node1 node2)))
      (let ((kernels1
              (loop for item in (graph-nodes tf1)
                    if (eql (node-type item) :JIT_KERNEL)
                      collect item))
            (kernels2
              (loop for item in (graph-nodes tf2)
                    if (eql (node-type item) :JIT_KERNEL)
                      collect item)))
        (ok (= (length kernels1) (length kernels2))
            (format nil "Scheduled ~a(cached) and ~a(no cached) kernels" (length kernels1) (length kernels2)))
        (ok (not (= (length kernels1) (count-compiled-kernels tf1)))
            (format nil "The scheduler cache reduced this ~a kernels" (count-compiled-kernels tf1)))))))
