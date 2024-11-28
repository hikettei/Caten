(in-package :caten/test-suite)

(defun compile-transformer (n-layers)
  (let ((model (Transformer 64 4 n-layers 1e-5 32 :max-seq-len 32)))
    (caten (forward model (make-tensor `(1 s)) (iconst 'n)))))

(defun count-compiled-kernels (graph)
  (length
   (remove-duplicates
    (loop for node in (graph-nodes graph)
          if (eql (node-type node) :JIT_KERNEL)
            collect (compiled-kernel-name (getattr node :kernel-info))))))

(defun hide-vars (code node kernels &key (test-with-memory-planner nil))
  (declare (type string code)) ;; set test-with-memory-planner=t when using memory-planner
  (setf code (string-downcase code))
  ;; [TODO] Maybe the args of :JIT_KERNEL does not reflects what the actual kernel args are.
  ;; bcuz they are updated by the memory planner.
  (when (null test-with-memory-planner)
    ;; removing all variables
    (setf code (cl-ppcre:regex-replace-all "val_[0-9]+" code "[TMP_SCALAR_VAL]")))
  (loop for nth upfrom 0
        for v in (map 'list #'(lambda (x) (compiled-kernel-name (getattr x :kernel-info))) kernels)
        do (setf code (cl-ppcre:regex-replace-all (format nil "~(~a~)" v) code (format nil "func"))))
  (loop for nth upfrom 0
        for v in (append (node-reads node) (node-writes node))
        do (setf code (cl-ppcre:regex-replace-all (format nil "~(~a~)" v) code (format nil "var~a" nth))))
  (when test-with-memory-planner
    (setf code (cl-ppcre:regex-replace-all "val_[0-9]+" code "[TMP_SCALAR_VAL]")))
  code)

(defun compare-two-cache-nodes (node1 node2 kernels no-mp)
  (when (not (eql (node-type node1) (node-type node2)))
    (return-from compare-two-cache-nodes nil))
  (if (eql (node-type node1) :JIT_KERNEL)
      (let* ((info1 (getattr node1 :kernel-info))
             (info2 (getattr node2 :kernel-info))
             (base-node1
               (if (getattr node1 :cached-p)
                   (find (compiled-kernel-name info1) kernels
                         :test #'(lambda (x y)
                                   (and (null (getattr y :cached-p)) (eql x (compiled-kernel-name (getattr y :kernel-info))))))
                   node1))
             (base-node2
               (if (getattr node2 :cached-p)
                   (find (compiled-kernel-name info2) kernels
                         :test #'(lambda (x y)
                                   (and (null (getattr y :cached-p)) (eql x (compiled-kernel-name (getattr y :kernel-info))))))
                   node2))
             (nmp (= no-mp 1))
             (code1 (hide-vars (compiled-kernel-code info1) base-node1 kernels :test-with-memory-planner nmp))
             (code2 (hide-vars (compiled-kernel-code info2) base-node2 kernels :test-with-memory-planner nmp)))
        (and       
         (equal (node-reads node1) (node-reads node2))
         (equal (node-writes node1) (node-writes node2))
         (if (equal code1 code2)
             t
             (ok nil
                 (format nil "The generated kernel still has a diff points:~%Code1[Using Schedule Cache]:~%~a~%Code2[No Schedule Cache]:~%~a
[Masks Removed]
Code1:
~a
Code2:
~a"
                         code1 code2
                         (compiled-kernel-code info1) (compiled-kernel-code info2))))))
      (and
       (equal (node-reads node1) (node-reads node2))
       (equal (node-writes node1) (node-writes node2)))))

(deftest transformer-schedule-cache-count-test
  (with-protect-jit
    (loop for i upfrom 1 below 6
          for tf = (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0) (compile-transformer i)))
          ;; [TODO] The number of kernels should be a constant regardless of layers!!
          do (ng (= (+ 14 (* 60 i)) (count-compiled-kernels tf))
                 (format nil "(Currently Failing ...) Compiled ~a kernels (expecting ~a)" (count-compiled-kernels tf) (+ 14 (* 60 i)))))))

(deftest transformer-schedule-cache-consistency-test
  (with-protect-jit
    (dolist (no-mp `(0 1))
      (testing (format nil "Running with NO_MEMORY_PLANNER=~a" no-mp)
        (let* ((n-layers 3)
               (tf1 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0 :AUTO_SCHEDULER 0 :NO_MEMORY_PLANNER no-mp) (compile-transformer n-layers))))
               (tf2 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 1 :AUTO_SCHEDULER 0 :NO_MEMORY_PLANNER no-mp) (compile-transformer n-layers))))
               (kernels
                 (loop for item in (append (graph-nodes tf1) (graph-nodes tf2))
                       if (eql (node-type item) :JIT_KERNEL)
                         collect item)))
          (ok (= (length (graph-nodes tf1)) (length (graph-nodes tf2)))
              (format nil "Scheduled ~a(no cache) and ~a(cached) kernels" (length (graph-nodes tf1)) (length (graph-nodes tf2))))
          (loop for node1 in (graph-nodes tf1)
                for node2 in (graph-nodes tf2)
                for nth upfrom 0
                for ok = (compare-two-cache-nodes node1 node2 kernels no-mp)
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
                (format nil "The scheduler cache reduced this ~a kernels" (count-compiled-kernels tf1)))))))))

(deftest transformer-schedule-cache-consistency-test-parallel
  (with-protect-jit
    (if (= 1 (ctx:getenv :CI))
        (skip "Unstable test on CI")
        (dolist (no-mp `(0 1))
          (testing (format nil "Running with NO_MEMORY_PLANNER=~a" no-mp)
            (let* ((n-layers 6)
                   (tf1 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0 :AUTO_SCHEDULER 0 :NO_MEMORY_PLANNER no-mp :parallel 4) (compile-transformer n-layers))))
                   (tf2 (avm-graph (ctx:with-contextvar (:NO_SCHEDULE_CACHE 0 :AUTO_SCHEDULER 0 :NO_MEMORY_PLANNER no-mp :parallel 0) (compile-transformer n-layers))))
                   (kernels
                     (loop for item in (append (graph-nodes tf1) (graph-nodes tf2))
                           if (eql (node-type item) :JIT_KERNEL)
                             collect item)))
              (ok (= (length (graph-nodes tf1)) (length (graph-nodes tf2)))
                  (format nil "Scheduled ~a(no cache) and ~a(cached) kernels" (length (graph-nodes tf1)) (length (graph-nodes tf2))))
              (loop for node1 in (graph-nodes tf1)
                    for node2 in (graph-nodes tf2)
                    for nth upfrom 0
                    for ok = (compare-two-cache-nodes node1 node2 kernels no-mp)
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
                    (format nil "The scheduler cache reduced this ~a kernels" (count-compiled-kernels tf1))))))))))
