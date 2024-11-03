(defpackage :caten/polyhedral/packing
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl :caten/polyhedral/ir :caten/polyhedral/config)
  (:export #:packing))

(in-package :caten/polyhedral/packing)

(defun get-mark-insertable-bands (schedule)
  (declare (type schedule schedule))
  (let ((node (schedule-get-root schedule))
        (next-nodes)
        (tileable-bands))
    ;; Enumerate all tilable bands
    (loop named tiling-search
          for n-children = (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
          while (> n-children 0) do
            ;; Reached to the maximum tile band size => Stop
            (loop named search-for-children
                  for nth upfrom 0 below n-children
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band)
                  if (eql type :schedule-node-band)
                    do (push band tileable-bands)
                       (push (schedule-node-get-child band 0) next-nodes)
                       (return-from search-for-children)
                  else
                    do (push band next-nodes))
            (when (= (length next-nodes) 0)
              (return-from tiling-search))
            (setf node (pop next-nodes)))
    tileable-bands))

(defun insert-mark (band)

  )

(defun packing (config ir)
  (declare (type polyhedral-ir ir))
  

  )
