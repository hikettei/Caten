(defpackage :caten/codegen/scop
  (:documentation "Lowers the schedule-item into Polyhedral IR")
  (:import-from
   :caten/codegen/helpers
   #:render-list
   #:gid)
  (:import-from
   :caten/codegen/renderer
   :Default-Renderer
   #:render-expr)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters
   #:iteration-space-strides)
  (:import-from
   :caten/polyhedral/ir
   #:make-polyhedral-ir)
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/codegen/expr :caten/isl)
  (:export #:scop))

(in-package :caten/codegen/scop)

(defmethod render-domain-body-from-group ((node Node) &aux (idx2domain (make-hash-table)) (device 'Default-Renderer))
  (assert (eql (node-type node) :Schedule-Item))
  (assert (getattr node :blueprint) () "Cannot create a domain w/o lowered blueprint")
  (values
   (with-output-to-string (out)
     (format out "{~%")
     (loop with domains = nil
           for node in (getattr node :blueprint) do
             (case (node-type node)
               (:FOR
                (let ((by (getattr node :by)))
                  (assert (expr-scalar-equivalent-p by (expr-const 1 :int64)) () "The node ~a is not a scop." node))
                (push node domains))
               (:ENDFOR
                (setf domains (remove (getattr node :idx) domains :key #'(lambda (x) (getattr x :idx)))))
               (otherwise
                (let ((domains (reverse domains)))
                  (setf (gethash (node-id node) idx2domain)
                        (if domains
                            (format nil "~a[~(~a~)]" (node-id node) (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains)))
                            (format nil "~a[]" (node-id node))))
                  (if domains
                      (format out "  ~a[~(~a~)] : ~(~a~);~%"
                              (node-id node)
                              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
                              (apply
                               #'concatenate
                               'string
                               (butlast
                                (loop for dom in domains
                                      collect (format nil "0 <= ~(~a~) and ~(~a~)" (getattr dom :idx) (render-expr device (getattr dom :below)))
                                      collect " and ")))
                              ;; [TODO] Dynamic Shape are >= 1
                              )
                      (format out "  ~a[];~%" (node-id node)))))))
     (format out "}"))
   idx2domain))

(defun render-domain-from-loops (node domains &aux (device 'Default-Renderer))
  (if domains
      (format nil "  ~a[~(~a~)] : ~(~a~);~%"
              (node-id node)
              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
              (apply
               #'concatenate
               'string
               (butlast
                (loop for dom in domains
                      collect (format nil "0 <= ~(~a~) and ~(~a~)" (getattr dom :idx) (render-expr device (getattr dom :below)))
                      collect " and "))))
      (format nil "  ~a[];~%" (node-id node))))

(defmethod render-band-node-in-domain (area related-domains idx2domain)
  "Creates a band node(mupa):
```
for (i=0; i<10; i++)
  for (j=0; j<10; j++)
    S1[i, j] // If you were here:
    for (k=0; k<10; k++)
      S[i, j, k]
```
-> then the partial schedule is:
```
[] -> {
  S1[i, j] -> [i, j];
  S2[i, j, k] -> [i, j];
}
```
Corresponds to the position of the subgraph in the parent schedule.
"
  (let ((related-functions
          (loop for node in area
                if (null (find (node-type node) `(:FOR :ENDFOR)))
                  collect node))
        (declared-ids (map 'list #'(lambda (x) (getattr x :idx)) related-domains)))
    (when (null declared-ids) (return-from render-band-node-in-domain "[]"))
    (with-output-to-string (out)
      (format out "[~%")
      (loop for idx in declared-ids
            for nth upfrom 0
            if (not (= nth 0))
              do (format out ",~%")
            do (format out "~% { ")
               (loop for funcall in related-functions
                     for nth upfrom 0
                     if (not (= nth 0))
                       do (format out "; ")
                     do (format out "~a -> [~(~a~)]"
                                (or (gethash (node-id funcall) idx2domain) (error ""))
                                idx))
               (format out " } "))
      (format out "~%]"))))

(defun render-access-rep (reader type-reader node idx2domain render-graph)
  (assert (eql (node-type node) :Schedule-Item))
  (assert (getattr node :blueprint) () "Cannot create a domain w/o lowered blueprint")
  (union-map-from-str
   (with-output-to-string (out)
     ;; [TODO] Dynamic Shape
     (format out "[~(~a~)] -> {~%" "")
     (maphash
      #'(lambda (idx dom)
          (let ((tgt-node (find idx render-graph :key #'(lambda (x) (and (null (find (node-type x) `(:FOR :ENDFOR))) (node-id x))))))
            (flet ((is-zero (axis)
                     (expr-scalar-equivalent-p axis (expr-const 0 :int64))))
              (when tgt-node
                (loop for var in (funcall reader tgt-node)
                      for typ in (funcall type-reader (read-type-relay tgt-node))
                      if (symbolp var)
                        ;; [TODO] (getattr node :reduction)
                        do (format out "  ~a -> ~(~a~)[~(~a~)];~%"
                                   dom
                                   var
                                   (apply
                                    #'concatenate
                                    'string
                                    (butlast
                                     (when typ
                                       (loop for stride in (iteration-space-strides typ)
                                             for nth upfrom 0
                                             for gid = (gid nth)
                                             if (is-zero stride)
                                               ;; [TODO] :Permute and :View (Offset, By)
                                               collect (format nil "0")
                                             else
                                               collect (format nil "~(~a~)" gid)
                                           collect ", "))))))))))
      idx2domain)
     (format out "}"))))

(defmethod analyze-scop ((node Node))
  "
Reference: https://www.researchgate.net/publication/347152973_PET-to-MLIR_A_polyhedral_front-end_for_MLIR

1. Schedule Construction
```2
     [DOMAIN_NODE]
           |
      [BAND_NODE] (= corresponds to a loop in short)
           |
     [SEQUENCE_NODE]
       /       \
  [FILTER]  [FILTER] (= corresponds to an operation in short)
      |         |
        ...
```
"
  (assert (eql (node-type node) :Schedule-Item))
  (assert (getattr node :blueprint) () "Cannot create a domain w/o lowered blueprint")
  (let* ((render-nodes (getattr node :blueprint))
         (deps "")) ;; TODO
    (multiple-value-bind (domain idx2domain) (render-domain-body-from-group node)
      (labels ((explore-schedule-tree (from to
                                       &key
                                         (parent-loops)
                                       &aux
                                         (schedule :nothing)
                                         (region (subseq render-nodes from to)))
                 (loop with count = from while (< count to)
                       for node = (nth count render-nodes) do
                         (case (node-type node)
                           (:FOR
                            ;; -> Band Node
                            (let* ((endfor
                                     (find (getattr node :idx) (nthcdr count render-nodes)
                                           :key #'(lambda (x) (and (eql (node-type x) :ENDFOR) (getattr x :idx)))
                                           :test #'equalp))
                                   (_ (when (null endfor) (error "scop: malformed rendering graph ~a" render-nodes)))
                                   (endfor-abs-position
                                     (position
                                      (node-id endfor)
                                      render-nodes
                                      :key #'node-id)))
                              (declare (ignore _))
                              (assert (>= endfor-abs-position count))
                              ;; for (...) {
                              ;;  T0[]       }
                              ;;  ...        } dom-schedule = schedule of this area
                              ;; }
                              (let ((dom-schedule
                                      (explore-schedule-tree
                                       (1+ count) endfor-abs-position
                                       :parent-loops `(,@parent-loops ,node))))
                                (setf
                                 schedule
                                 (if (eql schedule :nothing)
                                     dom-schedule
                                     (schedule-sequence schedule dom-schedule)))
                                (setf count endfor-abs-position)
                                ;; Move next to endfor
                                (incf count))))
                           (otherwise
                            ;; -> Filter Node
                            (let* ((inst-schedule
                                     (schedule-from-domain
                                      (union-set-from-str
                                       (format nil "[~(~a~)] -> { ~a }" deps (render-domain-from-loops node parent-loops))))))
                              (setf
                               schedule
                               (if (eql schedule :nothing)
                                   inst-schedule
                                   (schedule-sequence schedule inst-schedule)))
                              (incf count)))))
                 (when (eql schedule :nothing) (error "nothing was scheduled?"))
                 (let* ((partial-schedule
                          ;; (last parent-loops) or just parent-loops?
                          (render-band-node-in-domain region (last parent-loops) idx2domain)))
                   (when (not (string= "[]" partial-schedule))
                     (setf schedule (schedule-insert-partial-schedule schedule (multi-union-pw-aff-from-str partial-schedule)))))
                 schedule))
        (values
         (union-set-from-str
          (format
           nil
           "[~(~a~)] -> ~a"
           "" ;; [TODO] Dynamic SHape
           domain))
         (render-access-rep #'node-reads #'relay-read-iters node idx2domain render-nodes)
         (render-access-rep #'node-writes #'relay-write-iters node idx2domain render-nodes)
         (explore-schedule-tree 0 (length render-nodes)))))))

(defmethod scop ((node Node))
  (when (null (getattr node :allocate-p :allow-undefined t))
    (assert (eql (node-type node) :Schedule-Item))
    (assert (getattr node :blueprint) () "Cannot create a domain w/o lowered blueprint")
    (multiple-value-bind (domain read write schedule) (analyze-scop node)
      (setf (getattr node :polyhedral) (make-polyhedral-ir domain read write schedule))
      ;; Testing
      ;;(print (getattr node :polyhedral))
      node)))
