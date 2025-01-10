(defpackage :caten/codegen/polyhedral
  (:import-from :cffi #:foreign-funcall)
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl)
  (:export
   #:make-polyhedral-ir
   #:debug-render-to-clang
   #:Polyhedral-IR
   #:poly-schedule
   #:poly-domain
   #:poly-dependencies
   #:map-schedule-nodes
   #:->ast
   #:partial-schedule-node-id
   #:map-schedule-node-children
   #:schedule-node-get-undernearth-bands
   #:schedule-node-get-band-from-relative-idx
   #:render-schedule-node))

(in-package :caten/codegen/polyhedral)

(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule)
   (domain   :accessor poly-domain)
   (dependencies :accessor poly-dependencies)))

(defun make-polyhedral-ir (domain read write schedule)
  (let ((pg (make-instance 'Polyhedral-IR)))
    (setf (poly-schedule pg) schedule
          (poly-domain pg) domain)
    (let* ((access (union-access-info-from-sink read))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-schedule access schedule))
           (flow (union-access-info-compute-flow access))
           (RaW (union-flow-get-must-dependence flow))
           (access (union-access-info-from-sink write))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-may-source access read))
           (access (union-access-info-set-schedule access schedule))
           (flow   (union-access-info-compute-flow access))
           (WaW    (union-flow-get-must-dependence flow))
           (WaR    (union-flow-get-may-dependence flow))
           (dependencies
             (union-map-union
              (union-map-union WaR RaW)
              WaW)))
      (setf (poly-dependencies pg) dependencies)
      pg)))

(defmethod debug-render-to-clang ((pg Polyhedral-IR))
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (->ast (poly-schedule pg) 0))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))

(defmethod pprint-schedule ((schedule schedule))
  (let ((schedule (yaml:parse (schedule-to-str schedule))))
    (with-output-to-string (out)
      (format out "~%")
      (labels ((indent (n)
                 (make-string n :initial-element #\space))
               (separate-screen (indent &key (n 120))
                 (format out "~%~a~a~%" (indent indent) (make-string n :initial-element #\-)))
               (explore (schedule key &key (indent 0))
                 (cond
                   ((string= key "domain")
                    (format out "~adomain(~%" (indent indent))
                    (let ((domains (cl-ppcre:split
                                    ";"
                                    (cl-ppcre:regex-replace-all
                                     "{|}"
                                     (gethash key schedule)
                                     ""))))
                      (format out "~a"
                              (apply
                               #'concatenate
                               'string
                               (butlast
                                (loop for dom in domains
                                      collect (format nil "~a~a" (indent (+ indent 2)) dom)
                                      collect (format nil "~%"))))))
                    (format out "~a)" (indent indent)))
                   ((string= key "child")
                    (format out "~%~achild()" (indent indent))
                    (separate-screen indent)
                    (mapc
                     #'(lambda (x)
                         (explore (gethash key schedule) x :indent (+ indent 2)))
                     (reverse (alexandria:hash-table-keys (gethash key schedule)))))
                   ((string= key "schedule")
                    (let ((schedules (cl-ppcre:split
                                      " , "
                                      (cl-ppcre:regex-replace-all
                                       "{|}"
                                       (subseq (gethash key schedule) 1 (1- (length (gethash key schedule))))
                                       ""))))
                      (format out "~aschedule()" (indent indent))
                      (when schedules (format out "~%"))
                      (format out "~a"
                              (apply
                               #'concatenate
                               'string
                               (butlast
                                (loop for s in schedules
                                      for nth upfrom 0
                                      for separator = (if (= 1 (length schedules)) "-" (if (zerop nth) "┏" (if (= (length schedules) (1+ nth)) "┗" "┃")))
                                      collect (format nil "~a  ~a~a" (indent indent) separator s)
                                      collect (format nil "~%")))))))
                   ((or (string= key "sequence") (string= key "set"))
                    (format out "~a~a()" (indent indent) key)
                    (mapc
                     #'(lambda (x)
                         (mapc
                          #'(lambda (k)
                              (explore x k :indent (+ 2 indent)))
                          (alexandria:hash-table-keys x)))
                     (gethash key schedule)))
                   ((string= key "filter")
                    (format out "~%~afilter(~%" (indent indent))
                    (let ((domains (cl-ppcre:split
                                    ";"
                                    (cl-ppcre:regex-replace-all
                                     "{|}"
                                     (gethash key schedule)
                                     ""))))
                      (format
                       out
                       "~a"
                       (apply
                        #'concatenate
                        'string
                        (butlast
                         (loop for dom in domains
                           collect (format nil "~a~a" (indent (+ indent 2)) dom)
                           collect (format nil "~%")))))
                      (format out ")")))
                   ((or (string= key "permutable") (string= key "coincident"))
                    (format out "~%~a~a(~a)" (indent indent) key (gethash key schedule)))
                   ((or (string= key "mark"))
                    (format out "~amark(~a)" (indent indent) (gethash key schedule)))
                   (t (warn "pprint: the key ~a is not implemented." key)))))
        (mapc #'(lambda (x) (explore schedule x)) (reverse (alexandria:hash-table-keys schedule)))))))

(defmethod print-object ((pg Polyhedral-IR) stream)
  (print-unreadable-object (pg stream :type t)
    (format stream "~a~%[Kernel]:~%~a" (pprint-schedule (copy (poly-schedule pg))) (debug-render-to-clang pg))))

(defun map-schedule-nodes (f polyhedral-ir)
  "
```
(map-schedule-nodes f polyhedral-ir)
```
Iterates over the schedule nodes of a polyhedral-ir object. f is a lambda function which takes (type[keyword] node[schedule-node] mark[string]) as an argument.
This function returns a list of the results of applying f to each node. NIL is excluded in the list."
  (declare (type Polyhedral-IR polyhedral-ir) (type function f))
  (let* ((node (schedule-get-root (poly-schedule polyhedral-ir)))
         (next-nodes)
         (outputs))
    (loop named map-search
          for n-children = (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
          while (>= n-children 0) do
            (loop for nth upfrom 0 below n-children
                  for mark = (when (eql (schedule-node-get-type node) :schedule-node-mark) (identifier-name (schedule-node-mark-get-id node)))
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band) do
                    (let ((out (funcall f type band mark))) (when out (push out outputs)))
                    (push band next-nodes))
            (when (= (length next-nodes) 0) (return-from map-search))
            (setf node (pop next-nodes)))
    (nreverse outputs)))

(defun map-schedule-node-children (f schedule-node)
  (declare (type function f) (type isl::schedule-node schedule-node))
  (let* ((node schedule-node) (next-nodes) (outputs))
    (loop named map-search
          for n-children = (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
          while (>= n-children 0) do
            (loop for nth upfrom 0 below n-children
                  for mark = (when (eql (schedule-node-get-type node) :schedule-node-mark) (identifier-name (schedule-node-mark-get-id node)))
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band) do
                    (let ((out (funcall f type band mark))) (when out (push out outputs)))
                    (push band next-nodes))
            (when (= (length next-nodes) 0) (return-from map-search))
            (setf node (pop next-nodes)))
    (nreverse outputs)))

(defun schedule-node-get-undernearth-bands (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (map-schedule-node-children #'(lambda (type band mark) (declare (ignore mark)) (when (eql type :schedule-node-band) band)) schedule-node))

(defun schedule-node-get-band-from-relative-idx (schedule-node idx)
  (declare (type isl::schedule-node schedule-node) (type fixnum idx))
  (nth idx (schedule-node-get-undernearth-bands schedule-node)))

(defun gid (n) (intern (format nil "_gid~a" n)))

(defmethod ->ast (schedule rank)
  (macrolet ((set-option (name level)
	       `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_atomic_upper_bound" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:copy schedule))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))

(defun render-schedule-node (schedule-node)
  (let* ((schedule (isl:copy schedule-node))
         (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
         (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule))
         (p (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (p (isl::%isl-printer-set-output-format p 4))
         (q (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast-build-node)))
         (str (isl::%isl-printer-get-str q)))
    str))
