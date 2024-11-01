(defpackage :caten/polyhedral/ir
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
   #:poly-dependencies))

(in-package :caten/polyhedral/ir)

(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule)
   (domain   :accessor poly-domain)
   (dependencies :accessor poly-dependencies))
  (:documentation ""))

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
  (let* ((schedule (schedule-set-options (copy (poly-schedule pg)) :separate))
         (build (ast-build-from-context (set-from-str "{:}")))
         (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (ast-build-node-from-schedule build schedule))
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
                   (t (warn "pprint: the key ~a is not implemented." key)))))
        (mapc #'(lambda (x) (explore schedule x)) (reverse (alexandria:hash-table-keys schedule)))))))

(defmethod print-object ((pg Polyhedral-IR) stream)
  (print-unreadable-object (pg stream :type t)
    (format stream "~a~%[Kernel]:~%~a" (pprint-schedule (copy (poly-schedule pg))) (debug-render-to-clang pg))))

