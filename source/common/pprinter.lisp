(defpackage :caten/common.pprinter
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl)
  (:export #:pprint-isl-schedule))

(in-package :caten/common.pprinter)

(defmethod pprint-isl-schedule ((schedule schedule))
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
