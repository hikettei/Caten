(in-package :caten/air)

(defun helper/color (type)
  (case type
    (:movement "gray")
    (:node "#e6e6fa")
    (:chain "#f0e68c")
    (:input "#f0f8ff")
    (:parameter "#ff6347")
    (:module "#6495ed")))

(defgeneric ->dot (graph &key pathname open title)
  (:documentation "
```
(->dot graph &key (pathname \"/tmp/graph.dot\") (open t) (title \"node\"))
```

Visualizes the graph using graphviz(requirement). Set open=t to open the resulting image in the default browser. A tmp file is created at the pathname location. The graph is saved as a .png and .html file. The title is used in the html file."))

(defun render-list (list)
  (apply #'concatenate 'string
	 (butlast (loop for n in list
			append (list (format nil "~a" n) ", ")))))

(defmethod ->dot ((graph Graph) &key (pathname "/tmp/graph.dot") (open t) (title "node"))
  (with-open-file (stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "digraph computation_node {
  node[charset=\"UTF-8\"
       bgcolor = \"#EDEDED\"
       fontname = \"Migu 1M\"
       rankdir = TB
       nodesep = 0.8
       rankset = 1.1
       shape=\"record\"
       style=\"filled\"
       color=\"black\"
       penwidth=\"2\"];~%")
    (labels ((node (id label color style)
               (format stream "  ~a [label = \"~a\" fillcolor=\"~a\" style=\"~a\"];~%" id label color style))
             (render-attrs (str node attrs)
               (format nil "{~a|~a}" str
                       (with-output-to-string (out)
                         (dolist (attr attrs)
                           (when (not (find attr `(:_type_relay :_read_views)))
                             (when (getattr node attr)
                               (if (listp (getattr node attr))
                                   (when (every #'identity (getattr node attr))
                                     (format out "  :~a=~a" attr (getattr node attr)))
                                   (let ((v (getattr node attr)))
                                     (if (or (numberp v) (symbolp v) (stringp v) (null v) (eql t v)
                                             (keywordp v))
                                         (format out "  :~a=~a" attr (getattr node attr))
                                         (format out "  :~a=[unprintable: ~a]" attr (type-of v)))))))))))
             (node-name (node)
               (let ((name (case (node-class node)
                             (:Module (subseq (princ-to-string (node-type node)) 6))
                             (otherwise (princ-to-string (node-type node))))))
                 (if (getattr node :_type_relay :allow-undefined t)
                     (let ((relay (uiop:symbol-call :caten/codegen/shape-inference :inferred-type-vizualize-to-dot (getattr node :_type_relay))))
                       (format nil "~a|~a~a" (node-class node) name relay))
                     (format nil "~a|~a" (node-class node) name)))))
      (dolist (node (graph-nodes graph))
        (case (node-class node)
          (:UnaryOps
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :node) "filled, curve"))
          (:BinaryOps
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :node) "filled, curve"))
          (:TernaryOps
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :node) "filled, curve"))
          (:Buffer
           (case (node-type node)
             (:Allocate
              (node (node-id node)
                    (let ((nrank (getattr node :nrank)))
                      (with-output-to-string (out)
                        (format
                         out "{ALLOCATE|shape=[~a]~a|strides=[~a]|dtype=~a}"
                         (subseq (node-reads node) 0 nrank)
                         (if (every #'numberp (subseq (node-reads node) 0 nrank))
                             (format nil ", size=(~a)" (apply #'* (subseq (node-reads node) 0 nrank)))
                             "")
                         (subseq (node-reads node) nrank (* 2 nrank))
                         (getattr node :dtype))))
                    (helper/color :node) "filled, solid"))
             (:VIEW
              (node (node-id node)
                    (let ((nrank (getattr node :nrank)))
                      (flet ((subseq1p (x y z) (subseq x (1+ y) (1+ z))))
                        (with-output-to-string (out)
                          (format
                           out
                           "{VIEW|shape=[~a]~a|masks=[~a]~a~a}"
                           (subseq1p (node-reads node) 0 nrank)
                           (if (every #'numberp (subseq1p (node-reads node) 0 nrank))
                               (format nil ", size=(~a)" (apply #'* (subseq1p (node-reads node) 0 nrank)))
                               "")
	                   (let ((upfrom (subseq1p (node-reads node) nrank (* 2 nrank)))
	                         (below (subseq1p (node-reads node) (* 2 nrank) (* 3 nrank)))
                                 (by (subseq1p (node-reads node) (* 3 nrank) (* 4 nrank)))
                                 (bc (getattr node :broadcast)))
		             (render-list
		              (map 'list #'(lambda (x y z l) (format nil "(~a)" (render-list (list x y z l)))) upfrom below by bc)))
                           (format nil "|stride=~a" (subseq1p (node-reads node) (* 4 nrank) (* 5 nrank)))
                           (if (getattr node :permute)
                               (format nil "|permute=~a" (getattr node :permute))
                               "")))))
                    (helper/color :node) "filled, solid"))
             (otherwise
              (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :input) "filled, solid"))))
          (:INDEX-COMPONENTS
           (node (node-id node) (node-name node) (helper/color :node) "filled, solid"))
          (:Module
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :module) "filled, solid"))
          (:Graph
           (if (eql (node-type node) :Schedule-Item)
               (if (getattr node :allocate-p)
                   (let ((alloc (car (getattr node :items))))
                     (assert alloc)
                     (if (getattr alloc :from)
                          (node
                           (node-id node)
                           (format nil "Input[~a] ~a" (car (node-writes alloc)) (subseq (node-reads alloc) 0 (getattr alloc :nrank)))
                           (helper/color :input)
                           "filled, solid")
                          (node
                           (node-id node)
                           (format nil "TmpAlloc[~a]" (subseq (node-reads alloc) 0 (getattr alloc :nrank)))
                           (helper/color :chain)
                           "filled, solid")))
                   (if (getattr node :jitable)
                       (node (node-id node) (getattr node :name) (helper/color :node) "filled, solid")
                       (let ((node (car (getattr node :items))))
                         (assert node)
                         (node (node-id node) (format nil "[VMOP] ~a" (node-type node)) (helper/color :chain) "filled, solid"))))
               (node (node-id node) (node-name node) (helper/color :movement) "filled, solid")))
          (otherwise
           (node (node-id node) (node-name node) (helper/color :movement) "filled, solid")))))
    (dolist (node (graph-nodes graph))
      (dolist (w (node-writes node))
        (dolist (u (id->users graph w))
          (format stream "  ~a -> ~a;~%" (node-id node) (node-id u)))))
    (format stream "}"))
  (format t "(DOT=1) ->dot: Saving the graph \"~a\" at ~a, ~a.png, ~a.html.~%" title pathname pathname pathname)
  (when open
    (uiop:launch-program (list "dot" "-Tpng" pathname "-o" (format nil "~a.png" pathname)) :output t)
    (let ((htmlpath (format nil "~a.html" pathname)))
      (with-open-file (stream htmlpath :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format stream "<html><p><b><font size=\"5\">~a</b></p><body><img src=\"~a.png\"></body></html>" title pathname)
        (uiop:launch-program (list "open" htmlpath) :output t)))))

(defun compute-n-children (graph id)
  (let ((seen #+sbcl(make-hash-table :test #'equal :synchronized t :size 2048)
              #+clozure(make-hash-table :test #'equal :shared t :size 2048)
              #-(or sbcl clozure)(make-hash-table :test #'equal :size 2048))
        (count 0))
    (labels ((explore (id)
               (when (gethash id seen)
                 (return-from explore))
               (let ((val (id->value graph id)))
                 (when val
                   (setf (gethash id seen) t)
                   (incf count)
                   (mapc #'explore (node-reads val))))))
      (explore id))
    count))
;; [TODO] optimize screen-width automatically
(defparameter *indent* 0)
(defun pprint-graph (graph &key (screen-width 140) (stream t)
                     &aux (seen nil) (preserved (make-hash-table)) (stashed nil) (part 0) (static-gensym (make-hash-table)))
  "
```
(pprint-graph graph &key (screen-width 140) (stream t))
```

The function `pprint-graph` prints the graph in a tree-like structure. `screen-width` controls the width of the output, if the graph is too wide and cannot fit in the screen, it will be split into multiple pages.
"
  (declare (type graph graph))
  (when (null (graph-outputs graph)) (warn "pprint-grpah does nothing without graph-outputs."))
  (assert (>= screen-width 20) () "screen-width must be greater than 20.")
  (princ
   (with-output-to-string (out)
     (labels ((indent (lastp)
                (with-output-to-string (tmp)
                  (dotimes (i *indent*)
                    (if (find i (alexandria:hash-table-values preserved))
                        ;; intersection
                        (if (= i (1- *indent*))
                            (if lastp
                                (format tmp "└")
                                (format tmp "├"))
                            (format tmp "│"))
                        (princ " " tmp)))))
              (preserve (val &aux (node (id->value graph val)))
                (when node
                  (setf (gethash (node-id node) preserved) (1+ *indent*))))
              (static-gensym (id &key (prefix ""))
                (if (gethash id static-gensym)
                    (gethash id static-gensym)
                    (setf (gethash id static-gensym) (format nil "~a~a" prefix (hash-table-count static-gensym)))))
              (separate-screen ()
                (format out "=== [P: ~a] ====" part)
                (dotimes (i (- screen-width 15))
                  (princ "=" out))
                (format out "~%"))
              (restart-point (node)
                (format out "[P=~a, ID=~a]:~%" (- part 1) (static-gensym (node-id node))))
              (princ-node (node)
                ;; princ-node controls how the node is rendered.
                (case (node-type node)
                  (:SCHEDULE-ITEM
                   (if (getattr node :allocate-p)
                       (let ((alloc (car (getattr node :items))))
                         (assert alloc)
                         (princ-node alloc))
                       (if (getattr node :jitable)
                           (format nil "[KERNEL] ~a" (getattr node :name))
                           (let ((node (car (getattr node :items))))
                             (assert node)
                             (princ-node node)))))
                  (:Allocate
                   (format nil "Allocate[:~(~a~)] ~a" (getattr node :dtype) (subseq (node-reads node) 0 (getattr node :nrank))))
                  (:LOAD
                   (format nil "load(~a)" (getattr node :value)))
                  (otherwise
                   (format nil ":~a {~a}" (node-type node) (static-gensym (node-id node) :prefix "N")))))
              (pn (node lastp)
                (let ((item (format nil "~a~a~a~%" (indent lastp) (if (zerop *indent*) "" " ") (princ-node node))))
                  (princ item out)
                  (> (length item) screen-width)))
              (child-weights (node lastp-map)
                (let* ((weights (map 'list #'(lambda (x) (compute-n-children graph x)) (node-reads node)))
                       (paired (map 'list #'list weights (node-reads node) lastp-map)))
                  ;; Small children first
                  ;; argsort
                  (map 'list #'cdr (sort paired #'< :key #'car))))
              (explore (id &optional (lastp nil))
                (when (find id seen)
                  (let ((node (id->value graph id)))
                    (when node
                      (format out "~a ~a~%" (indent lastp) (princ-node node))
                      (remhash (node-id node) preserved))
                    (return-from explore)))
                (push id seen)
                (let ((node (id->value graph id)))
                  (when (null node) (return-from explore))
                  (let ((stash-p (pn node lastp)))
                    (remhash (node-id node) preserved)
                    (if stash-p
                        (let ((*indent* (+ 2 *indent*)))
                          (format out "~a [P=~a, ID=~a]~%" (indent lastp) part (static-gensym (node-id node)))
                          (setf seen (remove id seen)) (push id stashed))
                        (progn
                          (mapc #'preserve (node-reads node))
                          (let ((*indent* (+ 2 *indent*))
                                (lastp-map (make-list (length (node-reads node)))))
                            (when lastp-map (setf (car lastp-map) t))
                            (loop for pair in (child-weights node (reverse lastp-map))
                                  for nth upfrom 0
                                  do (explore (car pair) (second pair))))))))))
       (setf stashed (copy-list (graph-outputs graph)))
       (dotimes (i screen-width) (princ "=" out))
       (format out "~%")
       (loop while stashed
             for ids = stashed do
               (separate-screen)
               (incf part)
               (setf stashed nil preserved (make-hash-table))
               (let ((*indent* (if (= 0 part) 0 2)))
                 (mapc
                  #'(lambda (x)
                      (unless (= 0 part) (restart-point (id->value graph x)))
                      (explore x))
                  ids)))
       (dotimes (i screen-width) (princ "=" out))
       (format out "~%")))
   stream))
