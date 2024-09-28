(in-package :caten/air)

(defun helper/color (type)
  (case type
    (:movement "gray")
    (:node "#e6e6fa")
    (:chain "#f0e68c")
    (:input "#f0f8ff")
    (:parameter "#ff6347")
    (:module "#6495ed")))

(defmethod ->dot ((graph Graph) &key (pathname "/tmp/graph.dot") (open t) (title "node"))
  "Visualizes the graph using graphviz. Set open=t to open the resulting image in the default browser.
Requirements: DOT"
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
                           (when (getattr node attr)
                             (if (listp (getattr node attr))
                                 (when (every #'identity (getattr node attr))
                                   (format out "  :~a=~a" attr (getattr node attr)))
                                 (let ((v (getattr node attr)))
                                   (if (or (numberp v) (symbolp v) (stringp v) (null v) (eql t v)
                                           (keywordp v))
                                       (format out "  :~a=~a" attr (getattr node attr))
                                       (format out "  :~a=[unprintable: ~a]" attr (type-of v))))))))))
             (node-name (node)
               (let ((name (case (node-class node)
                             (:Module (subseq (princ-to-string (node-type node)) 6))
                             (otherwise (princ-to-string (node-type node))))))
                 (if (getattr node :_type_relay :allow-undefined t)
                     (let ((buffer (car (uiop:symbol-call :caten/ajit :relay-writes (uiop:symbol-call :caten/ajit :read-type-relay (getattr node :_type_relay))))))
                       (format nil "~a|~a ~a"
                               (node-class node) name
                               (uiop:symbol-call :caten/avm :buffer-shape buffer)))
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
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :input) "filled, solid"))
          (:INDEX-COMPONENTS
           (node (node-id node) (node-name node) (helper/color :node) "filled, solid"))
          (:Module
           (node (node-id node) (render-attrs (node-name node) node (getattrs node)) (helper/color :module) "filled, solid"))
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
