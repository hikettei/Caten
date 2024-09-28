(in-package :caten/air)

(defun helper/color (type)
  (case type
    (:movement "gray")
    (:node "#e6e6fa")
    (:chain "#f0e68c")
    (:input "#f0f8ff")
    (:parameter "#ff6347")))

(defmethod ->dot ((graph Graph) &key (pathname "/tmp/graph.dot") (open t))
  "Visualizes the graph using graphviz. Set open=t to open the resulting image in the default browser."
  (with-open-file (stream pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "digraph computation_node {
  node[charset=\"UTF-8\"
       bgcolor = \"#EDEDED\"
       rankdir = TB
       nodesep = 0.8
       rankset = 1.1
       shape=\"box\"
       style=\"filled\"
       color=\"black\"
       penwidth=\"2\"];~%")
    (dolist (node (graph-nodes graph))
      (format stream "  ~a [label = \"~a\" fillcolor=\"~a\" style=\"filled, solid\"];~%"
              (node-id node)
              (node-type node)
              (helper/color :movement)))
    (dolist (node (graph-nodes graph))
      (dolist (w (node-writes node))
        (dolist (u (id->users graph w))
          (format stream "  ~a -> ~a~a;~%" (node-id node) (node-id u) "[penwidth=\"2\"]"))))
    (format stream "}"))
  (format t "->dot: Saving the graph at ~a, ~a.png, ~a.html.~%" pathname pathname pathname)
  (when open
    (uiop:launch-program (list "dot" "-Tpng" pathname "-o" (format nil "~a.png" pathname)) :output t)
    (let ((htmlpath (format nil "~a.html" pathname)))
      (with-open-file (stream htmlpath :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format stream "<html><body><img src=\"~a.png\"></body></html>" pathname)
        (uiop:launch-program (list "open" htmlpath) :output t)))))
