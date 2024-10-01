(in-package :caten/air)

(define-page ("caten/air" "packages/caten.air.md")
  (body "aIR(=**A**bstract**IR**) is a Caten's internal IR and is used to represent computations expressed as either Let-Binding Based IR or a DAG.

Each nodes are represented using a `Node` structure, and their list (or tree structure) is maintained by the `Graph` CLOS class. The compilation is performed by efficiently manipulating and optimizing the graph using a pattern matcher.

Note that All nodes must be declared using the defnode macro before compilation. This allows node-related errors to be detected and reported during compilation.")
  
  (title "Node")
  (doc/struct "Node" 'Node)
  (doc/macro "defnode" 'defnode)
  (doc/function "make-node" 'make-node)
  (doc/function "copy-node" 'copy-node)
  (doc/function "getattrs" 'getattrs)
  (doc/function "getattr" 'getattr)
  (doc/function "remattr" 'remattr)
  
  (subtitle "Examples")
  (example-repl "(make-node :BinaryOps :ADD (list 'x) (list 'y 'z) :reduction t)")
  
  (title "Graph")
  (doc/class "Graph" 'Graph)
  (doc/class "FastGraph" 'FastGraph)
  
  (doc/generic "copy-graph" #'copy-graph)
  (doc/function "make-graph" #'make-graph)
  (doc/generic "graph-nodes" #'graph-nodes)
  (doc/generic "id->value" #'id->value)
  (doc/generic "id->users" #'id->users)
  (doc/generic "remnode" #'remnode)
  (doc/generic "insert-nodes" #'insert-nodes)
  (doc/function "->fast-graph" #'->fast-graph)
  (doc/generic "->graph" #'->graph)
  (doc/generic "verify-graph" #'verify-graph)
  
  (title "Simplifier")
  (doc/macro "defsimplifier" 'defsimplifier)
  (title "Visualizer")
  (doc/function "->dot" #'->dot))
