(in-package :caten/air)

(define-page ("caten/air" "packages/caten.air.md")
  (body "aIR(=**A**bstract**IR**) is a Caten's internal IR and is used to represent computations expressed as either Let-Binding Based IR or a DAG.

Each nodes are represented using a `Node` structure, and their list (or tree structure) is maintained by the `Graph` CLOS class. The compilation is performed by efficiently manipulating and optimizing the graph using a pattern matcher.

Note that All nodes must be declared using the defnode macro before compilation. This allows node-related errors to be detected and reported during compilation.")
  
  (title "Node")
  
  (subtitle "[struct] Node")
  (body (documentation (find-class 'Node) 't))
  (subtitle "[function] make-node")
  (body (documentation #'make-node 't))
  (subtitle "[macro] defnode")
  (body (documentation (macro-function 'defnode) 't))
  (subtitle "Examples")
  (example-repl "(make-node :BinaryOps :ADD (list 'x) (list 'y 'z) :reduction t)")
  
  (title "Graph")
  (body "Graph is a class that manages a list of nodes and provides various methods for manipulating them.")
  (subtitle "[class] Graph")
  (body (documentation (find-class 'Graph) 't))
  (title "FastGraph")
  (body "If the graph is DAG, use FastGraph to gain >10x speed improvement in a pattern matcher")
  (subtitle "[class] FastGraph")
  (body (documentation (find-class 'FastGraph) 't))
  (title "Attribute")
  (body "TODO")
  (title "Simplifier")
  (body "TODO")
  (title "Export to dot")
  (body "->dot"))
