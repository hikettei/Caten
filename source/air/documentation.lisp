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
  (body "A list of node is called graph")
  (title "FastGraph")
  (body "If the graph is DAG, use fastgraph")
  (title "Attribute")
  (body "")
  (title "Simplifier")
  (body "You can define pattern-matcher based simplifier")
  (title "Export to dot")
  (body "->dot"))
