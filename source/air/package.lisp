(in-package :cl-user)

(defpackage :caten/air
  (:use :cl :alexandria :trivia)
  (:export #:Node #:node-class #:node-p #:node->id #:id->node
   #:node-id #:node-type #:node-writes
   #:node-reads #:node-attrs #:print-node)
  (:export #:make-node #:copy-node)
  (:export #:lower #:mutate)
  (:export #:Graph #:make-graph #:copy-graph #:graph-p #:graph-seen #:graph-outputs #:Graph-nodes #:id->value #:id->users #:remnode #:verify-graph)
  (:export #:getattrs #:getattr #:remattr)
  (:export #:defsimplifier))
