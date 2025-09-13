(defpackage :caten/graph
  (:use :cl :alexandria :trivia)
  (:export #:Node #:node-class #:node-p #:node->id #:id->node
   #:node-id #:node-type #:node-writes
   #:node-reads #:node-attr #:print-node #:get-output-to)
  (:export #:make-node #:copy-node)
  (:export #:Graph #:FastGraph #:make-graph #:copy-graph #:graph-p #:graph-seen #:graph-outputs #:Graph-nodes #:id->value #:id->users #:remnode #:verify-graph
	   #:insert-nodes #:->graph #:->fast-graph #:%graph-nodes-table #:graph-weakly-connected-p #:->graph-with-tpsort #:tpsort-graph #:make-node-pointing-to-nth #:%node-get-type-relay #:graph-get-undefined-variables)
  (:export #:getattrs #:getattr #:remattr)
  (:export #:simplifier #:defsimplifier #:<Rule> #:node-ematch)
  (:export #:Attribute #:defnode #:debug/render-defined-nodes #:debug/attrs-by-module #:node-build-documentation-by-class #:verify-args #:dump-into-list)
  (:export #:->dot #:pprint-graph)
  ;; TypeRelay API
  (:export #:AType #:Relay #:make-relay #:relay-reads #:relay-writes #:node-type-relay #:read-type-relay #:graph-infer-type-relay))
