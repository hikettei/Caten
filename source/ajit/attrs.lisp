(in-package :caten/ajit)

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((fname :type string)
		 (jit-info :type JIT-Info)))

(defnode (:Render :FOR) () "
RenderGraph:
```
for(int idx=upfrom, below, by)
```
"
	 :slots ((idx)
		 (upfrom)
		 (below)
		 (by)
		 (scope :type (member :global :local))
		 (coincident) (permutable)))

(defnode (:Render :ENDFOR) () "
RenderGraph:
```
} // idx
```"
	 :slots ((idx)))

(defnode (:Render :FUNCALL) () "
RenderGraph:

```
FUNCALL(...)
```

The Render IR :FUNCALL has two behaviour depending on the value of `fname`.

If `fname` is specified, the node is a function call to the function `fname` with the arguments `args`. Other slots are ignored.

That is:

`FNAME(args ...)`

This pattern is created by using the `r/funcall-symbol` function.

Otherwise (i.e.: `fname` is nil), the node corresponds to the time(args) th computation in the graph where pipeline[idx] exists.

This pattern is created by using the `r/funcall` function.
"
	 :slots ((fname :initform nil)
                 (name) (args) (idx) (unroll-offsets)
		 (_packed) (_unrolled) (_metadata)))

(defnode (:Render :IF) () "
RenderGraph:
```
IF (condition) {
```"
	 :slots ((condition)))

(defnode (:Render :WHILE) () "
RenderGraph:
```
WHILE (condition) {
```"
	 :slots ((condition)))

(defnode (:Render :ENDWHILE) () "
RenderGraph:
```
} // endwhile
```")

(defnode (:Render :ELSE) () "
RenderGraph:
```
} ELSE {
```")

(defnode (:Render :ENDIF) () "
RenderGraph:
```
} // endif
```")

;; Temporary Nodes
(defnode (:IR :IR/FOR) () "" :slots ((_scalar_p)))
(defnode (:IR :IR/ENDFOR) () "")
(defnode (:TIME :GRAPH) () "" :slots ((id)))
