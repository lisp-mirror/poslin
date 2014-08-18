(in-package #:poslin)

(defmacro rstack (path)
  `([binding]-value (path-get ,path :rstack)))

(defmacro stack (path)
  `([binding]-value (path-get ,path :stack)))

(defmacro interpreter ()
  `(loop while (or (not (equal? pc <noop>))
		   rstack)
      do
	(match pc
	    t
	  (<noop>
	   (setf pc (pop rstack)))
	  ((<constant> c)
	   (push c (stack path))
	   (setf pc (or (pop rstack)
			<noop>)))
	  ((<prim> f)
	   (funcall f))
	  ((<thread> front back)
	   (match front
	       t
	     (<noop>
	      (setf pc back))
	     ((<constant> c)
	      (push c (stack path))
	      (setf pc back))
	     ((<prim> f)
	      (funcall f))
	     (_
	      (push back rstack)
	      (setf pc front))))
	  ((<quotation> s)
	   (format t "~S"
		   s)
	   (push s (stack path))
	   (setf pc (or (pop rstack)
			<noop>)))
	  (c
	   (push c (stack path))
	   (setf pc (or (pop rstack)
			<noop>))))))
