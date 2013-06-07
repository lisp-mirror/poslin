(in-package #:poslin)

(defmacro interpreter ()
  `(loop while (or pc rstack)
      do (if pc
	     (let ((curr (thread-curr pc)))
	       (typecase curr
		 (function
		  (funcall curr))
		 (thread
		  (aif (thread-next pc)
		       (push it rstack))
		  (setf pc curr))
		 (t (push curr (pstack-content (car path)))
		    (advance pc))))
	     (setf pc (pop rstack)))))
