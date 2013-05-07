(in-package #:poslin)

(defmacro interpreter ()
  `(loop while (or pc rstack)
      do (if pc
	     (let ((curr (car pc)))
	       (typecase curr
		 (function
		  (funcall curr))
		 (cons
		  (aif (cdr pc)
		       (push it rstack))
		  (setf pc curr))
		 (t
		  (pop pc)
		  (push-curr curr))))
	     (push (pop rstack)
		   pc))))