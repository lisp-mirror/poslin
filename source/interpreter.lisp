(in-package #:poslin)

(defmacro interpreter ()
  `(loop do
	(if pc
	    (let ((curr (car pc)))
	      (cond
		((functionp curr)
		 (funcall curr))
		((consp curr)
		 (setf rstack (append (cdr pc)
				      rstack))
		 (setf pc curr))
		(t (push curr (stacktree-stack (car path)))
		   (setf pc (cdr pc)))))
	    (if rstack
		(push (pop rstack)
		      pc)))
      until (not (or pc rstack))))

(defmacro! poslin (o!poslin-env &body code)
  `(progn
     ,@(mapcar #`(funcall ,g!poslin-env ',a1)
	       code)))