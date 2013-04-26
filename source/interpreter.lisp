(in-package #:poslin)

(defmacro interpreter ()
  `(loop do
	(let ((curr (car pc)))
	  (cond
	    ((functionp curr)
	     (funcall curr))
	    ((consp curr)
	     (push (cdr pc)
		   rstack)
	     (setf pc curr))
	    (t (push curr (stacktree-stack (car path)))
	       (setf pc (cdr pc)))))
      until (not (or pc rstack))))

(defmacro poslin (poslin-env &body code)
  `(progn
     ,@(mapcar #`(funcall ,poslin-env ',a1)
	       code)))