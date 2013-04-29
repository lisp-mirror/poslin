(in-package #:poslin)

(defmacro interpreter ()
  `(loop do
	(if pc
	    (let ((curr (car pc)))
	      (typecase curr
		(function (funcall curr))
		(cons (if (eq (car curr)
			      'quote)
			  (push-curr (cadr curr))
			  (progn
			    (aif (cdr pc)
				 (push it rstack))
			    (setf pc (car pc)))))
		(t (push-curr curr))))
	    (if rstack
		(push (pop rstack)
		      pc)))
      until (not (or pc rstack))))