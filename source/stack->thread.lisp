(in-package #:poslin)

(defun stack->thread (stack poslin-env)
  (with-pandoric (pc rstack path)
      poslin-env
    (let ((cut '()))
      (labels ((cut (l n acc)
		 (if l
		     (cut (cdr l)
			  (1+ n)
			  (if (member n cut
				      :test #'=)
			      acc
			      (cons (car l)
				    acc)))
		     acc)))
	(cut
	 (loop for v in #1=(stack-content stack)
	    for x from 1
	    collect
	      (typecase v
		(symbol
		 (if (eq v '!)
		     (let ((call (nth x #1#)))
		       (if (symbolp call)
			   (word-thread
			    (lookup
			     (if (immediate? call
					     path)
				 '!
				 (progn
				   (push x cut)
				   call))
			     path))
			   (call->thread call poslin-env)))
		     (if (immediate? v path)
			 (word-thread (lookup v path))
			 v)))
		(function
		 (lambda ()
		   (push-curr (funcall v))))
		(cons
		 (case (car v)
		   ((quote)
		    (cadr v))
		   ((lisp)
		    (lambda ()
		      (push-curr (eval v))))
		   (t (push-curr v))))
		(t v)))
	 0 '())))))

(defun call->thread (call poslin-env)
  (with-pandoric (path)
      poslin-env
    (typecase call
      (cons call)
      (function call)
      (stack (stack->thread call poslin-env))
      (symbol (aif (lookup call path)
		   (word-thread it)
		   (error "Tried to call undefined word ~A"
			  call))))))