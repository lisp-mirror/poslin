(in-package #:poslin)

(defun stack->thread (stack env)
  (nreverse
   (with-pandoric (path pc rstack ntable)
       env
     (loop for curr
	in (stack-content stack)
	collect
	  (typecase curr
	    (symbol
	     (if (immediate? curr)
		 (let ((thread (lookup-op curr)))
		   (if thread
		       thread
		       (perror undefined-operation
			       "No operation ~S"
			       curr)))
		 curr))
	    (cons
	     (if (symbol= (car curr)
			  'thread)
		 (cdr curr)
		 (lambda ()
		   (pop pc)
		   (push-curr curr))))
	    (t curr))))))

(defun callable->thread (callable env)
  (with-pandoric (path pc rstack ntable)
      env
    (typecase callable
      (symbol
       (let ((thread (lookup-op callable)))
	 (if thread
	     thread
	     (perror undefined-operation "No operation ~S"
		     callable))))
      (cons
       (let* ((id (car callable))
	      (sym? (symbolp id)))
	 (cond
	   ((and sym? (symbol= id 'quote))
	    (lambda ()
	      (pop pc)
	      (push-curr (cadr callable))))
	   ((and sym? (symbol= id 'lisp))
	    (lambda ()
	      (pop pc)
	      (push-curr (eval (cadr callable)))))
	   ((and sym? (symbol= id 'thread))
	    (cdr callable))
	   (t (perror malformed-call "Attempt to call ~A"
		 callable)))))
      (stack
       (stack->thread callable env))
      (function
       callable)
      (t (perror malformed-call "Attempt to call ~A"
		 callable)))))