(in-package #:poslin)

(defun stack->thread (stack env)
  (nreverse
   (with-pandoric (path)
       env
     (loop for curr
	in (stack-content stack)
	collect
	  (typecase curr
	    (symbol
	     (if (immediate? curr)
		 (lookup curr)
		 curr))
	    (cons
	     (if (symbol= (car curr)
			  'thread)
		 (cdr curr)
		 (lambda ()
		   curr)))
	    (t curr))))))

(defun callable->thread (callable env)
  (with-pandoric (path pc)
      env
    (typecase callable
      (symbol
       (aif (lookup callable)
	    it
	    (error "Tried to call ~A, which is not defined."
		   callable)))
      (cons
       (let ((id (car callable)))
	 (cond
	   ((symbol= id 'quote)
	    (lambda ()
	      (pop pc)
	      (cadr callable)))
	   ((symbol= id 'lisp)
	    (lambda ()
	      (pop pc)
	      (eval (cadr callable))))
	   ((symbol= id 'thread)
	    (cdr callable))
	   (t (error "Tried to call ~A"
		     callable)))))
      (stack
       (stack->thread callable env))
      (t (error "Tried to call ~A"
		callable)))))