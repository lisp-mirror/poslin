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
		 (let ((thread (lookup-op curr)))
		   (if (empty? thread)
		       (error "Operation ~A not defined"
			      curr)
		       thread))
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
       (let ((thread (lookup-op callable)))
	 (if (empty? thread)
	     (error "Operation ~A not defined"
		    thread)
	     thread)))
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