(in-package #:poslin)

(defun stack->thread (stack env)
  (nreverse
   (with-pandoric (path pc)
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
		   (pop pc)
		   (push-curr curr))))
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
       (let* ((id (car callable))
	      (sym? (symbolp id)))
	 (cond
	   ((and sym? (symbol= id 'quote))
	    (let ((callable (cadr callable)))
	      (if (immediate? callable)
		  (lookup-op callable)
		  (lambda ()
		    (pop pc)
		    callable))))
	   ((and sym? (symbol= id 'lisp))
	    (lambda ()
	      (pop pc)
	      (eval (cadr callable))))
	   ((and sym? (symbol= id 'thread))
	    (cdr callable))
	   (t (error "Tried to call ~A"
		     callable)))))
      (stack
       (stack->thread callable env))
      (function
       callable)
      (t (error "Tried to call ~A"
		callable)))))