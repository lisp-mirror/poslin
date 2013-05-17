(in-package #:poslin)

(defun callable->thread (callable poslin)
  (with-pandoric (path)
      poslin
    (typecase callable
      (symbol
       (aif (op-env-def (pstack-op-env (car path))
			callable)
	    (binding-val it)
	    (poserror poslin 'undefined-operation
		      "No operation ~A"
		      callable)))
      (pstack
       (stack->thread callable poslin))
      (cons
       (case (car callable)
	 ((lisp)
	  (make-thread :curr (eval (second callable))))
	 ((quote)
	  (make-thread :curr (second callable)))
	 (t (make-thread :curr callable))))
      (thread callable)
      (t (make-thread :curr callable)))))
