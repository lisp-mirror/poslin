(in-package #:poslin)

(defun callable->thread (callable poslin)
  (with-pandoric (path)
      poslin
    (typecase callable
      (symbol
       (aif (op-env-def (pstack-op-env (car path))
			callable)
	    (binding-val it)))
      (pstack
       (stack->thread callable poslin))
      (cons
       (case (car callable)
	 ((lisp)
	  (make-thread :curr (eval (second callable))))
	 ((quote)
	  (if (and (symbolp (second callable))
		   (op-env-imm (pstack-op-env (car path))
			       (second callable)))
	      (aif (op-env-def (pstack-op-env (car path))
			       (second callable))
		   (binding-val it))
	      (second callable)))
	 (t (make-thread :curr callable))))
      (thread callable)
      (t (make-thread :curr callable)))))
