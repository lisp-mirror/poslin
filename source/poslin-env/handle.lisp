(in-package #:poslin)

(defmacro handle ()
  `(block prim
     (if (and (symbolp v)
	      (op-env-imm #1=(pstack-op-env (car path))
			  v))
	 (aif (op-env-def #1# v)
	      (progn
		(setf pc (binding-val it))
		(interpreter))
	      (poslin-error undefined-operation
			    "No operation ~A"
			    v))
	 (if (and (consp v)
		  (symbol= (car v)
			   'quote))
	     (push (cadr v)
		   (pstack-content (car path)))
	     (push v (pstack-content (car path)))))))