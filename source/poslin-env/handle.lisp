(in-package #:poslin)

(defmacro handle ()
  `(if (and (symbolp v)
	    (op-env-imm #1=(pstack-op-env (car path))
			v))
       (aif (op-env-def #1# v)
	    (progn
	      (setf pc (binding-val it))
	      (interpreter))
	    (perror undefined-operation
		    "No operation ~A"
		    v))
       (ppush (car path)
	      v)))
