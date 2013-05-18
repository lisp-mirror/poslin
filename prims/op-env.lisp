(in-package #:poslin)

(defprim ?bo nil
    "( op-env sym -- binding )"
  (args (op-env sym)
    (if (op-env-p op-env)
	(if (symbolp sym)
	    (push-curr (op-env-def op-env sym))
	    (poslin-error malformed-op-name
			  "Attempt to get binding of ~A in ~A"
			  sym op-env))
	(poslin-error malformed-op-env
		      "Attempt to get binding of ~A in ~A"
		      sym op-env))))

(defprim ?i nil
    "( op-env sym -- bool )"
  (args (op-env sym)
    (if (op-env-p op-env)
	(if (symbolp sym)
	    (push-curr (op-env-imm op-env sym))
	    (poslin-error malformed-op-name
			  "Attempt to get binding of ~A in ~A"
			  sym op-env))
	(poslin-error malformed-op-env
		      "Attempt to get binding of ~A in ~A"
		      sym op-env))))

(defprim @bo nil
    "( op-env sym binding -- )"
  (args (op-env sym binding)
    (if (op-env-p op-env)
	(if (symbolp sym)
	    (if (or (binding-p binding)
		    (null binding))
		(progn
		  (setf (binding-val binding)
			(callable->thread (binding-val binding)
					  this))
		  (setf (op-env-def op-env sym)
			binding))
		(poslin-error malformed-binding
			      "Attempt to set binding of ~A in ~A to ~
                               ~A"
			      sym op-env binding))
	    (poslin-error malformed-op-name
			  "Attempt to set binding of ~A in ~A to ~A"
			  sym op-env binding))
	(poslin-error malformed-op-env
		      "Attempt to set binding of ~A in ~A to ~A"
		      sym op-env binding))))

(defprim @i nil
    "( op-env sym bool -- )"
  (args (op-env sym bool)
    (if (op-env-p op-env)
	(if (symbolp sym)
	    (if (or (eq bool nil)
		    (eq bool t))
		(setf (op-env-imm op-env sym)
		      bool)
		(poslin-error malformed-boolean
			      "Attempt to set immediateness of ~A in ~
                               ~A to ~A"
			      sym op-env bool))
	    (poslin-error malformed-op-name
			  "Attempt to set immediateness of ~A in ~
                           ~A to ~A"
			  sym op-env bool))
	(poslin-error malformed-op-env
		      "Attempt to set immediateness of ~A in ~
                       ~A to ~A"
		      sym op-env bool))))

(defprim @i_ nil
    "( op-env sym -- )"
  (args (op-env sym)
    (if (op-env-p op-env)
	(if (symbolp sym)
	    (op-env-remimm op-env sym)
	    (poslin-error malformed-op-name
			  "Attempt to remove immediateness of ~A in ~A"
			  sym op-env))
	(poslin-error malformed-op-env
		      "Attempt to remove immediateness of ~A in ~A"
		      sym op-env))))

(defprim eo* nil
    "( -- op-env )"
  (push-curr (make-op-env)))

(defprim eo+ nil
    "( op-env -- op-env-child )"
  (args (op-env)
    (if (or (op-env-p op-env)
	    (null op-env))
	(push-curr (make-op-env :par op-env))
	(poslin-error malformed-op-env
		      "Attempt to make child operation environment ~
                       of ~A"
		      op-env))))

(defprim ?eo- nil
    "( op-env -- op-env-par )"
  (args (op-env)
    (if (op-env-p op-env)
	(push-curr (op-env-par op-env))
	(poslin-error malformed-op-env
		      "Attempt to get parent operation environment ~
                       of ~A"
		      op-env))))

(defprim @eo- nil
    "( op-env op-env-par -- )"
  (args (op-env op-env-par)
    (if (and (op-env-p op-env)
	     (or (op-env-p op-env-par)
		 (null op-env-par)))
	(setf (op-env-par op-env)
	      op-env-par)
	(poslin-error malformed-op-env
		      "Attempt to set parent of operation ~
                       environment of ~A to ~A"
		      op-env op-env-par))))

(defprim §eo nil
    "( op-env -- op-env-copy )"
  (args (op-env)
    (if (op-env-p op-env)
	(push-curr (copy-op-env op-env))
	(poslin-error malformed-op-env
		      "Attempt to make copy of ~A"
		      op-env))))