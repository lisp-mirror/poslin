(in-package #:poslin)

(defprim ?n nil
    "( stack -- sym )"
  (args (pstack)
    (if (pstack-p pstack)
	(push-curr (pstack-name pstack))
	(poslin-error malformed-pstack
		      "Attempt to get name of ~A"
		      pstack))))

(defprim ?eo nil
    "( stack -- op-env )"
  (args (pstack)
    (if (pstack-p pstack)
	(push-curr (pstack-op-env pstack))
	(poslin-error malformed-pstack
		      "Attempt to get operation environment of ~A"
		      pstack))))

(defprim ?ev nil
    "( stack -- var-env )"
  (args (pstack)
    (if (pstack-p pstack)
	(push-curr (pstack-var-env pstack))
	(poslin-error malformed-pstack
		      "Attempt to get variable environment of ~A"
		      pstack))))

(defprim @n nil
    "( stack sym -- )"
  (args (pstack sym)
    (if (pstack-p pstack)
	(if (symbolp sym)
	    (setf (pstack-name pstack)
		  sym)
	    (poslin-error malformed-pstack-name
			  "Attempt to set name of ~A to ~A"
			  pstack sym))
	(poslin-error malformed-pstack
		      "Attempt to set name of ~A to ~A"
		      pstack sym))))

(defprim @eo nil
    "( stack op-env -- )"
  (args (pstack op-env)
    (if (pstack-p pstack)
	(if (op-env-p op-env)
	    (setf (pstack-op-env pstack)
		  op-env)
	    (poslin-error malformed-pstack
			  "Attempt to set operation environment of ~
                           ~A to ~A"
			  pstack op-env))
	(poslin-error malformed-op-env
		      "Attempt to set operation environment of ~
                       ~A to ~A"
		      pstack op-env))))

(defprim @ev nil
    "( stack var-env -- )"
  (args (pstack var-env)
    (if (pstack-p pstack)
	(if (var-env-p var-env)
	    (setf (pstack-var-env pstack)
		  var-env)
	    (poslin-error malformed-pstack
			  "Attempt to set variable environment of ~
                           ~A to ~A"
			  pstack var-env))
	(poslin-error malformed-var-env
		      "Attempt to set variable environment of ~
                       ~A to ~A"
		      pstack var-env))))

(defprim ?_ nil
    "( stack -- bool )"
  (args (pstack)
    (if (pstack-p pstack)
	(push-curr (null (pstack-content pstack)))
	(poslin-error malformed-pstack
		      "Attempt to check for bottom of ~A"
		      pstack))))

(defprim -> nil
    "( stack -- val )"
  (args (pstack)
    (if (pstack-p pstack)
	(if (pstack-content pstack)
	    (push-curr (pop (pstack-content pstack)))
	    (poslin-error bottom
			  "Attempt to pop bottom of ~A"
			  pstack))
	(poslin-error malformed-stack
		      "Attempt to pop ~A"
		      pstack))))

(defprim <- nil
    "( stack val -- )"
  (args (pstack val)
    (if (pstack-p pstack)
	(push val (pstack-content pstack))
	(poslin-error malformed-pstack
		      "Attempt to push ~A onto ~A"
		      val pstack))))

(defprim § nil
    "( stack -- )"
  (args (pstack)
    (if (pstack-p pstack)
	(if (pstack-content pstack)
	    (push (car (pstack-content pstack))
		  (pstack-content pstack))
	    (poslin-error bottom
			  "Attempt to duplicate bottom of ~A"
			  pstack))
	(poslin-error malformed-pstack
		      "Attempt to duplicate top of ~A"
		      pstack))))

(defprim <> nil
    "( stack -- )"
  (args (pstack)
    (if (pstack-p pstack)
	(if (and (pstack-content pstack)
		 (cdr (pstack-content pstack)))
	    (rotatef (car (pstack-content pstack))
		     (cadr (pstack-content pstack)))
	    (poslin-error bottom
			  "Attempt to swap with bottom of ~A"
			  pstack))
	(poslin-error malformed-pstack
		      "Attempt to swap on ~A"
		      pstack))))