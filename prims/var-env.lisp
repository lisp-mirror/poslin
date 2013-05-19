(in-package #:poslin)

(defprim ?bv nil
    "( var-env sym -- binding )"
  (args (var-env sym)
    (if (var-env-p var-env)
	(if (symbolp sym)
	    (push-curr (var-env-def var-env sym))
	    (poslin-error malformed-var-name
			  "Attempt to get binding of ~A in ~A"
			  sym var-env))
	(poslin-error malformed-var-env
		      "Attempt to get binding of ~A in ~A"
		      sym var-env))))

(defprim @bv nil
    "( var-env sym binding -- )"
  (args (var-env sym binding)
    (if (var-env-p var-env)
	(if (symbolp sym)
	    (if (or (binding-p binding)
		    (null binding))
		(setf (var-env-def var-env sym)
		      binding)
		(poslin-error malformed-binding
			      "Attempt to set binding of ~A in ~A to ~
                               ~A"
			      sym var-env binding))
	    (poslin-error malformed-var-name
			  "Attempt to set binding of ~A in ~A to ~
                           ~A"
			  sym var-env binding))
	(poslin-error malformed-var-env
		      "Attempt to set binding of ~A in ~A to ~
                       ~A"
		      sym var-env binding))))

(defprim ev* nil
    "( -- var-env )"
  (push-curr (make-var-env)))

(defprim ev+ nil
    "( var-env -- var-env-child )"
  (args (var-env)
    (if (or (var-env-p var-env)
	    (null var-env))
	(push-curr (make-var-env :par var-env))
	(poslin-error malformed-var-env
		      "Attempt to make child variable environment of ~
                       ~A"
		      var-env))))

(defprim ?ev- nil
    "( var-env -- var-env-par )"
  (args (var-env)
    (if (var-env-p var-env)
	(push-curr (var-env-par var-env))
	(poslin-error malformed-var-env
		      "Attempt to get parent variable environment of ~
                       ~A"
		      var-env))))

(defprim @ev- nil
    "( var-env var-env-par -- )"
  (args (var-env var-env-par)
    (if (and (var-env-p var-env)
	     (or (var-env-p var-env)
		 (null var-env)))
	(setf (var-env-par var-env)
	      var-env-par)
	(poslin-error malformed-var-env
		      "Attempt to set parent variable environment of ~
                       ~A to ~A"
		      var-env var-env-par))))

(defprim §ev nil
    "( var-env -- var-env-copy )"
  (args (var-env)
    (if (var-env-p var-env)
	(push-curr (copy-var-env var-env))
	(poslin-error malformed-var-env
		      "Attempt to make copy of ~A"
		      var-env))))