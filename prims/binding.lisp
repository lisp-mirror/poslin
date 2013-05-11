(in-package #:poslin)

(defprim @bo nil
  ; ( op-env name binding -- )
  (args (op-env name binding)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (if (binding-p binding)
		(setf (gethash name (op-env-defs op-env))
		      binding)
		(perror malformed-binding
			"Attempt to set ~A as binding of ~A in ~
                         operation environment ~A"
			binding name op-env))
	    (perror malformed-op-name
		    "Attempt to set ~A as binding of ~A in operation ~
                     environment ~A"
		    binding name op-env))
	(perror malformed-op-env
		"Attempt to set ~A as binding of ~A in ~A."
		binding name op-env))))

(defprim @bo_ nil
  ; ( op-env name -- )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (progn
	      (remhash name (op-env-defs op-env))
	      (remhash name (op-env-imm op-env)))
	    (perror malformed-op-name
		    "Attempt to unset ~A in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to unset ~A in ~A"
		name op-env))))

(defprim @bv nil
  ; ( var-env name binding -- )
  (args (var-env name binding)
    (if (var-env-p var-env)
	(if (symbolp name)
	    (if (binding-p binding)
		(setf (gethash name (var-env-defs var-env))
		      binding)
		(perror malformed-binding
			"Attempt to set ~A as binding of ~A in ~
                         variable environment ~A"
			binding name var-env))
	    (perror malformed-var-name
		    "Attempt to set ~A as binding of ~A in variable ~
                     environment ~A"
		    binding name var-env))
	(perror malformed-var-env
		"Attempt to set ~A as binding of ~A in ~A."
		binding name var-env))))

(defprim @bv_ nil
  ; ( var-env name -- )
  (args (var-env name)
    (if (var-env-p var-env)
	(if (symbolp name)
	    (progn
	      (remhash name (var-env-defs var-env)))
	    (perror malformed-car-name
		    "Attempt to unset ~A in ~A"
		    name var-env))
	(perror malformed-var-env
		"Attempt to unset ~A in ~A"
		name var-env))))

(defprim @b nil
  ; ( binding val -- )
  (args (binding val)
    (if (binding-p binding)
	(let ((val (if (and (consp val)
			    (symbol= (car val)
				     'thread))
		       (cdr val)
		       val)))
	  (setf (binding-val binding)
		val))
	(perror malformed-binding
		"Attempt to set ~A in ~A"
		val binding))))

(defprim @d nil
  ; ( binding doc -- )
  (args (binding doc)
    (if (binding-p binding)
	(if (stringp doc)
	    (setf (binding-doc binding)
		  doc)
	    (perror malformed-doc
		    "Attempt to set ~A as documentation of ~A"
		    doc binding))
	(perror malformed-binding
		"Attempt to set ~A as documentation of ~A"
		doc binding))))

(defprim ?bo nil
  ; ( op-env name -- binding )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (push-curr (gethash name (op-env-defs op-env)))
	    (perror malformed-op-name
		    "Attempt to get binding of ~A in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to get binding of ~A in ~A"
		name op-env))))

(defprim ?bv nil
  ; ( var-env name -- binding )
  (args (var-env name)
    (if (var-env-p var-env)
	(if (symbolp name)
	    (push-curr (gethash name (var-env-defs var-env)))
	    (perror malformed-var-name
		    "Attempt to get binding of ~A in ~A"
		    name var-env))
	(perror malformed-var-env
		"Attempt to get binding of ~A in ~A"
		name var-env))))

(defprim ?b nil
  ; ( binding -- val )
  (args (binding)
    (if (binding-p binding)
	(push-curr (binding-val binding))
	(perror malformed-binding
		"Attempt to get value of ~A"
		binding))))

(defprim ?d nil
  ; ( binding -- doc )
  (args (binding)
    (if (binding-p binding)
	(push-curr (binding-doc binding))
	(perror malformed-binding
		"Attempt to get documentation of ~A"
		binding))))

(defprim b* nil
  ; ( -- binding )
  (push-curr (make-binding)))