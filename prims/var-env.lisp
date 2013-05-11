(in-package #:poslin)

(defprim @ev nil
  ; ( [] var-env -- )
  (args (stack var-env)
    (if (stack-p stack)
	(if (or (var-env-p var-env)
		(null var-env))
	    (setf (stack-vars stack)
		  var-env)
	    (perror malformed-var-env
		    "Attempt to set ~A as variable environment of ~A"
		    var-env stack))
	(perror malformed-stack
		"Attempt to set ~A as variable environment of ~A"
		var-env stack))))

(defprim ev* nil
  ; ( -- var-env )
  (push-curr (make-var-env)))

(defprim ev+ nil
  ; ( var-env -- var-env-child )
  (args (var-env)
    (if (or (null var-env)
	    (var-env-p var-env))
	(push-curr (make-var-env :par var-env))
	(perror malformed-var-env
		"Attempt to make child variable environment of ~A"
		var-env))))

(defprim ev- nil
  ; ( var-env -- var-env-par )
  (args (var-env)
    (if (var-env-p var-env)
	(push-curr (var-env-par var-env))
	(if (null var-env)
	    (push-curr nil)
	    (perror malformed-var-env
		    "Attempt to get parent variable environment of ~A"
		    var-env)))))

(defprim @ev+ nil
  ; ( var-env var-env-par -- )
  (args (var-env var-env-par)
    (if (and (or (var-env-p var-env-par)
		 (null var-env-par))
	     (var-env-p var-env))
	(setf (var-env-par var-env)
	      var-env-par)
	(perror malformed-var-env
		"Attempt to set parent variable environment of ~A ~
                 to ~A"
		var-env var-env-par))))

(defprim §ev nil
  ; ( var-env -- var-env-copy )
  (args (var-env)
    (if (var-env-p var-env)
	(let ((copy (copy-var-env var-env)))
	  (setf (var-env-defs copy)
		(make-hash-table :test #'eq))
	  (maphash (lambda (k v)
		     (setf (gethash k (car-env-defs copy))
			   v))
		   (var-env-defs var-env))
	  (push-curr copy))
	(if (null var-env)
	    (push-curr nil)
	    (perror malformed-var-env
		    "Attempt to make variable environment copy of ~A"
		    var-env)))))

(defprim ?ev nil
  ; ( [] -- var-env )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-vars stack))
	(perror malformed-stack
		"Attempt to get variable environment of ~A"
		stack))))