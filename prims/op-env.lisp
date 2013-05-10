(in-package #:poslin)

(defprim @eo nil
  ; ( [] op-env -- )
  (args (stack op-env)
    (if (stack-p stack)
	(if (op-env-p op-env)
	    (setf (stack-ops stack)
		  op-env)
	    (perror malformed-op-env
		    "Attempt to set ~A as operation environment of ~A"
		    op-env stack))
	(perror malformed-stack
		"Attempt to set ~A as operation environment of ~A"
		op-env stack))))

(defprim eo* nil
  ; ( -- op-env )
  (push-curr (make-op-env)))

(defprim eo+ nil
  ; ( op-env -- op-env-child )
  (args (op-env)
    (if (or (null op-env)
	    (op-env-p op-env))
	(push-curr (make-op-env :par op-env))
	(perror malformed-op-env
		"Attempt to make child operation environment of ~A"
		op-env))))

(defprim @o nil
  ; ( op-env name callable -- )
  (args (op-env name callable)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (let ((thread (callable->thread callable this)))
	      (if thread
		  (setf (gethash name (op-env-defs op-env))
			(make-binding :val thread))
		  (perror malformed-callable
			  "Attempt to make ~A into an operation in ~
                           ~A with name ~A"
			  callable op-env name)))
	    (perror malformed-op-name
		    "Attempt to make ~A into an operation in ~
                     ~A with name ~A"
		    callable op-env name))
	(perror malformed-op-env
		"Attempt to make ~A into an operation in ~
                 ~A with name ~A"
		callable op-env name))))

(defprim @o_ nil
  ; ( op-env name -- )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (remhash name (op-env-defs op-env))
	    (perror malformed-op-name
		    "Attempt to undefine ~A in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to undefine ~A in ~A"
		name op-env))))