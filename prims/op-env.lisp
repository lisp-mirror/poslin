(in-package #:poslin)

(defprim @eo nil
  ; ( [] op-env -- )
  (args (stack op-env)
    (if (stack-p stack)
	(if (or (op-env-p op-env)
		(null op-env))
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

(defprim eo- nil
  ; ( op-env -- op-env-par )
  (args (op-env)
    (if (op-env-p op-env)
	(push-curr (op-env-par op-env))
	(if (null op-env)
	    (push-curr nil)
	    (perror malformed-op-env
		    "Attempt to get parent operation environment of ~A"
		    op-env)))))

(defprim @eo+ nil
  ; ( op-env op-env-par -- )
  (args (op-env op-env-par)
    (if (and (or (op-env-p op-env-par)
		 (null op-env-par))
	     (op-env-p op-env))
	(setf (op-env-par op-env)
	      op-env-par)
	(perror malformed-op-env
		"Attempt to set parent operation environment of ~A ~
                 to ~A"
		op-env op-env-par))))

(defprim §eo nil
  ; ( op-env -- op-env-copy )
  (args (op-env)
    (if (op-env-p op-env)
	(let ((copy (copy-op-env op-env)))
	  (setf (op-env-defs copy)
		(make-hash-table :test #'eq)
		(op-env-imm copy)
		(make-hash-table :test #'eq))
	  (maphash (lambda (k v)
		     (setf (gethash k (op-env-defs copy))
			   v))
		   (op-env-defs op-env))
	  (maphash (lambda (k v)
		     (setf (gethash k (op-env-imm copy))
			   v))
		   (op-env-imm op-env))
	  (push-curr copy))
	(if (null op-env)
	    (push-curr nil)
	    (perror malformed-op-env
		    "Attempt to make operation environment copy of ~A"
		    op-env)))))

(defprim ?eo nil
  ; ( [] -- op-env )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-ops stack))
	(perror malformed-stack
		"Attempt to get operation environment of ~A"
		stack))))

(defprim @i+ nil
  ; ( op-env name -- )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (setf (gethash name (op-env-imm op-env))
		  t)
	    (perror malformed-op-name
		    "Attempt to set ~A immediate in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to set ~A immediate in ~A"
		name op-env))))

(defprim @i- nil
  ; ( op-env name -- )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (setf (gethash name (op-env-imm op-env))
		  nil)
	    (perror malformed-op-name
		    "Attempt to set ~A delayed in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to set ~A delayed in ~A"
		name op-env))))

(defprim @i_ nil
  ; ( op-env name -- )
  (args (op-env name)
    (if (op-env-p op-env)
	(if (symbolp name)
	    (remhash name (op-env-imm op-env))
	    (perror malformed-op-name
		    "Attempt to unset ~A immediateness in ~A"
		    name op-env))
	(perror malformed-op-env
		"Attempt to unset ~A immediateness in ~A"
		name op-env))))