(in-package #:poslin)

(defstruct var-env
  par (defs (make-hash-table :test #'eq)))

(defmethod print-object ((object var-env)
			 stream)
  (format stream "#<VAR-ENV~{ ~S~}>"
	  (let ((vars '()))
	    (maphash (lambda (k v)
		       (declare (ignore v))
		       (push k vars))
		     (var-env-defs object))
	    vars)))

(defstruct op-env
  par (defs (make-hash-table :test #'eq))
  (imm (make-hash-table :test #'eq)))

(defmethod print-object ((object op-env)
			 stream)
  (format stream "#<OP-ENV~{ ~S~}>"
	  (let ((vars '()))
	    (maphash (lambda (k v)
		       (declare (ignore v))
		       (list (push k vars)
			     (multiple-value-bind (val found?)
				 (gethash k (op-env-imm object))
			       (if found?
				   val
				   '?))))
		     (op-env-defs object))
	    vars)))

(defstruct stack
  name content (vars (make-var-env))
  (ops (make-op-env)))

(defmethod print-object ((object stack)
			 stream)
  (format stream "#<STACK ~A [~{ ~S~} ]>"
	  (stack-name object)
	  (reverse (stack-content object))))