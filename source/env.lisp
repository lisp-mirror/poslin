(in-package #:poslin)

(defstruct var-env
  par (defs (make-hash-table :test #'eq)))

(defmethod print-object ((object var-env)
			 stream)
  (format stream "#<VAR-ENV~{~<~%~:; ~S~>~}>"
	  (labels ((rec (env acc)
		     (if (not env)
			 acc
			 (rec (var-env-par env)
			      (progn
				(maphash (lambda (k v)
					   (declare (ignore v))
					   (pushnew k acc))
					 (var-env-defs env))
				acc)))))
	    (rec object (list)))))

(defstruct op-env
  par (defs (make-hash-table :test #'eq))
  (imm (make-hash-table :test #'eq)))

(defmethod print-object ((object op-env)
			 stream)
  (format stream "#<OP-ENV~{~<~%~:; ~S~>~}>"
	  (labels ((rec (env acc)
		     (if (not env)
			 acc
			 (rec (op-env-par env)
			      (progn
				(maphash
				 (lambda (k v)
				   (declare (ignore v))
				   (pushnew
				    (if (gethash k (op-env-imm env))
					(list k t)
					k)
				    acc
				    :test #'equal))
				 (op-env-defs env))
				acc)))))
	    (rec object (list)))))