(in-package #:poslin)

(defstruct stack
  name content (vars (make-var-env))
  (ops (make-op-env)))

(defmethod print-object ((object stack)
			 stream)
  (format stream "#<STACK ~A [~{~<~%~:; ~S~>~} ]>"
	  (stack-name object)
	  (reverse (stack-content object))))