(in-package #:poslin)

(defprim @n nil
  ;; Sets the name of a stack
  ;; ( [] name -- )
  (args (stack name)
    (if (symbolp name)
	(if (stack-p stack)
	    (setf (stack-name stack)
		  name)
	    (perror "Attempt to set name of ~A to ~A"
		    stack name))
	(perror "Attempt to set name of stack ~A to ~A"
		stack name))))

(defprim ?n nil
  ;; Gets name of stack
  ;; ( [] -- name )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-name stack))
	(perror malformed-stack
		"Attempt to get name of ~S"
		stack))))