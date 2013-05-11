(in-package #:poslin)

(defprim [ t
  ;; Open a new stack
  ;; ( -- [ )
  (push (make-stack :vars (make-var-env :par (curr-var-env))
		    :ops (make-op-env :par (curr-op-env)))
	path))

(defprim ] t
  ;; Close a stack and leave it on it's parent stack
  ;; ( [ ... -- [ ... ] )
  (if (cdr path)
      (push-curr (pop path))
      (perror path
	      "Attempt to close root stack")))

(defprim %[ t
  ;; Open stack on top of stack
  ;; ( [ ... ] -- [ ... )
  (args (stack)
    (if (stack-p stack)
	(push stack path)
	(perror malformed-stack "Attempt to open ~S"
		stack))))

(defprim ]] t
  ;; Close all paths up to the one having the name on top
  ;; ( name -- ??? )
  (args (name)
    (aif (member name path
		 :key #'stack-name
		 :test #'string=)
	 (setf path it)
	 (perror path "No stack named ~S in path"
		 name))))

(defprim ^ nil
  ;; Get nth stack in path
  ;; ( n -- [ ... ] )
  (args (n)
    (if (and (integerp n)
	     (>= n 0))
	(aif (nth n path)
	     (push-curr it)
	     (perror path "Path exceeded"))
	(perror malformed-natural-number
		"Attempt to get ~A-th stack in path"
		n))))