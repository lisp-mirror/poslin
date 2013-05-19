(in-package #:poslin)

(defprim ! t
    "( callable -- ??? )"
  (args (callable)
    (aif (callable->thread callable this)
	 (progn
	   (if pc
	       (push pc rstack))
	   (setf pc (make-thread :curr it))
	   (interpreter))
	 (poslin-error unknown-op
		       "~A is not callable"
		       callable))))

(defprim & t
    "( callable -- thread )"
  (args (callable)
    (push-curr (callable->thread callable this))))