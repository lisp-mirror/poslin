(in-package #:poslin)

(defprim ! t
    "( callable -- ??? )"
  (args (callable)
    (if pc
	(push pc rstack))
    (setf pc (callable->thread callable this))
    (interpreter)))

(defprim & t
    "( callable -- thread )"
  (args (callable)
    (push-curr (callable->thread callable this))))