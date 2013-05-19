(in-package #:poslin)

(defprim ^ nil
    "( n -- stack )"
  (args (n)
    (if (and (integerp n)
	     (>= n 0))
	(aif (nth n path)
	     (push-curr it)
	     (poslin-error path
			   "Exceeded path"))
	(poslin-error malformed-natural
		      "Attempt to get ~Ath stack in path"
		      n))))

(defprim [ t
    "( -- [ )"
  (push (make-pstack :op-env (pstack-op-env (car path))
		     :var-env (pstack-var-env (car path)))
	path))

(defprim ] t
    "( [ ... -- [ ... ] )"
  (if (cdr path)
      (push-curr (pop path))
      (poslin-error path
		    "Attempt to close root stack")))

(defprim %[ t
    "( [ ... ] -- [ ... )"
  (args (pstack)
    (push pstack path)))