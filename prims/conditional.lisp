(in-package #:poslin)

(defprim <?> nil
    "( T val1 val2 -- val1 ) | ( NIL val1 val2 -- val2 )"
  (args (bool val1 val2)
    (push-curr (if bool
		   val1
		   val2))))