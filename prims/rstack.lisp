(in-package #:poslin)

(defprim r<- nil
    "( val -- )"
  (args (val)
    (push val rstack)))

(defprim r-> nil
    "( -- val )"
  (if rstack
      (push-curr (pop rstack))
      (poslin-error rstack-bottom
		    "Attempt to pop bottom of rstack")))