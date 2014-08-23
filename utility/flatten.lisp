(in-package #:poslin)

(defun flatten (structure)
  #+sbcl
  (typecase structure
    (cons
     (apply #'append
	    (mapcar #'flatten
		    structure)))
    (sb-impl::comma
     (flatten (sb-impl::comma-expr structure)))
    (t
     (list structure)))
  #-sbcl
  (if (consp structure)
      (apply #'append
	     (mapcar #'flatten
		     structure))
      (list structure))
  )
