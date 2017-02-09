(in-package #:poslin)

(defun flatten (structure)
  #.+optimization-parameters+
  (if (consp structure)
      (apply #'append
	     (mapcar #'flatten
		     structure))
      (list structure))
  )
