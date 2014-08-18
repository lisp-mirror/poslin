(in-package #:poslin)

(defun flatten (structure)
  (if (consp structure)
      (apply #'append
	     (mapcar #'flatten
		     structure))
      (list structure)))
