(in-package #:poslin)

(defun poserror (poslin error-type controlstring &rest args)
  (declare (type symbol error-type)
	   (type string controlstring)
	   (type function poslin)
	   (type (or cons null)
		 args))
  (the string
    (with-pandoric (ntable pc rstack)
	poslin
      (format nil "~&POSLIN-~A-ERROR~% ~A~%  PC: ~A~%  RSTACK: ~A"
	      error-type (apply #'format
				nil controlstring
				(mapcar (lambda (arg)
					  (posprint arg ntable))
					args))
	      (posprint pc ntable)
	      (mapcar (lambda (arg)
			(posprint arg ntable))
		      rstack)))))

(defmacro poslin-error (error-type controlstring &rest args)
  `(progn
     (princ (poserror this ',error-type ,controlstring ,@args))
     (return-from prim)))
