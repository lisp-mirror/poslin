(in-package #:poslin)

(defun poserror (pc rstack ntable error-type controlstring &rest args)
  (declare (type (or thread null)
		 pc)
	   (type (or cons null)
		 rstack
		 args)
	   (type hash-table ntable)
	   (type symbol error-type)
	   (type string controlstring))
  (the
   string
   (format nil "~&POSLIN-~A-ERROR~% ~A~%  PC: ~A~%  RSTACK: ~A"
	   error-type (apply #'format
			     nil controlstring
			     (mapcar (lambda (arg)
				       (posprint arg ntable))
				     args))
	   (posprint pc ntable)
	   (mapcar (lambda (arg)
		     (posprint arg ntable))
		   rstack))))

(defmacro poslin-error (error-type controlstring &rest args)
  `(progn
     (princ (poserror pc rstack ntable ',error-type ,controlstring
		      ,@args))
     (return-from prim)))
