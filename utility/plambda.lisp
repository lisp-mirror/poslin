(in-package #:poslin)

(defmacro! with-pandoric (syms o!box &body body)
  `(symbol-macrolet
       (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
		  syms))
     ,@body))

(defmacro plambda ((&rest largs)
		   (&rest pargs)
		   &body body)
  (let ((pargs (mapcar #'list
		       pargs)))
    `(let (this self)
       (setf this (lambda (,@largs)
		    ,@body)
	     self (dlambda
		    (:pandoric-get (sym)
				   ,(pget pargs))
		    (:pandoric-set (sym val)
				   ,(pset pargs))
		    (t (&rest args)
		       (apply this args)))))))

(defmacro defpan (name (&rest args)
		  &body body)
  `(defun ,name (self)
     ,(if args
	  `(with-pandoric (,@args)
	       self
	     ,@body)
	  `(progn
	     ,@body))))
