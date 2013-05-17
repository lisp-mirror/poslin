(in-package #:poslin)

(defmacro defnprim (name immediate? docstring &body body)
  `(setf *prims*
	 (cons '(,name ,immediate? ,docstring ,@body)
	       (remove ',name *prims*
		       :test #'eq
		       :key #'car))))

(defmacro defprim (name immediate? docstring &body body)
  `(defnprim ,name ,immediate? ,docstring
     (advance pc)
     ,@body))

(defmacro args (args &body body)
  `(let (,@(nreverse (mapcar #`(,a1 (pop (pstack-content (car path))))
			     args)))
     ,@body))
