(in-package #:poslin)

(defmacro defnprim (name immediate? docstring &body body)
  `(setf *prims*
	 (cons '(,name ,immediate? ,docstring ,@body)
	       (remove ',name *prims*
		       :test #'eq
		       :key #'car))))
