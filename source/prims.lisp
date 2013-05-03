(in-package #:poslin)

(defmacro defnprim (name immediate? &body body)
  `(setf *prims*
	 (cons (list* ',name ',immediate? ',body)
	       (remove ',name *prims*
		       :test #'eq
		       :key #'car))))

(defmacro defprim (name immediate? &body body)
  `(defnprim ,name ,immediate?
     (pop pc)
     ,@body))