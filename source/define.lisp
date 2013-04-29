(in-package #:poslin)

(defmacro defnprim (name immediate? &body body)
  `(push (list* ',name ',immediate? ',body)
	 *prims*))

(defmacro defprim (name immediate? &body body)
  `(defnprim ,name ,immediate?
     (pop pc)
     ,@body))

(defmacro addstd (&body code)
  `(setf *stdlib*
	 (append *stdlib* ',code)))