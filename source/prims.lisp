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

(defmacro defunary (&body names)
  `(progn
     ,@(mapcar #`(defprim ,a1 nil
		   (push-curr (,a1 (pop-curr))))
	       names)))

(defmacro defbinary (&body names)
  `(progn
     ,@(mapcar #`(defprim ,a1 nil
		   (let ((val (pop-curr)))
		     (push-curr (,a1 (pop-curr)
				     val))))
	       names)))