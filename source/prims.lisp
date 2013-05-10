(in-package #:poslin)

(defparameter *prims* '())

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
  `(locally
       ,@(mapcar #`(defprim ,a1 nil
		     (locally
			 #+sbcl(declare (sb-ext:muffle-conditions
					 style-warning))
			 (push-curr (,a1 (pop-curr)))))
		 names)))

(defmacro defbinary (&body names)
  `(locally
       ,@(mapcar #`(defprim ,a1 nil
		     (locally
			 #+sbcl(declare (sb-ext:muffle-conditions
					 style-warning))
			 (let ((val (pop-curr)))
			   (push-curr (,a1 (pop-curr)
					   val)))))
		 names)))