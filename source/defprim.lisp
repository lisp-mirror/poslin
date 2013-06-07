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

(defmacro defunary (&body ops)
  `(progn
     ,@(mapcar #`(defprim ,(car a1)
		     nil ,(cadr a1)
		   (push (,(car a1)
			   (pop (pstack-content (car path))))
			 (pstack-content (car path))))
	       (group ops 2))))

(defmacro! defbinary (&body ops)
  `(progn
     ,@(mapcar #`(defprim ,(car a1)
		     nil ,(cadr a1)
		   (let ((,g!tmp (pop (pstack-content (car path)))))
		     (push (,(car a1)
			     (pop (pstack-content (car path)))
			     ,g!tmp)
			   (pstack-content (car path)))))
	       (group ops 2))))

(defmacro args (args &body body)
  `(let (,@(nreverse (mapcar #`(,a1 (pop (pstack-content (car path))))
			     args)))
     ,@body))

(defmacro push-curr (val)
  `(push ,val (pstack-content (car path))))