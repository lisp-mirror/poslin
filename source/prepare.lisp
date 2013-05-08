(in-package #:poslin)

(defmacro install-prims ()
  `(progn
     ,@(mapcar #`(let ((thread (lambda ()
				 ,@(cddr a1))))
		   (setf (lookup-op ',(car a1))
			 thread)
		   ,@(if (cadr a1)
			 `((setf (immediate? ',(car a1))
				 t)))
		   (setf (gethash thread ntable)
			 ',(car a1))
		   (setf (gethash thread dtable)
			 ',(cddr a1)))
	       *prims*)))

(defmacro install-stdlib ()
  `(run-poslin this ,@*stdlib*))

(defmacro prepare ()
  `(progn
     (setf path (list (make-stack :name 'root)))
     (setf dtable (make-hash-table :test #'eq))
     (setf ntable (make-hash-table :test #'eq))
     (setf folder (osicat:environment-variable "POSLIN_HOME"))
     (install-prims)
     (install-stdlib)))