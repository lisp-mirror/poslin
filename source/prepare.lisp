(in-package #:poslin)

(defmacro install-prims ()
  `(progn
     ,@(mapcar #`(let ((thread (lambda ()
				 ,@(cddr a1))))
		   (setf (gethash ',(car a1)
				  (curr-dict))
			 (list thread))
		   ,@(if (cadr a1)
			 `((setf (gethash ',(car a1)
					  (curr-imm))
				 t)))
		   (setf (gethash thread dtable)
			 ',(cddr a1)))
	       *prims*)))

(defmacro install-stdlib ()
  `(run-poslin this ,@*stdlib*))

(defmacro prepare ()
  `(progn
     (setf path (list (make-stack :name 'root)))
     (setf dtable (make-hash-table :test #'eq))
     (install-prims)
     (install-stdlib)))