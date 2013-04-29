(in-package #:poslin)

(defmacro install-prims ()
  `(progn
     ,@(mapcar #`(let ((thread (lambda ()
				 ,@(cddr a1))))
		   (setf (gethash ',(car a1)
				  (stack-dict (curr)))
			 (make-word :name ',(car a1)
				    :thread thread))
		   ,@(if (cadr a1)
			 `((setf (gethash ',(car a1)
					  (stack-imm (curr)))
				 t)))
		   (setf (gethash thread dtable)
			 ',(cddr a1)))
	       *prims*)))

(defmacro install-stdlib ()
  `(poslin this
     ,@*stdlib*))