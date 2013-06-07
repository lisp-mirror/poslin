(in-package #:poslin)

;;; (name immediate? docstring . body)

(defmacro install-prims ()
  `(progn
     ,@(mapcar #`(let ((thread (lambda ()
				 (block prim
				   ,@(nthcdr 3 a1)))))
		   (setf (op-env-def (pstack-op-env (car path))
				     ',(first a1))
			 (make-binding :val thread
				       :doc ,(nth 2 a1)))
		   (setf (op-env-imm (pstack-op-env (car path))
				     ',(first a1))
			 ,(nth 1 a1))
		   (setf (gethash thread ntable)
			 ',(first a1))
		   (setf (gethash thread dtable)
			 '(block prim
			   ,@(nthcdr 3 a1))))
	       *prims*)))

(defmacro install (&optional standard-libraries)
  (let ((standard-libraries (or standard-libraries *stdlib*)))
    `(progn
       (setf ntable (make-hash-table :test #'eq)
	     dtable (make-hash-table :test #'eq)
	     folder (concatenate 'string
				 (namestring (ql:where-is-system
					      "poslin"))
				 "poslin/")
	     path (list (make-pstack :name 'root)))
       (install-prims)
       (mapcar (lambda (filename)
		 (poslin-load filename))
	       ',standard-libraries))))
