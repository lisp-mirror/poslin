(in-package #:poslin)

(defun lookup (word rem-path)
  (labels ((rec (dict)
	     (if dict
		 (if (eq (word-name dict)
			 word)
		     dict
		     (rec (word-prev dict))))))
    (let ((found (rec (stacktree-dict (car rem-path)))))
      (if found
	  found
	  (let ((next (cdr rem-path)))
	    (if next
		(lookup word next)))))))

(defmacro defnprim (name immediate? &body body)
  `(progn
     ,(if (member name *prims*
		 :key #'car
		 :test #'eq)
	 `(warn "Attempt to redefine Poslin prim ~A in ~A"
		',name '(defnprim ,name ,@body)))
     (setf *prims*
	   (nconc *prims* '((,name ,immediate? ,@body))))))

(defmacro defprim (name immediate? &body body)
  `(defnprim ,name ,immediate?
     (pop pc)
     ,@body))

(defmacro addstd (&body code)
  `(setf *stdlib*
	 (nconc *stdlib* '(,@code))))

(defmacro def-n-ary (n &body ops)
  `(progn
     ,@(mapcar #`(defprim ,a1 nil
		   (let ((args ()))
		     ,@(loop for x
			  below n
			  collect `(push (pop-curr)
					 args))
		     (push-curr (apply #',a1 args))))
	       ops)))