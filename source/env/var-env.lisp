(in-package #:poslin)

(defstruct var-env
  par (defs (constantly nil)))

(defun var-env-def (var-env name)
  (declare (type (or var-env null)
		 var-env)
	   (type symbol name))
  (the (values (or binding null)
	       &optional)
       (if var-env
	   (if-not (funcall (var-env-defs var-env)
			    name)
	     (var-env-def (var-env-par var-env)
			  name)))))

(defun (setf var-env-def)
    (binding var-env name)
  (declare (type binding binding)
	   (type var-env var-env)
	   (type symbol name))
  (let ((old (var-env-defs var-env)))
    (setf (var-env-defs var-env)
	  (lambda (sym)
	    (declare (type symbol sym))
	    (the (values (or binding null)
			 &optional)
		 (if (symbol= sym name)
		     binding
		     (funcall old sym)))))))
