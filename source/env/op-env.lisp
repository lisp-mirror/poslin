(in-package #:poslin)

(defstruct op-env
  par (defs (constantly nil))
  (imms (constantly nil)))

(defun op-env-def (op-env name)
  (declare (type (or op-env null)
		 op-env)
	   (type symbol name))
  (the (values (or binding null)
	       &optional)
       (if op-env
	   (aif (funcall (op-env-defs op-env)
			 name)
		it
		(op-env-def (op-env-par op-env)
			    name)))))

(defmethod (setf op-env-def)
    (binding op-env name)
  (declare (type (or binding null)
		 binding)
	   (type op-env op-env)
	   (type symbol name))
  (let ((old (op-env-defs op-env)))
    (setf (op-env-defs op-env)
	  (lambda (sym)
	    (declare (type symbol sym))
	    (if (symbol= sym name)
		binding
		(funcall old sym))))))

(defun op-env-imm (op-env name)
  (declare (type (or op-env
		     null)
		 op-env)
	   (type symbol name))
  (the (values boolean boolean &optional)
       (if op-env
	   (multiple-value-bind (val found?)
	       (funcall (op-env-defs op-env)
			name)
	     (if found?
		 (values val t)
		 (op-env-imm (op-env-par op-env)
			     name)))
	   (values nil nil))))

(defmethod (setf op-env-imm)
    (immediate? op-env name)
  (declare (type boolean immediate?)
	   (type op-env op-env)
	   (type symbol name))
  (let ((old (op-env-imms op-env)))
    (setf (op-env-defs op-env)
	  (lambda (sym)
	    (declare (type symbol sym))
	    (the (values boolean boolean &optional)
		 (if (symbol= sym name)
		     (values immediate? t)
		     (funcall old sym)))))))
