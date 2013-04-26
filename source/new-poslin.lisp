(in-package #:poslin)

(defmacro curr-par ()
  `(cadr path))

(defmacro pop-current ()
  `(pop (stacktree-stack (car path))))

(defmacro push-current (val)
  `(push ,val (stacktree-stack (car path))))

(defmacro new-poslin (&rest imported-vars)
  `(alet ,+registers+
     (install-prims)
     (dolist (v *stdlib*)
       (funcall this v))
     (plambda (v)
	 ,+registers+
       (poslin-handle ,@imported-vars)
       (car (last path)))))

(defmacro poslin-handle (&rest imported-vars)
  `(cond
     ((and (symbolp v)
	   (string= (symbol-name v)
		    "!"))
      (call (pop-current)
	    ,@imported-vars))
     ((consp v)
      (push-current (if (eq (car v)
			    'quote)
			(cadr v)
			(peval ,imported-vars v))))
     (t (push-current v))))

(defmacro! call (o!callable &rest imported-vars)
  `(etypecase ,g!callable
     ((function)
      (funcall ,g!callable))
     ((symbol)
      (push (word-thread (lookup ,g!callable path))
	    pc)
      (interpreter))
     ((cons)
      (if (eq (car ,g!callable)
	      'quote)
	  (push-current (cadr ,g!callable))
	  (push-current (peval ,imported-vars ,g!callable))))
     ((stacktree)
      (push (stacktree->thread ,g!callable)
	    pc)
      (interpreter))))