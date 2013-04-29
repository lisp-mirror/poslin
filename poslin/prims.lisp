(in-package #:poslin)

(setf *prims* '())

(defprim ! t
  (push (call->thread (pop-curr)
		      this)
	pc))

(defprim [ t
  (push (make-stack)
	path))

(defprim ] t
  (if (cdr path)
      (progn
	(push-par (curr))
	(pop path))
      (error "Tried to pop root stack")))

(defprim ]! t
  (if (cdr path)
      (progn
	(push-par (stack->thread (curr)
				 this))
	(pop path))
      (error "Tried to pop root stack")))

(defprim % nil
  (let ((name (pop-curr)))
    (if (symbolp name)
	(if (eq name '^)
	    (push-curr (par))
	    (aif (find name (stack-children (curr))
		       :key #'stack-name
		       :test #'eq)
		 (progn
		   (setf #1=(stack-content it)
			 (append #1# (curr-stack)))
		   (pop path)
		   (push it path))
		 (progn
		   (setf (stack-name (curr))
			 name)
		   (push (curr)
			 (stack-children (par))))))
	(error "Tried to name a stack with a non-symbol"))))

(defprim ~ nil
  (let ((name (pop-curr)))
    (if (symbolp name)
	(if (eq '^ name)
	    (push-curr (par))
	    (aif (find name (stack-children (curr))
		       :key #'stack-name
		       :test #'eq)
		 (push-curr it)
		 (error "There is no child stack ~A of stack ~A"
			name (stack-name (curr)))))
	(error "~A is not a symbol and cannot be a stack name"
	       name))))

;;; <- -> _ $ !* <> ^^ ~> [_] [%] [!] [?] & >r r> if