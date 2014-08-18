(in-package #:poslin)

(defmacro op-env (path)
  `([binding]-value (path-get ,path :op)))

(defmacro imm-env (path)
  `([binding]-value (path-get ,path :imm)))

(defun env-set (env key val)
  (insert env key (binding val)))

(defun env-set* (env &rest k/v*)
  (if k/v*
      (let ((k (first k/v*))
	    (v (second k/v*))
	    (rest (cddr k/v*)))
	(apply #'env-set*
	       (env-set env k v)
	       rest))
      env))

(defmacro poslin-setup-registers ()
  `(setf pc
	 <noop>
	 path
	 (<root>
	  (env-set* (<root-env> (fset:empty-map))
		    :stack '()
		    :op (<root-env> (fset:empty-map))
		    :imm (<root-env> (fset:empty-map))
		    ))))

(defmacro poslin-install-prims (&rest standards)
  `(progn
     (setf (op-env path)
	   (insert (op-env path)
		   :. (binding <noop> "no op"))
	   (imm-env path)
	   (insert (imm-env path)
		   :. (binding nil)))
     ,@(loop for standard in standards
	  append
	    (mapcar #`(let ((thread (<prim> (lambda ()
					      ,@(fourth a1)))))
			(setf (op-env path)
			      (insert (op-env path)
				      ',(first a1)
				      (binding thread ,(third a1)))
			      (imm-env path)
			      (insert (imm-env path)
				      ',(first a1)
				      (binding ,(second a1)))))
		    (symbol-value standard)))))

(defun immediate? (v path)
  (if (symbolp v)
      (aif (lookup (imm-env path)
		   v)
	   (if ([binding]-value it)
	       ([binding]-value (lookup (op-env path)
					v))))))

(defmacro new-poslin (&rest standards)
  `(let (pc rstack path)
     (poslin-setup-registers)
     (poslin-install-prims ,@standards)
     (plambda (v)
	 (pc path rstack)
       (let ((thread (immediate? v path)))
	 (cond
	   (thread
	    (setf pc thread))
	   ((typep v '[quotation])
	    (match v
		t
	      ((<quotation> s)
	       (push s (stack path)))))
	   (t
	    (push v (stack path)))))
       (interpreter)
       path)))
