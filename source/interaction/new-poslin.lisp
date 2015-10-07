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
		    :features '(:prim)
		    ))))

(defmacro poslin-install-prims (&rest standards)
  `(progn
     (setf (op-env path)
	   (insert (op-env path)
		   :. (binding <noop> "no op"))
	   (imm-env path)
	   (insert (imm-env path)
		   :. (binding <false>)))
     ,@(loop for standard in standards
	  append
	    (mapcar #`(setf (op-env path)
                            (insert (op-env path)
                                    ',(intern (first a1)
                                              :keyword)
                                    (binding (<prim> (lambda ()
                                                       ,@(fourth a1))
                                                     ,(first a1))
                                             ,(third a1)))
                            (imm-env path)
                            (insert (imm-env path)
                                    ',(intern (first a1)
                                              :keyword)
                                    (binding ,(if (second a1)
                                                  '<true>
                                                  '<false>))))
                    (symbol-value standard)))))

(defun immediate? (v path)
  (if (symbolp v)
      (aif (lookup (imm-env path)
		   v)
	   (if (and (not (eq it <meta-nothing>))
		    (eq ([binding]-value it)
			<true>))
	       ([binding]-value (lookup (op-env path)
					v))))))

(defmacro new-poslin (&rest standards)
  `(the function
        (alet (pc rstack path
                  (stepping nil))
          (poslin-setup-registers)
          (poslin-install-prims ,@standards)
          (plambda (v)
              (this pc path rstack stepping)
            (let ((thread (immediate? v path)))
              (cond
                (thread
                 (setf pc thread))
                ((typep v '<quotation>)
                 (push (<quotation>-val v)
                       (stack path)))
                (t
                 (push v (stack path)))))
            (interpreter)
            path))))
