(in-package #:poslin)

(defmacro op-env (path)
  `([binding]-value (path-get ,path :op)))

(defmacro imm-env (path)
  `([binding]-value (path-get ,path :imm)))

(defmacro poslin-setup-registers ()
  `(setf pc
	 <noop>
	 path
	 (<root>
	  (map (:stack (binding '()))
               (:op (binding (empty-map <meta-nothing>)))
               (:imm (binding (empty-set)))
               (:features (binding (set :prim)))
               :default <meta-nothing>))))

(defmacro poslin-install-prims (&rest standards)
  `(progn
     (setf (op-env path)
	   (with (op-env path)
                 :. (binding <noop>)))
     ,@(loop for standard in standards
	  nconc
	    (mapcar #`(progn
                        (setf (op-env path)
                              (with (op-env path)
                                    ',(intern (first a1)
                                              :keyword)
                                    (binding (<prim> (lambda ()
                                                       ,(fourth a1))
                                                     ,(first a1)))))
                        ,@(when (second a1)
                                `((setf (imm-env path)
                                        (with (imm-env path)
                                              ',(intern (first a1)
                                                        :keyword))))))
                    (symbol-value standard)))))

(defun immediate? (v path)
  (lookup (imm-env path)
          v))

(defun get-op (v path)
  (avif (lookup (op-env path)
                v)
        ([binding]-value it)
        (unwind (format nil "Attempt to call undefined operation `~A`"
                        v)
                (list 'undefined-operation-error v))))

(defmacro new-poslin (&rest standards)
  `(the function
        (alet (pc rstack path
                  (stepping nil))
          (poslin-setup-registers)
          (poslin-install-prims ,@standards)
          (plambda (v)
              (this pc path rstack stepping)
            (cond
              ((immediate? v path)
               (setf pc (get-op v path)))
              ((typep v '<quotation>)
               (push (<quotation>-val v)
                     (stack path)))
              (t
               (push v (stack path))))
            (interpreter)
            path))))
