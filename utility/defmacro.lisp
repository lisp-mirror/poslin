(in-package #:poslin)

#-sbcl
(defmacro defmacro/g! (name (&rest args)
		       &body body)
  (let ((syms (remove-duplicates (remove-if-not #'g!sym?
						(flatten body)))))
    `(defmacro ,name (,@args)
       (let ,(mapcar #`(,a1 (gensym ,(string-downcase (subseq
						       (symbol-name
							a1)
						       2))))
		     syms)
	 ,@body))))

(defmacro defmacro/g! (name (&rest args)
                       &body body)
  (let (g!symbols)
    (flet ((g!walker (subform context env)
             (declare (ignore context))
             (typecase subform
               (symbol
                (when (and (g!sym? subform)
                           (not (sb-walker:var-lexical-p subform env)))
                  (pushnew subform g!symbols))
                subform)
               (t subform))))
      (sb-walker:walk-form `(progn ,@body)
                           nil #'g!walker)
      `(defmacro ,name (,@args)
         (let (,@(mapcar #`(,a1 (gensym ,(subseq (symbol-name a1)
                                                 2)))
                         g!symbols))
           ,@body)))))

(defmacro defmacro! (name (&rest args)
		     &body body)
  (let* ((os (remove-if-not #'o!sym?
			    (flatten args)))
	 (gs (mapcar #'o!->g!
		     os)))
    `(defmacro/g! ,name (,@args)
       `(let ,(mapcar #2`(,a1 ,a2)
		      (list ,@gs)
		      (list ,@os))
	  ,(progn ,@body)))))
