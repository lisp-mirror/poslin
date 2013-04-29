(in-package #:poslin)

(defmacro defmacro/g! (name args &body body)
  (let ((syms (remove-duplicates (remove-if-not #'g!sym?
						(flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
	      #`(,a1 (gensym
		      ,(string-downcase (subseq (symbol-name a1)
						2))))
	      syms)
	 ,@body))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!sym?
			    args))
	 (gs (mapcar #'o!->g!
		     os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #2`(,a1 ,a2)
		      (list ,@gs)
		      (list ,@os))
	  ,(progn ,@body)))))