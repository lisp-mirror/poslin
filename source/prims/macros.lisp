(in-package #:poslin)

(defmacro push-stack (value)
  `(push ,value (stack path)))

(defmacro pop-stack ()
  `(if (stack path)
       (pop (stack path))
       (unwind "Bottom of Stack popped"
               :stack-bottom-error)))

(defmacro stack-call (fn)
  `(push-stack (,fn (pop-stack))))

#+nil
(defmacro stack-args ((&rest args)
		      &body body)
  `(let (,@(mapcar #`(,a1 (pop-stack))
		   (reverse args)))
     ,@body))

(defmacro stack-args ((&rest args)
                      &body body)
  `(let (,@(mapcar #`(,(if (consp a1)
                           (car a1)
                           a1)
                       (pop-stack))
                   (reverse args)))
     ,@(mapcar #`(unless (typep ,(first a1)
                                ',(second a1))
                   (unwind "Type error"
                           (list :type-error ,(first a1))))
               (remove-if-not #'consp
                              (reverse args)))
     ,@body))
