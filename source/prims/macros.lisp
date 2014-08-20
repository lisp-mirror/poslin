(in-package #:poslin)

(defmacro push-stack (value)
  `(push ,value (stack path)))

(defmacro pop-stack ()
  `(pop (stack path)))

(defmacro stack-call (fn)
  `(push-stack (,fn (pop-stack))))

(defmacro stack-args ((&rest args)
		      &body body)
  `(let (,@(mapcar #`(,a1 (pop-stack))
		   (reverse args)))
     ,@body))
