(in-package #:poslin)

(defmacro push-stack (value)
  `(push ,value (stack path)))

(defmacro pop-stack ()
  `(if (stack path)
       (pop (stack path))
       (error "Bottom of stack popped")))

(defmacro stack-call (fn)
  `(push-stack (,fn (pop-stack))))

(defmacro stack-args ((&rest args)
		      &body body)
  `(let (,@(mapcar #`(,a1 (pop-stack))
		   (reverse args)))
     ,@body))
