(in-package #:poslin)

(defmacro! run-poslin (o!poslin-env &body code)
  `(progn
     ,@(mapcar #`(funcall ,g!poslin-env ',a1)
	       code)))
