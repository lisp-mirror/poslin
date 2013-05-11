(in-package #:poslin)

(defmacro! run-poslin (o!poslin-env &body code)
  `(locally
       (declare (ignorable ,g!poslin-env))
     ,@(mapcar #`(funcall ,g!poslin-env ',a1)
	       code)))