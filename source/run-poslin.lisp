(in-package #:poslin)

(defmacro run-poslin (poslin-env &body code)
  `(progn
     ,@(mapcar #`(funcall ,poslin-env ',a1)
	       code)))
