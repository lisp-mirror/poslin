(in-package #:poslin)

(defmacro poslin (poslin-env &body code)
  `(progn
     ,@(mapcar #`(funcall ,poslin-env ',a1)
	       code)))