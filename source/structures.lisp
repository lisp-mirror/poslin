(in-package #:poslin)

(defstruct stacktree
  name stack children dict)

(defmethod print-object ((object stacktree)
			 stream)
  (format stream "#<ST ~A {~{ ~S~} } ~{ ~A~}>"
	  (stacktree-name object)
	  (stacktree-stack object)
	  (mapcar #'stacktree-name
		  (stacktree-children object))))

(defun find-child (childname stacktree)
  (find childname (stacktree-children stacktree)
	:key #'stacktree-name
	:test #'eq))

(defstruct word
  name stack thread prev immediate)