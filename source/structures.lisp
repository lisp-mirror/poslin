(in-package #:poslin)

(defstruct stacktree
  name stack children dict)

(defmethod print-object ((object stacktree)
			 stream)
  (format stream "#<ST ~A: |~{ ~A~} | c:~{ ~A~}
     ~A>"
	  (stacktree-name object)
	  (stacktree-stack object)
	  (stacktree-children object)
	  (stacktree-dict object)))

(defun find-child (childname stacktree)
  (find childname (stacktree-children stacktree)
	:key #'stacktree-name
	:test #'eq))

(defstruct word
  name stack thread prev)

(defmethod print-object ((object word)
			 stream)
  (labels ((words (word acc)
	     (if (not word)
		 (nreverse acc)
		 (words (word-prev word)
			(cons (word-name word)
			      acc)))))
    (format stream "#<DICT~{ ~A~}>"
	    (words object ()))))