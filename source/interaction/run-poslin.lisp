(in-package #:poslin)

(defun sym-to-keyword (obj)
  #.+optimization-parameters+
  (if (symbolp obj)
      (values (intern (symbol-name obj)
		      :keyword))
      obj))

(defmacro! run-poslin (o!poslin &body code)
  `(dolist (v ',(mapcar #'sym-to-keyword
			code)
	    (with-pandoric (path)
		,g!poslin
	      path))
     #.+optimization-parameters+
     (funcall ,g!poslin v)))
