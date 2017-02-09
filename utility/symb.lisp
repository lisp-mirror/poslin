(in-package #:poslin)

(defun mkstr (&rest objs)
  #.+optimization-parameters+
  (the string
    (values
     (format nil "~{~A~}"
	     objs))))

(defun symb (&rest objs)
  #.+optimization-parameters+
  (the symbol
    (values
     (intern (apply #'mkstr
		    objs)))))
