(in-package #:poslin)

(defun mkstr (&rest objs)
  (format nil "~{~A~}"
	  objs))

(defun symb (&rest objs)
  (values (intern (apply #'mkstr
			 objs))))