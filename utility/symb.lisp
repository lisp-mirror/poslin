(in-package #:poslin)

(defun mkstr (&rest objs)
  (the string
    (values
     (format nil "~{~A~}"
	     objs))))

(defun symb (&rest objs)
  (the symbol
    (values
     (intern (apply #'mkstr
		    objs)))))
