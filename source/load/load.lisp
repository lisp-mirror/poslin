(in-package #:poslin)

(defun pos-load (poslin filename)
  (declare (type function poslin)
	   (type string filename))
  (pos-eval (pos-read-file filename poslin)
	    poslin))

(defmacro poslin-load (filename)
  `(pos-load this ,filename))