(in-package #:poslin)

(defstruct [binding]
  value
  (doc ""
       :type string))

(defun binding (val &optional docstring)
  (make-[binding] :value val
		  :doc (if docstring
			   docstring
			   "")))
