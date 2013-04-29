(in-package #:poslin)

(defstruct word
  name thread)

(defstruct stack
  name content (dict (make-hash-table :test #'eq))
  (imm (make-hash-table :test #'eq))
  children)

(defun lookup (word path)
  (if path
      (aif (gethash word (stack-dict (curr)))
	   it
	   (lookup word (cdr path)))))

(defun immediate? (word path)
  (if path
      (multiple-value-bind (val found?)
	  (gethash word (stack-imm (curr)))
	(if found?
	    val
	    (immediate? word (cdr path))))))