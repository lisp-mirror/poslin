(in-package #:poslin)

(defun find-word (word path)
  (if path
      (aif (gethash word (curr-dict))
	   it
	   (find-word word (cdr path)))))

(defmacro lookup (word)
  `(find-word ,word path))

(defun find-imm (word path)
  (if path
      (multiple-value-bind (val found?)
	  (gethash word (curr-imm))
	(if found?
	    val
	    (find-imm word (cdr path))))))

(defmacro immediate? (word)
  `(find-imm ,word path))