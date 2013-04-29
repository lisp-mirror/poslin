(in-package #:poslin)

(defun g!sym? (s)
  (and (symbolp s)
       (> (length #1=(symbol-name s))
	  2)
       (string= #1# "G!"
		:end1 2)))

(defun o!sym? (s)
  (and (symbolp s)
       (> (length #1=(symbol-name s))
	  2)
       (string= #1# "O!"
		:end1 2)))

(defun o!->g! (s)
  (symb "G!" (subseq (symbol-name s)
		     2)))