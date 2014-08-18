(in-package #:poslin)

(defun !sym? (c s)
  (and (symbolp s)
       (> (length #1=(symbol-name s))
	  2)
       (char= (elt #1# 1)
	      #\!)
       (char= (elt #1# 0)
	      c)))

(defun g!sym? (s)
  (!sym? #\G s))

(defun o!sym? (s)
  (!sym? #\O s))

(defun o!->g! (s)
  (if (o!sym? s)
      (symb "G!" (subseq (symbol-name s)
			 2))
      (error "Tried to call O!->G! on ~S"
	     s)))
