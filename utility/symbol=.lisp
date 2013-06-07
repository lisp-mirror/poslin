(in-package #:poslin)

(defun symbol= (sym1 sym2)
  (declare (type symbol sym1 sym2))
  (the boolean
       (string= (symbol-name sym1)
		(symbol-name sym2))))
