(in-package #:poslin)

(defstruct <quotation>
  (val nil
       :type symbol))

(defun <quotation> (val)
  (make-<quotation> :val val))
