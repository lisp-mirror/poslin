(in-package #:poslin)

(defstruct <quotation>
  (val nil
       :type symbol))

(defun <quotation> (val)
  #.+optimization-parameters+
  (declare (type symbol val))
  (make-<quotation> :val val))
