(in-package #:poslin)

(defun car-or-x (x)
  #.+optimization-parameters+
  (if (consp x)
      (car x)
      x))
