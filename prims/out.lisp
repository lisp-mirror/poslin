(in-package #:poslin)

(defprim o<- nil
    "( val -- )"
  (args (val)
    (push val out)))

(defprim o_ nil
    "( -- )"
  (setf out '()))