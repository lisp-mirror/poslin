(in-package #:poslin)

(define-constant +registers+
    '(pc path rstack dtable)
  :test #'equal
  :documentation "A list of the standard registers of Poslin.")

(defparameter *prims* '()
  "List of primary operator descriptions in Poslin. CAR of entry is
  the name of the operator, CADR designates immediateness, CDDR of
  entry is the body of the associated lambda form.")

(defparameter *stdlib* '()
  "Poslin code to execute in a new Poslin environment.")