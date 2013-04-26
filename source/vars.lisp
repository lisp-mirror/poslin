(in-package #:poslin)

(define-constant +registers+
    '(pc path rstack)
  :test #'equal
  :documentation "A list of the standard registers of Poslin.")

(defvar *prims* '()
  "List of primary operator descriptions in Poslin. CAR of entry is
  the name of the operator, CDR of entry is the body of the associated
  lambda form.")

(defvar *stdlib* '()
  "Poslin code to execute in a new Poslin environment.")