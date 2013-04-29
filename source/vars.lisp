(in-package #:poslin)

(define-constant +registers+
    '(pc rstack path dtable)
  :test #'equal)

(defparameter *prims* '())

(defparameter *stdlib* '())