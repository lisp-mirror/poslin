(in-package #:poslin)

(defparameter <true>
  '<true>)

(defparameter <false>
  '<false>)

(deftype [bool] ()
  `(member <true> <false>))
