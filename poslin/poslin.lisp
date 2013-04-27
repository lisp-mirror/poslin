(in-package #:poslin)

(defparameter *poslin*
  (new-poslin))

(defmacro stdposlin (&body code)
  `(poslin *poslin*
     ,@code))

(defun reset-stdposlin ()
  (setf *poslin* (new-poslin)))