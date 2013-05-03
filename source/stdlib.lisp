(in-package #:poslin)

(defmacro addstd (&body code)
  `(setf *stdlib*
	 (append *stdlib* ',code)))