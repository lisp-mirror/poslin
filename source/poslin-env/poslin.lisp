(in-package #:poslin)

(defmacro poslin (&optional standard-libraries)
  `(alet ,+registers+
     (install ,standard-libraries)
     (plambda (v)
	 ,+registers+
       (handle))))
