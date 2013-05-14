(in-package #:poslin)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

(defmacro! a2if (get then &optional else)
  `(multiple-value-bind (it ,g!found?)
       ,get
     (if ,g!found?
	 ,then
	 ,else)))

(defmacro alambda (args &body body)
  `(labels ((self ,args
	      ,@body))
     #'self))

(defmacro alet (letargs &body body)
  `(let (this ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     this))