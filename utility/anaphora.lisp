(in-package #:poslin)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

(defmacro alet ((&rest letargs)
                &body body)
  `(let (this ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro avif (test then &optional else)
  (let ((g!test (gensym "test")))
    `(multiple-value-bind (it ,g!test)
         ,test
       (if ,g!test
           ,then
           ,else))))
