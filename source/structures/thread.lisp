(in-package #:poslin)

(defadt [thread]
  <noop>
  (<prim> function)
  (<constant> t)
  (<thread> [thread] [thread]))

(defmatch thread-front ([thread])
    [thread]
  ((<thread> front _)
   front)
  (prim prim))

(defmatch thread-back ([thread])
    [thread]
  ((<thread> _ back)
   back)
  (_ <noop>))

(defmacro defnprim (standard name immediate? docstring &body body)
  `(push '(,(intern name :keyword)
	   ,immediate? ,docstring ,body)
	 ,standard))

(defmacro defprim (standard name immediate? docstring &body body)
  `(defnprim ,standard ,name ,immediate? ,docstring
     ,@body
     (setf pc (thread-back pc))))

(defun thread<-stack (stack)
  (labels ((_rec (front back)
	     (if back
		 (<thread> (if (typep front '[thread])
			       front
			       (<constant> front))
			   (_rec (first back)
				 (rest back)))
		 (if (typep front '[thread])
		     front
		     (<constant> front)))))
    (if stack
	(let ((stack (reverse stack)))
	  (_rec (first stack)
		(rest stack)))
	<noop>)))
