(in-package #:poslin)

(defun format-thread (thread ntable)
  (typecase thread
    (cons
     (if (cdr thread)
	 (format nil "~A ~A"
		 (format-thread (car thread)
				ntable)
		 (format-thread (cdr thread)
				ntable))
	 (format nil "~A"
		 (format-thread (car thread)
				ntable))))
    (function
     (format nil "OP{~A}"
	     (gethash thread ntable)))
    (t
     (format nil "~S"
	     thread))))

(defmacro! formatt (thread)
  `(format-thread ,thread ntable))