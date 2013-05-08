(in-package #:poslin)

(defmacro perror (type text &rest args)
  `(progn
     (let ((remt (copy-list pc))
	   (remr (copy-list rstack)))
       (setf pc '())
       (setf rstack '())
       (format t "~&POSLIN-~A-ERROR~% Remaining PC: ~A~% ~
                  Remaining RSTACK:  ~A~%  ~A"
	       ',type
	       (formatt remt)
	       (formatt remr)
	       (format nil ,text ,@args)))))