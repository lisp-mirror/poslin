(in-package #:poslin)

(defprim error t
  (args (type text)
    (push-curr (lambda ()
		 (let ((rpc pc)
		       (rrs rstack))
		   (setf pc '())
		   (setf rstack '())
		   (format t "~&POSLIN-~A-ERROR~% Remaining PC: ~A~% ~
                              Remaining RSTACK: ~A~%  ~A"
			   type (formatt rpc)
			   (formatt rrs)
			   text))))))