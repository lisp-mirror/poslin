(in-package #:poslin)

(defun repl (poslin &rest files)
  (handler-case
      (let ((*print-circle* t))
	(loop for file in files
	   do
	     (loop for v in (poslin-read-file file *parse-order*)
		do
		  (funcall poslin v)))
	(with-pandoric (path stepping)
	    poslin
	  (format t "[ ")
	  #1=(loop for obj in (reverse (stack path))
		do
		  (progn
		    (poslin-print obj *standard-output*)
		    (format t " ")))
	  (format t "]")
	  (loop do
	       (progn
		 (format t "~%> ")
		 (finish-output)
		 (dolist (v (poslin-read-block *standard-input*
					       *parse-order*))
		   (case v
                     (:quit
                      (return-from repl
                        (stack path)))
                     (:step
                      (setf stepping t))
                     (:unstep
                      (setf stepping nil))
                     (t
                      (funcall poslin v))))
		 (format t "[ ")
		 #1# (format t "]")))))
    (t (err)
      (format t "~A"
	      err)
      (with-pandoric (path pc rstack)
	  poslin
	(format t "~%~%PC:~%~A~%~%RSTACK:~%~A~%~%STACK:~%~A~%"
		(poslin-print pc nil)
		(poslin-print rstack nil)
		(poslin-print (stack path)
			      nil))
	(setf pc <noop>))
      (when (y-or-n-p "~%Continue?")
	(repl poslin)))))

(defun repl0 ()
  (repl (new-poslin *prim*)))

(defun repl1 ()
  (repl (new-poslin *prim*)
	"~/src/Poslin/poslin-specification/libs/base.poslin"
	))

#+sbcl
(defun repl-dyn ()
  (apply #'repl
	 (new-poslin *prim*)
	 (rest sb-ext:*posix-argv*)))
