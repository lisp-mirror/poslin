(in-package #:poslin)

(defun poslin-repl ()
  (block repl
    (let ((pos (poslin-env)))
      (loop do
	   (progn
	     (format t "~&[~{~A:~}~{ ~S~}]"
		     (with-pandoric (path)
			 pos
		       (mapcar #'stack-name
			       (reverse path)))
		     (with-pandoric (path)
			 pos
		       (reverse (curr-stack))))
	     (finish-output)
	     (format t "~&> ")
	     (finish-output)
	     (let ((line (read-line)))
	       (loop for el
		  in (read-from-string (concatenate 'string
						    "(" line ")"))
		  do (if (and (symbolp el)
			      (string= el 'exit))
			 (with-pandoric (out)
			     pos
			   (return-from repl out))
			 (funcall pos el)))))))))