(in-package #:poslin)

(defun poslin-repl ()
  (block repl
    (let ((pos (poslin-env)))
      (loop do
	   (progn
	     (format t "~&[~{~A:~}~{~<~%~:; ~S~>~}]"
		     (with-pandoric (path)
			 pos
		       (mapcar #'stack-name
			       (reverse path)))
		     (with-pandoric (path ntable)
			 pos
		       (reverse
			(mapcar
			 (alambda (el)
			   (typecase el
			     (function
			      (format
			       nil "OP{~A}"
			       (gethash el ntable)))
			     (cons
			      (cons (self (car el))
				    (self (cdr el))))
			     (stack
			      (format nil "[ ~A ]"
				      (stack-name el)))
			     (t el)))
			 (curr-stack)))))
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