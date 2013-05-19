(in-package #:poslin)

(defun repl (&optional (poslin (poslin)))
  (loop do
       (progn
	 (with-pandoric (ntable path)
	     poslin
	   (format t "[~{~S:~}~{~<~%~:; ~A~>~}]~%> "
		   (reverse (mapcar #'pstack-name
				    path))
		   (reverse (mapcar (lambda (val)
				      (posprint val ntable))
				    (pstack-content (car path)))))
	   (finish-output))
	 (aif (pos-read-line t poslin)
	      (pos-eval it poslin)
	      (return-from repl
		(get-pandoric poslin 'out))))))