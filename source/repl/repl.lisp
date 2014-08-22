(in-package #:poslin)

(defun repl (poslin &rest files)
  (let ((*print-circle* t))
    (loop for file in files
       do
	 (loop for v in (poslin-read-file file *parse-order*)
	    do
	      (funcall poslin v)))
    (with-pandoric (path)
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
	       (if (eq v :quit)
		   (return-from repl
		     (stack path))
		   (funcall poslin v)))
	     (format t "[ ")
	     #1# (format t "]"))))))

(defun repl0 ()
  (repl (new-poslin *prim*)))

(defun repl1 ()
  (repl (new-poslin *prim*)
	"~/src/Poslin/poslin-specification/libs/base.poslin"
	))
