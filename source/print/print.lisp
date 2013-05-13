(in-package #:poslin)

(defun posprint (val ntable)
  (declare (type hash-table ntable))
  (the 
   string
   (typecase val
     (function
      (format nil "OP{~S}"
	      (gethash val ntable)))
     (symbol
      (format nil "~S"
	      val))
     (pstack
      (format nil "[~S]"
	      (pstack-name val)))
     (thread
      (format nil "TH{~{~<~% ~:; ~A~>~} }"
	      (labels ((rec (thread acc)
			 (declare (type (or thread null)
					thread)
				  (type (or cons null)
					acc))
			 (the
			  (or cons null)
			  (if (not thread)
			      (nreverse acc)
			      (rec (thread-next thread)
				   (cons (posprint (thread-curr
						    thread)
						   ntable)
					 acc))))))
		(rec val '()))))
     (op-env
      (format nil "OP-ENV<~36R>"
	      (sxhash val)))
     (var-env
      (format nil "VAR-ENV<~36R>"
	      (sxhash val)))
     (t (format nil "~A"
		val)))))

(defmacro poslin-print (val)
  `(posprint ,val ntable))
