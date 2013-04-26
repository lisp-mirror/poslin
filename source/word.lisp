(in-package #:poslin)

(defun lookup (word rem-path)
  (labels ((rec (dict)
	     (if dict
		 (if (eq (word-name dict)
			 word)
		     word
		     (rec (word-prev dict))))))
    (let ((found (rec (stacktree-dict (cdr rem-path)))))
      (if found
	  found
	  (let ((next (cdr rem-path)))
	    (if next
		(lookup word next)))))))

(defmacro defnprim (name &body body)
  `(setf *prims*
	 (nconc *prims* '(,name ,@body))))

(defmacro defprim (name &body body)
  `(defnprim ,name
     ,@body
     (pop pc)))

(defmacro addstd (&body code)
  `(setf *stdlib*
	 (nconc *stdlib* ,code)))