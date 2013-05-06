(in-package #:poslin)

(defmacro handle ()
  `(if (symbolp v)
       (if (immediate? v)
	   (progn
	     (push (lookup-op v)
		   pc)
	     (interpreter))
	   (push-curr v))
       (if (and (consp v)
		(string= (car v)
			 'quote))
	   (push-curr (cadr v))
	   (push-curr v))))

(defmacro poslin-env ()
  `(alet ,+registers+
     (prepare)
     (plambda (v)
	 ,+registers+
       (handle)
       out)))