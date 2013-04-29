(in-package #:poslin)

(defmacro poslin-handle ()
  `(typecase v
     (symbol
      (aif (lookup v path)
	   (if (immediate? v path)
	       (progn
		 (push (word-thread it)
		       pc)
		 (interpreter))
	       (push-curr v))
	   (push-curr v)))
     (cons
      (if (eq (car v)
	      'quote)
	  (push-curr (cdr v))
	  (if (eq (car v)
		  'lisp)
	      (push-curr (eval (cadr v)))
	      (push-curr v))))
     (t (push-curr v))))