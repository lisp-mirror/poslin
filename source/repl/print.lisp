(in-package #:poslin)

(defgeneric poslin-print (object stream)
  (:method (object stream)
    (format stream "~S"
	    object)))

(defmethod poslin-print ((object null)
			 stream)
  (format stream "[]"))

(defmethod poslin-print ((object cons)
			 stream)
  (format stream "[..]"))

(defmethod poslin-print ((object symbol)
			 stream)
  (format stream "~A"
	  (symbol-name object)))

(defmatch poslin-print ([env] t)
    (or string null)
  (((<root-env> m)
    stream)
   (format stream "env<~A..>"
	   (poslin-print (fset:arb m)
			 nil)))
  (((<env> m p)
    stream)
   (format stream "env<~A..><~A>"
	   (poslin-print (fset:arb m)
			 nil)
	   (poslin-print p nil))))

(defmethod poslin-print ((object [binding])
			 stream)
  (format stream "b<~A>"
	  (poslin-print ([binding]-value object)
			nil)))

(defmatch poslin-print ([thread] t)
    (or string null)
  ((<noop> stream)
   (format stream "P{.}"))
  (((<constant> c)
    stream)
   (format stream "#{~A}"
	   (poslin-print c nil)))
  (((<prim> _ n)
    stream)
   (format stream "P{~A}"
	   n))
  (((<thread> f b)
    stream)
   (format stream "{~A ~A}"
	   (poslin-print f nil)
	   (let ((bp (poslin-print b nil)))
	     (if (typep b '<thread>)
		 (subseq bp 1 (1- (length bp)))
		 bp)))))

(defmatch poslin-print ([nothing] t)
    (or string null)
  ((<meta-nothing> stream)
   (format stream "<NOTHING>")))