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

(defmethod poslin-print ((object [env])
			 stream)
  (aif ([env]-parent object)
       (format stream "env<~A><~A>"
	       (poslin-print (fset:arb ([env]-content object))
			     nil)
	       (poslin-print it nil))
       (format stream "env<~A>"
	       (poslin-print (fset:arb ([env]-content object))
			     nil))))

(defmethod poslin-print ((object [binding])
			 stream)
  (format stream "b<~A>"
	  (poslin-print ([binding]-value object)
			nil)))

(defmethod poslin-print ((object (eql <noop>))
			 stream)
  (format stream "P{.}"))

(defmethod poslin-print ((object <constant>)
			 stream)
  (format stream "#{~A}"
	  (poslin-print (<constant>-val object)
			nil)))

(defmethod poslin-print ((object <prim>)
			 stream)
  (format stream "P{~A}"
	  (<prim>-name object)))

(defmethod poslin-print ((object <thread>)
			 stream)
  (format stream "{~A ~A}"
	  (poslin-print (<thread>-front object)
			nil)
	  (let* ((b (<thread>-back object))
		 (bp (poslin-print b nil)))
	    (if (<thread>-p b)
		(subseq bp 1 (1- (length bp)))
		bp))))

(defmethod poslin-print ((object (eql <meta-nothing>))
			 stream)
  (format stream "<NOTHING>"))
