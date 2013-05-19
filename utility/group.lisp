(in-package #:poslin)

(defun group (source n)
  (if (zerop n)
      (error "zero length group")
      (labels ((rec (source acc)
		 (let ((rest (nthcdr n source)))
		   (if (consp rest)
		       (rec rest (cons (subseq source 0 n)
				       acc))
		       (nreverse (cons source acc))))))
	(if source
	    (rec source '())
	    '()))))