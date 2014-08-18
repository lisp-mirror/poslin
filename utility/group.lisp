(in-package #:poslin)

(defun group (source n)
  (if (<= n 0)
      (error "~A length group"
	     n)
      (labels ((_rec (source acc)
		 (let ((rest (nthcdr n source)))
		   (if (consp rest)
		       (_rec rest (cons (subseq source 0 n)
					acc))
		       (nreverse (cons source acc))))))
	(if source
	    (_rec source '())
	    '()))))
