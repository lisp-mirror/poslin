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

(labels ((group-check (group n)
           (if group
               (aif (rest group)
                    (and (= (length (first group))
                            n)
                         (group-check it n))
                    (<= (length (first group))
                        n))
               t)))
  (for-all
      ((size an-index)
       (list (a-list 'a-char)))
    (let ((size (1+ size)))
      (labels ((check (group)
                 (group-check group size)))
        (is check (group list size))))))
