(in-package #:poslin)

(defprim folder nil
    "( -- path )"
  (push-curr folder))

(defprim cd nil
    "( path -- )"
  (args (path)
    (if (stringp path)
	(unless (string= "" path)
	  (if (char= (elt path 0)
		     #\/)
	      (setf folder path)
	      (loop for f
		 in (split-sequence #\/ path
				    :remove-empty-subseqs t)
		 do (cond
		      ((string= ".." f)
		       (setf folder
			     (aif (position #\/ folder
					    :from-end t
					    :end (1- (length folder)))
				  (subseq folder 0 (1+ it))
				  folder)))
		      ((string= "." f))
		      (t (setf folder
			       (concatenate 'string
					    folder f "/")))))))
	(poslin-error malformed-path
		      "Attempt to change folder by ~A"
		      path))))