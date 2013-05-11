(in-package #:poslin)

(defprim >> nil
  ;; Load poslin file
  ;; ( filename -- ??? )
  (args (filename)
    (if (stringp filename)
	(let ((filename
	       (if (char= (elt filename 0)
			  #\/)
		   filename
		   (concatenate 'string
				folder
				filename))))
	  (with-open-file (stream filename)
	    (let ((eof (gensym "eof")))
	      (do ((curr (read-line stream nil eof)
			 (read-line stream nil eof)))
		  ((eq curr eof))
		(multiple-value-bind (w pos)
		    (read-from-string curr nil eof)
		  (setf curr (subseq curr pos))
		  (loop while (not (eq w eof))
		     do (progn
			  (funcall this w)
			  (multiple-value-bind (nw npos)
			      (read-from-string curr nil eof)
			    (setf curr (subseq curr npos)
				  w nw)))))))))
	(perror malformed-filename
		"Attempt to open file ~S"
		filename))))

(defprim cd nil
  ;;
  ;; ( pathdiff -- )
  (args (pathdiff)
    (if (char= (elt pathdiff 0)
	       #\/)
	(setf folder pathdiff)
	(labels ((split (string acc)
		   (if (string= "" string)
		       (nreverse acc)
		       (let ((/pos (position #\/ string)))
			 (if /pos
			     (split (subseq string (1+ /pos))
				    (cons (subseq string 0 /pos)
					  acc))
			     (nreverse (cons string acc)))))))
	  (loop for diff
	     in (split pathdiff '())
	     do (cond
		  ((string= ".." diff)
		   (setf folder
			 (subseq folder 0
				 (1+ (position #\/ folder
					       :from-end t
					       :end
					       (1- (length
						    folder)))))))
		  ((string= "." diff))
		  ((string= "~" diff)
		   (setf folder
			 (let ((home (osicat:environment-variable
				      "HOME")))
			   (if (char= (elt home (1- (length home)))
				      #\/)
			       home
			       (concatenate 'string
					    home "/")))))
		  (t (setf folder
			   (concatenate 'string
					folder diff "/")))))))))

(defprim folder nil
  (push-curr folder))

(defprim >out nil
  ;; Push onto output
  ;; ( val -- )
  (args (val)
    (push val out)))

(defprim out_ nil
  ;; Delete output
  ;; ( -- )
  (setf out nil))