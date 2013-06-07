(in-package #:poslin)

(defun pos-read (stream poslin)
  (with-pandoric (path)
      poslin
    (let* ((eof (gensym "eof"))
	   (prep (make-thread))
	   (thread)
	   (begin))
      (do ((word (read stream nil eof)
		 (read stream nil eof)))
	  ((or (eq word eof)
	       (and (symbolp word)
		    (symbol= word 'exit))))
	(if (and (symbolp word)
		 (op-env-imm (pstack-op-env (car path))
			     word))
	    (setf (thread-curr prep)
		  (binding-val (op-env-def (pstack-op-env (car path))
					   word)))
	    (if (and (consp word)
		     (symbol= (car word)
			      'quote))
		(setf (thread-curr prep)
		      (cadr word))
		(setf (thread-curr prep)
		      word)))
	(if thread
	    (setf (thread-next thread)
		  prep))
	(if (not begin)
	    (setf begin prep))
	(setf thread prep)
	(setf prep (make-thread)))
      begin)))

(defun pos-read-string (string poslin)
  (pos-read (make-string-input-stream string)
	    poslin))

(defun pos-read-line (stream poslin)
  (pos-read-string (read-line stream nil (values))
		   poslin))

(defun pos-read-file (filename poslin)
  (with-pandoric (folder)
      poslin
    (let ((file-content "")
	  (eof (gensym "eof")))
      (with-open-file (stream (concatenate 'string
					   folder filename ".poslin")
			      :if-does-not-exist nil)
	(if stream
	    (progn
	      (do ((line (read-line stream nil eof)
			 (read-line stream nil eof)))
		  ((eq line eof))
		(setf file-content (concatenate 'string
						file-content " "
						(aif (position #\;
							       line)
						     (subseq line 0
							     it)
						     line))))
	      (pos-read-string file-content poslin)))))))

(defmacro poslin-read (stream)
  `(pos-read ,stream this))

(defmacro poslin-read-string (string)
  `(pos-read-string ,string this))

(defmacro poslin-read-line (&optional (stream *standard-input*))
  `(pos-read-line ,stream this))

(defmacro poslin-read-file (filename)
  `(pos-read-file ,filename this))
