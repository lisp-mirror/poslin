(in-package #:poslin)

(defun pos-read (stream path)
  (let* ((eof (gensym "eof"))
	 (prep (make-thread))
	 (thread)
	 (begin))
    (do ((word (read stream nil eof)
	       (read stream nil eof)))
	((eq word eof)
	 (symbol= word 'exit))
      (if (and (symbolp word)
	       (op-env-imm (pstack-op-env (car path))
			   word))
	  (setf (thread-curr prep)
		(binding-val (op-env-def (pstack-op-env (car path))
					 word)))
	  (setf (thread-curr prep)
		word))
      (if begin
	  (setf (thread-next thread)
		prep)
	  (setf begin prep))
      (setf thread prep)
      (setf prep (make-thread)))
    begin))

(defun pos-read-string (string path)
  (pos-read (make-string-input-stream string)
	    path))

(defun pos-read-line (stream path)
  (pos-read-string (read-line stream nil (values))
		   path))

(defun pos-read-file (folder filename path pc rstack ntable)
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
					      (aif (position #\; line)
						   (subseq line 0 it)
						   line))))
	    (pos-read-string file-content path))
	  (poserror pc rstack ntable 'file-not-found
		    "File ~A could not be found in ~A"
		    filename folder)))))

(defmacro poslin-read (stream)
  `(pos-read ,stream path))

(defmacro poslin-read-string (string)
  `(pos-read-string ,string path))

(defmacro poslin-read-line (&optional (stream *standard-input*))
  `(pos-read-line ,stream path))

(defmacro poslin-read-file (filename)
  `(pos-read-file folder ,filename path pc rstack ntable))
