(in-package #:poslin)

(defun find-thread (poslin-env word)
  (declare (type function poslin-env)
	   (type symbol word))
  (with-pandoric (path)
      poslin-env
    (let ((w (lookup word path)))
      (if w
	  (word-thread w)
	  (warn "No word ~A found in Poslin ~A"
		word poslin-env)))))

(defun pp-thread (poslin-env thread)
  (declare (type function poslin-env))
  (with-pandoric (dtable)
      poslin-env
    (if (listp thread)
	(mapcar (curry #'pp-thread
		       poslin-env)
		thread)
	(if (functionp thread)
	    (list '! (gethash thread dtable))
	    thread))))

(defun thread-cost (thread)
  (if (consp thread)
      (loop for subthread
	 in thread
	 sum (thread-cost subthread))
      1))