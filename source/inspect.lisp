(in-package #:poslin)

(defun find-thread (word pos)
  (with-pandoric (path pc rstack ntable)
      pos
    (let ((thread (lookup-op word)))
      (format t "~A"
	      (formatt thread))
      thread)))

(defun prim-number (pos)
  (with-pandoric (ntable)
      pos
    (hash-table-count ntable)))

(defun op-number (pos)
  (with-pandoric (path)
      pos
    (hash-table-count (curr-opdefs))))

(defun imm-number (pos)
  (with-pandoric (path)
      pos
    (hash-table-count (curr-imm))))

(defun statistics (pos)
  (format t "~&Operators: ~D~%Primary operators: ~D~%Immediate ~
             operators: ~D~%"
	  (op-number pos)
	  (prim-number pos)
	  (imm-number pos)))

(defun prims (pos)
  (with-pandoric (ntable)
      pos
    (let ((prims '()))
      (maphash (lambda (k v)
		 (declare (ignore k))
		 (push v prims))
	       ntable)
      prims)))