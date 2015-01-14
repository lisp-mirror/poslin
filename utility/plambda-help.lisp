(in-package #:poslin)

(defun pget (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
		  ,(car a1))
	       letargs)
     (t (error "Unknown pandoric get: ~A"
	       sym))))

(defun pset (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
		  (setq ,(car a1)
			val))
	       letargs)
     (t (error "Unknown pandoric set: ~A ~A"
	       sym val))))

(declaim (inline get-pandoric))
(defun get-pandoric (box sym)
  (declare (type function box)
           (type symbol sym))
  (funcall box
	   :pandoric-get sym))

(defsetf get-pandoric (box sym)
    (val)
  `(progn
     (funcall ,box
	      :pandoric-set ,sym ,val)
     ,val))
