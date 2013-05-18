(in-package #:poslin)

(defun list->thread (list poslin)
  (with-pandoric (path)
      poslin
    (let ((thread))
      (loop for obj
	 in list
	 do (setf thread
		  (make-thread
		   :curr
		   (if (and (symbolp obj)
			    (op-env-imm (pstack-op-env
					 (car path))
					obj))
		       (if-not (binding-val (op-env-def
					     (pstack-op-env
					      (car path))
					     obj))
			 (poserror poslin 'undefined-operation
				   "No operation ~A"
				   obj))
		       obj)
		   :next thread)))
      thread)))

(defun stack->thread (pstack poslin)
  (list->thread (pstack-content pstack)
		poslin))
