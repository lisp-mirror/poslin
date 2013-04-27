(in-package #:poslin)

#.(setf *prims* nil)
#.(setf *imm-prims* nil)

(defprim ||)

(defimmprim !
  (let ((to-call (pop-curr)))
    (etypecase to-call
      (symbol (push (word-thread (lookup to-call path))
		    pc)
	      (interpreter))
      (function (push to-call pc)
		(interpreter))
      (stacktree (push (stacktree->thread to-call this)
		       pc)
		 (interpreter))
      (cons (if (eq (car to-call)
		    'quote)
		(push (cadr to-call)
		      (stacktree-stack (curr)))
		(push (eval to-call)
		      (stacktree-stack (curr))))))))

(defimmprim [
  (push (make-stacktree)
	path))

(defimmprim ]
  (pop path))

(defprim %
  (let ((sname (pop-curr)))
    (if (symbolp sname)
	(if (string= (symbol-name sname)
		     "^")
	    (push (curr-par)
		  path)
	    (let ((found (find-child sname (curr-par))))
	      (if found
		  (progn
		    (setf (stacktree-stack found)
			  (append (stacktree-stack (curr))
				  (stacktree-stack found)))
		    (setf path (cons found (cdr path))))
		  (progn
		    (setf (stacktree-name (curr))
			  sname)
		    (push (curr)
			  (stacktree-children (curr-par)))))))
	(error "Expected a symbol as stackname, got ~A"
	       sname))))

(defprim ~
  (let ((sname (pop-curr)))
    (if (symbolp sname)
	(let ((found (find-child sname (curr))))
	  (if found
	      (push-curr found)
	      (error "Stack ~A has no child ~A."
		     (stacktree-name (curr))
		     sname)))
	(error "Expected a symbol as stackname, got ~A"
	       sname))))

(defprim ->
  (push-curr (pop (stacktree-stack (pop-curr)))))

(defprim <-
  (push (pop-curr)
	(stacktree-stack (pop-curr))))

(defprim _
  (pop-curr))

(defprim !+
  (let ((stack (pop-curr)))
    (setf #1=(stacktree-dict (curr))
	  (make-word :name (stacktree-name stack)
		     :stack (stacktree-stack stack)
		     :thread (stacktree->thread stack this)
		     :prev #1#))))

(defprim !*
  (let ((stack (pop-curr)))
    (setf #1=(stacktree-dict (curr))
	  (make-word :name (stacktree-name stack)
		     :stack (stacktree-stack stack)
		     :thread (cdr (butlast (stacktree->thread stack
							      this)))
		     :prev #1#))))

(defprim <>
  (rotatef (car #1=(stacktree-stack (curr)))
	   (cadr #1#)))

(defprim ^^
  (let ((n (pop-curr)))
    (push-curr (nth n path))))

(defprim ~>
  (push-curr (find-child (pop-curr)
			 (pop-curr))))

(defimmprim [!]
  (setf #1=(stacktree-children (curr))
	(remove (pop-curr)
		#1#
		:test #'eq
		:key #'stacktree-name)))

(def-n-ary 1
  car cdr first rest last abs exp print)

(def-n-ary 2
  + - * / mod expt log cons)