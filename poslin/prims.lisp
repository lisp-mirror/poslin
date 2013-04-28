(in-package #:poslin)

#.(setf *prims* nil)

(defprim ! t
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
		(if (eq (car to-call)
			'lisp)
		    (push (eval (cadr to-call))
			  (stacktree-stack (curr)))
		    (progn
		      (push to-call pc)
		      (interpreter))))))))

(defprim [ t
  (push (make-stacktree)
	path))

(defprim ] t
  (if (cdr path)
      (pop path)
      (error "Tried to close root stack!")))

(defprim } t
  (if (cdr path)
      (progn
	(push (curr)
	      (stacktree-stack (curr-par)))
	(pop path))
      (error "Tried to close root stack!")))

(defprim }! t
  (if (cdr path)
      (progn
	(push (butlast (cdr (stacktree->thread (curr)
					       this)))
	      (stacktree-stack (curr-par)))
	(pop path))
      (error "Tried to close root stack!")))

(defprim % nil
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

(defprim ~ nil
  (let ((sname (pop-curr)))
    (if (symbolp sname)
	(if (string= (symbol-name sname)
		     "^")
	    (push-curr (curr-par))
	    (let ((found (find-child sname (curr))))
	      (if found
		  (push-curr found)
		  (error "Stack ~A has no child ~A."
			 (stacktree-name (curr))
			 sname))))
	(error "Expected a symbol as stackname, got ~A"
	       sname))))

(defprim -> nil
  (push-curr (pop (stacktree-stack (pop-curr)))))

(defprim <- nil
  (push (pop-curr)
	(stacktree-stack (pop-curr))))

(defprim _ nil
  (pop-curr))

(defprim $ nil
  (push-curr (car (stacktree-stack (curr)))))

(defprim !+ nil
  (let ((stack (pop-curr)))
    (setf #1=(stacktree-dict (curr))
	  (make-word :name (stacktree-name stack)
		     :stack (stacktree-stack stack)
		     :thread (stacktree->thread stack this)
		     :prev #1#))))

(defprim !* nil
  (let ((stack (pop-curr)))
    (setf #1=(stacktree-dict (curr))
	  (make-word :name (stacktree-name stack)
		     :stack (stacktree-stack stack)
		     :thread (cdr (butlast (stacktree->thread stack
							      this)))
		     :prev #1#))))

(defprim <> nil
  (rotatef (car #1=(stacktree-stack (curr)))
	   (cadr #1#)))

(defprim ^^ nil
  (let ((n (pop-curr)))
    (push-curr (nth n path))))

(defprim ~> nil
  (push-curr (find-child (pop-curr)
			 (pop-curr))))

(defprim [_] t
  (setf #1=(stacktree-children (curr))
	(remove (pop-curr)
		#1#
		:test #'eq
		:key #'stacktree-name)))

(defprim [%] nil
  (push-curr (stacktree-name (curr))))

(defprim [~] nil
  (push-curr (curr)))

(defprim [!] nil
  (setf (word-immediate (lookup (pop-curr)
				path))
	t))

(defprim [?] nil
  (setf (word-immediate (lookup (pop-curr)
				path))
	nil))

(defprim & nil
  (let ((found (lookup (pop-curr)
		       path)))
    (if found
	(push-curr (word-thread found))
	(error "No such word"))))

(defprim >r nil
  (push (pop-curr)
	rstack))

(defprim r> nil
  (push-curr (pop rstack)))

(defprim print nil
  (format (pop-curr)
	  "~A"
	  (pop-curr)))

(defprim if nil
  (let ((else (pop-curr))
	(then (pop-curr)))
    (if (pop-curr)
	(push-curr then)
	(push-curr else))))

(def-n-ary 1
  car cdr first rest last abs exp not)

(def-n-ary 2
  + - * / mod expt log cons append > < >= <= = /= eq equal)

(defprim and nil
  (let ((val (pop-curr)))
    (and (pop-curr)
	 val)))

(defprim or nil
  (let ((val (pop-curr)))
    (or (pop-curr)
	val)))