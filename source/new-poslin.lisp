(in-package #:poslin)

(defmacro curr ()
  `(car path))

(defmacro curr-par ()
  `(cadr path))

(defmacro pop-curr ()
  `(pop (stacktree-stack (curr))))

(defmacro push-curr (val)
  `(push ,val (stacktree-stack (curr))))

(defmacro pop-par ()
  `(pop (stacktree-stack (curr-par))))

(defmacro push-par (val)
  `(push ,val (stacktree-stack (curr-par))))

(defmacro new-poslin ()
  `(alet ,+registers+
     (setf dtable (make-hash-table :test #'eq))
     (setf path (list (make-stacktree :name 'root)))
     (install-prims)
     (dolist (v *stdlib*)
       (funcall this v))
     (plambda (v)
	 ,+registers+
       (poslin-handle)
       (values (curr)
	       (car (last path))))))

(defmacro poslin-handle ()
  `(cond
     ((and (symbolp v)
	   (let ((found (lookup v path)))
	     (if found
		 (word-immediate found))))
      (call v))
     ((consp v)
      (if (eq (car v)
	      'quote)
	  (push-curr (cadr v))
	  (if (eq (car v)
		  'lisp)
	      (push-curr (eval (cadr v)))
	      (progn
		(push v pc)
		(interpreter)))))
     (t (push-curr v))))

(defmacro! call (o!callable)
  `(etypecase ,g!callable
     (function
      (push-curr (funcall ,g!callable)))
     (symbol
      (let ((found (lookup ,g!callable path)))
	(if found
	    (progn
	      (push (word-thread found)
		    pc)
	      (interpreter))
	    (error "Tried to call undefined word ~A"
		   ,g!callable))))
     (cons
      (if (eq (car ,g!callable)
	      'quote)
	  (push-curr (cadr ,g!callable))
	  (if (eq (car ,g!callable)
		  'lisp)
	      (push-curr (eval ,g!callable))
	      (progn
		(push ,g!callable pc)
		(interpreter)))))
     (stacktree
      (push (stacktree->thread ,g!callable this)
	    pc)
      (interpreter))))

(defmacro install-prims ()
  `(progn
     ,@(mapcar #`(let ((thread (lambda ()
				 ,@(cddr a1))))
		   (setf
		    (stacktree-dict (curr))
		    (make-word :name ',(car a1)
			       :stack `(,(lambda ()
						 ,@(cddr a1)))
			       :thread thread
			       :prev (stacktree-dict (curr))
			       :immediate ,(cadr a1)))
		   (setf (gethash thread dtable)
			 ',(car a1)))
	       *prims*)))

(defun stacktree->thread (stacktree poslin-env)
  (with-pandoric (pc path rstack)
      poslin-env
    (append
     (list (lambda ()
	     (push (make-stacktree)
		   path)
	     (setf pc (cdr pc))))
     (let ((cut '()))
       (labels ((cut-out (l n acc)
		  (if l
		      (if (member n cut
				  :test #'=)
			  (cut-out (cdr l)
				   (1+ n)
				   acc)
			  (cut-out (cdr l)
				   (1+ n)
				   (cons (car l)
					 acc)))
		      acc)))
	 (cut-out
	  (loop for v
	     in (stacktree-stack stacktree)
	     for x from 1
	     collect
	       (cond
		 ((and (symbolp v)
		       (string= (symbol-name v)
				"!"))
		  (let ((callable (nth x (stacktree-stack
					  stacktree))))
		    (if (and (symbolp callable)
			     (string= (symbol-name callable)
				      "!"))
			(word-thread (lookup '! path))
			(progn
			  (push x cut)
			  (callable->thread callable poslin-env)))))
		 ((and (symbolp v)
		       (let ((found (lookup v path)))
			 (and found (word-immediate found))))
		  (word-thread (lookup v path)))
		 ((consp v)
		  (if (eq (car v)
			  'quote)
		      (cadr v)
		      (if (eq (car v)
			      'lisp)
			  (lambda ()
			    (push-curr (eval v)))
			  (lambda ()
			    (push v pc)))))
		 ((functionp v)
		  (lambda ()
		    (push-curr (funcall v))))
		 (t v)))
	  0 '())))
     (list (lambda ()
	     (pop path)
	     (setf pc (cdr pc)))))))

(defun callable->thread (callable poslin-env)
  (with-pandoric (path)
      poslin-env
    (typecase callable
      (function callable)
      (symbol (let ((found (lookup callable path)))
		(if found
		    (word-thread found)
		    (error "Tried to call undefined word ~A"
			   callable))))
      (stacktree (stacktree->thread callable poslin-env))
      (cons callable))))