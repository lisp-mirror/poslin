(in-package #:poslin)

(defparameter *prims* '())

(defprim ! t
  (push (callable->thread (pop-curr)
			  this)
	pc)
  (interpreter))

(defprim & t
  (push-curr (callable->thread (pop-curr)
			       this)))

(defprim @ nil
  (let ((thread (callable->thread (pop-curr)
				  this)))
    (setf (lookup-op (pop-curr))
	  thread)))

(defprim @_ nil
  (remhash (pop-curr)
	   (curr-opdefs)))

(defprim [ t
  (push (make-stack :vars (make-var-env :par (curr-var-env))
		    :ops (make-op-env :par (curr-op-env)))
	path))

(defprim ] t
  (if (cdr path)
      (push-curr (pop path))
      (error "Attempt to pop root stack")))

(defprim @! nil
  (setf (immediate? (pop-curr))
	t))

(defprim @~ nil
  (setf (immediate? (pop-curr))
	nil))

(defprim @? nil
  (remhash (pop-curr)
	   (curr-imm)))

(defprim § nil
  (let* ((var (pop-curr))
	 (val (lookup-var var)))
    (if (empty? val)
	(error "Variable ~A not set"
	       var)
	(push-curr val))))

(defprim §_ nil
  (remhash (pop-curr)
	   (curr-vardefs)))

(defprim [% t
  (let ((stack (pop-curr)))
    (if (stack-p stack)
	(push stack path)
	(error "Tried to enter ~A"
	       stack))))

(defprim <> nil
  (if (cdr (curr-stack))
      (rotatef (car (curr-stack))
	       (cadr (curr-stack)))
      (error "Tried to swap with bottom")))

(defprim -> nil
  (push-curr (pop (stack-content (pop-curr)))))

(defprim <- nil
  (push (pop-curr)
	(stack-content (car (curr-stack)))))

(defprim _ nil
  (pop-curr))

(defprim ^ nil
  (let ((n (pop-curr)))
    (aif (nth n path)
	 (push-curr it)
	 (error "Poslin path is smaller than ~A"
		n))))

(defprim $ nil
  (if (curr-stack)
      (push-curr (car (curr-stack)))
      (error "No item on stack to duplicate")))

(defprim >> nil
  (let ((eof (gensym "eof")))
    (with-open-file (stream (pop-curr))
      (do ((curr (read stream nil eof)))
	  ((eq curr eof))
	(funcall this curr)))))

(defprim << nil
  (push (pop-curr)
	out))

(defprim >r nil
  (push (pop-curr)
	rstack))

(defprim r> nil
  (push-curr (pop rstack)))