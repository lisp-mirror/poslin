(in-package #:poslin)

(defparameter *prims*
  '())

(defprim ! t
  (push (callable->thread (pop-curr)
			  this)
	pc)
  (interpreter))

(defprim & t
  (push-curr (cons 'thread (callable->thread (pop-curr)
					     this))))

(defprim [ t
  (push (make-stack)
	path))

(defprim ] t
  (if (cdr path)
      (push-curr (pop path))
      (error "Tried to close root stack")))

(defprim << nil
  (push (pop-curr)
	out))

(defprim || nil)

(defprim % nil
  (let ((curr (pop-curr)))
    (if (stack-p curr)
	(push curr path)
	(error "Tried to open ~A"
	       curr))))

(defprim @ nil
  (let ((val (pop-curr)))
    (setf (gethash (pop-curr)
		   (curr-dict))
	  (if (and (consp val)
		   (symbol= (car val)
			    'thread))
	      (cdr val)
	      val))))

(defprim _@ nil
  (remhash (pop-curr)
	   (curr-dict)))

(defprim <> nil
  (rotatef (car (curr-stack))
	   (cadr (curr-stack))))

(defprim -> nil
  (push-curr (pop (stack-content (pop-curr)))))

(defprim <- nil
  (push (pop-curr)
	(stack-content (car (curr-stack)))))

(defprim _ nil
  (pop-curr))

(defprim ^ nil
  (aif (nth (pop-curr)
	    path)
       (push-curr it)
       (error "Tried to access stack above root")))

(defprim @! t
  (setf (gethash (pop-curr)
		 (curr-imm))
	t))

(defprim @~ t
  (setf (gethash (pop-curr)
		 (curr-imm))
	nil))

(defprim @? t
  (remhash (pop-curr)
	   (curr-imm)))

(defprim $ nil
  (push-curr (car (curr-stack))))

(defprim >> nil
  (let ((eof (gensym "eof")))
    (with-open-file (stream (pop-curr))
      (do ((curr (read stream nil eof)))
	  ((eq curr eof))
	(funcall this curr)))))