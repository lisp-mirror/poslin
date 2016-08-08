(in-package #:poslin)

(defparameter <noop>
  '<noop>)

(defstruct <prim>
  (fun #'identity
       :type function)
  (name ""
	:type string))

(defun <prim> (f n)
  (make-<prim> :fun f
	       :name n))

(defstruct <constant>
  (val <meta-nothing>
       :type t))

(defun <constant> (v)
  (make-<constant> :val v))

(defstruct <thread>
  (front <noop>
	 :type (or (eql <noop>)
		   <prim> <constant> <thread> <handled>))
  (back <noop>
	:type (or (eql <noop>)
		  <prim> <constant> <thread> <handled>)))

(defun <thread> (f b)
  (make-<thread> :front f
		 :back b))

(defstruct <handled>
  (thread <noop>
          :type (or (eql <noop>)
                    <prim> <thread> <handled>))
  (handle <noop>
          :type (or (eql <noop>)
                    <prim> <constant> <thread> <handled>)))

(defun <handled> (th h)
  (make-<handled> :thread th
                  :handle h))

(defun elementary? (th)
  (or (eq th <noop>)
      (typep th '(or <prim> <constant>))
      (and (typep th '<handled>)
           (elementary? (<handled>-thread th)))))

(defun complex-thread? (th)
  (or (typep th '<thread>)
      (and (typep th '<handled>)
           (complex-thread? (<handled>-thread th)))))

(deftype thread ()
  '(or <prim> <constant> <thread> <handled> (eql <noop>)))

(defgeneric thread-front (thread)
  (:method ((thread (eql <noop>)))
    <noop>)
  (:method ((thread <prim>))
    thread)
  (:method ((thread <constant>))
    thread)
  (:method ((thread <thread>))
    (<thread>-front thread))
  (:method ((thread <handled>))
    (thread-front (<handled>-thread thread))))

(defgeneric thread-back (thread)
  (:method ((thread (eql <noop>)))
    <noop>)
  (:method ((thread <prim>))
    <noop>)
  (:method ((thread <constant>))
    <noop>)
  (:method ((thread <thread>))
    (<thread>-back thread))
  (:method ((thread <handled>))
    (<handled> (thread-back (<handled>-thread thread))
               (<handled>-handle thread))))

(defmacro defnprim (standard name immediate? docstring &body body)
  `(push '(,name ,immediate? ,docstring
           (block op
             ,@body))
	 ,standard))

(defmacro defprim (standard name immediate? docstring &body body)
  `(defnprim ,standard ,name ,immediate? ,docstring
     ,@body
     (setf pc (thread-back pc))))

(deftype [thread] ()
  `(or (eql <noop>)
       <prim> <constant> <thread> <handled>))

(defun thread<-stack (stack)
  (labels ((_rec (front back)
	     (if back
		 (<thread> (if (typep front '[thread])
			       front
			       (<constant> front))
			   (_rec (first back)
				 (rest back)))
		 (if (typep front '[thread])
		     front
		     (<constant> front)))))
    (if stack
	(let ((stack (reverse stack)))
	  (_rec (first stack)
		(rest stack)))
	<noop>)))
