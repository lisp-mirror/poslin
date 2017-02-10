(in-package #:poslin)

(deftype [thread] ()
  `(or (eql <noop>)
       <prim> <constant> <thread> <handled>))

(defparameter <noop>
  '<noop>)

(defstruct <prim>
  (fun #'identity
       :type function)
  (name ""
	:type string))

(defun <prim> (f n)
  #.+optimization-parameters+
  (declare (type function f)
           (type string n))
  (the <prim>
    (make-<prim> :fun f
                 :name n)))

(defstruct <constant>
  (val <meta-nothing>
       :type t))

(defun <constant> (v)
  #.+optimization-parameters+
  (the <constant>
    (make-<constant> :val v)))

(defstruct <thread>
  (front <noop>
	 :type [thread])
  (back <noop>
	:type [thread]))

(defun <thread> (f b)
  #.+optimization-parameters+
  (declare (type [thread] f b))
  (the <thread>
    (make-<thread> :front f
                   :back b)))

(defstruct <handled>
  (thread <noop>
          :type (or (eql <noop>)
                    <prim> <thread> <handled>))
  (handle <noop>
          :type [thread]))

(defun <handled> (th h)
  #.+optimization-parameters+
  (declare (type (or (eql <noop>)
                     <prim> <thread> <handled>)
                 th)
           (type [thread] h))
  (make-<handled> :thread th
                  :handle h))

(defun elementary? (th)
  #.+optimization-parameters+
  (declare (type [thread] th))
  (or (eq th <noop>)
      (typep th '(or <prim> <constant>))
      (and (typep th '<handled>)
           (elementary? (<handled>-thread th)))))

(defun complex-thread? (th)
  #.+optimization-parameters+
  (declare (type [thread] th))
  (or (typep th '<thread>)
      (and (typep th '<handled>)
           (complex-thread? (<handled>-thread th)))))

(deftype thread ()
  '(or <prim> <constant> <thread> <handled> (eql <noop>)))

(defgeneric thread-front (thread)
  (:method ((thread (eql <noop>)))
    #.+optimization-parameters+
    <noop>)
  (:method ((thread <prim>))
    #.+optimization-parameters+
    (the <prim> thread))
  (:method ((thread <constant>))
    #.+optimization-parameters+
    (the <constant> thread))
  (:method ((thread <thread>))
    #.+optimization-parameters+
    (the [thread] (<thread>-front thread)))
  (:method ((thread <handled>))
    #.+optimization-parameters+
    (the [thread] (thread-front (<handled>-thread thread)))))

(defgeneric thread-back (thread)
  (:method ((thread (eql <noop>)))
    #.+optimization-parameters+
    <noop>)
  (:method ((thread <prim>))
    #.+optimization-parameters+
    <noop>)
  (:method ((thread <constant>))
    #.+optimization-parameters+
    <noop>)
  (:method ((thread <thread>))
    #.+optimization-parameters+
    (the [thread] (<thread>-back thread)))
  (:method ((thread <handled>))
    #.+optimization-parameters+
    (the <handled>
      (<handled> (thread-back (<handled>-thread thread))
                 (<handled>-handle thread)))))

(defmacro defnprim (standard name immediate? docstring &body body)
  `(push '(,name ,immediate? ,docstring
           (block op
             ,@body))
	 ,standard))

(defmacro defprim (standard name immediate? docstring &body body)
  `(defnprim ,standard ,name ,immediate? ,docstring
     ,@body
     (setf pc (thread-back pc))))

(defun thread<-stack (stack)
  #.+optimization-parameters+
  (declare (type (or cons null)
                 stack))
  (the [thread]
    (labels ((_rec (front back)
               #.+optimization-parameters+
               (declare (type (or cons null)
                              back))
               (the [thread]
                 (if back
                     (<thread> (if (typep front '[thread])
                                   front
                                   (<constant> front))
                               (_rec (first back)
                                     (rest back)))
                     (if (typep front '[thread])
                         front
                         (<constant> front))))))
      (if stack
          (let ((stack (reverse stack)))
            (declare (type (or cons null)
                           stack))
            (_rec (first stack)
                  (rest stack)))
          <noop>))))
