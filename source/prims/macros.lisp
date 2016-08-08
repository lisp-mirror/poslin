(in-package #:poslin)

(defparameter *cons-pool*
  nil)
(defparameter *cons-count*
  0)
(defparameter *cons-limit*
  100)

#-nil
(defmacro push-stack (value)
  `(if *cons-pool*
       (setf (stack path)
             (let ((cell *cons-pool*))
               (setf *cons-pool* (cdr *cons-pool*)
                     (car cell)
                     ,value
                     (cdr cell)
                     (stack path))))
       (push ,value (stack path))))
#+nil
(defmacro push-stack (value)
  `(push ,value (stack path)))

#-nil
(defmacro pop-stack ()
  `(if (stack path)
       (if (< *cons-count* *cons-limit*)
           (let* ((stack (stack path))
                  (new-stack (cdr stack))
                  (item (car stack)))
             (setf (car stack)
                   nil
                   (cdr stack)
                   *cons-pool*
                   *cons-pool* stack
                   (stack path)
                   new-stack)
             item)
           (pop (stack path)))
       (unwind "Bottom of Stack popped"
               :stack-bottom-error)))
#+nil
(defmacro pop-stack ()
  `(pop (stack path)))

(defmacro stack-call (fn)
  `(push-stack (,fn (pop-stack))))

#+nil
(defmacro stack-args ((&rest args)
		      &body body)
  `(let (,@(mapcar #`(,a1 (pop-stack))
		   (reverse args)))
     ,@body))

(defmacro stack-args ((&rest args)
                      &body body)
  `(let (,@(mapcar #`(,(if (consp a1)
                           (car a1)
                           a1)
                       (pop-stack))
                   (reverse args)))
     (if (not (and ,@(mapcar #`(typep ,(first a1)
                                      ',(second a1))
                             (remove-if-not #'consp
                                            (reverse args)))))
         (unwind "Type error"
                 (list :type-error ,@(mapcar #`,(if (consp a1)
                                                    (car a1)
                                                    a1)
                                             args)))
         (progn
           ,@body))))
