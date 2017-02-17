(in-package #:poslin)

(defstruct [exception]
  (string ""
          :type string)
  (data <meta-nothing>)
  (stack '()
         :type (or cons null)))

(defun <exception> (string data stack)
  (declare (type string string)
           (type (or cons null)
                 stack))
  (the [exception]
    (make-[exception] :string string
                      :data data
                      :stack stack)))

(define-condition unhandled-poslin-exception ()
  ())

(defmacro! pthrow (o!exception)
  `(block ,g!throw
     (push-stack ,g!exception)
     (if (typep pc '<handled>)
         (progn
           (push-rstack (<handled>-handle pc))
           (setf pc <noop>))
         (progn
           (setf pc <noop>)
           (loop
              (if (rstack)
                  (let ((,g!el (pop-rstack)))
                    (when (typep ,g!el '<handled>)
                      (push-rstack (<handled>-handle ,g!el))
                      (setf pc <noop>)
                      (return-from ,g!throw)))
                  (signal 'unhandled-poslin-exception)))))))

(defmacro! unwind (o!message o!data)
  `(pthrow (<exception> ,g!message ,g!data (list* pc (rstack)))))
