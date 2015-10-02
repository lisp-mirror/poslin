(in-package #:poslin)

(defstruct [exception]
  (string ""
          :type string)
  (data <meta-nothing>)
  (stack '()
         :type (or cons null)))

(defun <exception> (string data stack)
  (make-[exception] :string string
                    :data data
                    :stack stack))

(define-condition unhandled-poslin-exception ()
  ())

(defmacro! pthrow (o!exception)
  `(progn
     (push-stack ,g!exception)
     (if (typep pc '<handled>)
         (progn
           (push (<handled>-handle pc)
                 rstack)
           (setf pc <noop>))
         (progn
           (setf pc <noop>)
           (loop
                (if rstack
                    (let ((ex (pop (stack path)))
                          (el (pop rstack)))
                      (push-stack
                       (make-[exception] :string ([exception]-string ex)
                                         :data ([exception]-data ex)
                                         :stack (cons el
                                                      ([exception]-stack ex))))
                      (when (typep el '<handled>)
                        (push (<handled>-handle el)
                              rstack)
                        (setf pc <noop>)
                        (return)))
                    (signal 'unhandled-poslin-exception)))))))

(defmacro! unwind (o!message o!data)
  `(pthrow (<exception> ,g!message ,g!data (list pc))))
