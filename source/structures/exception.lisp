(in-package #:poslin)

(defstruct [exception]
  (string ""
          :type string)
  (data <meta-nothing>)
  (stack '()
         :type (or cons null)))

(defun <exception> (string data pc)
  (make-[exception] :string string
                    :data data
                    :stack (list pc)))

(define-condition unhandled-poslin-exception ()
  ())

(defmacro! unwind (o!string o!data)
  `(progn
     (push-stack (<exception> ,g!string ,g!data pc))
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
                       (make-[exception] :string ,g!string
                                         :data ,g!data
                                         :stack (cons el
                                                      ([exception]-stack ex))))
                      (when (typep el '<handled>)
                        (push (<handled>-handle el)
                              rstack)
                        (setf pc <noop>)
                        (return)))
                    (signal 'unhandled-poslin-exception)))))))
