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
         (let ((,g!message ([exception]-string ,g!exception))
               (,g!data ([exception]-data ,g!exception)))
           (setf pc <noop>)
           (loop
              (if (rstack)
                  (let ((,g!ex (pop (stack path)))
                        (,g!el (pop-rstack)))
                    (when (typep ,g!el '<handled>)
                      (push-rstack (<handled>-handle ,g!el))
                      (setf pc <noop>)
                      (return-from ,g!throw))
                    (push-stack
                     (make-[exception] :string ,g!message
                                       :data ,g!data
                                       :stack (cons ,g!el
                                                    ([exception]-stack ,g!ex)))))
                  (signal 'unhandled-poslin-exception)))))))

(defmacro! unwind (o!message o!data)
  `(pthrow (<exception> ,g!message ,g!data (list pc))))
