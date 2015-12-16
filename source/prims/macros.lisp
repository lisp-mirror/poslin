(in-package #:poslin)

(defmacro push-stack (value)
  `(push ,value (stack path)))

(defmacro pop-stack ()
  `(if (stack path)
       (pop (stack path))
       (unwind "Bottom of Stack popped"
               :stack-bottom-error)))

(defmacro! op-fail (o!message o!data)
  `(progn
     (unwind ,g!message ,g!data)
     (return-from op)))

(defmacro arg-pop ()
  `(if (stack path)
       (pop (stack path))
       (op-fail "Bottom of Stack popped"
                :stack-bottom-error)))

(defmacro stack-call (fn)
  `(push-stack (,fn (arg-pop))))

(defmacro stack-args ((&rest args)
                      &body body)
  `(let (,@(mapcar #`(,(if (consp a1)
                           (car a1)
                           a1)
                       (arg-pop))
                   (reverse args)))
     (if (not (and ,@(mapcar #`(typep ,(first a1)
                                      ',(second a1))
                             (remove-if-not #'consp
                                            (reverse args)))))
         (unwind "Type error"
                 (list :type-error
                       ,@(mapcar #`,(if (consp a1)
                                        (car a1)
                                        a1)
                                 args)))
         (progn
           ,@body))))
