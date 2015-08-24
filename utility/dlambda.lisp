(in-package #:poslin)

(defmacro! dlambda (&body ds)
  `(the function
     (lambda (&rest ,g!args)
       (case (car ,g!args)
         ,@(mapcar #`(,(if (eq t (car a1))
                           t
                           (list (car a1)))
                       (apply (lambda ,@(cdr a1))
                              ,(if (eq t (car a1))
                                   g!args
                                   `(cdr ,g!args))))
                   ds)))))
