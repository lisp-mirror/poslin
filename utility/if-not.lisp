(in-package #:poslin)

(defmacro! if-not (o!expr &body else)
  `(if ,g!expr
       ,g!expr
       (progn ,@else)))