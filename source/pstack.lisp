(in-package #:poslin)

(defstruct pstack
  name content (op-env (make-op-env))
  (var-env (make-var-env)))

(defmacro ppush (pstack val)
  `(push ,val (pstack-content ,pstack)))
