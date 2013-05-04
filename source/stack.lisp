(in-package #:poslin)

(defstruct var-env
  par (defs (make-hash-table :test #'eq)))

(defstruct op-env
  par (defs (make-hash-table :test #'eq))
  (imm (make-hash-table :test #'eq)))

(defstruct stack
  name content (vars (make-var-env))
  (ops (make-op-env)))