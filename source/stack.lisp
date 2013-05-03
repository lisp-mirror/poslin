(in-package #:poslin)

(defstruct stack
  name content (dict (make-hash-table :test #'eq))
  (imm (make-hash-table :test #'eq)))