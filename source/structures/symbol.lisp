(in-package #:poslin)

(defun poslin-symbol? (obj)
  #.+optimization-parameters+
  (the boolean
    (and (symbolp obj)
         (or (keywordp obj)
             (not (symbol-package obj))))))
