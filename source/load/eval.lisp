(in-package #:poslin)

(defun pos-eval (thread poslin)
  (with-pandoric (pc rstack path ntable out folder)
      poslin
    (setf pc thread)
    (interpreter)))

(defmacro poslin-eval (thread)
  `(progn
     (setf pc ,thread)
     (interpreter)))
