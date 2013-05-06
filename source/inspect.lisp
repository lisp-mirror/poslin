(in-package #:poslin)

(defun find-thread (word pos)
  (with-pandoric (path)
      pos
    (lookup-op word)))