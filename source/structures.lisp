(in-package #:poslin)

(defstruct stacktree
  name stack children dict)

(defstruct word
  name stack thread prev)