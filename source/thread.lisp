(in-package #:poslin)

(defstruct thread
  curr next)

(defmacro advance (place)
  `(setf ,place (thread-next ,place)))
