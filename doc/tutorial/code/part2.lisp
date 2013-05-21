(defpackage #:poslin-tutorial
  (:use #:cl #:poslin)
  (:export #:repl2))

(in-package #:poslin-tutorial)

(defun repl2 ()
  (repl (poslin ("dummy"))))