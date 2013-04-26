;;;; package.lisp

(defpackage #:poslin
  (:shadowing-import-from
   #:alexandria
   #:flatten)
  (:use #:cl #:alexandria #:let-over-lambda))

