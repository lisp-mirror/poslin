;;;; package.lisp

(defpackage #:poslin
  (:use #:cl #:split-sequence #:cl-ppcre
	)
  (:shadowing-import-from
   #:fset
   #:compare #:lookup #:@ #:with
   )
  )
