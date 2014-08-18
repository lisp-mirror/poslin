;;;; package.lisp

(defpackage #:poslin
  (:use #:cl #:cl-adt #:equal #:split-sequence #:cl-ppcre
	)
  (:shadowing-import-from
   #:maybe
   #:[maybe] #:<just> #:<nothing> #:extract #:just-extract #:map
   )
  (:shadowing-import-from
   #:fset
   #:compare #:lookup #:@ #:with
   )
  )
