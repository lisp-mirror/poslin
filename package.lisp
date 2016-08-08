;;;; package.lisp

(defpackage #:poslin
  (:use #:cl #:split-sequence #:cl-ppcre
	)
  (:shadowing-import-from
   #:fset
   #:compare #:lookup #:@ #:with #:less #:map #:set #:empty-map #:empty-set
   #:domain
   )
  (:shadowing-import-from
   #:cl-quickcheck
   #:quickcheck #:for-all
   #:is
   #:an-index #:a-list #:a-char
   )
  )
