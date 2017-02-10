;;;; package.lisp

(defpackage #:poslin
  (:use #:cl #:split-sequence #:cl-ppcre
	)
  (:shadowing-import-from
   #:fset
   #:compare #:lookup #:@ #:with #:less #:map #:set #:empty-map #:empty-set
   #:domain #:arb
   )
  (:shadowing-import-from
   #:cl-quickcheck
   #:quickcheck #:for-all
   #:is
   #:an-index #:a-list #:a-char
   )
  )

(in-package #:poslin)

(defparameter +optimization-parameters+
  `(declare (optimize (speed 3)
                      (space 2)
                      (safety 0)
                      (debug 0)
                      (compilation-speed 0))))
