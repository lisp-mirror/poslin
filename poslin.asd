;;;; poslin.asd

(asdf:defsystem #:poslin
  :description "Describe poslin here"
  :author "Thomas Bartscher <thomas.bartscher@gmail.com>"
  :license "EUPL V1.1"
  :depends-on ("alexandria" "let-over-lambda")
  :components
  ((:file "package")
   (:module
    "source"
    :depends-on ("package")
    :components
    ((:file "vars")
     (:file "structures")
     (:file "word"
	    :depends-on ("vars" "structures"))
     (:file "interpreter")
     (:file "new-poslin"
	    :depends-on ("vars" "structures" "interpreter"))
     ))
   (:module
    "poslin"
    :depends-on ("package" "source")
    :components
    ((:file "prims")
     (:file "stdlib"
	    :depends-on ("prims"))
     (:file "poslin"
	    :depends-on ("prims" "stdlib"))
     ))
   ))