;;;; poslin.asd

(asdf:defsystem #:poslin
  :description "Describe poslin here"
  :author "Thomas Bartscher <thomas.bartscher@gmail.com>"
  :license "EUPL V1.1"
  :depends-on ("alexandria")
  :components
  ((:file "package")
   (:module
    "utility"
    :depends-on ("package")
    :components
    ((:file "symb")
     (:file "sharp-backquote"
	    :depends-on ("symb"))
     (:file "defmacro!help")
     (:file "defmacro!"
	    :depends-on ("symb" "defmacro!help" "sharp-backquote"))
     (:file "anaphora")
     (:file "dlambda"
	    :depends-on ("defmacro!" "sharp-backquote"))
     (:file "plambda-help"
	    :depends-on ("sharp-backquote"))
     (:file "plambda"
	    :depends-on ("plambda-help" "dlambda"))
     (:file "symbol=")
     (:file "if-not")
     ))
   (:module
    "source"
    :depends-on ("package" "utility")
    :components
    ((:file "registers")
     (:file "thread")
     (:module
      "env"
      :components
      ((:file "binding")
       (:file "op-env"
	      :depends-on ("binding"))
       (:file "var-env"
	      :depends-on ("binding"))
       ))
     (:file "pstack"
	    :depends-on ("env"))
     (:file "run-poslin")
     (:module
      "print"
      :components
      ((:file "print")
       (:file "error")
       ))
     (:module
      "load"
      :components
      ((:file "read")
       (:file "interpreter")
       (:file "eval"
	      :depends-on ("interpreter"))
       (:file "load"
	      :depends-on ("read" "eval"))
       ))
     (:module
      "poslin-env"
      :depends-on ("registers" "thread" "pstack" "print" "load")
      :components
      ((:file "handle")
       (:file "install")
       (:file "poslin")
       ))
     ))
   (:module
    "prims"
    :depends-on ("package" "source")
    :components
    (
     ))
   (:module
    "startup"
    :depends-on ("package" "source")
    :components
    (
     ))
   (:module
    "repl"
    :depends-on ("package" "source" "prims" "startup")
    :components
    (
     ))
   ))
