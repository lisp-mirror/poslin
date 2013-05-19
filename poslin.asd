;;;; poslin.asd

(asdf:defsystem #:poslin
  :description "Describe poslin here"
  :author "Thomas Bartscher <thomas.bartscher@gmail.com>"
  :license "EUPL V1.1"
  :version "0.1"
  :depends-on ("alexandria" "split-sequence")
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
     (:file "group")
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
     (:file "defprim")
     (:module
      "to-thread"
      :components
      ((:file "stack")
       (:file "callable"
	      :depends-on ("stack"))
       ))
     ))
   (:module
    "startup"
    :depends-on ("package" "source")
    :components
    ((:file "prims")
     (:file "stdlib")
     ))
   (:module
    "prims"
    :depends-on ("package" "source" "startup")
    :components
    ((:file "nop")
     (:file "call")
     (:file "binding")
     (:file "op-env")
     (:file "var-env")
     (:file "pstack")
     (:file "path")
     (:file "out")
     (:file "load")
     (:file "conditional")
     (:file "imported")
     (:file "rstack")
     (:file "folder")
     ))
   (:module
    "repl"
    :depends-on ("package" "source" "prims" "startup")
    :components
    ((:file "repl")
     ))
   ))
