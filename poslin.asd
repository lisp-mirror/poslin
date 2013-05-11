;;;; poslin.asd

(asdf:defsystem #:poslin
  :description "Describe poslin here"
  :author "Thomas Bartscher <thomas.bartscher@gmail.com>"
  :license "EUPL V1.1"
  :depends-on ("alexandria" "osicat")
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
     ))
   (:module
    "source"
    :depends-on ("package" "utility")
    :components
    ((:file "registers")
     (:file "prims")
     (:file "stdlib")
     (:file "format-thread")
     (:file "binding")
     (:file "env")
     (:file "stack"
	    :depends-on ("env"))
     (:file "error"
	    :depends-on ("format-thread"))
     (:file "run-poslin")
     (:file "conv"
	    :depends-on ("env" "stack" "error"))
     (:file "word"
	    :depends-on ("binding" "env" "conv"))
     (:file "interpreter"
	    :depends-on ("conv"))
     (:file "prepare"
	    :depends-on ("conv" "stack" "run-poslin"))
     (:file "poslin-env"
	    :depends-on ("registers" "conv" "word" "interpreter"
				     "prepare"))
     (:file "to-thread"
	    :depends-on ("stack" "error" "word"))
     ))
   (:module
    "prims"
    :depends-on ("package" "source")
    :components
    ((:file "call")
     (:file "nop")
     (:file "error")
     (:file "binding")
     (:file "stackname")
     (:file "op-env")
     (:file "var-env")
     (:file "path")
     (:file "stack-ops")
     (:file "in-out")
     (:file "low-level-stacks")
     (:file "conditionals")
     (:file "printing")
     (:file "imported")
     ))
   (:module
    "startup"
    :depends-on ("package" "source")
    :components
    ((:file "stdlib")
     ))
   (:module
    "repl"
    :depends-on ("package" "source" "startup")
    :components
    ((:file "repl")
     ))
   ))