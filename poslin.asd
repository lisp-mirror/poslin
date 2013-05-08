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
     (:file "stack")
     (:file "error"
	    :depends-on ("format-thread"))
     (:file "run-poslin")
     (:file "conv"
	    :depends-on ("stack" "error"))
     (:file "word"
	    :depends-on ("stack" "conv"))
     (:file "interpreter"
	    :depends-on ("conv"))
     (:file "prepare"
	    :depends-on ("conv" "stack" "run-poslin"))
     (:file "poslin-env"
	    :depends-on ("registers" "conv" "word" "interpreter"
				     "prepare"))
     (:file "to-thread"
	    :depends-on ("stack" "error" "word"))
     (:file "inspect")
     ))
   (:module
    "poslin"
    :depends-on ("package" "source")
    :components
    ((:file "prims")
     (:file "stdlib"
	    :depends-on ("prims"))
     ))
   (:module
    "repl"
    :depends-on ("package" "source" "poslin")
    :components
    ((:file "repl")
     ))
   ))