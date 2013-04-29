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
     ))
   (:module
    "source"
    :depends-on ("package" "utility")
    :components
    ((:file "vars")
     (:file "convenience")
     (:file "structure"
	    :depends-on ("convenience"))
     (:file "interpreter"
	    :depends-on ("convenience"))
     (:file "define"
	    :depends-on ("vars"))
     (:file "poslin-handle"
	    :depends-on ("convenience" "interpreter"))
     (:file "poslin")
     (:file "prepare-poslin"
	    :depends-on ("vars" "poslin"))
     (:file "new-poslin"
	    :depends-on ("vars" "structure" "prepare-poslin"))
     (:file "stack->thread"
	    :depends-on ("structure"))
     ))
   (:module
    "poslin"
    :depends-on ("package" "source")
    :components
    ((:file "prims")
     ))
   ))