;;;; poslin.asd

(asdf:defsystem #:poslin
  :serial t
  :description "Describe poslin here"
  :author "Thomas Bartscher <thomas.bartscher@weltraumschlangen.de>"
  :license "EUPL V1.1"
  :depends-on ("split-sequence"
               "cl-ppcre"
	       "fset"
               "cl-quickcheck"
               )
  :components
  ((:file "package")
   (:module
    "utility"
    :depends-on ("package")
    :components
    ((:file "symb")
     (:file "flatten")
     (:file "sharp-backquote"
            :depends-on ("symb"
                         ))
     (:file "defmacro-help")
     (:file "defmacro"
            :depends-on ("symb"
                         "flatten"
                         "sharp-backquote"
                         "defmacro-help"
                         ))
     (:file "dlambda"
            :depends-on ("defmacro"
                         ))
     (:file "plambda-help"
            :depends-on ("sharp-backquote"
                         ))
     (:file "plambda"
            :depends-on ("dlambda"
                         "plambda-help"
                         ))
     (:file "anaphora")
     (:file "group"
            :depends-on ("anaphora"))
     ))
   (:module
    "source"
    :depends-on ("package"
                 "utility"
                 )
    :components
    ((:module
      "structures"
      :components
      ((:file "nothing")
       (:file "symbol")
       (:file "binding")
       (:file "environment"
              :depends-on ("binding"
                           ))
       (:file "thread")
       (:file "path"
              :depends-on ("environment"
                           "nothing"
                           ))
       (:file "bool")
       (:file "compare")
       (:file "quotation")
       ))
     (:module
      "interaction"
      :depends-on ("structures"
                   )
      :components
      ((:file "interpreter")
       (:file "new-poslin"
              :depends-on ("interpreter"
                           ))
       (:file "run-poslin"
              :depends-on ("new-poslin"
                           ))
       ))
     (:module
      "prims"
      :depends-on ("structures"
                   "interaction"
                   )
      :components
      ((:file "macros")
       (:file "prim"
              :depends-on ("macros"
                           ))
       ))
     (:module
      "repl"
      :depends-on ("structures"
                   "interaction"
                   "prims"
                   )
      :components
      ((:file "read")
       (:file "print")
       (:file "repl"
              :depends-on ("read"
                           "print"
                           ))
       ))
     ))
   ))
