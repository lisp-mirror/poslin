(load "./poslin.asd")

(ql:quickload "poslin")

(save-lisp-and-die (or (second sb-ext:*posix-argv*)
                       "poslin0")
                   :toplevel #'poslin::repl-dyn
                   :executable t
                   :purify t
                   :compression 9)
