(in-package #:poslin)

(defun symbol= (s1 s2)
  (string= (symbol-name s1)
	   (symbol-name s2)))

(defmacro curr ()
  `(car path))

(defmacro curr-stack ()
  `(stack-content (curr)))

(defmacro curr-name ()
  `(stack-name (curr)))

(defmacro curr-op-env ()
  `(stack-ops (curr)))

(defmacro curr-var-env ()
  `(stack-vars (curr)))

(defmacro curr-opdefs ()
  `(op-env-defs (curr-op-env)))

(defmacro curr-imm ()
  `(op-env-imm (curr-op-env)))

(defmacro curr-vardefs ()
  `(var-env-defs (curr-var-env)))

(defmacro pop-curr ()
  `(pop (curr-stack)))

(defmacro push-curr (val)
  `(push ,val (curr-stack)))

(defmacro par ()
  `(cadr path))

(defmacro par-stack ()
  `(stack-content (par)))

(defmacro par-name ()
  `(stack-name (par)))

(defmacro par-op-env ()
  `(stack-ops (par)))

(defmacro par-var-env ()
  `(stack-vars (par)))

(defmacro par-opdefs ()
  `(op-env-defs (par-op-env)))

(defmacro par-imm ()
  `(op-env-imm (par-op-env)))

(defmacro par-vardefs ()
  `(var-env-defs (par-var-env)))

(defmacro pop-par ()
  `(pop (par-stack)))

(defmacro push-par (val)
  `(push ,val (par-stack)))