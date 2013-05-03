(in-package #:poslin)

(defmacro curr ()
  `(car path))

(defmacro curr-stack ()
  `(stack-content (curr)))

(defmacro pop-curr ()
  `(pop (curr-stack)))

(defmacro push-curr (val)
  `(push ,val (curr-stack)))

(defmacro curr-dict ()
  `(stack-dict (curr)))

(defmacro curr-imm ()
  `(stack-imm (curr)))

(defmacro curr-name ()
  `(stack-name (curr)))

(defmacro par ()
  `(cadr path))

(defmacro par-stack ()
  `(stack-content (par)))

(defmacro pop-par ()
  `(pop (par-stack)))

(defmacro push-par (val)
  `(push ,val (par-stack)))

(defmacro par-dict ()
  `(stack-dict (par)))

(defmacro par-imm ()
  `(stack-imm (par)))

(defmacro par-name ()
  `(stack-name (par)))

(defun symbol= (s1 s2)
  (string= (symbol-name s1)
	   (symbol-name s2)))