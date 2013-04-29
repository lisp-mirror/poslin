(in-package #:poslin)

(defmacro curr ()
  `(car path))

(defmacro par ()
  `(cadr path))

(defmacro curr-stack ()
  `(stack-content (curr)))

(defmacro par-stack ()
  `(stack-content (par)))

(defmacro pop-curr ()
  `(pop (curr-stack)))

(defmacro pop-par ()
  `(pop (par-stack)))

(defmacro push-curr (val)
  `(push ,val (curr-stack)))

(defmacro push-par (val)
  `(push ,val (par-stack)))

(defmacro set-immediate (word &optional (val t))
  `(setf (gethash ',word (stack-imm (curr)))
	 ,val))