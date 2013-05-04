(in-package #:poslin)

(let ((empty (gensym "empty")))
  (defun empty ()
    empty)

  (defun empty? (val)
    (eq val empty)))

(defun find-op (word op-env)
  (if op-env
      (multiple-value-bind (thread found?)
	  (gethash word (op-env-defs op-env))
	(if found?
	    thread
	    (find-op word (op-env-par op-env))))
      (empty)))

(defun find-var (word var-env)
  (if var-env
      (multiple-value-bind (val found?)
	  (gethash word (var-env-defs var-env))
	(if found?
	    val
	    (find-var word (var-env-par var-env))))
      (empty)))

(defun find-imm (word op-env)
  (if op-env
      (multiple-value-bind (imm? found?)
	  (gethash word (op-env-imm op-env))
	(if found?
	    imm?
	    (find-imm word (op-env-par op-env))))))

(defmacro lookup-op (word)
  `(find-op ,word (curr-op-env)))

(defmacro lookup-var (word)
  `(find-var ,word (curr-var-env)))

(defmacro immediate? (word)
  `(find-imm ,word (curr-op-env)))

(defmethod (setf find-op)
    (val word op-env)
  (setf (gethash word (op-env-defs op-env))
	val))

(defmethod (setf find-var)
    (val word var-env)
  (setf (gethash word (var-env-defs var-env))
	val))

(defmethod (setf find-imm)
    (val word op-env)
  (setf (gethash word (op-env-imm op-env))
	val))