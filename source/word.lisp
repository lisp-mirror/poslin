(in-package #:poslin)

(defun find-op (word op-env)
  (if op-env
      (let ((found (gethash word (op-env-defs op-env))))
	(if found
	    found
	    (find-op word (op-env-par op-env))))))

(defun find-var (word var-env)
  (if var-env
      (let ((found (gethash word (var-env-defs var-env))))
	(if found
	    found
	    (find-var word (var-env-par var-env))))))

(defun find-imm (word op-env)
  (if op-env
      (multiple-value-bind (imm? found?)
	  (gethash word (op-env-imm op-env))
	(if found?
	    imm?
	    (find-imm word (op-env-par op-env))))))

(defmacro lookup-opb (word)
  `(find-op ,word (curr-op-env)))

(defmacro! lookup-op (o!word)
  `(let ((found (lookup-opb ,g!word)))
     (if found
	 (binding-val found)
	 (perror undefined-operation
		 "No operation ~A"
		 (formatt ,g!word)))))

(defmacro lookup-varb (word)
  `(find-var ,word (curr-var-env)))

(defmacro! lookup-var (o!word)
  `(let ((found (lookup-varb ,g!word)))
     (if found
	 (binding-val found)
	 (perror unbound-var
		 "Variable ~A is unbound"
		 (formatt ,g!word)))))

(defmacro immediate? (word)
  `(find-imm ,word (curr-op-env)))

(defmethod (setf find-op)
    (binding word op-env)
  (setf (gethash word (op-env-defs op-env))
	binding))

(defmethod (setf find-var)
    (binding word var-env)
  (setf (gethash word (var-env-defs var-env))
	binding))

(defmethod (setf find-imm)
    (val word op-env)
  (setf (gethash word (op-env-imm op-env))
	val))