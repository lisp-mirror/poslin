(in-package #:poslin)

(defparameter *prim*
  '())

;;;; control
(defnprim *prim* "!" t
    "sets the program counter"
  (let ((op (pop-stack)))
    (setf pc (typecase op
	       (symbol
		([binding]-value (lookup (op-env path)
					 op)))
	       (null
		<noop>)
	       (cons
		(thread<-stack op))
	       ([thread]
		op)
	       (t
		(<constant> op))))))

(defprim *prim* "&" t
    "finds or converts to a thread"
  (let ((op (pop-stack)))
    (push-stack (typecase op
		  (symbol
		   ([binding]-value (lookup (op-env path)
					    op)))
		  (null
		   <noop>)
		  (cons
		   (thread<-stack op))
		  ([thread]
		   op)
		  (t
		   (<constant> op))))))

(defprim *prim* "#" t
    "makes a constant thread of value"
  (push-stack (let ((val (pop-stack)))
		(<constant> val))))

(defprim *prim* "t:" nil
    "pushes the front of a thread"
  (stack-call thread-front))

(defprim *prim* "t::" nil
    "pushes the back of a thread"
  (stack-call thread-back))

(defprim *prim* "<?>" nil
    "if"
  (stack-args (bool then else)
    (push-stack (if (eq bool <true>)
		    then
		    else))))

(defprim *prim* "r<-" nil
    "push onto return stack"
  (push (pop-stack)
	rstack))

(defprim *prim* "r->" nil
    "pop from return stack"
  (push-stack (pop rstack)))

;;;; path
(defprim *prim* "p->" nil
    "pops the top of the path"
  (let ((e (path-top path)))
    (setf path
	  (path-pop path))
    (push-stack e)))

(defprim *prim* "p<-" nil
    "pushes onto the path"
  (setf path
	(path-push path (pop-stack))))

(defprim *prim* "p:" nil
    "returns nth environment on path"
  (push-stack (path-nth path (pop-stack))))

(defprim *prim* "p!" nil
    "set current environment"
  (setf path
	(path-set path (pop-stack))))

;;;; environment
(defprim *prim* "e*" nil
    "returns a fresh environment"
  (push-stack (<root-env> (fset:empty-map))))

(defprim *prim* "e->" nil
    "environment lookup"
  (stack-args (e k)
    (push-stack (lookup e k))))

(defprim *prim* "e<-" nil
    "environment set"
  (stack-args (e k v)
    (push-stack (insert e k v))))

(defprim *prim* "e+>" nil
    "parent of environment"
  (push-stack (get-parent (pop-stack))))

(defprim *prim* "e<+" nil
    "set parent of environment"
  (stack-args (e p)
    (push-stack (set-parent e p))))

;;;; binding
(defprim *prim* "b*" nil
    "create fresh binding"
  (push-stack (binding <meta-nothing>)))

(defprim *prim* "b->" nil
    "read binding"
  (push-stack ([binding]-value (pop-stack))))

(defprim *prim* "b<-" nil
    "set binding"
  (stack-args (b v)
    (setf ([binding]-value b)
	  v)))

(defprim *prim* "b+>" nil
    "binding doc"
  (push-stack ([binding]-doc (pop-stack))))

(defprim *prim* "b<+" nil
    "set binding doc"
  (stack-args (b d)
    (setf ([binding]-doc b)
	  d)))

;;;; stack
(defprim *prim* "[]" nil
    "create a fresh stack"
  (push-stack '()))

(defprim *prim* "<-" nil
    "push"
  (stack-args (s v)
    (push-stack (cons v s))))

(defprim *prim* ":" nil
    "top"
  (push-stack (first (pop-stack))))

(defprim *prim* "_" nil
    "drop"
  (push-stack (rest (pop-stack))))

(defprim *prim* "<>" nil
    "swap"
  (stack-args (s)
    (push-stack (list* (second s)
		       (first s)
		       (cddr s)))))

;;;; nothing
(defprim *prim* ".." nil
    "returns the mate-nothing value"
  (push-stack <meta-nothing>))

;;;; boolean
(defprim *prim* "true" nil
    "returns true value"
  (push-stack <true>))

(defprim *prim* "false" nil
    "returns false value"
  (push-stack <false>))

(defprim *prim* "and" nil
    "and"
  (stack-args (b1 b2)
    (push-stack (match (b1 b2)
		    [bool]
		  ((<true> <true>)
		   <true>)
		  ((_ _)
		   <false>)))))

(defprim *prim* "or" nil
    "or"
  (stack-args (b1 b2)
    (push-stack (match (b1 b2)
		    [bool]
		  ((<true> _)
		   <true>)
		  ((_ <true>)
		   <true>)
		  ((_ _)
		   <false>)))))

(defprim *prim* "not" nil
    "not"
  (stack-args (b)
    (push-stack (match b
		    [bool]
		  (<true> <false>)
		  (<false> <true>)))))

;;;; comparison
(defprim *prim* "==" nil
    "identity comparison"
  (stack-args (x y)
    (push-stack (if (eq x y)
		    <true>
		    <false>))))

(defprim *prim* "=?" nil
    "comparison operator"
  (stack-args (x y)
    (push-stack (case (compare x y)
		  (:less <less>)
		  (:equal <equal>)
		  (:greater <greater>)
		  (:unequal <unequal>)))))

(defprim *prim* ".<" nil
    "returns value vor 'less'"
  (push-stack <less>))

(defprim *prim* ".>" nil
    "returns value vor 'greater'"
  (push-stack <greater>))

(defprim *prim* ".=" nil
    "returns value vor 'equal'"
  (push-stack <equal>))

(defprim *prim* "./=" nil
    "returns value vor 'unequal'"
  (push-stack <unequal>))

;;;; arithmetic
(defprim *prim* "+" nil
    "addition"
  (stack-args (x y)
    (push-stack (+ x y))))

(defprim *prim* "*" nil
    "multiplication"
  (stack-args (x y)
    (push-stack (* x y))))

(defprim *prim* ".-" nil
    "negation"
  (stack-call -))

(defprim *prim* "./" nil
    "reciprocal"
  (stack-call /))

(defprim *prim* "log" nil
    "logarithm"
  (stack-args (exponent base)
    (push-stack (log exponent base))))

(defprim *prim* "pow" nil
    "exponentiation"
  (stack-args (base power)
    (expt base power)))

(defprim *prim* "round" nil
    "correct rounding"
  (stack-call round))

(defprim *prim* "floor" nil
    "round down"
  (stack-call floor))

(defprim *prim* "ceiling" nil
    "round up"
  (stack-call ceiling))
