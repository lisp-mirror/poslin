(in-package #:poslin)

(defparameter *prim*
  '())

;;;; control
(defnprim *prim* "!" t
    "sets the program counter"
  (let ((op (pop-stack)))
    (let ((b (thread-back pc)))
      (unless (eq b <noop>)
        (push (thread-back pc)
              rstack))
      (setf pc (typecase op
                 (null
                  <noop>)
                 (symbol
                  (if (poslin-symbol? op)
                      (let ((binding (lookup (op-env path)
                                             op)))
                        (if (eq binding <meta-nothing>)
                            (error "Attempt to call undefined operation `~A`"
                                   op)
                            ([binding]-value (lookup (op-env path)
                                                     op))))
                      (if (eq op <noop>)
                          <noop>
                          (<constant> op))))
                 (cons
                  (thread<-stack op))
                 (<constant>
                  op)
                 (<prim>
                  op)
                 (<thread>
                  op)
                 (t
                  (<constant> op)))))))

(defprim *prim* "&" t
    "finds or converts to a thread"
  (let ((op (pop-stack)))
    (push-stack (typecase op
		  (null
		   <noop>)
		  (symbol
		   (if (poslin-symbol? op)
                       (let ((binding (lookup (op-env path)
                                              op)))
                         (if (eq <meta-nothing> binding)
                             (error "Attempt to inline undefined operation `~A`"
                                    op)
                             ([binding]-value binding)))
                       (if (eq op <noop>)
                           <noop>
                           (<constant> op))))
		  (cons
		   (thread<-stack op))
		  (<constant>
		   op)
		  (<prim>
		   op)
		  (<thread>
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
    (push-stack (ecase bool
                  (<true> then)
                  (<false> else)))))

(defprim *prim* "r<-" nil
    "push onto return stack"
  (push (pop-stack)
	rstack))

(defprim *prim* "r->" nil
    "pop from return stack"
  (if rstack
      (push-stack (pop rstack))
      (error "Attempt to pop from empty return stack")))

;;;; path
(defprim *prim* "p->" nil
    "pops the top of the path"
  (let ((e (path-top path))
	(p (path-pop path)))
    (if p
	(progn
	  (setf path p)
	  (push-stack e))
        (error "Attempt to pop bottom of stack"))))

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
    (push-stack (aif (lookup e k)
		     it
		     <meta-nothing>))))

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

(defprim *prim* "e_" nil
    "delete from environment"
  (stack-args (e k)
    (push-stack (drop e k))))

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
  (stack-args (st)
    (if st
        (push-stack (first st))
        (error "Attempt to pop from empty stack"))))

(defprim *prim* "_" nil
    "drop"
  (stack-args (st)
    (if st
        (push-stack (rest st))
        (error "Attempt to drop from empty stack"))))

(defprim *prim* "<>" nil
    "swap"
  (stack-args (s)
    (if (and s (rest s))
        (push-stack (list* (second s)
                           (first s)
                           (cddr s)))
        (if s
            (error "Attempt to swap on stack of size one")
            (error "Attempt to swap on empty stack")))))

;;;; nothing
(defprim *prim* ".." nil
    "returns the meta-nothing value"
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
    (push-stack (if (and (eq b1 <true>)
			 (eq b2 <true>))
		    <true>
		    <false>))))

(defprim *prim* "or" nil
    "or"
  (stack-args (b1 b2)
    (push-stack (if (or (eq b1 <true>)
			(eq b2 <true>))
		    <true>
		    <false>))))

(defprim *prim* "not" nil
    "not"
  (stack-args (b)
    (push-stack (if (eq b <true>)
		    <false>
		    <true>))))

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

;;;; arrays
(defprim *prim* "a*" nil
    "make array"
  (push-stack (make-array (pop-stack)
			  :initial-element <meta-nothing>
			  :element-type `(or (eql <meta-nothing>)
					     [binding]))))

(defprim *prim* "a<-" nil
    "set in array"
  (stack-args (array n v)
    (let ((array (copy-seq array)))
      (push-stack (setf (aref array n)
			v)))))

(defprim *prim* "a->" nil
    "get from array"
  (stack-args (array n)
    (push-stack (aref array n))))

;;;; type
(defprim *prim* "type" nil
    "returns the type of an object"
  (stack-args (object)
    (push-stack (etypecase object
                  (rational '|·Precise|)
                  (float '|·Imprecise|)
                  (null '|·EmptyStack|)
                  (cons '|·Stack|)
                  (symbol
                   (cond
                     ((keywordp object)
                      '|·Symbol|)
                     ((eq <noop> object)
                      '|·Prim|)
                     ((or (eq <true> object)
                          (eq <false> object))
                      '|·Boolean|)
                     ((or (eq <less> object)
                          (eq <equal> object)
                          (eq <greater> object)
                          (eq <unequal> object))
                      '|·Comparison|)
                     ((eq <meta-nothing> object)
                      '|·Nothing|)
                     ((or (eq object '|·EmptyStack|)
                          (eq object '|·Stack|)
                          (eq object '|·Symbol|)
                          (eq object '|·Prim|)
                          (eq object '|·Boolean|)
                          (eq object '|·Comparison|)
                          (eq object '|·Nothing|)
                          (eq object '|·ConstantThread|)
                          (eq object '|·Thread|)
                          (eq object '|·Binding|)
                          (eq object '|·Environment|)
                          (eq object '|·Type|)
                          (eq object '|·Array|)
                          (eq object '|·Precise|)
                          (eq object '|·Imprecise|)
                          )
                      '|·Type|)
                     (t
                      (error "Malformed lisp symbol found: ~S"
                             object))))
                  (<prim>
                   '|·Prim|)
                  (<constant>
                   '|·ConstantThread|)
                  (<thread>
                   '|·Thread|)
                  ([binding]
                   '|·Binding|)
                  ([env]
                   '|·Environment|)
                  (array
                   '|·Array|)
                  ))))

;;;; errors
(defprim *prim* "error" nil
    "raises an error"
  (stack-args (error-string)
    (if (stringp error-string)
        (error "~A"
               error-string)
        (error "Attempt to call an error on non-string object ~A"
               (poslin-print error-string nil)))))

;;;; symbols
(defprim *prim* "s*" nil
    "returns a unique symbol"
  (push-stack (gensym "×unique×")))

;;;; loading
(defprim *prim* "load" nil
    "loads a file as poslin code"
  (stack-args (path)
    (unless (stringp path)
      (error "Got ~S as a path, need a string."
             (poslin-print path nil)))
    (push (thread-back pc)
          rstack)
    (setf pc <noop>)
    (mapcar this (poslin-read-file path *parse-order*))))
