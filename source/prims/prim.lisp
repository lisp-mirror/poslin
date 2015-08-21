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

(defprim *prim* "thread-front" nil
    "pushes the front of a thread"
  (stack-call thread-front))

(defprim *prim* "thread-back" nil
    "pushes the back of a thread"
  (stack-call thread-back))

(defprim *prim* "thread-concat" nil
    "combines two threads into one"
  (stack-args (front back)
    (push-stack (<thread> front back))))

(defprim *prim* "->prim" nil
    "converts an object into a primary thread"
  (stack-args (string obj)
    (if (stringp string)
        (push-stack (<prim> (lambda ()
                              (with-pandoric (pc)
                                  this
                                (setf pc obj)))
                            string))
        (error "expected a string in `->prim`, got ~A instead"
               (poslin-print string nil)))))

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
(defprim *prim* "path-pop" nil
    "pops the top of the path"
  (let ((e (path-top path))
	(p (path-pop path)))
    (if p
	(progn
	  (setf path p)
	  (push-stack e))
        (error "Attempt to pop bottom of stack"))))

(defprim *prim* "path-push" nil
    "pushes onto the path"
  (setf path
	(path-push path (pop-stack))))

(defprim *prim* "path-access" nil
    "returns nth environment on path"
  (push-stack (path-nth path (pop-stack))))

(defprim *prim* "path-set" nil
    "set current environment"
  (setf path
	(path-set path (pop-stack))))

;;;; environment
(defparameter *empty-env* (<root-env> (fset:empty-map)))
(defprim *prim* ".empty-env" nil
    "returns a fresh environment"
  (push-stack *empty-env*))

(defprim *prim* "env-lookup" nil
    "environment lookup"
  (stack-args (e k)
    (push-stack (aif (lookup e k)
		     it
		     <meta-nothing>))))

(defprim *prim* "env-set" nil
    "environment set"
  (stack-args (e k v)
    (unless ([env]-p e)
      (error "Got ~A instead of an environment in `e<-`"
             (poslin-print e nil)))
    (unless (poslin-symbol? k)
      (error "Got ~A instead of a symbol in `e<-`"
             k))
    (unless ([binding]-p v)
      (error "Got ~A instead of a binding in `e<-`"
             v))
    (push-stack (insert e k v))))

(defprim *prim* "env-parent" nil
    "parent of environment"
  (push-stack (get-parent (pop-stack))))

(defprim *prim* "env-parent-set" nil
    "set parent of environment"
  (stack-args (e p)
    (push-stack (set-parent e p))))

(defprim *prim* "env-drop" nil
    "delete from environment"
  (stack-args (e k)
    (push-stack (drop e k))))

(defprim *prim* "env-symbols" nil
    "returns a stack containing all symbols defined in the given environment"
  (stack-args (e)
    (push-stack (sort (fset:convert 'list
                                    (fset:domain ([env]-content e)))
                      (lambda (a b)
                        (string> (symbol-name a)
                                 (symbol-name b)))))))

;;;; binding
(defprim *prim* "new-binding" nil
    "create fresh binding"
  (push-stack (binding <meta-nothing>)))

(defprim *prim* "retrieve" nil
    "read binding"
  (push-stack ([binding]-value (pop-stack))))

(defprim *prim* "store" nil
    "set binding"
  (stack-args (b v)
    (setf ([binding]-value b)
	  v)))

(defprim *prim* "binding-doc" nil
    "binding doc"
  (push-stack ([binding]-doc (pop-stack))))

(defprim *prim* "binding-doc-set" nil
    "set binding doc"
  (stack-args (b d)
    (setf ([binding]-doc b)
	  d)))

;;;; stack
(defprim *prim* ".empty-stack" nil
    "create a fresh stack"
  (push-stack '()))

(defprim *prim* "push" nil
    "push"
  (stack-args (s v)
    (push-stack (cons v s))))

(defprim *prim* "top" nil
    "top"
  (stack-args (st)
    (if st
        (push-stack (first st))
        (error "Attempt to pop from empty stack"))))

(defprim *prim* "drop" nil
    "drop"
  (stack-args (st)
    (if st
        (push-stack (rest st))
        (error "Attempt to drop from empty stack"))))

(defprim *prim* "swap" nil
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
(defprim *prim* ".nothing" nil
    "returns the meta-nothing value"
  (push-stack <meta-nothing>))

;;;; boolean
(defprim *prim* ".true" nil
    "returns true value"
  (push-stack <true>))

(defprim *prim* ".false" nil
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
(defprim *prim* "same?" nil
    "identity comparison"
  (stack-args (x y)
    (push-stack (if (eq x y)
		    <true>
		    <false>))))

(defprim *prim* "compare" nil
    "comparison operator"
  (stack-args (x y)
    (push-stack (case (compare x y)
		  (:less <less>)
		  (:equal <equal>)
		  (:greater <greater>)
		  (:unequal <unequal>)))))

(defprim *prim* ".less" nil
    "returns value vor 'less'"
  (push-stack <less>))

(defprim *prim* ".greater" nil
    "returns value vor 'greater'"
  (push-stack <greater>))

(defprim *prim* ".equal" nil
    "returns value vor 'equal'"
  (push-stack <equal>))

(defprim *prim* ".unequal" nil
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

(defprim *prim* "negation" nil
    "negation"
  (stack-call -))

(defprim *prim* "reciprocal" nil
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
(defprim *prim* "new-array" nil
    "make array"
  (push-stack (make-array (pop-stack)
			  :initial-element <meta-nothing>
			  :element-type `(or (eql <meta-nothing>)
					     [binding]))))

(defprim *prim* "array-set" nil
    "set in array"
  (stack-args (array n v)
    (let ((array (copy-seq array)))
      (setf (aref array n)
            v)
      (push-stack array))))

(defprim *prim* "array-lookup" nil
    "get from array"
  (stack-args (array n)
    (push-stack (aref array n))))

(defprim *prim* "array-concat" nil
    "concatenate two arrays"
  (stack-args (a1 a2)
    (if (and (vectorp a1)
             (vectorp a2))
        (push-stack (concatenate 'vector
                                 a1 a2))
        (error "Expected two arrays for `>a<` but got ~A and ~A"
               (poslin-print a1 nil)
               (poslin-print a2 nil)))))

;;;; strings
(defprim *prim* "->string" nil
    "convert into a string"
  (stack-args (obj)
    (push-stack (poslin-print obj nil))))

(defprim *prim* "string-concat" nil
    "concatenate two strings"
  (stack-args (s1 s2)
    (if (and (stringp s1)
             (stringp s2))
        (push-stack (concatenate 'string
                                 s1 s2))
        (error "Expected two strings for `>string<` but got ~A and ~A"
               (poslin-print s1 nil)
               (poslin-print s2 nil)))))

(defprim *prim* "print" nil
    "prints a string to the standard output"
  (stack-args (string)
    (unless (typep string 'string)
      (error "Tried to print ~A"
             (poslin-print string nil)))
    (print string)))

;;;; type
(defprim *prim* "type" nil
    "returns the type of an object"
  (stack-args (object)
    (push-stack (typecase object
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
                     ((or
                       (eq object '|·Nothing|)
                       (eq object '|·Symbol|)
                       (eq object '|·Boolean|)
                       (eq object '|·Comparison|)
                       (eq object '|·Type|)
                       (eq object '|·ConstantThread|)
                       (eq object '|·Prim|)
                       (eq object '|·Thread|)
                       (eq object '|·Precise|)
                       (eq object '|·Imprecise|)
                       (eq object '|·EmptyStack|)
                       (eq object '|·Stack|)
                       (eq object '|·Binding|)
                       (eq object '|·Environment|)
                       (eq object '|·Array|)
                       )
                      '|·Type|)
                     (t
                      (error "Malformed lisp symbol found: ~S
This is an error in the implementation.
Please report this bug to thomas.bartscher@weltraumschlangen.de"
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
                  (t
                   (error "Unknown lisp object found: ~S
This is an error in the implementation.
Please report this bug to thomas.bartscher@weltraumschlangen.de"
                          object))
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
(defprim *prim* "unique-symbol" nil
    "returns a unique symbol"
  (push-stack (gensym "×unique×")))

(defprim *prim* "symbol-concat" nil
    "returns a symbol whose name is made up of the names of two other
symbols"
  (stack-args (sym1 sym2)
    (unless (and (symbolp sym1)
                 (symbolp sym2))
      (error "Expected two symbols, got ~A and ~A instead"
             (poslin-print sym1 nil)
             (poslin-print sym2 nil)))
    (push-stack (intern (concatenate 'string
                                     (symbol-name sym1)
                                     (symbol-name sym2))
                        :keyword))))

;;;; loading
(defprim *prim* "load" nil
    "loads a file as poslin code"
  (stack-args (path)
    (unless (stringp path)
      (error "Got ~A as a path, need a string."
             (poslin-print path nil)))
    (push (thread-back pc)
          rstack)
    (setf pc <noop>)
    (mapcar this (poslin-read-file path *parse-order*))))
