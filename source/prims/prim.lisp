(in-package #:poslin)

;;;; control
(defparameter *control-prims*
  '())

(defnprim *control-prims* "!" t
    "sets the program counter"
  (stack-args (op)
    (let ((b (thread-back pc)))
      (unless (eq b <noop>)
        (push (thread-back pc)
              rstack))
      (setf pc (typecase op
                 (symbol
                  (if (poslin-symbol? op)
                      (avif (lookup (op-env path)
                                    op)
                            ([binding]-value it)
                            (op-fail (format nil "Attempt to call undefined operation `~A`"
                                             op)
                                     (list :undefined-operation-error op)))
                      (if (eq op <noop>)
                          <noop>
                          (<constant> op))))
                 (<constant>
                  op)
                 (<prim>
                  op)
                 (<thread>
                  op)
                 (<handled>
                  op)
                 (t
                  (<constant> op)))))))

(defprim *control-prims* "&" t
    "finds or converts to a thread"
  (stack-args (op)
    (push-stack (typecase op
		  (null
		   <noop>)
		  (symbol
		   (if (poslin-symbol? op)
                       (avif (lookup (op-env path)
                                     op)
                             ([binding]-value it)
                             (op-fail (format nil "Attempt to inline undefined operation `~A`"
                                              op)
                                      (list :undefined-operation-error op)))
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
                  (<handled>
                   op)
		  (t
		   (<constant> op))))))

(defprim *control-prims* "#" t
    "makes a constant thread of value"
  (stack-args (val)
    (push-stack (<constant> val))))

(defprim *control-prims* "->elem-thread" nil
    "converts an object into a primary thread"
  (stack-args ((string string)
               obj)
    (push-stack (<prim> (lambda ()
                          (with-pandoric (pc)
                              this
                            (setf pc obj)))
                        string))))

(defprim *control-prims* "?" nil
    "if"
  (stack-args ((bool [bool])
               then else)
    (push-stack (case bool
                  (<true> then)
                  (<false> else)))))

(defprim *control-prims* "r<-" nil
    "push onto return stack"
  (push (arg-pop)
	rstack))

(defprim *control-prims* "r->" nil
    "pop from return stack"
  (if rstack
      (push-stack (pop rstack))
      (op-fail "Attempt to pop from empty return stack"
               :rstack-bottom-error)))

;;;; thread
(defparameter *thread-prims*
  '())

(defprim *thread-prims* "thread-front" nil
    "pushes the front of a thread"
  (stack-call thread-front))

(defprim *thread-prims* "thread-back" nil
    "pushes the back of a thread"
  (stack-call thread-back))

(defprim *thread-prims* "thread-concat" nil
    "combines two threads into one"
  (stack-args ((front thread)
               (back thread))
    (push-stack (<thread> front back))))


;;;; path
(defparameter *path-prims*
  '())

(defprim *path-prims* "path-pop" nil
    "pops the top of the path"
  (let ((e (path-top path))
	(p (path-pop path)))
    (if p
	(progn
	  (setf path p)
	  (push-stack e))
        (op-fail "Attempt to pop path bottom"
                 :path-bottom-error))))

(defprim *path-prims* "path-push" nil
    "pushes onto the path"
  (stack-args ((env map))
    (setf path
          (path-push path env))))

(defprim *path-prims* "path-access" nil
    "returns nth environment on path"
  (push-stack (path-nth path (arg-pop))))

(defprim *path-prims* "path-set" nil
    "set current environment"
  (stack-args ((env map))
    (setf path
          (path-set path env))))

;;;; sets
(defparameter *set-prims*
  '())

(defparameter *empty-set* (empty-set))
(defprim *set-prims* ".empty-set" nil
    "returns the empty set"
  (push-stack *empty-set*))

(defprim *set-prims* "set-lookup" nil
    "set lookup"
  (stack-args ((s set)
               v)
    (push-stack (if (lookup s v)
                    <true>
                    <false>))))

(defprim *set-prims* "set-insert" nil
    "insert into set"
  (stack-args ((s set)
               v)
    (push-stack (with s v))))

(defprim *set-prims* "set-drop" nil
    "drop from set"
  (stack-args ((s set)
               v)
    (push-stack (less s v))))

(defprim *set-prims* "set-arbitrary" nil
    "return arbitrary element of set"
  (stack-args ((s set))
    (push-stack (fset:arb s))))

;;;; dictionaries
(defparameter *dict-prims*
  '())

(defparameter *empty-dict* (empty-map <meta-nothing>))
(defprim *dict-prims* ".empty-dict" nil
    "returns the empty dictionary"
  (push-stack *empty-dict*))

(defprim *dict-prims* "dict-lookup" nil
    "dictionary lookup"
  (stack-args ((m map)
               k)
    (push-stack (lookup m k))))

(defprim *dict-prims* "dict-insert" nil
    "insert into dictionary"
  (stack-args ((m fset:map)
               k v)
    (push-stack (with m k v))))

(defprim *dict-prims* "dict-drop" nil
    "drop from dictionary"
  (stack-args ((m map)
               k)
    (push-stack (less m k))))

(defprim *dict-prims* "dict-domain" nil
    "domain of dictionary"
  (stack-args ((m map))
    (push-stack (domain m))))

;;;; binding
(defparameter *binding-prims*
  '())

(defprim *binding-prims* "new-binding" nil
    "create fresh binding"
  (push-stack (binding <meta-nothing>)))

(defprim *binding-prims* "retrieve" nil
    "read binding"
  (stack-args ((binding [binding]))
    (push-stack ([binding]-value binding))))

(defprim *binding-prims* "store" nil
    "set binding"
  (stack-args ((b [binding])
               v)
    (setf ([binding]-value b)
	  v)))

;;;; stack
(defparameter *stack-prims*
  '())

(defprim *stack-prims* ".empty-stack" nil
    "create a fresh stack"
  (push-stack '()))

(defprim *stack-prims* "push" nil
    "push"
  (stack-args ((s (or cons null))
               v)
    (push-stack (cons v s))))

(defprim *stack-prims* "top" nil
    "top"
  (stack-args ((st (or cons null)))
    (if st
        (push-stack (first st))
        (op-fail "Attempt to pop from empty stack"
                 :stack-bottom-error))))

(defprim *stack-prims* "drop" nil
    "drop"
  (stack-args ((st (or cons null)))
    (if st
        (push-stack (rest st))
        (op-fail "Attempt to drop from empty stack"
                 :stack-bottom-error))))

(defprim *stack-prims* "swap" nil
    "swap"
  (stack-args ((s (or cons null)))
    (if (and s (rest s))
        (push-stack (list* (second s)
                           (first s)
                           (cddr s)))
        (op-fail (if s
                     "Attempt to swap on stack of size one"
                     "Attempt to swap on empty stack")
                 :stack-bottom-error))))

;;;; nothing
(defprim *control-prims* ".nothing" nil
    "returns the meta-nothing value"
  (push-stack <meta-nothing>))

;;;; boolean
(defparameter *boolean-prims*
  '())

(defprim *boolean-prims* ".true" nil
    "returns true value"
  (push-stack <true>))

(defprim *boolean-prims* ".false" nil
    "returns false value"
  (push-stack <false>))

(defprim *boolean-prims* "and" nil
    "and"
  (stack-args ((b1 [bool])
               (b2 [bool]))
    (push-stack (if (and (eq b1 <true>)
			 (eq b2 <true>))
		    <true>
		    <false>))))

(defprim *boolean-prims* "or" nil
    "or"
  (stack-args ((b1 [bool])
               (b2 [bool]))
    (push-stack (if (or (eq b1 <true>)
			(eq b2 <true>))
		    <true>
		    <false>))))

(defprim *boolean-prims* "not" nil
    "not"
  (stack-args ((b [bool]))
    (push-stack (if (eq b <true>)
		    <false>
		    <true>))))

;;;; comparison
(defparameter *comparison-prims*
  '())

(defprim *comparison-prims* "same?" nil
    "identity comparison"
  (stack-args (x y)
    (push-stack (if (eq x y)
		    <true>
		    <false>))))

(defprim *comparison-prims* "compare" nil
    "comparison operator"
  (stack-args (x y)
    (push-stack (case (compare x y)
		  (:less <less>)
		  (:equal <equal>)
		  (:greater <greater>)
		  (:unequal <unequal>)))))

(defprim *comparison-prims* ".less" nil
    "returns value vor 'less'"
  (push-stack <less>))

(defprim *comparison-prims* ".greater" nil
    "returns value vor 'greater'"
  (push-stack <greater>))

(defprim *comparison-prims* ".equal" nil
    "returns value vor 'equal'"
  (push-stack <equal>))

(defprim *comparison-prims* ".unequal" nil
    "returns value vor 'unequal'"
  (push-stack <unequal>))

;;;; arithmetic
(defparameter *arith-prims*
  '())

(defprim *arith-prims* "+" nil
    "addition"
  (stack-args ((x number)
               (y number))
    (push-stack (+ x y))))

(defprim *arith-prims* "*" nil
    "multiplication"
  (stack-args ((x number)
               (y number))
    (push-stack (* x y))))

(defprim *arith-prims* "negation" nil
    "negation"
  (stack-args ((x number))
    (push-stack (- x))))

(defprim *arith-prims* "reciprocal" nil
    "reciprocal"
  (stack-args ((x number))
    (if (zerop x)
        (op-fail "Division by zero"
                 :zero-division-error)
        (push-stack (/ x)))))

(defprim *arith-prims* "log" nil
    "logarithm"
  (stack-args ((exponent number)
               (base number))
    (push-stack (log exponent base))))

(defprim *arith-prims* "pow" nil
    "exponentiation"
  (stack-args ((base number)
               (power number))
    (push-stack (expt base power))))

(defprim *arith-prims* "round" nil
    "correct rounding"
  (stack-args ((x real))
    (push-stack (round x))))

(defprim *arith-prims* "floor" nil
    "round down"
  (stack-args ((x real))
    (push-stack (round x))))

(defprim *arith-prims* "ceiling" nil
    "round up"
  (stack-args ((x real))
    (push-stack (ceiling x))))

;;;; arrays
(defparameter *array-prims*
  '())

(defprim *array-prims* "new-array" nil
    "make array"
  (stack-args ((size (integer 0)))
    (push-stack (make-array size
                            :initial-element <meta-nothing>))))

(defprim *array-prims* "array-set" nil
    "set in array"
  (stack-args ((array (and vector (not string)))
               (n (integer 0))
               v)
    (if (<= (length array)
            n)
        (op-fail "Tried to index array out of bounds"
                 (list :array-index-error array n v))
        (let ((array (copy-seq array)))
          (setf (aref array n)
                v)
          (push-stack array)))))

(defprim *array-prims* "array-lookup" nil
    "get from array"
  (stack-args ((array (and vector (not string)))
               (n (integer 0)))
    (if (<= (length array)
            n)
        (op-fail "Tried to index array out of bounds"
                 (list :array-index-error array n))
        (push-stack (aref array n)))))

(defprim *array-prims* "array-concat" nil
    "concatenate two arrays"
  (stack-args ((a1 (and vector (not string)))
               (a2 (and vector (not string))))
    (push-stack (concatenate 'vector
                             a1 a2))))

(defprim *array-prims* "array-size" nil
    "returns the size of the array"
  (stack-args ((array (and vector (not string))))
    (push-stack (array-total-size array))))

;;;; strings
(defparameter *string-prims*
  '())

(defprim *string-prims* "->string" nil
    "convert into a string"
  (stack-args (obj)
    (push-stack (poslin-print obj nil))))

(defprim *string-prims* "string-concat" nil
    "concatenate two strings"
  (stack-args ((s1 string)
               (s2 string))
    (push-stack (concatenate 'string
                             s1 s2))))

(defprim *string-prims* "string-set" nil
    "set in string"
  (stack-args ((string string)
               (n (integer 0))
               (v character))
    (if (<= (length string)
            n)
        (op-fail "Tried to index string out of bounds"
                 (list :string-index-error string n v))
        (let ((string (copy-seq string)))
          (setf (elt string n)
                v)
          (push-stack string)))))

(defprim *string-prims* "string-lookup" nil
    "get from string"
  (stack-args ((string string)
               (n (integer 0)))
    (if (<= (length string)
            n)
        (op-fail "Tried to index string out of bounds"
                 (list :string-index-error string n))
        (push-stack (elt string n)))))

(defprim *string-prims* "string-size" nil
    "returns the size of the array"
  (stack-args ((string string))
    (push-stack (length string))))

;;;; characters
(defparameter *char-prims*
  '())

(defprim *char-prims* "int->char" nil
    "converts an integer into a character"
  (stack-args ((int (integer 0)))
    (push-stack (code-char int))))

(defprim *char-prims* "char->int" nil
    "converts a character into an integer"
  (stack-args ((char character))
    (push-stack (char-code char))))

;;;; type
(defparameter *type-prims*
  '())

(defprim *type-prims* "type" nil
    "returns the type of an object"
  (stack-args (object)
    (push-stack (typecase object
                  (rational '|:Precise|)
                  (float '|:Imprecise|)
                  (character '|:Character|)
                  (null '|:EmptyStack|)
                  (cons '|:Stack|)
                  (symbol
                   (cond
                     ((or (keywordp object)
                          (not (symbol-package object)))
                      '|:Symbol|)
                     ((eq <noop> object)
                      '|:ElementaryThread|)
                     ((or (eq <true> object)
                          (eq <false> object))
                      '|:Boolean|)
                     ((or (eq <less> object)
                          (eq <equal> object)
                          (eq <greater> object)
                          (eq <unequal> object))
                      '|:Comparison|)
                     ((eq <meta-nothing> object)
                      '|:Nothing|)
                     ((or
                       (eq object '|:Nothing|)
                       (eq object '|:Symbol|)
                       (eq object '|:Boolean|)
                       (eq object '|:Comparison|)
                       (eq object '|:Type|)
                       (eq object '|:ConstantThread|)
                       (eq object '|:ElementaryThread|)
                       (eq object '|:Thread|)
                       (eq object '|:HandledThread|)
                       (eq object '|:Exception|)
                       (eq object '|:Precise|)
                       (eq object '|:Imprecise|)
                       (eq object '|:EmptyStack|)
                       (eq object '|:Stack|)
                       (eq object '|:Binding|)
                       (eq object '|:Environment|)
                       (eq object '|:Array|)
                       (eq object '|:String|)
                       (eq object '|:Character|)
                       (eq object '|:Stream|)
                       (eq object '|:Dict|)
                       (eq object '|:Set|)
                       )
                      '|:Type|)
                     (t
                      (error "Malformed lisp symbol found: ~S
Please report this bug to thomas.bartscher@weltraumschlangen.de"
                             object))))
                  (<prim>
                   '|:ElementaryThread|)
                  (<constant>
                   '|:ConstantThread|)
                  (<thread>
                   '|:Thread|)
                  (<handled>
                   '|:HandledThread|)
                  ([exception]
                   '|:Exception|)
                  ([binding]
                   '|:Binding|)
                  (map
                   '|:Dict|)
                  (set
                   '|:Set|)
                  (array
                   (if (stringp object)
                       '|:String|
                       '|:Array|))
                  (stream
                   '|:Stream|)
                  (t
                   (error "Unknown lisp object found: ~S
Please report this bug to thomas.bartscher@weltraumschlangen.de"
                          object))
                  ))))

;;;; exceptions
(defparameter *exception-prims*
  '())

(defprim *exception-prims* "throw" nil
    "throws an exception"
  (stack-args ((exception [exception]))
    (pthrow exception)))

(defprim *exception-prims* "handle" nil
    "constructs a handled thread"
  (stack-args ((thread thread)
               (handle thread))
    (push-stack (if (typep thread '<constant>)
                    thread
                    (<handled> thread handle)))))

(defprim *exception-prims* "new-exception" nil
    "creates a new exception"
  (stack-args ((message string)
               data (stack (or cons null)))
    (push-stack (<exception> message data stack))))

(defprim *exception-prims* "exception-message" nil
    "gets the message of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-string exception))))

(defprim *exception-prims* "exception-data" nil
    "gets the meta data of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-data exception))))

(defprim *exception-prims* "exception-stack" nil
    "gets the unwound return stack of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-stack exception))))

(defprim *exception-prims* "thread-handle" nil
    "returns the handle of a handled thread"
  (stack-args ((thread <handled>))
    (push-stack (<handled>-thread thread))))

;;;; symbols
(defparameter *symbol-prims*
  '())

(defprim *symbol-prims* "unique-symbol" nil
    "returns a unique symbol"
  (push-stack (gensym "|unique")))

(defprim *symbol-prims* "symbol-concat" nil
    "returns a symbol whose name is made up of the names of two other
symbols"
  (stack-args ((sym1 symbol)
               (sym2 symbol))
    (push-stack (intern (concatenate 'string
                                     (symbol-name sym1)
                                     (symbol-name sym2))
                        :keyword))))

;;;; loading
(defprim *control-prims* "load" nil
    "loads a file as poslin code"
  (stack-args ((path string))
    (push (thread-back pc)
          rstack)
    (setf pc <noop>)
    (mapcar this (poslin-read-file path *parse-order*))))

;;;; streams
(defparameter *stream-prims*
  '())

(defprim *stream-prims* "open" nil
    "makes a file handle"
  (stack-args ((filename string)
               (direction (member :|write| :|input| :|rw|)))
    (push-stack (open filename
                      :direction (case direction
                                   (:|write| :output)
                                   (:|read| :input)
                                   (:|rw| :io))))))

(defprim *stream-prims* ".stdin" nil
    "leaves the standard input stream"
  (push-stack *standard-input*))

(defprim *stream-prims* ".stdout" nil
    "leaves the standard output stream"
  (push-stack *standard-output*))

(defprim *stream-prims* "close" nil
    "closes a stream"
  (stack-args ((stream stream))
    (close stream)))

(defprim *stream-prims* "read-char" nil
    "read a character from a stream"
  (stack-args ((stream stream))
    (push-stack (read-char stream nil <meta-nothing>))))

(defprim *stream-prims* "write-char" nil
    "write a character to a stream"
  (stack-args ((stream stream)
               (char character))
    (write-char char stream)))
