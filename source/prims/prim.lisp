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
                 #+nil  ; same as below with `cons`
                 (null
                  <noop>)
                 (symbol
                  (if (poslin-symbol? op)
                      (avif (lookup (op-env path)
                                    op)
                            ([binding]-value it)
                            (unwind (format nil "Attempt to call undefined operation `~A`"
                                            op)
                                    (list :undefined-operation-error op)))
                      (if (eq op <noop>)
                          <noop>
                          (<constant> op))))
                 #+nil  ; this seems like something that should be done
                                        ; explicitely
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
                  (<constant> op)))))))

(defprim *prim* "&" t
    "finds or converts to a thread"
  (let ((op (pop-stack)))
    (push-stack (typecase op
		  (null
		   <noop>)
		  (symbol
		   (if (poslin-symbol? op)
                       (avif (lookup (op-env path)
                                     op)
                             ([binding]-value it)
                             (unwind (format nil "Attempt to inline undefined operation `~A`"
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
  (stack-args ((front thread)
               (back thread))
    (push-stack (<thread> front back))))

(defprim *prim* "->elem-thread" nil
    "converts an object into a primary thread"
  (stack-args ((string string)
               obj)
    (push-stack (<prim> (lambda ()
                          (with-pandoric (pc)
                              this
                            (setf pc obj)))
                        string))))

(defprim *prim* "?" nil
    "if"
  (stack-args ((bool [bool])
               then else)
    (push-stack (case bool
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
      (unwind "Attempt to pop from empty return stack"
              :rstack-bottom-error)))

;;;; path
(defprim *prim* "path-pop" nil
    "pops the top of the path"
  (let ((e (path-top path))
	(p (path-pop path)))
    (if p
	(progn
	  (setf path p)
	  (push-stack e))
        (unwind "Attempt to pop path bottom"
                :path-bottom-error))))

(defprim *prim* "path-push" nil
    "pushes onto the path"
  (stack-args ((env map))
    (setf path
          (path-push path env))))

(defprim *prim* "path-access" nil
    "returns nth environment on path"
  (push-stack (path-nth path (pop-stack))))

(defprim *prim* "path-set" nil
    "set current environment"
  (stack-args ((env map))
    (setf path
          (path-set path env))))

;;;; sets
(defparameter *empty-set* (empty-set))
(defprim *prim* ".empty-set" nil
    "returns the empty set"
  (push-stack *empty-set*))

(defprim *prim* "set-lookup" nil
    "set lookup"
  (stack-args ((s set)
               v)
    (push-stack (if (lookup s v)
                    <true>
                    <false>))))

(defprim *prim* "set-insert" nil
    "insert into set"
  (stack-args ((s set)
               v)
    (push-stack (with s v))))

(defprim *prim* "set-drop" nil
    "drop from set"
  (stack-args ((s set)
               v)
    (push-stack (less s v))))

(defprim *prim* "set-arbitrary" nil
    "return arbitrary element of set"
  (stack-args ((s set))
    (push-stack (fset:arb s))))

;;;; dictionaries
(defparameter *empty-dict* (empty-map <meta-nothing>))
(defprim *prim* ".empty-dict" nil
    "returns the empty dictionary"
  (push-stack *empty-dict*))

(defprim *prim* "dict-lookup" nil
    "dictionary lookup"
  (stack-args ((m map)
               k)
    (push-stack (lookup m k))))

(defprim *prim* "dict-insert" nil
    "insert into dictionary"
  (stack-args ((m fset:map)
               k v)
    (push-stack (with m k v))))

(defprim *prim* "dict-drop" nil
    "drop from dictionary"
  (stack-args ((m map)
               k)
    (push-stack (less m k))))

(defprim *prim* "dict-domain" nil
    "domain of dictionary"
  (stack-args ((m map))
    (push-stack (domain m))))

#|
;;;; environment
(defprim *prim* "env-lookup" nil
    "environment lookup"
  (stack-args ((e cons)
               (k symbol))
    (push-stack (aif (lookup e k)
		     it
		     <meta-nothing>))))

(defprim *prim* "env-parent-set" nil
    "set parent of environment"
  (stack-args ((e cons)
               (p cons))
    (push-stack (set-parent e p))))

(defprim *prim* "env-drop" nil
    "delete from environment"
  (stack-args ((e cons)
               (k symbol))
    (push-stack (drop e k))))

(defprim *prim* "env-symbols" nil
    "returns a stack containing all symbols defined in the given environment"
  (stack-args ((e cons))
    (push-stack (sort (fset:convert 'list
                                    (fset:domain ([env]-content e)))
                      (lambda (a b)
                        (string> (symbol-name a)
                                 (symbol-name b)))))))
|#

;;;; binding
(defprim *prim* "new-binding" nil
    "create fresh binding"
  (push-stack (binding <meta-nothing>)))

(defprim *prim* "retrieve" nil
    "read binding"
  (stack-args ((binding [binding]))
    (push-stack ([binding]-value binding))))

(defprim *prim* "store" nil
    "set binding"
  (stack-args ((b [binding])
               v)
    (setf ([binding]-value b)
	  v)))

(defprim *prim* "binding-doc" nil
    "binding doc"
  (stack-args ((binding [binding]))
    (push-stack ([binding]-doc binding))))

(defprim *prim* "binding-doc-set" nil
    "set binding doc"
  (stack-args ((b [binding])
               (d string))
    (setf ([binding]-doc b)
	  d)))

;;;; stack
(defprim *prim* ".empty-stack" nil
    "create a fresh stack"
  (push-stack '()))

(defprim *prim* "push" nil
    "push"
  (stack-args ((s (or cons null))
               v)
    (push-stack (cons v s))))

(defprim *prim* "top" nil
    "top"
  (stack-args ((st (or cons null)))
    (if st
        (push-stack (first st))
        (unwind "Attempt to pop from empty stack"
                :stack-bottom-error))))

(defprim *prim* "drop" nil
    "drop"
  (stack-args ((st (or cons null)))
    (if st
        (push-stack (rest st))
        (unwind "Attempt to drop from empty stack"
                :stack-bottom-error))))

(defprim *prim* "swap" nil
    "swap"
  (stack-args ((s (or cons null)))
    (if (and s (rest s))
        (push-stack (list* (second s)
                           (first s)
                           (cddr s)))
        (unwind (if s
                    "Attempt to swap on stack of size one"
                    "Attempt to swap on empty stack")
                :stack-bottom-error))))

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
  (stack-args ((b1 [bool])
               (b2 [bool]))
    (push-stack (if (and (eq b1 <true>)
			 (eq b2 <true>))
		    <true>
		    <false>))))

(defprim *prim* "or" nil
    "or"
  (stack-args ((b1 [bool])
               (b2 [bool]))
    (push-stack (if (or (eq b1 <true>)
			(eq b2 <true>))
		    <true>
		    <false>))))

(defprim *prim* "not" nil
    "not"
  (stack-args ((b [bool]))
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
  (stack-args ((x number)
               (y number))
    (push-stack (+ x y))))

(defprim *prim* "*" nil
    "multiplication"
  (stack-args ((x number)
               (y number))
    (push-stack (* x y))))

(defprim *prim* "negation" nil
    "negation"
  (stack-args ((x number))
    (push-stack (- x))))

(defprim *prim* "reciprocal" nil
    "reciprocal"
  (stack-args ((x number))
    (if (zerop x)
        (unwind "Division by zero"
                :zero-division-error)
        (push-stack (/ x)))))

(defprim *prim* "log" nil
    "logarithm"
  (stack-args ((exponent number)
               (base number))
    (push-stack (log exponent base))))

(defprim *prim* "pow" nil
    "exponentiation"
  (stack-args ((base number)
               (power number))
    (expt base power)))

(defprim *prim* "round" nil
    "correct rounding"
  (stack-args ((x real))
    (push-stack (round x))))

(defprim *prim* "floor" nil
    "round down"
  (stack-args ((x real))
    (push-stack (round x))))

(defprim *prim* "ceiling" nil
    "round up"
  (stack-args ((x real))
    (push-stack (ceiling x))))

;;;; arrays
(defprim *prim* "new-array" nil
    "make array"
  (stack-args ((size (integer 0)))
    (push-stack (make-array size
                            :initial-element <meta-nothing>))))

(defprim *prim* "array-set" nil
    "set in array"
  (stack-args ((array (and vector (not string)))
               (n (integer 0))
               v)
    (if (<= (length array)
            n)
        (unwind "Tried to index array out of bounds"
                (list :array-index-error array n v))
        (let ((array (copy-seq array)))
          (setf (aref array n)
                v)
          (push-stack array)))))

(defprim *prim* "array-lookup" nil
    "get from array"
  (stack-args ((array (and vector (not string)))
               (n (integer 0)))
    (if (<= (length array)
            n)
        (unwind "Tried to index array out of bounds"
                (list :array-index-error array n))
        (push-stack (aref array n)))))

(defprim *prim* "array-concat" nil
    "concatenate two arrays"
  (stack-args ((a1 (and vector (not string)))
               (a2 (and vector (not string))))
    (push-stack (concatenate 'vector
                             a1 a2))))

(defprim *prim* "array-size" nil
    "returns the size of the array"
  (stack-args ((array (and vector (not string))))
    (push-stack (array-total-size array))))

;;;; strings
(defprim *prim* "->string" nil
    "convert into a string"
  (stack-args (obj)
    (push-stack (poslin-print obj nil))))

(defprim *prim* "string-concat" nil
    "concatenate two strings"
  (stack-args ((s1 string)
               (s2 string))
    (push-stack (concatenate 'string
                             s1 s2))))

(defprim *prim* "string-set" nil
    "set in string"
  (stack-args ((string string)
               (n (integer 0))
               (v character))
    (if (<= (length string)
            n)
        (unwind "Tried to index string out of bounds"
                (list :string-index-error string n v))
        (let ((string (copy-seq string)))
          (setf (elt string n)
                v)
          (push-stack string)))))

(defprim *prim* "string-lookup" nil
    "get from string"
  (stack-args ((string string)
               (n (integer 0)))
    (if (<= (length string)
            n)
        (unwind "Tried to index string out of bounds"
                (list :string-index-error string n))
        (push-stack (elt string n)))))

(defprim *prim* "string-size" nil
    "returns the size of the array"
  (stack-args ((string string))
    (push-stack (length string))))

;;;; characters
(defprim *prim* "int->char" nil
    "converts an integer into a character"
  (stack-args ((int (integer 0)))
    (push-stack (code-char int))))

(defprim *prim* "char->int" nil
    "converts a character into an integer"
  (stack-args ((char character))
    (push-stack (char-code char))))

;;;; type
(defprim *prim* "type" nil
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
#+nil  ; replace with `throw` and `new-exception`
(defprim *prim* "unwind" nil
    "throws an exception"
  (stack-args ((string string)
               data)
    (unwind string data)))

(defprim *prim* "throw" nil
    "throws an exception"
  (stack-args ((exception [exception]))
    (pthrow exception)))

(defprim *prim* "handle" nil
    "constructs a handled thread"
  (stack-args ((thread thread)
               (handle thread))
    (push-stack (if (typep thread '<constant>)
                    thread
                    (<handled> thread handle)))))

(defprim *prim* "new-exception" nil
    "creates a new exception"
  (stack-args ((message string)
               data (stack (or cons null)))
    (push-stack (<exception> message data stack))))

(defprim *prim* "exception-message" nil
    "gets the message of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-string exception))))

(defprim *prim* "exception-data" nil
    "gets the meta data of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-data exception))))

(defprim *prim* "exception-stack" nil
    "gets the unwound return stack of an exception"
  (stack-args ((exception [exception]))
    (push-stack ([exception]-stack exception))))

(defprim *prim* "thread-handle" nil
    "returns the handle of a handled thread"
  (stack-args ((thread <handled>))
    (push-stack (<handled>-thread thread))))

;;;; symbols
(defprim *prim* "unique-symbol" nil
    "returns a unique symbol"
  (push-stack (gensym "|unique")))

(defprim *prim* "symbol-concat" nil
    "returns a symbol whose name is made up of the names of two other
symbols"
  (stack-args ((sym1 symbol)
               (sym2 symbol))
    (push-stack (intern (concatenate 'string
                                     (symbol-name sym1)
                                     (symbol-name sym2))
                        :keyword))))

;;;; loading
(defprim *prim* "load" nil
    "loads a file as poslin code"
  (stack-args ((path string))
    (push (thread-back pc)
          rstack)
    (setf pc <noop>)
    (mapcar this (poslin-read-file path *parse-order*))))

;;;; streams
(defprim *prim* "open" nil
    "makes a file handle"
  (stack-args ((filename string)
               (direction (member :|write| :|input| :|rw|)))
    (push-stack (open filename
                      :direction (case direction
                                   (:|write| :output)
                                   (:|read| :input)
                                   (:|rw| :io))))))

(defprim *prim* ".stdin" nil
    "leaves the standard input stream"
  (push-stack *standard-input*))

(defprim *prim* ".stdout" nil
    "leaves the standard output stream"
  (push-stack *standard-output*))

(defprim *prim* "close" nil
    "closes a stream"
  (stack-args ((stream stream))
    (close stream)))

(defprim *prim* "read-char" nil
    "read a character from a stream"
  (stack-args ((stream stream))
    (push-stack (read-char stream nil <meta-nothing>))))

(defprim *prim* "write-char" nil
    "write a character to a stream"
  (stack-args ((stream stream)
               (char character))
    (write-char char stream)))
