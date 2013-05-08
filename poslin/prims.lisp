(in-package #:poslin)

(defparameter *prims* '())

;;; No operation
(defprim || nil
  ;; Does nothing
  ;; ( -- )
  )

;;; Error
(defprim error t
  (args (type text)
    (push-curr (lambda ()
		 (let ((rpc pc)
		       (rrs rstack))
		   (setf pc '())
		   (setf rstack '())
		   (format t "~&POSLIN-~A-ERROR~% Remaining PC: ~A~% ~
                              Remaining RSTACK: ~A~%  ~A"
			   type (formatt rpc)
			   (formatt rrs)
			   text))))))

;;; Calling
(defprim ! t
  ;; Calls operation on top of stack
  ;; ( op -- ??? )
  (let* ((callable (pop-curr))
	 (thread (callable->thread callable this)))
    (if (empty? thread)
	(perror undefined-operation "No operation ~S"
		callable)
	(progn
	  (push thread pc)
	  (interpreter)))))
	

(defprim & t
  ;; Puts thread of word on top onto stack
  ;; ( op -- thread )
  (let* ((callable (pop-curr))
	 (op (callable->thread callable this)))
    (if (empty? op)
	(perror undefined-operation "No operation ~S"
		callable)
	(push-curr (cons 'thread op)))))

(defprim ?& nil
  ;;
  ;; ( val -- b )
  (args (val)
    (push-curr (or (functionp val)
		   (and (consp val)
			(member (car val)
				'(quote thread lisp)
				:test #'symbol=))
		   (and (symbolp val)
			(not (empty? (lookup-op val))))))))

;;; Write
(defprim @n nil
  ;; Sets the name of the stack
  ;; ( [] name -- )
  (args (stack name)
    (if (symbolp name)
	(if (stack-p stack)
	    (setf (stack-name stack)
		  name)
	    (perror "Attempt to set name of ~S to ~S"
		    stack name))
	(perror "Attempt to set name of stack ~S to ~S"
		stack name))))

(defprim @o nil
  ;; Sets word in operation environment to thread of top
  ;; ( op-env name op -- )
  (let* ((op (pop-curr))
	 (thread (callable->thread op this))
	 (name (pop-curr))
	 (op-env (pop-curr)))
    (if (empty? thread)
	(perror undefined-operation "No operation ~S"
		name)
	(if (symbolp name)
	    (if (op-env-p op-env)
		(setf (gethash name (op-env-defs op-env))
		      thread)
		(perror malformed-op-env
			"Attempt to set operation in ~S"
			op-env))
	    (perror malformed-op-name
		    "Attempt to set name ~S in operation environment"
		    name)))))

(defprim @o_ nil
  ;; Removes word from operation environment
  ;; ( op-env name -- )
  (args (op-env name)
    (if (symbolp name)
	(if (op-env-p op-env)
	    (remhash name (op-env-defs op-env))
	    (perror malformed-op-env
		    "Attempt to remove operation definition ~S from ~
                     ~S"
		    name op-env))
	(perror malformed-op-name
		"Attempt to remove operation definition ~S from ~
                 operation environment ~S"
		name op-env))))

(defprim @i+ nil
  ;; Sets word to be immediate
  ;; ( op-env name -- )
  (args (op-env name)
    (if (symbolp name)
	(if (op-env-p op-env)
	    (setf (gethash name (op-env-imm op-env))
		  t)
	    (perror malformed-op-env
		    "Attempt to set immediateness of ~S in operation ~
                     environment ~S"
		    name op-env))
	(perror malformed-op-name
		"Attempt to set immediateness of ~S in operation ~
                 environment ~S"
		name op-env))))

(defprim @i_ nil
  ;; Unsets immediateness (it is looked up in the parent environment
  ;; from now on)
  ;; ( op-env name -- )
  (args (op-env name)
    (if (symbolp name)
	(if (op-env-p op-env)
	    (remhash name (op-env-imm op-env))
	    (perror malformed-op-env
		    "Attempt to remove immediateness of ~S in ~
                     operation environment ~S"
		    name op-env))
	(perror malformed-op-name
		"Attempt to remove immediateness of ~S in operation ~
                 environment ~S"
		name op-env))))

(defprim @i- nil
  ;; Sets word to not be immediate
  ;; ( op-env name -- )
  (args (op-env name)
    (if (symbolp name)
	(if (op-env-p op-env)
	    (setf (gethash name (op-env-imm op-env))
		  nil)
	    (perror malformed-op-env
		    "Attempt to set immediateness of ~S in operation ~
                     environment ~S"
		    name op-env))
	(perror malformed-op-name
		"Attempt to set immediateness of ~S in operation ~
                 environment ~S"
		name op-env))))

(defprim @v nil
  ;; Sets variable
  ;; ( var-env name value -- )
  (args (var-env name value)
    (if (symbolp name)
	(if (var-env-p var-env)
	    (setf (find-var name var-env)
		  value)
	    (perror malformed-var-env
		    "Attempt to set variable ~A in ~A"
		    name var-env))
	(perror malformed-var-name
		"Attempt to use ~A as variable name in ~A"
		name var-env))))

(defprim @v_ nil
  ;; Unsets variable
  ;; ( var-env name -- )
  (args (var-env name)
    (if (symbolp name)
	(if (var-env-p var-env)
	    (remhash name (var-env-defs var-env))
	    (perror malformed-var-env
		    "Attempt to unset ~A in ~A"
		    name var-env))
	(perror malformed-var-name
		"Attempt to unset ~A in variable environment ~A"
		name var-env))))

(defprim @eo nil
  ;; Sets operation environment of stack
  ;; ( [ ... ] op-env -- )
  (args (stack op-env)
    (if (op-env-p op-env)
	(if (stack-p stack)
	    (setf (stack-ops stack)
		  op-env)
	    (perror malformed-stack
		    "Attempt to set ~S as operation environment of ~S"
		    op-env stack))
	(perror malformed-op-env
		"Attempt to set ~S as operation environment of stack ~S"
		op-env stack))))

(defprim @eo+ nil
  ;; Makes child operation environment
  ;; ( op-env-par -- op-env-child )
  (args (op-env-par)
    (if (or (op-env-p op-env-par)
	    (null op-env-par))
	(push-curr (make-op-env :par op-env-par))
	(perror malformed-op-env
		"Attempt to set ~S as parent of operation environment"
		op-env-par))))

(defprim @ev nil
  ;; Sets variable environment of stack
  ;; ( [ ... ] var-env -- )
  (args (stack env)
    (if (var-env-p env)
	(if (stack-p stack)
	    (setf (stack-vars stack)
		  env)
	    (perror malformed-stack
		    "Attempt to set ~S as variable environment of ~S"
		    env stack))
	(perror malformed-var-env
		"Attempt to set ~S as variable environment of stack ~S"
		env stack))))

(defprim @ev+ nil
  ;; Makes child variable environment
  ;; ( var-env-par -- var-env-child )
  (args (var-env-par)
    (if (or (var-env-p var-env-par)
	    (null var-env-par))
	(push-curr (make-var-env :par var-env-par))
	(perror malformed-var-env
		"Attempt to set ~S as parent of variable environment"
		var-env-par))))

;;; Read
(defprim ?n nil
  ;; Gets name of stack
  ;; ( [] -- name )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-name stack))
	(perror malformed-stack
		"Attempt to get name of ~S"
		stack))))

(defprim ?o nil
  ;; Gets thread from word
  ;; ( op-env name -- thread )
  (args (op-env name)
    (if (symbolp name)
	(if (op-env-p op-env)
	    (multiple-value-bind (thread found?)
		(gethash name (op-env-defs op-env))
	      (if found?
		  (push-curr thread)
		  (perror undefined-operation "No operation ~S"
			  name)))
	    (perror malformed-op-env
		    "Attempt to lookup operation ~S in ~S"
		    name op-env))
	(perror malformed-op-name
		"Attempt to lookup operation ~S in ~S"
		name op-env))))

(defprim ?v nil
  ;; Gets value from word
  ;; ( var-env name -- val )
  (args (var-env name)
    (if (symbolp name)
	(if (var-env-p var-env)
	    (multiple-value-bind (val found?)
		(gethash name (var-env-defs var-env))
	      (if found?
		  (push-curr val)
		  (perror unbound-var "Variable ~S is unbound in ~S"
			  name var-env)))
	    (if (null var-env)
		(perror unbound-var "Variable ~S is unbound in ~S"
			name var-env)
		(perror malformed-var-env
			"Attempt to lookup ~S in ~S"
			name var-env)))
	(perror malformed-var-name
		"Attempt to lookup ~S in variable environment ~S"
		name var-env))))

(defprim ?eo nil
  ;; Gets operation environment of stack
  ;; ( [] -- op-env )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-ops stack))
	(perror malformed-stack
		"Attempt to get operation environment of ~S"
		stack))))

(defprim ?eo- nil
  ;; Gets parent operation environment
  ;; ( op-env-child -- op-env-par)
  (args (op-env-child)
    (if (op-env-p op-env-child)
	(push-curr (op-env-par op-env-child))
	(perror malformed-op-env
		"Attempt to get parent operation environment of ~S"
		op-env-child))))

(defprim ?ev nil
  ;; Gets variable environment of stack
  ;; ( [] -- var-env )
  (args (stack)
    (if (stack-p stack)
	(push-curr (stack-vars stack))
	(perror malformed-stack
		"Attempt to get variable environment of ~S"
		stack))))

(defprim ?ev- nil
  ;; Gets parent variable environment
  ;; ( var-env-child -- var-env-par)
  (args (var-env-child)
    (if (var-env-p var-env-child)
	(push-curr (var-env-par var-env-child))
	(perror malformed-var-env
		"Attempt to get parent variable environment of ~S"
		var-env-child))))

;;; Stack operations
(defprim [ t
  ;; Open a new stack
  ;; ( -- [ )
  (push (make-stack :vars (make-var-env :par (curr-var-env))
		    :ops (make-op-env :par (curr-op-env)))
	path))

(defprim ] t
  ;; Close a stack and leave it on it's parent stack
  ;; ( [ ... -- [ ... ] )
  (if (cdr path)
      (push-curr (pop path))
      (perror path
	      "Attempt to close root stack")))

(defprim %[ t
  ;; Open stack on top of stack
  ;; ( [ ... ] -- [ ... )
  (args (stack)
    (if (stack-p stack)
	(push stack path)
	(perror malformed-stack "Attempt to open ~S"
		stack))))

(defprim ]] t
  ;; Close all paths up to the one having the name on top
  ;; ( name -- ??? )
  (args (name)
    (aif (member name path
		 :key #'stack-name
		 :test #'string=)
	 (setf path it)
	 (perror path "No stack named ~S in path"
		 name))))

(defprim <- nil
  ;; Push onto stack
  ;; ( [ ... ] a -- )
  (args (stack val)
    (if (stack-p stack)
	(push val (stack-content stack))
	(perror malformed-stack
		"Attempt to push ~S onto ~S"
		val stack))))

(defprim -> nil
  ;; Pop from stack
  ;; ( [ ... a ] -- a )
  (args (stack)
    (if (stack-p stack)
	(if (stack-content stack)
	    (push-curr (pop (stack-content stack)))
	    (perror bottom "Attempt to pop bottom"))
	(perror malformed-stack "Attempt to pop from ~A"
		stack))))

(defprim %[ t
  ;; Open stack on top
  ;; ( [ ... ] -- [ ... )
  (args (stack)
    (if (stack-p stack)
	(push stack path)
	(perror malformed-stack "Attempt to open ~A"
		stack))))

(defprim ^ nil
  ;; Get nth stack in path
  ;; ( n -- [ ... ] )
  (args (n)
    (aif (nth n path)
	 (push-curr it)
	 (perror path "Path exceeded"))))

(defprim § nil
  ;; Duplicate top of stack
  ;; ( n -- n n )
  (aif (curr-stack)
       (push-curr (car it))
       (perror bottom "Attempt to duplicate bottom")))

(defprim <> nil
  ;; Swap two elements on top of stack
  ;; ( a b -- b a )
  (if (cdr (curr-stack))
      (rotatef (car (curr-stack))
	       (cadr (curr-stack)))
      (perror bottom "Attempt to swap with bottom")))

(defprim _ nil
  ;; Drop top of stack
  ;; ( a -- )
  (if (curr-stack)
      (pop-curr)
      (perror bottom "Attempt to drop bottom")))

(defprim ?_ nil
  ;; Return T when at bottom of stack, NIL otherwise
  ;; ( [ ] -- t )
  ;; ( [ ... ] -- nil )
  (args (stack)
    (if (stack-p stack)
	(push-curr (null (stack-content stack)))
	(perror malformed-stack "Attempt to test for bottom of ~A"
		stack))))

;;; Input Output
(defprim >> nil
  ;; Load poslin file
  ;; ( filename -- ??? )
  (args (filename)
    (if (or (stringp filename)
	    (pathnamep filename))
	(with-open-file (stream filename)
	  (let ((eof (gensym "eof")))
	    (do ((curr (read stream nil eof)))
		((eq curr eof))
	      (funcall this curr))))
	(perror malformed-filename
		"Attempt to open file ~S"
		filename))))

(defprim >o nil
  ;; Push onto output
  ;; ( val -- )
  (args (val)
    (push val out)))

(defprim o_ nil
  ;; Delete output
  ;; ( -- )
  (setf out nil))

;;; rstack manipulation
(defprim r> nil
  ;; Pop top of rstack
  ;; ( -- n )
  (push-curr (if rstack
		 (pop rstack)
		 (perror rstack-bottom
			 "Attempt to pop empty rstack"))))

(defprim >r nil
  ;; Push on top of rstack
  ;; ( val -- )
  (args (val)
    (push val rstack)))

;;; pc manipulation
(defprim pc> nil
  ;; Pop top of program counter
  ;; ( -- n )
  (push-curr (if pc
		 (pop pc)
		 (perror pc-bottom "Tried to pop empty pc"))))

(defprim >pc nil
  ;; Push onto program counter
  ;; ( val -- )
  (args (val)
    (push val pc)))

;;; conditionals
(defnprim ?=> nil
  ;; Only usable in thread
  ;; If top of current stack T, execute next and discard rest of
  ;; thread, if NIL jump over next
  ;; ( T ?=> a ... -- a )
  ;; ( NIL ?=> a ... -- ... )
  (args (b)
    (setf pc
	  (if b
	      (list (cadr pc))
	      (cddr pc)))))

(defprim <?> nil
  ;; If third to last is nil, leave last, else leave second to last
  ;; ( T then else <?> -- a )
  ;; ( NIL then else <?> -- b )
  (args (test then else)
    (push-curr (if test
		   then
		   else))))

;;; Printing
(defprim print nil
  ;; Print top of current stack
  ;; ( val -- )
  (args (val)
    (if (symbolp val)
	(format t "~S"
		val)
	(format t "~A"
		val))))

(defprim newline nil
  ;; Print newline
  ;; ( -- )
  (format t "~%"))

;;; Numeric
(defunary
  exp abs oddp evenp sin cos tan asin acos atan)

(defbinary
  + - * / = /= < > <= >= expt log max min)

;;; Logic
(defunary
  not)

(defbinary
  and or)

;;; Lists
(defunary
  first rest)

(defbinary
  cons)

;;; General comparison
(defbinary
  eq)