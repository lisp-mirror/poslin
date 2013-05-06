(in-package #:poslin)

(defparameter *prims* '())

;;; No operation
(defprim || nil
  ;; Does nothing
  ;; ( -- )
  )

;;; Calling
(defprim ! t
  ;; Calls operation on top of stack
  ;; ( op -- ??? )
  (let* ((callable (pop-curr))
	 (op (callable->thread callable this)))
    (if (empty? op)
	(error "No operation ~A"
	       callable)
	(progn
	  (push op pc)
	  (interpreter)))))

(defprim & t
  ;; Puts thread of word on top onto stack
  ;; ( op -- thread )
  (let* ((callable (pop-curr))
	 (op (callable->thread callable this)))
    (if (empty? op)
	(error "No operation ~A"
	       callable)
	(push-curr (cons 'thread op)))))

;;; Write
(defprim @n nil
  ;; Sets the name of the current stack
  ;; ( name -- )
  (let ((name (pop-curr)))
    (if (and (symbolp name)
	     (not (eq name nil)))
	(setf (curr-name)
	      name)
	(error "Attempt to set stack name to ~A"
	       name))))

(defprim @o nil
  ;; Sets word in operation environment to thread of top
  ;; ( name op -- )
  (let ((val (callable->thread (pop-curr)
			       this)))
    (setf (lookup-op (pop-curr))
	  val)))

(defprim @o_ nil
  ;; Removes word from operation environment (THIS DOES NOT LOOK INTO
  ;; PARENT ENVIRONMENTS!)
  ;; ( name -- )
  (remhash (pop-curr)
	   (curr-opdefs)))

(defprim @i+ nil
  ;; Sets word to be immediate
  ;; (name -- )
  (setf (immediate? (pop-curr))
	t))

(defprim @i_ nil
  ;; Unsets immediateness (it is looked up in the parent environment
  ;; from now on)
  ;; ( name -- )
  (remhash (pop-curr)
	   (curr-imm)))

(defprim @i- nil
  ;; Sets word to not be immediate
  ;; ( name -- )
  (setf (immediate? (pop-curr))
	nil))

(defprim @v nil
  ;; Sets variable
  ;; ( name value -- )
  (let ((val (pop-curr)))
    (setf (lookup-var (pop-curr))
	  val)))

(defprim @v_ nil
  ;; Unsets variable
  ;; ( name -- )
  (remhash (pop-curr)
	   (curr-vardefs)))

(defprim @eo nil
  ;; Sets operation environment of stack
  ;; ( [ ... ] op-env -- )
  (let ((env (pop-curr)))
    (if (op-env-p env)
	(setf (stack-ops (pop-curr))
	      env)
	(error "Attempt to set ~A as operation environment"
	       env))))

(defprim @ev nil
  ;; Sets variable environment of stack
  ;; ( [ ... ] var-env -- )
  (let ((env (pop-curr)))
    (if (var-env-p env)
	(setf (stack-vars (pop-curr))
	      env)
	(error "Attempt to set ~A as variable environment"
	       env))))

;;; Read
(defprim ?n nil
  ;; Gets name of current stack
  ;; ( -- name )
  (push-curr (curr-name)))

(defprim ?o nil
  ;; Gets thread from word
  ;; ( name -- thread )
  (let* ((name (pop-curr))
	 (found (lookup-op name)))
    (if (empty? found)
	(error "No operation ~A"
	       name)
	(push-curr found))))

(defprim ?v nil
  ;; Gets value from word
  ;; ( name -- val )
  (let* ((name (pop-curr))
	 (found (lookup-var name)))
    (if (empty? found)
	(error "~A is not set"
	       name)
	(push-curr found))))

(defprim ?eo nil
  ;; Gets operation environment of stack
  ;; ( [ ... ] -- op-env )
  (push-curr (stack-ops (pop-curr))))

(defprim ?ev nil
  ;; Gets variable environment of stack
  ;; ( [ ... ] -- var-env )
  (push-curr (stack-vars (pop-curr))))

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
  (push-curr (pop path)))

(defprim <- nil
  ;; Push onto stack, discarding stack
  ;; ( [ ... ] a -- )
  (push (pop-curr)
	(stack-content (pop-curr))))

(defprim -> nil
  ;; Pop from stack
  ;; ( [ ... a ] -- a )
  (push-curr (pop (stack-content (pop-curr)))))

(defprim %[ t
  ;; Open stack on top
  ;; ( [ ... ] -- [ ... )
  (let ((stack (pop-curr)))
    (if (stack-p stack)
	(push stack path)
	(error "Attempt to open ~A"
	       stack))))

(defprim ^ nil
  ;; Get nth stack in path
  ;; ( n -- [ ... ] )
  (aif (nth (pop-curr)
	    path)
       (push-curr it)
       (error "Path exceeded")))

(defprim § nil
  ;; Duplicate top of stack
  ;; ( n -- n )
  (aif (curr-stack)
       (push-curr (car it))
       (error "Attempt to duplicate bottom")))

(defprim <> nil
  ;; Swap two elements on top of stack
  ;; ( a b -- b a )
  (if #2=(cdr #1=(curr-stack))
      (rotatef (car #1#)
	       (car #2#))
      (error "Attempt to swap with bottom")))

(defprim _ nil
  ;; Drop top of stack
  ;; ( a -- )
  (if (curr-stack)
      (pop-curr)
      (error "Attempt to drop bottom")))

(defprim ?_ nil
  ;; Return T when at bottom of stack, NIL otherwise
  ;; ( [ -- [ t )
  ;; ( [ ... -- [ ... nil )
  (push-curr (not (curr-stack))))

;;; Input Output
(defprim >> nil
  ;; Load poslin file
  ;; ( filename -- ??? )
  (with-open-file (stream (pop-curr))
    (let ((eof (gensym "eof")))
      (do ((curr (read stream nil eof)))
	  ((eq curr eof))
	(funcall this curr)))))

(defprim o< nil
  ;; Push onto output
  ;; ( n -- )
  (push (pop-curr)
	out))

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
		 (error "Attempt to pop empty rstack"))))

(defprim >r nil
  ;; Push on top of rstack
  ;; ( n -- )
  (push (pop-curr)
	rstack))

;;; pc manipulation
(defprim pc> nil
  ;; Pop top of program counter
  ;; ( -- n )
  (push-curr (if pc
		 (pop pc)
		 (error "Tried to pop empty pc"))))

(defprim >pc nil
  ;; Push onto program counter
  ;; ( n -- )
  (push (pop-curr)
	pc))

;;; conditionals
(defnprim ?=> nil
  ;; Only usable in thread
  ;; If top of current stack T, execute next and discard rest of
  ;; thread, if NIL jump over next
  ;; ( T ?=> a ... -- a )
  ;; ( NIL ?=> a ... -- ... )
  (setf pc
	(if (pop-curr)
	    (list (cadr pc))
	    (cddr pc))))

(defprim <?> nil
  ;; If third to last is nil, leave last, else leave second to last
  ;; ( T a b <?> -- a )
  ;; ( NIL a b <?> -- b )
  (let ((else (pop-curr))
	(then (pop-curr)))
    (push-curr (if (pop-curr)
		   then
		   else))))

;;; Printing
(defnprim print nil
  ;; Print top of current stack
  ;; ( n -- )
  (print (pop-curr)))

;;; Inspection
(defprim ?ops t
  ;; Print operations in current environment
  ;; ( -- )
  (labels ((rec (env acc)
	     (if (not env)
		 acc
		 (let ((acc (copy-list acc)))
		   (maphash (lambda (k v)
			      (declare (ignore v))
			      (pushnew k acc))
			    (op-env-defs env))
		   (rec (op-env-par env)
			acc)))))
    (format t "~&~{~S ~}"
	    (rec (curr-op-env)
		 '()))))