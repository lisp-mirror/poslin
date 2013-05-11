(in-package #:poslin)

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