(in-package #:poslin)

(defprim ! t
  ;; Calls operation on top of stack
  ;; ( op -- ??? )
  (let* ((callable (pop-curr))
	 (thread (callable->thread callable this)))
    (if (empty? thread)
	(perror undefined-operation "No operation ~A"
		(formatt callable))
	(progn
	  (push thread pc)
	  (interpreter)))))
	

(defprim & t
  ;; Puts thread of word on top onto stack
  ;; ( op -- thread )
  (let* ((callable (pop-curr))
	 (op (callable->thread callable this)))
    (if (empty? op)
	(perror undefined-operation "No operation ~A"
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
			(lookup-opb val))))))