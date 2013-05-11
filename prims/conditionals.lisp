(in-package #:poslin)

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