(in-package #:poslin)

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