(in-package #:poslin)

(defprim >> nil
    "( filename -- ??? )"
  (args (filename)
    (if (stringp filename)
	(poslin-eval (poslin-read-file filename))
	(poslin-error malformed-filename
		      "Tried to load file ~A"
		      filename))))