(in-package #:poslin)

(defprim >> nil
    "( filename -- ??? )"
  (args (filename)
    (if (stringp filename)
	(aif (poslin-read-file filename)
	     (poslin-eval it)
	     (poslin-error "No file ~A found in ~A"
			   filename folder))
	(poslin-error malformed-filename
		      "Tried to load file ~A"
		      filename))))