(in-package #:poslin)

(defprim ?b nil
    "( binding -- value )"
  (args (binding)
    (if (binding-p binding)
	(push (binding-val binding)
	      (pstack-content (car path)))
	(poslin-error 'malformed-binding
		      "Attempt to set value of ~A"
		      binding))))

(defprim ?d nil
    "( binding -- docstring )"
  (args (binding)
    (if (binding-p binding)
	(push (binding-doc binding)
	      (pstack-content (car path)))
	(poslin-error 'malformed-binding
		      "Attempt to set docstring of ~A"
		      binding))))

(defprim @b nil
    "( binding value -- )"
  (args (binding value)
    (if (binding-p binding)
	(setf (binding-val binding)
	      value)
	(poslin-error 'malformed-binding
		      "Attempt to set value of ~A to ~A"
		      binding value))))

(defprim @d nil
    "( binding docstring -- )"
  (args (binding docstring)
    (if (binding-p binding)
	(if (stringp docstring)
	    (setf (binding-doc binding)
		  docstring)
	    (poslin-error 'malformed-string
			  "Attempt to set docstring of ~A to ~A"
			  binding docstring))
	(poslin-error 'malformed-binding
		      "Attempt to set docstring of ~A to ~A"
		      binding docstring))))

(defprim b* nil
    "( -- binding )"
  (push (make-binding)
	(pstack-content (car path))))