(in-package #:poslin)

(defstruct [env]
  (content (fset:empty-map)
	   :type fset:map)
  (parent nil
	  :type (or [env] null)))

(defmethod print-object ((object [env])
                         stream)
  (format stream "#<[env] ~A ~A>"
          (fset:size ([env]-content object))
          ([env]-parent object)))

(defun <root-env> (content)
  (make-[env] :content content))

(defun <env> (content parent)
  (make-[env] :content content
	      :parent parent))

(defmethod lookup ((collection [env])
		   (key symbol))
  (aif (@ ([env]-content collection)
	  key)
       it
       (aif ([env]-parent collection)
	    (@ it key)
	    <meta-nothing>)))

(defmethod compare ((x [env])
                    (y [env]))
  (if (and (fset:equal? ([env]-content x)
                        ([env]-content y))
           (fset:equal? ([env]-parent x)
                        ([env]-parent y)))
      :equal
      :unequal))

(defgeneric insert (collection key value)
  (:method ((collection [env])
	    (key symbol)
	    (value [binding]))
    (<env> (with ([env]-content collection)
		 key value)
	   ([env]-parent collection))))

(defgeneric get-parent (hierarchical)
  (:method ((hierarchical [env]))
    (aif ([env]-parent hierarchical)
	 it
	 <meta-nothing>)))

(defgeneric set-parent (hierarchical parent)
  (:method ((hierarchical [env])
	    (parent [env]))
    (<env> ([env]-content hierarchical)
	   parent)))

(defgeneric drop (collection key)
  (:method ((collection [env])
            (key symbol))
    (<env> (less ([env]-content collection)
                 key)
           ([env]-parent collection))))
