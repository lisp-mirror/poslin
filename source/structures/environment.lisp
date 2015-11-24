(in-package #:poslin)

(defun <root-env> (content)
  (list (fset:empty-map)))

(defun <env> (content parent)
  (list* content parent))

(defmethod lookup ((collection null)
                   key)
  (values nil nil))

(defmethod lookup ((collection cons)
                   key)
  (multiple-value-bind (result found?)
      (lookup (car collection)
              key)
    (if found?
        result
        (lookup (cdr collection)
                key))))

(defgeneric insert (collection key value)
  (:method ((collection cons)
            key value)
    (cons (with (car collection)
                key value)
          (cdr collection))))

(defgeneric get-parent (hierarchical)
  (:method ((hierarchical cons))
    (aif (cdr hierarchical)
         it
         <meta-nothing>)))

(defgeneric set-parent (hierarchical parent)
  (:method ((hierarchical cons)
            (parent (eql '<meta-nothing>)))
    (list (car hierarchical)))
  (:method ((hierarchical cons)
            (parent cons))
    (cons (car hierarchical)
          parent)))

(defgeneric drop (collection key)
  (:method ((collection cons)
            key)
    (cons (less (car collection)
                key)
          (cdr collection))))
