(in-package #:poslin)

(defmacro new-poslin ()
  `(alet ,+registers+
     (setf path (list (make-stack :name 'root)))
     (setf dtable (make-hash-table :test 'eq))
     (install-prims)
     (install-stdlib)
     (plambda (v)
	 ,+registers+
       (poslin-handle))))