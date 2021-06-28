(in-package #:poslin)

(defun <root> (dict)
  #.+optimization-parameters+
  (declare (type map dict))
  (the [binding]
    (binding (list dict))))

(defun path-push (dict path)
  #.+optimization-parameters+
  (declare (type map dict)
           (type [binding] path))
  (push dict ([binding]-value path)))

(defun path-pop (path)
  #.+optimization-parameters+
  (declare (type [binding] path))
  (the map
    (pop ([binding]-value path))))

(defun path-top (path)
  #.+optimization-parameters+
  (declare (type [binding] path))
  (the map
    (car ([binding]-value path))))

(defun path-nth (path integer)
  #.+optimization-parameters+
  (declare (type [binding] path)
           (type integer integer))
  (the map
    (if (< integer 0)
        (error "Negative path access")
        (aif (nth integer ([binding]-value path))
             it
             (error "Path bottom error")))))

(defun path-get (path symbol)
  #.+optimization-parameters+
  (declare (type [binding] path)
           (type symbol symbol))
  (@ (path-top path)
     symbol))

(defun path-set (path dict)
  #.+optimization-parameters+
  (declare (type [binding] path)
           (type map dict))
  (setf ([binding]-value path)
        (cons dict (cdr ([binding]-value path)))))

(defun path-length (path)
  #.+optimization-parameters+
  (declare (type [binding] path))
  (the (integer (0))  ; putting an empty stack onto the path has undefined
                      ; consequences
    (length (the (or cons null)
              ([binding]-value path)))))
