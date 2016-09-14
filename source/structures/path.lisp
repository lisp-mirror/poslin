(in-package #:poslin)

(defun <root> (dict)
  (binding (list dict)))

(defun path-push (dict path)
  (push dict ([binding]-value path)))

(defun path-pop (path)
  (pop ([binding]-value path)))

(defun path-top (path)
  (car ([binding]-value path)))

(defun path-nth (path integer)
  (if (< integer 0)
      (error "Negative path access")
      (aif (nth integer ([binding]-value path))
           it
           (error "Path bottom error"))))

(defun path-get (path symbol)
  (aif (@ (path-top path)
          symbol)
       it
       <meta-nothing>))

(defun path-set (path dict)
  (path-push dict (path-pop path)))

(defun path-length (path)
  (length ([binding]-value path)))
