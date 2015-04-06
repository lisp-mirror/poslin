(in-package #:poslin)

(defstruct [path]
  (content (<root-env> (fset:map))
	   :type [env])
  (parent nil
	  :type (or [path] null)))

(defun <root> (env)
  (make-[path] :content env))

(defun <path> (env path)
  (make-[path] :content env
	       :parent path))

(defun path-push (path env)
  (<path> env path))

(defun path-pop (path)
  ([path]-parent path))

(defun path-top (path)
  ([path]-content path))

(defun path-nth (path integer)
  (if (< integer 0)
      'negative-path-access-error
      (if (= integer 0)
	  (path-top path)
	  (aif (path-pop path)
	       (path-nth it (1- integer))
	       'path-bottom-error))))

(defun path-get (path symbol)
  (aif (@ (path-top path)
	  symbol)
       it
       <meta-nothing>))

(defun path-set (path env)
  (<path> env (path-pop path)))

(defun path-length (path)
  (if path
      (1+ (path-length ([path]-parent path)))
      0))
