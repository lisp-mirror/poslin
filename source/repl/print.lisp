(in-package #:poslin)

(defgeneric poslin-print (object stream)
  (:method (object stream)
    (format stream "~S"
	    object)))

(defmethod poslin-print ((object null)
			 stream)
  (format stream "[ ]"))

(defmethod poslin-print ((object cons)
			 stream)
  (format stream "[~{ ~A~} ]"
	  (mapcar (lambda (obj)
		    (poslin-print obj nil))
		  (reverse object))))

(defmethod poslin-print ((object array)
                         stream)
  (format stream "a[~{ ~A~} ]a"
          (map 'list (lambda (obj)
                       (poslin-print obj nil))
               object)))

(defun make-delimiter-string (string)
  (labels ((_rec (acc)
             (let* ((char (code-char (+ (random (- (char-code #\z)
                                                   (char-code #\a)))
                                        (char-code #\a))))
                    (nstring (concatenate 'string
                                          acc (string char))))
               (if (search nstring string)
                   (_rec nstring)
                   nstring))))
    (_rec " :")))

(defmethod poslin-print ((object string)
                         stream)
  (if (search "\" " object)
      (let ((delim (make-delimiter-string object)))
        (format stream "$~A~%~A~A"
                delim object delim))
      (format stream "\"~A\""
              object)))

(defmethod poslin-print ((object symbol)
			 stream)
  (format stream "~A"
	  (symbol-name object)))

(defmethod poslin-print ((object [env])
			 stream)
  (aif ([env]-parent object)
       (format stream "env<~A><~A>"
	       (poslin-print (fset:arb ([env]-content object))
			     nil)
	       (poslin-print it nil))
       (format stream "env<~A>"
	       (poslin-print (fset:arb ([env]-content object))
			     nil))))

(defmethod poslin-print ((object [binding])
			 stream)
  (format stream "b<..>"))

(defmethod poslin-print ((object (eql <noop>))
			 stream)
  (format stream "P{.}"))

(defmethod poslin-print ((object <constant>)
			 stream)
  (format stream "#{~A}"
	  (poslin-print (<constant>-val object)
			nil)))

(defmethod poslin-print ((object <prim>)
			 stream)
  (format stream "P{~A}"
	  (<prim>-name object)))

(defmethod poslin-print ((object <thread>)
			 stream)
  (format stream "{~A ~A}"
	  (poslin-print (<thread>-front object)
			nil)
	  (let* ((b (<thread>-back object))
		 (bp (poslin-print b nil)))
	    (if (<thread>-p b)
		(subseq bp 1 (1- (length bp)))
		bp))))

(defmethod poslin-print ((object (eql <meta-nothing>))
			 stream)
  (format stream "<NOTHING>"))

(defun show-env (env &optional (envmap (fset:empty-map)))
  (let ((found (lookup envmap env)))
    (if found
        (aif ([env]-parent env)
             (multiple-value-bind (n m)
                 (show-env it envmap)
               (values (format nil "e~A:~A"
                               found n)
                       m))
             (values (format nil "e~A"
                             found)
                     envmap))
        (let* ((envnum (fset:size envmap))
               (nenvmap (with envmap env envnum)))
          (aif ([env]-parent env)
               (multiple-value-bind (n m)
                   (show-env it nenvmap)
                 (values (format nil "e~A:~A"
                                 envnum n)
                         m))
               (values (format nil "e~A"
                               envnum)
                       nenvmap))))))

(defun show-path ([path] &optional (envmap (fset:empty-map)))
  (aif ([path]-parent [path])
       (multiple-value-bind (en em)
           (show-env ([path]-content [path])
                     envmap)
         (concatenate 'string
                      en " " (show-path it em)))
       (show-env ([path]-content [path])
                 envmap)))

(defmacro print-status ()
  `(progn
     (format t "Return Stack:~%~A~%
Path:~%~A~%
Program Counter:~%~A~%
Stack:~%~A~%"
             (poslin-print rstack nil)
             (show-path path)
             (poslin-print pc nil)
             (poslin-print (stack path)
                           nil))))
