;;;;; TODO: Add printer for boolean values that doesn't make them look like
;;;;; characters.
(in-package #:poslin)

(defgeneric poslin-print (object stream)
  (:method (object stream)
    #.+optimization-parameters+
    (format stream "~S"
	    object)))

(defmethod poslin-print ((object null)
			 stream)
  #.+optimization-parameters+
  (format stream "[]"))

(defmethod poslin-print ((object double-float)
                         stream)
  #.+optimization-parameters+
  (format stream "~A"
          (let* ((printed (format nil "~S"
                                  object))
                 (length (length printed)))
            (if (string= (subseq printed (- length 2)
                                 length)
                         "d0")
                (subseq printed 0 (- length 2))
                (cl:map 'string (lambda (char)
                                  (if (char= char #\d)
                                      #\e
                                      char))
                        printed)))))

(defmethod poslin-print ((object cons)
			 stream)
  #.+optimization-parameters+
  (format stream "[~{ ~A~} ]"
	  (mapcar (lambda (obj)
                    #.+optimization-parameters+
		    (poslin-print obj nil))
		  (reverse object))))

(defmethod poslin-print ((object set)
                         stream)
  #.+optimization-parameters+
  (format stream "[~{ ~A~} ]set"
          (mapcar (lambda (obj)
                    #.+optimization-parameters+
                    (poslin-print obj nil))
                  (fset:convert 'list
                                object))))

(defmethod poslin-print ((object map)
                         stream)
  #.+optimization-parameters+
  (format stream "[~{ ~A~} ]dict"
          (mapcar (lambda (obj)
                    #.+optimization-parameters+
                    (format nil "[ ~A ~A ]"
                            (poslin-print (car obj)
                                          nil)
                            (poslin-print (cdr obj)
                                          nil)))
                  (fset:convert 'list
                                object))))

(defmethod poslin-print ((object array)
                         stream)
  #.+optimization-parameters+
  (format stream "[~{ ~A~} ]array"
          (cl:map 'list (lambda (obj)
                          #.+optimization-parameters+
                          (poslin-print obj nil))
                  object)))

(defun make-delimiter-string (string)
  #.+optimization-parameters+
  (labels ((_rec (acc)
             #.+optimization-parameters+
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
  #.+optimization-parameters+
  (if (search "\" " object)
      (let ((delim (make-delimiter-string object)))
        (format stream "$~A~%~A~A"
                delim object delim))
      (format stream "\"~A\""
              object)))

(defmethod poslin-print ((object character)
                         stream)
  #.+optimization-parameters+
  (cond
    ((char= object #\Newline)
     (format stream "<newline>"))
    ((char= object #\Tab)
     (format stream "<tab>"))
    (t
     (format stream "<~A>"
             (substitute #\- #\Space
                         (string-downcase (cl-unicode:unicode-name object)))))))

(defmethod poslin-print ((object symbol)
			 stream)
  #.+optimization-parameters+
  (format stream "~A"
	  (symbol-name object)))

(defparameter *binding-numbers*
  (empty-map))
(declaim (type map *binding-numbers*))
(defparameter *binding-counter*
  0)
(declaim (type (integer 0)
               *binding-counter*))
(defmethod poslin-print ((object [binding])
			 stream)
  #.+optimization-parameters+
  (aif (@ *binding-numbers* object)
       (format stream "b<~A>"
               it)
       (progn
         (setf *binding-numbers*
               (with *binding-numbers*
                     object (incf *binding-counter*)))
         (format stream "b<~A>"
                 *binding-counter*))))

(defmethod poslin-print ((object (eql <noop>))
			 stream)
  #.+optimization-parameters+
  (format stream "P{.}"))

(defmethod poslin-print ((object <constant>)
			 stream)
  #.+optimization-parameters+
  (format stream "#{~A}"
	  (poslin-print (<constant>-val object)
			nil)))

(defmethod poslin-print ((object <prim>)
			 stream)
  #.+optimization-parameters+
  (format stream "P{~A}"
	  (<prim>-name object)))

(defmethod poslin-print ((object <thread>)
			 stream)
  #.+optimization-parameters+
  (format stream "{~A ~A}"
	  (poslin-print (<thread>-front object)
			nil)
	  (let* ((b (<thread>-back object))
		 (bp (poslin-print b nil)))
	    (if (<thread>-p b)
		(subseq bp 1 (1- (length bp)))
		bp))))

(defmethod poslin-print ((object <handled>)
                         stream)
  #.+optimization-parameters+
  (format stream "~A>>~A"
          (poslin-print (<handled>-thread object)
                        nil)
          (poslin-print (<handled>-handle object)
                        nil)))

(defmethod poslin-print ((object [exception])
                         stream)
  #.+optimization-parameters+
  (format stream "[[EXCEPTION ~A ~A+~D ~A]]"
          (poslin-print ([exception]-data object)
                        nil)
          (poslin-print (first ([exception]-stack object))
                        nil)
          (1- (length ([exception]-stack object)))
          (poslin-print ([exception]-string object)
                        nil)))

(defmethod poslin-print ((object (eql <meta-nothing>))
			 stream)
  #.+optimization-parameters+
  (format stream "<<NOTHING>>"))

(defmethod poslin-print ((object stream)
                         stream)
  #.+optimization-parameters+
  (format stream "<<stream>>"))

(defmethod show-path (([path] cons)
                      &optional (stream t))
  #.+optimization-parameters+
  (format stream "[~{ ~A~%~}]"
          (nreverse (mapcar (lambda (dict)
                              (poslin-print dict nil))
                            [path]))))

(defmethod show-path (([path] [binding])
                      &optional (stream t))
  #.+optimization-parameters+
  (show-path ([binding]-value [path])
             stream))

(defmethod show-path ([path] &optional (stream t))
  #.+optimization-parameters+
  (format stream "!CORRUPTED PATH!"))

(defmacro print-status ()
  `(progn
     (format t "~%
======================================================================
Return Stack:~%~A~%
Program Counter:~%~A~%
Path:~%~A~%
Stack:~%~A~%"
             (poslin-print (rstack)
                           nil)
             (poslin-print pc nil)
             (show-path path nil)
             (poslin-print (stack path)
                           nil))))
