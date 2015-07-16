(in-package #:poslin)

(define-parse-tree-synonym begin-word
    (:alternation (:positive-lookbehind :start-anchor)
		  (:positive-lookbehind :whitespace-char-class)))

(define-parse-tree-synonym end-word
    (:alternation (:positive-lookahead :end-anchor)
		  (:positive-lookahead :whitespace-char-class)))

(define-parse-tree-synonym sign?
    (:alternation #\+ #\- :void))

(define-parse-tree-synonym digits?
    (:greedy-repetition 0 nil :digit-class))

(define-parse-tree-synonym digits
    (:sequence :digit-class digits?))

(define-parse-tree-synonym anything
    (:alternation :everything #\Newline))

(define-parse-tree-synonym integer
    (:sequence begin-word sign? digits end-word))

(define-parse-tree-synonym float
    (:sequence begin-word sign? (:alternation
				 (:sequence digits #\. digits?)
				 (:sequence #\. digits))
	       end-word))

(define-parse-tree-synonym ratio
    (:sequence begin-word sign? (:sequence digits #\/ digits)
	       end-word))

(define-parse-tree-synonym quotation
    (:sequence begin-word #\'
	       (:greedy-repetition 1 nil :non-whitespace-char-class)
	       end-word))

(define-parse-tree-synonym symbol
    (:sequence begin-word
	       (:non-greedy-repetition 1 nil
				       :non-whitespace-char-class)
	       end-word))

(define-parse-tree-synonym long-string
    (:sequence
     begin-word #\$
     (:named-register "delimiter"
		      (:greedy-repetition 1 nil :everything))
     #\Newline (:non-greedy-repetition 0 nil anything)
     (:back-reference "delimiter")
     end-word))

(define-parse-tree-synonym short-string
    (:sequence
     begin-word #\"
     (:non-greedy-repetition 0 nil anything)
     #\" end-word))

(define-parse-tree-synonym infinite-string
    (:sequence
     begin-word #\$ #\Newline))

(define-parse-tree-synonym faulty-string
    (:sequence
     begin-word #\$ (:greedy-repetition 0 nil anything)
     :end-anchor))

(defparameter *parse-order*
  '(infinite-string long-string short-string faulty-string integer
    float ratio quotation symbol))

(defun extract-matches (parse-tree string)
  (let ((finds (all-matches parse-tree string)))
    (loop for (start end)
       on finds by #'cddr
       collect
	 (subseq string start end))))

(defun cut-matches (parse-tree string)
  (let ((finds (all-matches parse-tree string))
	(nstring (copy-seq string)))
    (loop for (start end)
       on finds by #'cddr
       do (setf nstring
		(substitute-if #\Space (constantly t)
			       nstring
			       :start start
			       :end end)))
    nstring))

(defun all-tokens (string parse-trees)
  (labels ((_rec (string pts acc)
	     (if pts
		 (let ((curr (first pts))
		       (rest (rest pts)))
		   (let ((found (extract-matches curr string))
			 (firsts (mapcar #'first
					 (group (all-matches curr
							     string)
						2)))
			 (nstring (cut-matches curr string)))
		     (_rec nstring rest (append (mapcar (lambda (a b)
							  (list a curr
								b))
							firsts found)
						acc))))
		 acc)))
    (mapcar #'rest
	    (sort (_rec string parse-trees '())
		  #'<
		  :key #'first))))

(defgeneric convert-token (type token)
  (:method ((type (eql 'symbol))
	    (token string))
    (intern token :keyword))
  (:method ((type (eql 'quotation))
	    (token string))
    (<quotation> (intern (subseq token 1)
			 :keyword)))
  (:method ((type (eql 'ratio))
	    (token string))
    (apply #'/
	   (mapcar #'parse-integer
		   (split-sequence #\/ token))))
  (:method ((type (eql 'float))
	    (token string))
    (read-from-string token))
  (:method ((type (eql 'integer))
	    (token string))
    (parse-integer token))
  (:method ((type (eql 'infinite-string))
            (token string))
    'infinite-string-error)
  (:method ((type (eql 'faulty-string))
	    (token string))
    'open-string-error)
  (:method ((type (eql 'long-string))
	    (token string))
    (let ((delimiter-length
	   (length
	    (first
	     (extract-matches
	      `(:sequence #\$
			  (:non-greedy-repetition 1 nil :everything)
			  #\Newline)
	      token)))))
      (subseq token delimiter-length (- (length token)
					delimiter-length -2))))
  (:method ((type (eql 'short-string))
            (token string))
    (subseq token 1 (1- (length token)))))

(defun poslin-read-from-string (string parse-trees)
  (mapcar (lambda (tk)
	    (apply #'convert-token
		   tk))
	  (all-tokens string parse-trees)))

(defun poslin-read-block (stream parse-trees)
  (let ((string ""))
    (do ((curr (read-line stream)
	       (read-line stream)))
	((or (not (scan 'faulty-string
                         (cut-matches 'long-string
                                      #1=(concatenate 'string
                                                      string curr
                                                      #(#\Newline)))))
             (scan 'infinite-string
                   #1#))
         (poslin-read-from-string #1# parse-trees))
      (setf string
	    (concatenate 'string
			 string curr #(#\Newline))))))

(defun poslin-read (stream parse-trees)
  (let ((eof (gensym "eof"))
	(string ""))
    (do ((curr (read-line stream nil eof)
	       (read-line stream nil eof)))
	((eq curr eof)
	 (poslin-read-from-string string parse-trees))
      (setf string
	    (concatenate 'string
			 string #(#\Newline)
			 curr)))))

(defun poslin-read-file (file parse-trees)
  (with-open-file (stream file)
    (poslin-read stream parse-trees)))
