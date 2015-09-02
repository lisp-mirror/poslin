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
     begin-word
     (:alternation
      (:sequence #\$ (:greedy-repetition 0 nil anything))
      (:sequence #\" (:non-greedy-repetition 0 nil anything))
      )
     :end-anchor))

(define-parse-tree-synonym character
    (:sequence
     begin-word #\<
     (:greedy-repetition 1 nil :non-whitespace-char-class)
     #\> end-word))

(defparameter *parse-order*
  '(infinite-string long-string short-string faulty-string integer
    float ratio quotation character symbol))

#|
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
|#

(defgeneric convert-token (type token)
  (:method ((type (eql 'symbol))
	    (token string))
    (intern token :keyword))
  (:method ((type (eql 'character))
            (token string))
    (let ((length (length token)))
      (assert (>= length 3))
      (if (= length 3)
          (elt token 1)
          (aif (cl-unicode:character-named
                (subseq token 1 (1- (length token))))
               it
               (let ((name (subseq token 1 (1- (length token)))))
                 (cond
                   ((string-equal name "tab")
                    #\Tab)
                   ((string-equal name "newline")
                    #\Newline)
                   (t
                    (error "Invalid character ~A"
                           token))))))))
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
    (error "Infinite string found"))
  (:method ((type (eql 'faulty-string))
	    (token string))
    (error "faulty string read"))
  (:method ((type (eql 'long-string))
	    (token string))
    (let ((delimiter-length (1+ (position #\Newline token
                                          :test #'char=))))
      (subseq token delimiter-length (- (length token)
					delimiter-length -2))))
  (:method ((type (eql 'short-string))
            (token string))
    (subseq token 1 (1- (length token)))))

(defun retrieve-token (string parse-order)
  (let ((tokens (stable-sort
                 (remove-if-not #'second
                                (mapcar (lambda (parse-tree)
                                          (multiple-value-bind (begin end)
                                              (scan parse-tree string)
                                            (list parse-tree begin end)))
                                        parse-order))
                 #'<
                 :key #'second)))
    (apply #'values
           (first tokens))))

(defun next-token (string parse-order)
  (multiple-value-bind (token-type begin end)
      (retrieve-token string parse-order)
    (when begin
      (values (convert-token token-type (subseq string begin end))
              (subseq string (1+ end))))))

(defun poslin-read-from-string (string parse-trees)
  (let ((rest string)
        (result '()))
    (loop
       (multiple-value-bind (token unread)
           (next-token rest parse-trees)
         (if unread
             (progn
               (push token result)
               (setf rest unread))
             (return))))
    (nreverse result)))

(defun poslin-read-block (stream parse-trees)
  (let ((line (read-line stream))
        (tokens '())
        (in-string? nil))
    (loop
       (progn
         (when in-string?
           (setf line (read-line stream)))
         (multiple-value-bind (token-type begin end)
             (retrieve-token line parse-trees)
           (if in-string?
               (progn
                 (setf in-string?
                       (concatenate 'string
                                    in-string? #(#\Newline)
                                    line))
                 (multiple-value-bind (token-type begin end)
                     (retrieve-token in-string? parse-trees)
                   (unless (eq token-type 'faulty-string)
                     (progn
                       (push (convert-token token-type (subseq in-string? begin end))
                             tokens)
                       (setf line (subseq in-string? end)
                             in-string? nil)))))
               (if token-type
                   (if (eq token-type 'faulty-string)
                       (setf in-string? line)
                       (progn
                         (push (convert-token token-type (subseq line begin end))
                               tokens)
                         (setf line (subseq line end))))
                   (return (nreverse tokens)))))))))

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
