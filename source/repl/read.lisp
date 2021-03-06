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
(declaim (type (or cons null)
               *parse-order*))

(defgeneric convert-token (type token)
  (:method ((type (eql 'symbol))
	    (token string))
    #.+optimization-parameters+
    (the symbol
      (intern token :keyword)))
  (:method ((type (eql 'character))
            (token string))
    #.+optimization-parameters+
    (the character
      (let ((length (length token)))
        (declare (type (integer 0)
                       length))
        (assert (>= length 3))
        (if (= length 3)
            (elt token 1)
            (aif (cl-unicode:character-named
                  (subseq token 1 (1- (length token))))
                 it
                 (let ((name (subseq token 1 (1- (length token)))))
                   (declare (type string name))
                   (cond
                     ((string-equal name "tab")
                      #\Tab)
                     ((string-equal name "newline")
                      #\Newline)
                     (t
                      (error "Invalid character ~A"
                             token)))))))))
  (:method ((type (eql 'quotation))
	    (token string))
    #.+optimization-parameters+
    (the <quotation>
      (<quotation> (intern (subseq token 1)
                           :keyword))))
  (:method ((type (eql 'ratio))
	    (token string))
    #.+optimization-parameters+
    (the rational
      (apply #'/
             (mapcar #'parse-integer
                     (split-sequence #\/ token)))))
  (:method ((type (eql 'float))
	    (token string))
    (the (or float double-float)
      (let ((*read-default-float-format* 'double-float))
        (read-from-string token))))
  (:method ((type (eql 'integer))
	    (token string))
    (the integer
      (parse-integer token)))
  (:method ((type (eql 'infinite-string))
            (token string))
    (error "Infinite string found"))
  (:method ((type (eql 'faulty-string))
	    (token string))
    (error "faulty string read"))
  (:method ((type (eql 'long-string))
	    (token string))
    (the string
      (let ((delimiter-length (1+ (position #\Newline token
                                            :test #'char=))))
        (declare (type (integer 1)
                       delimiter-length))
        (subseq token delimiter-length (- (length token)
                                          delimiter-length -2)))))
  (:method ((type (eql 'short-string))
            (token string))
    #.+optimization-parameters+
    (the string
      (subseq token 1 (1- (length token))))))

(defun first-tokens (string parse-order)
  #.+optimization-parameters+
  (declare (type string string)
           (type (or cons null)
                 parse-order))
  (the (or cons null)
    (let ((tokens (mapcar (lambda (parse-tree)
                            (multiple-value-bind (begin end)
                                (scan parse-tree string)
                              (list parse-tree begin end)))
                          parse-order)))
      (declare (type (or cons null)
                     tokens))
      (values (stable-sort (remove-if-not #'second
                                          tokens)
                           #'<
                           :key #'second)
              (mapcar #'first
                      (remove-if #'second
                                 tokens))))))

(defun retrieve-token (string parse-order)
  #.+optimization-parameters+
  (declare (type string string)
           (type (or cons null)
                 parse-order))
  (let ((tokens (stable-sort
                 (remove-if-not #'second
                                (mapcar (lambda (parse-tree)
                                          (multiple-value-bind (begin end)
                                              (scan parse-tree string)
                                            (list parse-tree begin end)))
                                        parse-order))
                 #'<
                 :key #'second)))
    (declare (type (or cons null)
                   tokens))
    (apply #'values
           (first tokens))))

(defun poslin-read-from-string (string parse-order)
  #.+optimization-parameters+
  (declare (type string string)
           (type (or cons null)
                 parse-order))
  (labels ((_token< (t1 t2)
             #.+optimization-parameters+
             (declare (type (or cons null)
                            t1 t2))
             (the boolean
               (let ((tb1 (second t1))
                     (tb2 (second t2)))
                 (declare (type (integer 0)
                                tb1 tb2))
                 (cond
                   ((< tb1 tb2)
                    t)
                   ((> tb1 tb2)
                    nil)
                   ((= tb1 tb2)
                    (labels ((_rec (t1 t2 parse-order)
                               #.+optimization-parameters+
                               (the boolean
                                 (let ((curr (first parse-order)))
                                   (if curr
                                       (cond
                                         ((eq t1 curr)
                                          t)
                                         ((eq t2 curr)
                                          nil)
                                         (t
                                          (_rec t1 t2 (rest parse-order))))
                                       (error "unknown token types ~S ~S"
                                              t1 t2))))))
                      (_rec (first t1)
                            (first t2)
                            parse-order)))))))
           (_insert (token found acc)
             #.+optimization-parameters+
             (declare (type (or cons null)
                            token found acc))
             (the (or cons null)
               (if found
                   (let ((curr (first found)))
                     (declare (type (or cons null)
                                    curr))
                     (if (_token< token curr)
                         (nconc (nreverse (cons token acc))
                                found)
                         (_insert token (rest found)
                                  (cons curr acc))))
                   (nreverse (cons token acc)))))
           (_first-ones (found n acc)
             (declare (type (or (integer 0)
                                null)
                            n))
             #.+optimization-parameters+
             (the (values (or cons null)
                          (or cons null))
               (if found
                   (let ((curr (first found)))
                     (declare (type (or cons null)
                                    curr))
                     (if n
                         (locally (declare (type (integer 0)
                                                 n))
                           (if (<= (the (integer 0)
                                        (second curr))
                                   n)
                               (_first-ones (rest found)
                                            n (cons curr acc))
                               (values (first (last acc))
                                       (mapcar #'first
                                               acc))))
                         (_first-ones (rest found)
                                      (third curr)
                                      (list curr))))
                   (values (first (last acc))
                           (mapcar #'first
                                   acc)))))
           (_rec (string found tokens)
             #.+optimization-parameters+
             (declare (type string string)
                      (type (or cons null)
                            found tokens))
             (multiple-value-bind (token recompute)
                 (_first-ones found nil '())
               (declare (type (or cons null)
                              token recompute))
               (if token
                   (let* ((token-type (first token))
                          (begin (second token))
                          (end (third token))
                          (nstring (subseq string end))
                          (found (mapcar (lambda (tok)
                                           (declare (type (or cons null)
                                                          tok))
                                           (list (first tok)
                                                 (- (the (integer 0)
                                                      (second tok))
                                                    end)
                                                 (- (the (integer 0)
                                                      (third tok))
                                                    end)))
                                         (remove-if (lambda (a)
                                                      (member (first a)
                                                              recompute))
                                                    found))))
                     (declare (type symbol token-type)
                              (type (integer 0)
                                    begin end)
                              (type string nstring)
                              (type (or cons null)
                                    found))
                     (_rec nstring (reduce (lambda (found recomp)
                                             (multiple-value-bind (b e)
                                                 (scan recomp nstring)
                                               (declare (type (or (integer 0)
                                                                  null)
                                                              b e))
                                               (if b
                                                   (locally
                                                       (declare
                                                        (type (integer 0)
                                                              b e))
                                                     (_insert (list recomp b e)
                                                              found '()))
                                                   found)))
                                           recompute
                                           :initial-value found)
                           (cons (convert-token token-type
                                                (subseq string begin end))
                                 tokens)))
                   (nreverse tokens)))))
    (_rec string (first-tokens string parse-order)
          '())))

(defun poslin-read-block (stream parse-trees)
  #.+optimization-parameters+
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
  #.+optimization-parameters+
  (let ((eof (gensym "eof"))
	(string ""))
    (declare (type symbol eof)
             (type string string))
    (do ((curr (read-line stream nil eof)
	       (read-line stream nil eof)))
	((eq curr eof)
	 (poslin-read-from-string string parse-trees))
      (setf string
	    (concatenate 'string
			 string #(#\Newline)
			 curr)))))

(defun poslin-read-file (file parse-trees)
  #.+optimization-parameters+
  (with-open-file (stream file)
    (poslin-read stream parse-trees)))
