(in-package #:poslin)

(defun repl (poslin &rest files)
  #.+optimization-parameters+
  (declare (type function poslin))
  (handler-case
      (let ((*print-circle* t))
        (loop for file in files
           do
             (loop for v in (poslin-read-file file *parse-order*)
                do
                  (funcall poslin v)))
        (with-pandoric (path stepping)
            poslin
          (poslin-print (stack path)
                        t)
          (loop do
               (progn
                 (if stepping
                     (format t "~%* ")
                     (format t "~%> "))
                 (finish-output)
                 (dolist (v (poslin-read-block *standard-input*
                                               *parse-order*))
                   (case v
                     (:quit
                      (return-from repl
                        (stack path)))
                     (:step
                      (setf stepping t))
                     (:unstep
                      (setf stepping nil))
                     (infinite-string-error
                      (format t "Read error: infinite string~%"))
                     (t
                      (funcall poslin v))))
                 (poslin-print (stack path)
                               t)))))
    (unhandled-poslin-exception ()
      (with-pandoric (path)
          poslin
        (let ((ex (first (stack path))))
          (format t "~%~%~A~%~%"
                  ([exception]-string ex))))
      (repl poslin))
    (t (err)
      (with-pandoric (path pc rstack)
          poslin
        (print-status)
        (setf pc <noop>)
        (setf rstack nil))
      (format t "~%~%Implementation error~%~A~%~%"
              err)
      (when (y-or-n-p "~%Continue?")
        (repl poslin)))))

(defmacro one-of (&body options)
  `(case (random ,(length options))
     ,@(loop for x from 0
          for option in options
          collect `(,x ,option))))

(defun repl-dyn ()
  #.+optimization-parameters+
  (format t "
******        **        ****   ****        **** ***  ****
********    ******    *******  ****        **** ***  ****
 **   ***  ***  ***  ***    **  **          **   ***  **
 **    ** ***    *** **     *   **          **   ***  **
 **   *** **      ** ***        **          **   **** **
 *******  **      **  *****     **          **   *******
 *****    **      **    *****   **          **   *******
 **       **      **       ***  **          **   ** ****
 **       ***    ***  *     **  **      **  **   **  ***
 **        ***  ***  **    ***  **      **  **   **  ***
****        ******   ********  *********** **** ****  ***
****          **       ****    *********** **** ****  ***
=========================================================
=========================================================

© 2015-2016 Thomas Bartscher
Version ~A~%"
          (asdf:component-version (asdf:find-system "poslin")))
  (let ((*random-state* (make-random-state t)))
    (format t "~A~%"
            (one-of
              "Everything is possible. Watch out!"
              "Contains parts for a screw factory. Screws not included."
              "Forget scope. Then invent it yourself."
              "Yell if you want something."
              "An interactive compiler."
              )))
  (apply #'repl
         (new-poslin *control-prims* *thread-prims* *path-prims* *set-prims*
                     *dict-prims* *binding-prims* *stack-prims* *boolean-prims*
                     *comparison-prims* *arith-prims* *array-prims*
                     *string-prims* *char-prims* *type-prims* *exception-prims*
                     *symbol-prims* *stream-prims*)
         #+sbcl (rest sb-ext:*posix-argv*)
         #+lispworks system:*line-arguments*
         #+cmu extensions:*command-line-words*
         #+clisp ext:*args*
         #+clozure ccl:*unprocessed-command-line-arguments*
         #-(or sbcl lispworks cmu clisp clozure)
         '()))
