(in-package #:poslin)

(defun repl (poslin &rest files)
  (declare (optimize (debug 0)
                     (safety 0)
                     (speed 3))
           (type function poslin))
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
                 (format t "~%> ")
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
                     (t
                      (funcall poslin v))))
                 (poslin-print (stack path)
                               t)))))
    (t (err)
      (format t "~A"
              err)
      (with-pandoric (path pc rstack)
          poslin
        (print-status)
        (setf pc <noop>)
        (setf rstack nil))
      (when (y-or-n-p "~%Continue?")
        (repl poslin)))))

(defun repl0 ()
  (repl (new-poslin *prim*)))

(defun repl1 ()
  (repl (new-poslin *prim*)
        "~/src/Poslin/lib/base.poslin"
        ))

#+sbcl
(defun repl-dyn ()
  (format t "
*******      ****       ****   ****        **** ***  ****
********    ******     **  **  ****        **** ***  ****
 **   ***  ***  ***   **    **  **          **   ***  **
 **    ** ***    *** **    **   **          **   ***  **
 **   *** **      ** ***        **          **   **** **
 *******  **      **  *****     **          **   *******
 ******   **      **    *****   **          **   *******
 **       **      **       ***  **          **   ** ****
 **       ***    ***  **    **  **      **  **   **  ***
 **        ***  ***  **    **   **      **  **   **  ***
****        ******    ******   *********** **** ****  ***
****         ****      ****    *********** **** ****  ***
=========================================================
=========================================================

© 2015 Thomas Bartscher
0.1.0pr5
")
  (setf *random-state* (make-random-state t))
  (format t "~A~%"
          (case (random 5)
            (0 "Everything is possible. Watch out!")
            (1 "Contains parts for a screw factory. Screws not included.")
            (2 "Forget scope. Then invent it yourself.")
            (3 "Yell if you want something.")
            (4 "The interactive compiler.")
            ))
  (apply #'repl
         (new-poslin *prim*)
         (rest sb-ext:*posix-argv*)))
