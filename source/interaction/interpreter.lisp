(in-package #:poslin)

(defmacro stack (path)
  `(the (or cons null)
     ([binding]-value (path-get ,path :stack))))

(defmacro rstack ()
  `(the (or cons null)
     ([binding]-value rstack)))

(defmacro push-rstack (obj)
  `(push ,obj (rstack)))

(defmacro pop-rstack ()
  `(pop (rstack)))

(defmacro null-rstack ()
  `(setf (rstack)
         nil))

(defmacro interpreter ()
  `(loop while (or (not (eq pc <noop>))
                   (rstack))
      do
        (progn
          (when (and stepping
                     (typep pc 'thread)
                     (not (eq pc <noop>))
                     (elementary? (thread-front pc)))
            (print-status)
            (read-line))
          (if (eq pc <noop>)
              #1=(setf pc (or (pop-rstack)
                              <noop>))
              (cond
                ((typep pc '<constant>)
                 (push (<constant>-val pc)
                       (stack path))
                 #1#)
                ((typep pc '<prim>)
                 (funcall (<prim>-fun pc)))
                ((complex-thread? pc)
                 (let ((front (thread-front pc)))
                   (declare (type [thread] front))
                   (cond
                     ((typep front '<constant>)
                      (push (<constant>-val front)
                            (stack path))
                      (setf pc (thread-back pc)))
                     ((typep front '<prim>)
                      (funcall (<prim>-fun front)))
                     (t
                      (push-rstack (thread-back pc))
                      (setf pc front)))))
                ((typep pc '<handled>)
                 (let ((th (<handled>-thread pc)))
                   (declare (type [thread] th))
                   (if (eq th <noop>)
                       #1#
                       (funcall (<prim>-fun th)))))
                ((typep pc '<quotation>)
                 (push (<quotation>-val pc)
                       (stack path))
                 #1#)
                (t
                 (push pc (stack path))
                 #1#))))))
