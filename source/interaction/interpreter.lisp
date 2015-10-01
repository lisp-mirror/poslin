(in-package #:poslin)

(defmacro rstack (path)
  `([binding]-value (path-get ,path :rstack)))

(defmacro stack (path)
  `([binding]-value (path-get ,path :stack)))

#+nil
(defmacro interpreter ()
  `(loop while (or (not (eq pc <noop>))
                   rstack)
      do
        (progn
          (when (and stepping
                     (if (typep pc '<thread>)
                         (let ((front (<thread>-front pc)))
                           (or (typep front '<constant>)
                               (typep front '<prim>)))
                         (not (eq pc <noop>))))
            (print-status)
            (read-line))
          (if (eq pc <noop>)
              (setf pc (or (pop rstack)
                           <noop>))
              (typecase pc
                (<constant>
                 (push (<constant>-val pc)
                       (stack path))
                 (setf pc (or (pop rstack)
                              <noop>)))
                (<prim>
                 (funcall (<prim>-fun pc)))
                (<thread>
                 (let ((front (<thread>-front pc)))
                   (if (eq front <noop>)
                       (setf pc (<thread>-back pc))
                       (typecase front
                         (<constant>
                          (push (<constant>-val front)
                                (stack path))
                          (setf pc (<thread>-back pc)))
                         (<prim>
                          (funcall (<prim>-fun front)))
                         (t
                          (push (<thread>-back pc)
                                rstack)
                          (setf pc front))))))
                (<quotation>
                 (push (<quotation>-val pc)
                       (stack path))
                 (setf pc (or (pop rstack)
                              <noop>)))
                (t
                 (push pc (stack path))
                 (setf pc (or (pop rstack)
                              <noop>))))))))

(defmacro interpreter ()
  `(loop while (or (not (eq pc <noop>))
                   rstack)
      do
        (progn
          (when (and stepping
                     (typep pc 'thread)
                     (not (eq pc <noop>))
                     (elementary? (thread-front pc)))
            (print-status)
            (read-line))
          (if (eq pc <noop>)
              #1=(setf pc (or (pop rstack)
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
                   (cond
                     ((typep front '<constant>)
                      (push (<constant>-val front)
                            (stack path))
                      (setf pc (thread-back pc)))
                     ((typep front '<prim>)
                      (funcall (<prim>-fun front)))
                     (t
                      (push (thread-back pc)
                            rstack)
                      (setf pc front)))))
                ((typep pc '<handled>)
                 (let ((th (<handled>-thread pc)))
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
