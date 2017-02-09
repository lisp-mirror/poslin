(in-package #:poslin)

(defstruct [binding]
  value)

(defun binding (val)
  #.+optimization-parameters+
  (the [binding]
    (make-[binding] :value val)))

(defmethod compare ((x [binding])
                    (y [binding]))
  #.+optimization-parameters+
  (the (member :equal :unequal)
    (if (eq x y)
        :equal
        :unequal)))
