(in-package #:poslin)

(defstruct [binding]
  value)

(defun binding (val)
  (make-[binding] :value val))

(defmethod compare ((x [binding])
                    (y [binding]))
  (if (eq x y)
      :equal
      :unequal))
