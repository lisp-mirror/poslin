(in-package #:poslin)

(defprim r> nil
  ;; Pop top of rstack
  ;; ( -- n )
  (push-curr (if rstack
		 (pop rstack)
		 (perror rstack-bottom
			 "Attempt to pop empty rstack"))))

(defprim >r nil
  ;; Push on top of rstack
  ;; ( val -- )
  (args (val)
    (push val rstack)))

(defprim pc> nil
  ;; Pop top of program counter
  ;; ( -- n )
  (push-curr (if pc
		 (pop pc)
		 (perror pc-bottom "Tried to pop empty pc"))))

(defprim >pc nil
  ;; Push onto program counter
  ;; ( val -- )
  (args (val)
    (push val pc)))