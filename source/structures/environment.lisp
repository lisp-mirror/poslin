(in-package #:poslin)

(defadt [env]
  (<root-env> fset:map)
  (<env> fset:map [env]))

(defmatch lookup ([env] symbol)
    (or [binding] null)
  (((<env> m p)
    k)
   (aif (@ m k)
	it
	(lookup p k)))
  (((<root-env> m)
    k)
   (aif (@ m k)
	it)))

(defmatch insert ([env] symbol [binding])
    [env]
  (((<root-env> m)
    k v)
   (<root-env> (with m k v)))
  (((<env> m p)
    k v)
   (<env> (with m k v)
	  p)))

(defmatch get-parent ([env])
    [env]
  ((<env> _ p)
   p))

(defmatch set-parent ([env] [env])
    [env]
  (((<root-env> m)
    p)
   (<env> m p))
  (((<env> m _)
    p)
   (<env> m p)))
