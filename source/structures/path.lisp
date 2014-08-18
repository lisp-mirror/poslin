(in-package #:poslin)

(defadt [path]
  (<root> [env])
  (<path> [env] [path]))

(defmatch path-push ([path] [env])
    [path]
  ((p e)
   (<path> e p)))

(defmatch path-pop ([path])
    [maybe]
  ((<path> _ p)
   (<just> p))
  (_ <nothing>))

(defmatch path-top ([path])
    [env]
  ((<root> e)
   e)
  ((<path> e _)
   e))

(defmatch path-nth ([path] integer)
    [env]
  (((<root> e)
    0)
   e)
  (((<path> e _)
    0)
   e)
  (((<path> _ p)
    n)
   (if (> n 0)
       (path-nth p (1- n))
       (error "Attempt to get ~Ath of path"
	      n))))

(defmatch path-get ([path] symbol)
    t
  (((<root> e)
    k)
   (aif (lookup e k)
	it
	<meta-nothing>))
  (((<path> e p)
    k)
   (aif (lookup e k)
	it
	(path-get p k))))

(defmatch path-set ([path] [env])
    [path]
  (((<root> _)
    e)
   (<root> e))
  (((<path> _ p)
    e)
   (<path> e p)))
