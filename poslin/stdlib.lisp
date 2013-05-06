(in-package #:poslin)

(defparameter *stdlib* '())

(addstd
  ;; Getting the current stack
  ;; ( -- [ ... ] )
  ~
  [ 0 ^ &
  ] @o !)

(addstd
  ;; Setting the current operation environment
  ;; ( op-env -- )
  @~o
  [ ~ &
    <> &
    @eo &
  ] @o !)

(addstd
  ;; Getting the current operation environment
  ;; ( -- op-env )
  ?~o
  [ ~ &
    ?eo &
  ] @o !)

(addstd
  ;; Setting the current variable environment
  ;; ( var-env -- )
  @~v
  [ ~ &
    <> &
    @ev &
  ] @o !)

(addstd
  ;; Getting the current variable environment
  ;; ( -- var-env)
  ?~v
  [ ~ &
    ?ev &
  ] @o !)

(addstd
  ;; Closing a stack and discarding it
  ;; ( [ ... -- )
  ]_
  [ '] &
    _ &
  ] @o !
  ]_ @i+ !)

(addstd
  ;; Closing a stack and compiling it to a thread under a name
  ;; ( name [ ... -- )
  ]@
  [ '] &
    @o &
  ] @o !
  ]@ @i+ !)

(addstd
  ;; Push onto parent stack
  ;; ( [ ... n -- n [ ... )
  ^<-
  [ 1 ^ &
    <> &
    <- &
  ]@)

(addstd
  ;; Pop from parent stack
  ;; ( n [ ... -- [ ... n )
  ^->
  [ 1 ^ &
    -> &
  ]@)

(addstd
  ;; Duplicate second element
  ;; ( a b -- a b a )
  >§
  [ >r &
    § &
    r> &
    <> &
  ]@)

(addstd
  ;; Pop from stack and move stack to top
  ;; ( [ ... a ] -- a [ ... ] )
  <>>
  [ § &
    -> &
    <> &
  ]@)

(addstd
  ;; Push onto stack and move stack to second
  ;; ( a [ ... ] b -- [ ... b ] a )
  <<>
  [ >r &
    § &
    r> &
    <- &
    <> &
  ]@)

(addstd
  ;; Drop all from current stack
  ;; ( ... -- )
  __
  [ ?_ &
    || [ _ & __ '! & ] <?> &
    '! &
  ]@)

(addstd
  ;; I have no idea how this works, but it does.
  ;; Define operation that uses it's own stack
  ;; ( name [ ... -- )
  ]@+
  [ '[ &
       ''[ '& &
       ^-> &
       ''] '& &
    ']_ &
    ']@ &
  ]@
  ]@+ @i+ !)