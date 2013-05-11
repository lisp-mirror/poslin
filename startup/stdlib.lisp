(in-package #:poslin)

(defparameter *stdlib* '())

(addstd
  "op-env.poslin" >> !
  "var-env.poslin" >> !
  "path.poslin" >> !
  )

#|
(addstd
  ;; Pushes the current stack
  ;; ( -- [] )
  0 ^ !
  ?eo !
  ~
  [ 0 ^ &
  ] @o !)

(addstd
  ;; Get current operation environment
  ;; ( -- oe )
  ~ !
  ?eo !
  ?~eo
  [ ~ &
    ?eo &
  ] @o !)

(addstd
  ;; Parent stack
  ;; ( -- [] )
  ?~eo !
  ^^
  [ 1 ^ &
  ] @o !)

(addstd
  ;; Push to parent
  ;; ( [ ... a -- a [ ... )
  ?~eo !
  ^<-
  [ ^^ &
    <> &
    <- &
  ] @o !)

(addstd
  ;; Pop parent
  ;; ( a [ ... -- [ ... a )
  ?~eo !
  ^->
  [ ^^ &
    -> &
  ] @o !)

(addstd
  ;; Set current operation environment
  ;; ( op-env -- )
  ?~eo !
  @~eo
  [ ~ &
    <> &
    @eo &
  ] @o !)

(addstd
  ;; Set in current operation environment
  ;; (name op -- )
  ?~eo !
  @~o
  [ ?~eo &
    '[ &
       ^-> &
       ^-> &
       ^-> &
       <> &
       @o &
    '] &
    _ &
  ] @o !)

(addstd
  ;; Lookup in the current operation environment
  ;; ( name -- thread )
  ?~o
  [ ?~eo &
    <> &
    ?o &
  ] @~o !)

(addstd
  ;; Get current variable environment
  ;; ( -- var-env )
  ?~ev
  [ ~ &
    ?ev &
  ] @~o !)

(addstd
  ;; Set current variable environment
  ;; ( var-env -- )
  @~ev
  [ ~ &
    <> &
    @ev &
  ] @~o !)

(addstd
  ;; Set in current variable environment
  ;; ( name val -- )
  @~v
  [ ?~ev &
    '[ &
       ^-> &
       ^-> &
       ^-> &
       <> &
       @v &
    '] &
    _ &
  ] @~o !)

(addstd
  ;; Get from current variable environment
  ;; ( name -- val )
  ?~v
  [ ?~ev &
    <> &
    ?v &
  ] @~o !)

(addstd
  ;; Remove from current operation environment
  ;; ( name -- )
  @~o_
  [ ?~eo &
    <> &
    @o_ &
  ] @~o !)

(addstd
  ;; Remove from current variable environment
  ;; ( name -- )
  @~v_
  [ ?~ev &
    <> &
    @v_ &
  ] @~o !)

(addstd
  ;; Set word to be immediate in current operation environment
  ;; ( name -- )
  @~i+
  [ ?~eo &
    <> &
    @i+ &
  ] @~o !)

(addstd
  ;; Unset word immediateness in current operation environment
  ;; ( name -- )
  @~i_
  [ ?~eo &
    <> &
    @i_ &
  ] @~o !)

(addstd
  ;; Set word delayed in current operation environment
  ;; ( name -- )
  @~i-
  [ ?~eo &
    <> &
    @i- &
  ] @~o !)

(addstd
  ;; Close stack and push thread
  ;; ( [ ... -- thread )
  ]&
  [ '] &
    '& &
  ] @~o !
  ]& @~i+ !)

(addstd
  ;; Close stack and define operator
  ;; ( name [ ... -- )
  ]@
  [ '] &
    @~o &
  ] @~o !
  ]@ @~i+ !)

(addstd
  ;; Set name of current stack
  ;; ( name -- )
  @~n
  [ ~ &
    <> &
    @n &
  ]@)

(addstd
  ;; Get name of current stack
  ;; ( -- name )
  ?~n
  [ ~ &
    ?n &
  ]@)

(addstd
  ;; Delayed call (use for recursion)
  ;; ( op -- op ! )
  !&
  [ ''! &
  ]@
  !& @~i+ !)

(addstd
  ;; Calling if (use for recursion)
  ;; ( b op1 op2 -- ??? )
  <?>!
  [ <?> '& &
    '!& &
  ]@
  <?>! @~i+ !)

(addstd
  ;; Checks for bottom of current stack
  ;; ( [ -- [ t )
  ;; ( [ ... -- [ ... nil )
  ?~_
  [ ~ &
    ?_ &
  ]@)

(addstd
  ;; Drop everything
  ;; ( ... -- )
  __
  [ ?~_ &
    ||
    [ _ & 
      __ !& ]
    <?>!
  ]@)

(addstd
  ;; Open stack and name it
  ;; ( sym -- sym [ )
  {
  [ § &
    '[ &
       ^-> &
       @~n &
  ]@
  { @~i+ !)

(addstd
  ;; Close stack and drop it
  ;; ( [ ... -- )
  ]_
  [ '] &
    _ &
  ]@
  ]_ @~i+ !)

(addstd
  ;; Close stack and convert to thread with lexical environment and
  ;; name
  ;; ( [ ... -- thread )
  }
  [ ']
    '[ &
       ''[ &
           ?~eo &
	   @eo+ '& &
	   @~eo '& &
	   ?~ev &
	   @ev+ '& &
	   @~ev '& &
	   ^-> &
	   § &
	   ^<- &
	   ?n &
	   @~n '& &
	   ^-> &
	   '& &
       '']_ &
    ']& &
  ]@
  } @~i+ !)

(addstd
  ;; Defines operation that uses fresh stack with saved lexical
  ;; environment
  ;; ( name [ ... -- )
  }@
  [ '} &
    @~o &
  ]@
  }@ @~i+ !)

(addstd
  ;; Increases by 1
  ;; ( n -- n+1 )
  1+
  [ 1 + &
  ]@)

(addstd
  ;; Decreases by 1
  ;; ( n -- n-1 )
  1-
  [ 1 - &
  ]@)

(addstd
  ;; returns nth element of list
  ;; ( list n -- el )
  nth
  [ § &
    0 <= &
    [ _ &
      first &
    ]
    [ 1- &
      <> &
      rest &
      <> &
      nth !&
    ]
    <?>!
  ]@)

(addstd
  ;; Computes triangle number
  ;; ( n -- n*(n+1)/2 )
  triangle
  [ § &
    1+ &
    * &
    2 / &
  ]@)

(addstd
  ;; prints log message
  ;; ( n -- n )
  logprint
  [ print &
    § &
    ": " print &
    print &
    newline &
  ]@)

(addstd
  ;; Delayed set
  ;; ( n val -- n val @~v )
  @v&
  [ @~v '& &
  ]@
  @v& @~i+ !)

(addstd
  ;; Delayed get
  ;; ( n -- n ?~v )
  &v
  [ ?~v '& &
  ]@
  &v @~i+ !)

(addstd
  ;; Gets from parent and saves under name
  ;; ( name -- name ^-> & @v& )
  arg
  [ ^-> '& &
    '@v& &
  ]@
  arg @~i+ !)

(addstd
  ;; Computes factorial
  ;; ( n -- n! )
  factorial
  { n arg
    n &v
    1 <= &
    [ 1 ]
    [ n &v
      1- &
      factorial !&
      n &v
      * &
    ]
    <?>!
    ^<- &
  }@)

(addstd
  ;;
  ;; ( val1 val2 -- val1 val1 val2 )
  °§
  [ >r &
    § &
    r> &
  ]@)

(addstd
  ;;
  ;; ( val1 val2 -- val1 val2 val1 )
  <>§
  [ °§ &
    <> &
  ]@)

(addstd
  ;;
  ;; ( x [ ... ] y -- [ ... y ] x )
  <<>
  [ °§ &
    <- &
    <> &
  ]@)

(addstd
  ;;
  ;; ( [ ... x ] -- x [ ... ] )
  <>>
  [ § &
    -> &
    <> &
  ]@)

(addstd
  ;;
  ;; ( sym -- val )
  ?^v
  [ ?~ev &
    ?ev- &
    <> &
    ?v &
  ]@)

(addstd
  ;;
  ;; ( sym -- sym ?^v )
  &^v
  [ ?^v '& &
  ]@
  &^v @~i+ !)

(addstd
  ;;
  ;; ( [ ... ] op -- [ ??? ] )
  <!
  [ <> &
    '%[ &
    ^-> &
    '! &
    '] &
  ]@)

(addstd
  ;;
  ;; ( ...1 [ ...2 op -- ??? [ ...2 )
  <^!
  [ ^^ &
    <> &
    <! &
  ]@)

(addstd
  ;;
  ;;
  step
  { thread arg
    thread &v
    rest &
    [ thread &v
      rest &
      first &
      § &
      ?& &
      <^! ^<- <?>!
    ]
    ||
    <?>!
    thread
    thread &v
    rest &
    rest &
    cons &
    ^<- &
  }@)|#