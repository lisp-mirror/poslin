(in-package #:poslin)

#.(setf *stdlib* ())

(addstd
  [ <^> % !
    1 ^^ '! <> '! ]
  <^> ~ ! !* !
  <^> [_])

(addstd
  [ <~> % !
    ~ '! <> '! ]
  <~> ~ ! !* !
  <~> [_])

(addstd
  [ ^-> % !
    1 ^^ '!
    -> '! ]
  ^-> ~ ! !* !
  ^-> [_])

(addstd
  [ ^<- % !
    <^> '! <- '! ]
  ^<- ~ ! !* !
  ^<- [_])

(addstd
  [ { % !
    '[ ^-> '! % '! ]
  { ~ ! !* !
  { [_]
  { [!] !)

(addstd
  [ }* % !
    [%] '! ^<- '! '] '!
    $ '! ~ '! !* '!
    '[_] ]
  }* ~ ! !* !
  }* [_]
  }* [!] !)

(addstd
  }+ {
       [%] '! ^<- '! '] '!
       $ '! ~ '! !+ '!
       '[_]
     }*
  }+ [!] !)

(addstd
  || { }+)

(addstd
  r_ {
       r> '! _ '!
     }*)

(addstd
  eat {
        nil eq '!
	|| eat if '! '!
      }*)

(addstd
  help {
"Prims:
  !   - Immediate. Call the top of the stack on the stack.
  [   - Immediate. Opens an anonymous stack.
  ]   - Immediate. Closes the current stack.
  }   - Immediate. Closes the current stack and pushes it onto its
        parent stack.
  }!  - Immediate. Closes the current stack, converts it into a thread
        and pushes that thread onto its parent stack.
  %   - Sets the name of the current stack to the top of the current
        stack.
  ~   - Pushes the child stack with name of the top of the current
        stack onto the current stack.
  ->  - Pops the top of the stack on top of the current stack onto the
        current stack.
  <-  - Pushes the top of the current stack onto the stack before the
        top.
  _   - Drops the top of the current stack.
  $   - Duplicates the top of the current stack. Doesn't consume.
  <>  - Swaps the top two elements of the current stack.
  ^^  - Pushes the nth stack in the path onto the current stack.
  ~>  - Pushes the child stack with the name of the top of the current
        stack of the stack that is second on the current stack onto
        the current stack.
  [_] - Immediate. Deletes the child stack with the name of the top of
        the current stack.
  [%] - Pushes the name of the current stack onto the current stack.
  [~] - Pushes the current stack onto the current stack.
  !*  - Creates a new word from the stack on top of the current
        stack. The created word doesn't use it's own substack.
  !+  - Creates a new word from the stack on top of the current
        stack. The created word uses it's own anonymous substack.
  [!] - Sets the word on top of the current stack to be immediate.
  [?] - Sets the word on top of the current stack to not be immediate.
  &   - Pushes the thread of the word on top of the current stack onto
        the stack.
  >r  - Pushes the top of the current stack onto the return stack.
  r>  - Pushes the top of the return stack onto the current stack.
  if  - If the third value on the current stack is nil, leaves the
        top, otherwise leaves the second value.
  PRINT CAR CDR FIRST REST LAST ABS EXP NOT
  + - * / MOD EXPT LOG CONS APPEND > < >= <= = /= EQ EQUAL AND OR
Standard words:
  <^>  - Leaves the parent stack before the top of the current stack.
  <~>  - Leaves the child stack with the name of the top of the
         current stack before the new top of the current stack.
  ^<-  - Pushes the top of the current stack onto the parent stack.
  ^->  - Pushes the top of the parent stack onto the current stack.
  {    - Immediate. Opens a new stack with the name of the top of the
         current stack.
  }*   - Immediate. Closes the current stack while defining is as a
         direct word and then deleting it from the children of its
         parent.
  }+   - Immediate. Closes the current stack while defining it as a
         substack word and then deleting it from the children of its
         parent.
  ||   - Null operation. Does nothing
  R_   - Drops the top of the return stack.
  EAT  - Drops the top of the current stack until dropping NIL.
  HELP - Prints this help.
"
         t print '! }*)