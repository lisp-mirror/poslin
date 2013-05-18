(in-package #:poslin)

(defunary
  exp "( n -- n^e )"
  not "( T -- NIL ) | ( NIL -- T )"
  print "( val -- val )"
  oddp "( n -- bool )"
  evenp "( n -- bool )")

(defbinary
  + "( n1 n2 -- n1+n2 )"
  - "( n1 n2 -- n1-n2 )"
  * "( n1 n2 -- n1*n2 )"
  / "( n1 n2 -- n1/n2 )"
  = "( n1 n2 -- bool )"
  /= "( n1 n2 -- bool )"
  < "( n1 n2 -- bool )"
  > "( n1 n2 -- bool )"
  <= "( n1 n2 -- bool )"
  >= "( n1 n2 -- bool )"
  log "( n1 n2 -- logn1n2 )"
  expt "( n1 n2 -- n1^n2 )"
  and "( bool1 bool2 -- bool1ANDbool2 )"
  or "( bool1 bool2 -- bool1ORbool2 )"
  eq "( val1 val2 -- bool )"
  max "( n1 n2 -- max )"
  min "( n1 n2 -- min )")