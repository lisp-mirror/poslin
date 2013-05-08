0. INDEX
   Index
   Installation
   Usage
   Known problems

1. INSTALLATION
   You need asdf and either quicklisp or alexandria and osicat to run
   poslin. Put the poslin sources into a directory where asdf can find
   them. Set the environment variable POSLIN_HOME to
   "[source-dir]/poslin/" where [source-dir] is the path of the source
   directory. This is necessary so poslin can find it's standard
   library and load it.

2. USAGE
   Start your Common Lisp REPL, load poslin and enter the poslin
   package.

   You can either run poslin in a REPL or feed a defined poslin
   environment a list of commands.

   To run the REPL just run
   > (poslin-repl)
   If poslin has been set up right you will see the following:
   [ROOT:]
   >
   The first line prints the current stack path followed by the
   contents of the current stack. The second line is the prompt. The
   REPL uses the function READ-FROM-STRING, so Common Lisps read
   macros are available. List structures consequently need to be
   entered on one line or the REPL will terminate with a read error.

   To run a defined poslin with a list of commands you use the macros
   POSLIN-ENV and RUN-POSLIN. POSLIN-ENV returns a fresh poslin
   environment that has the primary operators and standard library
   loaded. RUN-POSLIN takes a poslin environment as it's first
   parameter and a &REST parameter for the commands to execute in that
   poslin environment.
   If you want to use a throwaway environment, just type
   > (poslin-run (poslin-env)
   enter the poslin code and close the lisp form. POSLIN-ENV might
   take a moment to run.

3. KNOWN PROBLEMS
   SBCL1.1.7.0 emits a style warning when running POSLIN-ENV.
   ; in: POSLIN-ENV
   ;     (POSLIN::PREPARE)
   ; --> PROGN POSLIN::INSTALL-PRIMS PROGN LET LAMBDA FUNCTION
   ; --> POSLIN::ARGS LET POSLIN::POP-CURR IF POSLIN::PERROR PROGN LET
   ; --> FORMAT FORMAT
   ; ==>
   ;   NIL
   ;
   ; caught STYLE-WARNING:
   ;   This is not a UNSIGNED-BYTE:
   ;     NIL
   ;   See also:
   ;     The SBCL Manual, Node "Handling of Types"
   ;
   ; compilation unit finished
   ;   caught 1 STYLE-WARNING condition
   This does not seem to pose any problems when running poslin but is
   just visually annoying. Any pointers to where that warning might
   come from are welcome.

   Trying to compile POSLIN-REPL into an executable via
   (sb-ext:save-lisp-and-die "poslin" :executable t :toplevel
   			     #'poslin-repl)
   seems to work at first, but the resulting REPL never executes any
   operators and just fills up the root stack with anything fed to it.
