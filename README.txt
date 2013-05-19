0. INDEX
   Index
   Installation
   Usage
   Known problems

1. INSTALLATION
   You need quicklisp installed. Put the poslin folder somewhere where
   quicklisp will be able to find it, then load it in your preferred
   Common Lisp REPL via
   (ql:quickload "poslin")

2. USAGE
   Start your Common Lisp REPL and load poslin.

   You can either run poslin in a REPL or feed a defined poslin
   environment a list of commands.

   To run the REPL just run
   > (poslin:repl)
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
   > (run-poslin (poslin)
   enter the poslin code and close the lisp form. POSLIN-ENV might
   take a moment to run.

3. KNOWN PROBLEMS
   Poslins error handling is crude to say the least. All kinds of
   stupid errors and typos can crash a running poslin session.
