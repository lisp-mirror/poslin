# Installation


## Dependencies

You need [sbcl](http://sbcl.org) and [quicklisp](https://www.quicklisp.org).

## Compiling

Simply run `make` in the toplevel of the repository.

## Installation

### Linux

Run `make install`. This will install the poslin binary `poslin0` and the
scripts `poslin1` and `poslin` in `/usr/local/bin` as well as putting the poslin
standard libraries in `/usr/local/lib/poslin`.

If you want to install in another prefix, specify run make with the `PREFIX`
environment variable set to the desired prefix, like this:
`PREFIX=~/.local make install`. The binary/scripts will end up in the `bin`
subdirectory, the libraries in the `lib/poslin` subdirectory.

When running `make install` as root user, you either need to have quicklisp
available for the root user or you need to run `make` as a user with quicklisp
available beforehand.
