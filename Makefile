PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
LIBDIR ?= $(PREFIX)/lib/poslin
P0 = $(BINDIR)/poslin0
P1 = $(BINDIR)/poslin1
P2 = $(BINDIR)/poslin

all: build/poslin0
.PHONY: all

build/poslin0: poslin.asd package.lisp utility/symb.lisp utility/flatten.lisp \
	utility/sharp-backquote.lisp utility/defmacro-help.lisp \
	utility/defmacro.lisp utility/dlambda.lisp utility/plambda-help.lisp \
	utility/plambda.lisp utility/anaphora.lisp utility/group.lisp \
	source/structures/nothing.lisp source/structures/symbol.lisp \
	source/structures/binding.lisp source/structures/thread.lisp \
	source/structures/path.lisp source/structures/bool.lisp \
	source/structures/compare.lisp source/structures/quotation.lisp \
	source/structures/exception.lisp source/interaction/interpreter.lisp \
	source/interaction/new-poslin.lisp source/interaction/run-poslin.lisp \
	source/prims/macros.lisp source/prims/prim.lisp source/repl/read.lisp \
	source/repl/print.lisp source/repl/repl.lisp ./compile-poslin.lisp
	sbcl --noinform --disable-ldb --lose-on-corruption \
		--dynamic-space-size 8192 \
		--userinit ./sbclinit \
		--load ./compile-poslin.lisp
	mv -v ./poslin0 build/poslin0

.PHONY: clean
clean:
	rm -vf ./build/poslin0

.PHONY: install
install: all
	mkdir -pv $(BINDIR)
	mkdir -pv $(LIBDIR)
	cp -rv ./lib/* $(LIBDIR)/
	cp -v ./build/poslin0 $(P0)
	echo "#!/bin/sh\n" > $(BINDIR)/poslin1
	echo "exec $(P0) $(LIBDIR)/base.poslin "$$"*" >> $(P1)
	echo "#!/bin/sh\n" > $(P2)
	echo "exec $(P1) $(LIBDIR)/supplemental/package.poslin \\" \
		>> $(P2)
	echo "	$(LIBDIR)/supplemental/generic-op.poslin "$$"*" >> $(P2)
	chmod +x $(P1)
	chmod +x $(P2)

.PHONY: libinstall
libinstall:
	mkdir -pv $(LIBDIR)
	cp -rv ./lib/* $(LIBDIR)/
