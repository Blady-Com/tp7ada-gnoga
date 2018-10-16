# Makefile for TP7Ada

BUILDER=gprbuild
CLEANER=gprclean
INSTALLER=gprinstall
XAdaLibPath=/usr/local/xnadalib-2018
PREFIX=$(CURDIR)/inst

all: examples

tp7ada:
	$(BUILDER) -p -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr -P src/tp7.gpr

.PHONY: examples
examples:
	cd examples && $(XAdaLibPath)/bin/zbmcompile -i -v Courbes_Mesg -G strings -X ignore courbes
	cd examples && $(XAdaLibPath)/bin/zbmcompile -i -v Surfaces_Mesg -G strings -X ignore surfaces
	cd examples && $(XAdaLibPath)/bin/zbmcompile -i -v Sudoku_Mesg -G strings -X ignore sudoku
	$(BUILDER) -p -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr -P examples/examples.gpr

install:
	$(INSTALLER) -p -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr --prefix=$(PREFIX) --install-name=tp7ada-gnoga -P src/tp7.gpr

uninstall:
	$(INSTALLER) -p -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr --uninstall --prefix=$(PREFIX) --install-name=tp7ada-gnoga -P src/tp7.gpr

rm-docs:
	GPR_PROJECT_PATH=$(XAdaLibPath)/lib/gnat:$(XAdaLibPath)/share/gpr gnatdoc --no-subprojects -P src/tp7.gpr

clean:
	$(CLEANER) -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr -P src/tp7.gpr
	$(CLEANER) -aP$(XAdaLibPath)/lib/gnat -aP$(XAdaLibPath)/share/gpr -P examples/examples.gpr
