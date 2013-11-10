TARGETS=setup validationtest cWriter 
CPP_TARGETS=setupBase.o
NATIVE_TARGETS=$(TARGETS:=.native)
DEBUG_TARGETS=$(TARGETS:=.d.byte)
CFLAGS=-annot
OCAMLBUILD=ocamlbuild -cflags $(CFLAGS)
RM=rm
PYTHON=python

all:	$(CPP_TARGETS)
	$(OCAMLBUILD) $(NATIVE_TARGETS) $(DEBUG_TARGETS)

clean:
	$(RM) -rf _build
	$(RM) -f *.native
	$(RM) -f *.d.byte

test:	all
	cd tests && $(PYTHON) run_tests.py

setupBase.o:
	g++ -g -O -Wall -c setupBase.h
