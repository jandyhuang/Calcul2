#!/bin/sh
PATH=.:..:$PATH
/bin/rm -f test test.ml test.cmi test.cmi test.cmo test.o test.cpp
cWriter.native
if [ -f test.cpp ]; then
g++ -g -O -Wall -o test -I.. test.cpp
./test
fi
/bin/rm -f test test.ml test.cmi test.cmi test.cmo test.o test.cpp

