#!/bin/sh
PATH=.:..:$PATH
/bin/rm -f test test.ml test.cmi test.cmi test.cmo test.o test.cpp
g++ -g -O -Wall -o test -I.. -x c++ - 
./test
/bin/rm -f test test.ml test.cmi test.cmi test.cmo test.o test.cpp

