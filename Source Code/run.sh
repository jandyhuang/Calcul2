#!/bin/sh

# compile the input file
./calcul < $1 

# compile cpp file
g++ -o output output.cpp

# run the cpp executation
./output | tee output.out
