#!/bin/bash

cat $1 >input.alg
export PKG_CONFIG_PATH=/opt/local/lib/pkgconfig
./compiler
g++ -o $2 output.c `pkg-config --cflags --libs opencv`

