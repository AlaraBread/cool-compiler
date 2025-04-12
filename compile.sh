#!/bin/bash

cd src
ghc Main.hs -prof -fprof-auto -outputdir ../build -o ../build/Main
cd ..
cool --type "$1"
./build/Main "${1%.*}.cl-type" +RTS -xc
gcc -g -no-pie -static "${1%.*}.s"
