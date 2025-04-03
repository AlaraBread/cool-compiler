#!/bin/bash

cd src
ghc Main.hs -prof -fprof-auto -outputdir ../build
cd ..
cool --type "$1"
./src/Main "${1%.*}.cl-type" +RTS -xc
gcc -g -no-pie -static "${1%.*}.s"
