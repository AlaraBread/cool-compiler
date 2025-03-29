#!/bin/bash

cd src
ghc main.hs -outputdir ../build
cd ..
cool --type "$1"
./src/main "${1%.*}.cl-type"
gcc -no-pie -static "${1%.*}.s"
