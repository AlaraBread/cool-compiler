#!/usr/bin/env fish

mkdir -p build
cd src
ghc Main.hs -outputdir ../build -o ../build/Main
cd ..

nix develop .#static --command ./test2.fish
