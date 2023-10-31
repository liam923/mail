#!/usr/bin/env sh

# This script will compile all 

cd "${0%/*}"
cd ..

for f in examples/*.mail; do
  cabal run mailc -v0 -- "$f" -o examples --llvm-only
done
