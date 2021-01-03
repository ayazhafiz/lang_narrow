#!/bin/sh
# Run from top level of repository

for f in examples/*.txt; do
  dune exec "src/main.exe" -- -M "eval" "$f" &> "examples/$(basename "$f" .txt).eval"
  dune exec "src/main.exe" -- -M "codegenC-rt" "$f" &> "examples/$(basename "$f" .txt).c"
done
