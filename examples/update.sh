#!/bin/bash
# Run from top level of repository

for f in examples/*.{eval,c,out,emit}; do
  rm "$f"
done

for f in examples/*.txt; do
  printf "\e[1mProcessing $f...\e[0m\n"

  bf="examples/$(basename "$f" .txt)"
  dune exec "src/main.exe" -- -M "eval" "$f" &> "$bf.eval"
  dune exec "src/main.exe" -- -M "codegenC-rt" "$f" &> "$bf.c"

  c_emit_file="examples/$(basename "$f" .txt).c.emit"
  gcc "$bf.c" -o "$bf.out"
  ./$bf.out &> "$bf.c.emit"

  # I love fancy colors don't hate me
  printf "\n\e[4meval:\e[0m\n\e[33m"
  cat "$bf.eval"
  printf "\e[0m\n"
  printf "\n\e[4mc emit:\e[0m\n\e[36m"
  cat "$bf.c.emit"
  printf "\e[0m\n"
done
