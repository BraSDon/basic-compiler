#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

filename=$1

echo "Filepath input: examples/$filename.txt"
echo "Filepath output: examples/c-files/$filename.c"
echo "Compiling..."

cargo run -r $filename
cd examples/c-files/
gcc -o ./$filename.out $filename.c

echo "Running... \n"
./$filename.out
