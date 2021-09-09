#!/bin/bash
# Run a BIT benchmark

path="run.scm"
filename="${path%.*}" 

pushd bit-scheme > /dev/null 2>&1

cat /dev/stdin > "$path"
sed -i 's/(run)/(display (run))/' "$path" > /dev/null 2>&1 
make "run.c" && make "run" 
./run

popd > /dev/null 2>&1
