#!/bin/bash
# Run a BIT benchmark

path="run.scm"
filename="${path%.*}" 

pushd bit-scheme > /dev/null 2>&1

rm -rf "$path"
while read line
do
    echo "$line" >> "$path"
done

sed -i 's/(run)/(display (run))/' "$path" > /dev/null 2>&1 
make "run.c" && make "run" 
./run

popd > /dev/null 2>&1
