#!/bin/bash
# Run a picobit benchmark


path="run.scm"
filename="${path%.*}" 

pushd fpicobit > /dev/null 2>&1

cat /dev/stdin > "$path"

sed -i 's/(run)/(display (run))/' "$path" > /dev/null 2>&1 
./picobit "$path"
./picobit-vm "$filename".hex
popd > /dev/null 2>&1
