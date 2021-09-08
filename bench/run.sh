#!/bin/bash

tests=$(ls *.scm)

space() {
    echo 
    echo 
    echo 
    echo 
}

pushd ../src/vm_c >> /dev/null
make clean >> /dev/null
make
popd >> /dev/null
space

for test in $tests
do
    echo "=============================================="
    echo "=============================================="
    echo "Running $test"
    cat "$test" | time ../src/vm_c/rVM 
    space
done
