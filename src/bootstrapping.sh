#!/bin/bash

./rsc -l max rsc.scm > rsc.rvm
./rsc -l empty bonjour.scm > bonjour.rvm

gsi rsc.scm -l max rsc.scm > fancy-rsc.rvm
gsi rsc.scm -l empty bonjour.scm > fancy-bonjour.rvm

./rsc -t rs -l max -o rsc-btsp0.rs rsc.scm
sed -f rs-activate-debug.sed rsc-btsp0.rs > /dev/null
rustc debug-rsc-btsp0.rs -o rsc-btsp0 2> /dev/null
echo 'Finished compiling rsc-btsp0'

echo '(putchar 66)(putchar 111)(putchar 110)(putchar 106)(putchar 111)(putchar 117)(putchar 114)(putchar 46)(putchar 10)-1'|./rsc-btsp0 2> bonjour.err.txt | tee > bonjour0.rvm
echo 'Finished compiling bonjour0.rvm from rsc-btsp0'

gsi rsc.scm -t rs -l max -o fancy-rsc-btsp0.rs rsc.scm
sed -f rs-activate-debug.sed fancy-rsc-btsp0.rs > /dev/null
rustc debug-rsc-btsp0.rs -o fancy-rsc-btsp0 2> /dev/null
echo 'Finished compiling fancy-rsc-btsp0'

echo '(putchar 66)(putchar 111)(putchar 110)(putchar 106)(putchar 111)(putchar 117)(putchar 114)(putchar 46)(putchar 10)'|./fancy-rsc-btsp0 2> fancy-bonjour.err.txt | tee > fancy-bonjour0.rvm
echo 'Finished compiling fancy-bonjour0.rvm from fancy-rsc-btsp0'

echo 'diff bonjour.rvm and bonjour0.rvm'
diff bonjour.rvm bonjour0.rvm
echo 'diff fancy-bonjour.rvm and fancy-bonjour0.rvm'
diff fancy-bonjour.rvm fancy-bonjour0.rvm 

echo 'Starting Second-generation Rust rvm compilation'
echo 'Starting rsc-btsp1...'
cat rsc.scm | ./rsc-btsp0 2> rsc1.err.txt | tee > rsc1.rvm

echo 'Starting fancy-rsc-btsp1...'
cat rsc.scm | ./fancy-rsc-btsp0  2> fancy-rsc1.err.txt | tee > fancy-rsc1.rvm

echo 'diff rsc.rvm and rsc1.rvm'
diff rsc.rvm rsc1.rvm
echo 'diff fancy-rsc.rvm and fancy-rsc1.rvm'
diff fancy-rsc.rvm fancy-rsc1.rvm 

echo 'Done.'
