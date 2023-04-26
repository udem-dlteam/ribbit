#!/bin/bash

./rsc -l max -o rsc.rvm rsc.scm
./rsc -l empty -o bonjour.rvm bonjour.scm 

gsi rsc.scm -l max -o fancy-rsc.rvm rsc.scm 
gsi rsc.scm -l empty -o fancy-bonjour.rvm bonjour.scm

./rsc -t rs -l max -o rsc-btsp0.rs rsc.scm
sed -f rs-activate-debug.sed rsc-btsp0.rs > /dev/null
rustc debug-rsc-btsp0.rs 2> /dev/null
rustc rsc-btsp0.rs 2> /dev/null
echo 'Finished compiling rsc-btsp0'

cp host/rs/rvm.rs blank-rvm.rs

echo '(putchar 66)(putchar 111)(putchar 110)(putchar 106)(putchar 111)(putchar 117)(putchar 114)(putchar 46)(putchar 10)-1'|./rsc-btsp0 2> bonjour.err.txt | tee > bonjour0.rvm
echo 'Finished compiling bonjour0.rvm from rsc-btsp0'

replacement="$(cat bonjour0.rvm)"
sed -e s/");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{"/$replacement/ -e "w bonjour0.rs" blank-rvm.rs > /dev/null
rustc bonjour0.rs 2> /dev/null
echo 'Writing output of bonjour0.rs'
./bonjour0

gsi rsc.scm -t rs -l max -o fancy-rsc-btsp0.rs rsc.scm
sed -f rs-activate-debug.sed fancy-rsc-btsp0.rs > /dev/null
rustc debug-rsc-btsp0.rs -o debug-fancy-rsc-btsp0 2> /dev/null
rustc fancy-rsc-btsp0.rs 2> /dev/null
echo 'Finished compiling fancy-rsc-btsp0'

echo '(putchar 66)(putchar 111)(putchar 110)(putchar 106)(putchar 111)(putchar 117)(putchar 114)(putchar 46)(putchar 10)-1'|./fancy-rsc-btsp0 2> fancy-bonjour.err.txt | tee > fancy-bonjour0.rvm
echo 'Finished compiling fancy-bonjour0.rvm from fancy-rsc-btsp0'

replacement="$(cat fancy-bonjour0.rvm)"
sed -e s/");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{"/$replacement/ -e "w fancy_bonjour0.rs" blank-rvm.rs > /dev/null
rustc fancy_bonjour0.rs 2> /dev/null
echo 'Writing output of fancy_bonjour0.rs'
./fancy_bonjour0

echo 'diff bonjour.rvm and bonjour0.rvm'
diff bonjour.rvm bonjour0.rvm
echo 'diff fancy-bonjour.rvm and fancy-bonjour0.rvm'
diff fancy-bonjour.rvm fancy-bonjour0.rvm 

cat bonjour.scm|./fancy-rsc-btsp0 2> file-fancy-bonjour.err.txt | tee > file-fancy-bonjour0.rvm

replacement="$(cat file-fancy-bonjour0.rvm)"
sed -e s/");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{"/$replacement/ -e "w file_fancy_bonjour0.rs" blank-rvm.rs > /dev/null
rustc file_fancy_bonjour0.rs 2> /dev/null
echo 'Writing output of file_fancy_bonjour0.rs'
./file_fancy_bonjour0

echo 'diff fancy-bonjour.rvm and file-fancy-bonjour0.rvm'
diff fancy-bonjour.rvm file-fancy-bonjour0.rvm 

echo 'diff bonjour.rvm and fancy-bonjour0.rvm'
diff bonjour.rvm fancy-bonjour0.rvm

echo '(display "hello!\n")' > h.scm

echo 'Comparing timing of -l min and -l max on rsc-btsp0.rs'
echo 'min'
time ./rsc -t rs -l min -c ./rsc-btsp0 -o h.rs h.scm 
rustc h.rs 2> /dev/null
./h
#echo 'Timing with debug, tracing on'
#time ./rsc -t rs -l min -c ./debug-rsc-btsp0 -o h.rs h.scm 2> /dev/null
#rustc h.rs 2> /dev/null
#./h
echo 'max'
time ./rsc -t rs -l max -c ./rsc-btsp0 -o h.rs h.scm 
rustc h.rs 2> /dev/null
./h
#echo 'Timing with debug, tracing on'
#time ./rsc -t rs -l max -c ./debug-rsc-btsp0 -o h.rs h.scm 2> /dev/null
#rustc h.rs 2> /dev/null
#./h

echo 'Comparing timing of -l min and -l max on fancy-rsc-btsp0.rs'
echo 'min'
time ./rsc -t rs -l min -c ./fancy-rsc-btsp0 -o rs h.scm 
rustc h.rs 2> /dev/null
./h
#echo 'Timing with debug, tracing on'
#time ./rsc -t rs -l min -c ./debug-fancy-rsc-btsp0 -o rs h.scm 2> /dev/null
#rustc h.rs 2> /dev/null
#./h
echo 'max'
time ./rsc -t rs -l max -c ./fancy-rsc-btsp0 -o rs h.scm 
rustc h.rs 2> /dev/null
./h
#echo 'Timing with debug, tracing on'
#time ./rsc -t rs -l max -c ./debug-fancy-rsc-btsp0 -o rs h.scm 2> /dev/null
#rustc h.rs 2> /dev/null
#./h

echo 'Starting Second-generation Rust rvm compilation'
echo 'Starting rsc-btsp1...'
time (cat rsc.scm | ./rsc-btsp0 2> rsc1.err.txt | tee > rsc1.rvm)

echo 'Starting fancy-rsc-btsp1...'
time (cat rsc.scm | ./fancy-rsc-btsp0  2> fancy-rsc1.err.txt | tee > fancy-rsc1.rvm)

echo 'diff rsc.rvm and rsc1.rvm'
diff rsc.rvm rsc1.rvm
echo 'diff fancy-rsc.rvm and fancy-rsc1.rvm'
diff fancy-rsc.rvm fancy-rsc1.rvm 
 
echo 'Done.'
