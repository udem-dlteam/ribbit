#!/bin/bash

# Compiling rsc and debugging rsc to Rust
./rsc -t rs -l max -o rsc-btsp0.rs rsc.scm
rustc rsc-btsp0.rs 2> /dev/null
sed -nf rs-activate-debug.sed rsc-btsp0.rs
rustc debug-rsc-btsp0.rs 2> /dev/null

# Compiling the fancy rscs to Rust
gsi rsc.scm -t rs -l max -o fancy-rsc-btsp0.rs rsc.scm
rustc fancy-rsc-btsp0.rs 2> /dev/null
gsi rsc.scm -t rs -l max --enable-feature debug -o debug-fancy-rsc-btsp0.rs rsc.scm
rustc debug-fancy-rsc-btsp0.rs 2> /dev/null

# Compiling non-fancy rscs to Scheme
./rsc -t scm -l max -o test-rsc.scm rsc.scm
gsc -exe test-rsc.scm

sed -nf turn-on-debug.sed test-rsc.scm 
mv debug-sed-result.scm dbg-rsc.scm
gsc -exe dbg-rsc.scm

# Compiling fancy rscs to Scheme
gsi rsc.scm -t scm -l max -o fancy-test-rsc.scm rsc.scm
gsc -exe fancy-test-rsc.scm

gsi rsc.scm -t scm -l max --enable-feature debug -o fancy-dbg-rsc.scm rsc.scm
gsc -exe fancy-dbg-rsc.scm



echo "----Compiling min-----"
echo "Rust rsc"
time ./rsc -t rs -l empty -c "./rsc-btsp0" -o min.rs lib/min.scm

echo "Scheme rsc"
time ./rsc -t rs -l empty -c "./test-rsc" -o min.rs lib/min.scm

echo "Rust rsc with debug"
time ./rsc -t rs -l empty -c "./debug-rsc-btsp0" -o min.rs lib/min.scm 2> /dev/null

echo "Scheme rsc with debug"
time ./rsc -t rs -l empty -c "./dbg-rsc" -o min.rs lib/min.scm > /dev/null

echo "Fancy Rust rsc"
time ./rsc -t rs -l empty -c "./fancy-rsc-btsp0" -o min.rs lib/min.scm

echo "Fancy Scheme rsc"
time ./rsc -t rs -l empty -c "./fancy-test-rsc" -o min.rs lib/min.scm

echo "Fancy Rust rsc with debug"
time ./rsc -t rs -l empty -c "./debug-fancy-rsc-btsp0" -o min.rs lib/min.scm 2> /dev/null

echo "Fancy Scheme rsc with debug"
time ./rsc -t rs -l empty -c "./fancy-dbg-rsc" -o min.rs lib/min.scm > /dev/null


echo "----Compiling max-----"
echo "Rust rsc"
time ./rsc -t rs -l min -c "./rsc-btsp0" -o minmax.rs lib/max.scm

echo "Scheme rsc"
time ./rsc -t rs -l min -c "./test-rsc" -o minmax.rs lib/max.scm

echo "Rust rsc with debug"
time ./rsc -t rs -l min -c "./debug-rsc-btsp0" -o minmax.rs lib/max.scm 2> /dev/null

echo "Scheme rsc with debug"
time ./rsc -t rs -l min -c "./dbg-rsc" -o minmax.rs lib/max.scm > /dev/null

echo "Fancy Rust rsc"
time ./rsc -t rs -l min -c "./fancy-rsc-btsp0" -o minmax.rs lib/max.scm

echo "Fancy Scheme rsc"
time ./rsc -t rs -l min -c "./fancy-test-rsc" -o minmax.rs lib/max.scm

echo "Fancy Rust rsc with debug"
time ./rsc -t rs -l min -c "./debug-fancy-rsc-btsp0" -o minmax.rs lib/max.scm 2> /dev/null

echo "Fancy Scheme rsc with debug"
time ./rsc -t rs -l min -c "./fancy-dbg-rsc" -o minmax.rs lib/max.scm > /dev/null

rm *sc-btsp0.rs *sc-btsp0 *est-rsc.scm *est-rsc *bg-rsc.scm *bg-rsc min.rs minmax.rs

echo "Done."
