# Script usefull for diffing the output of two rsc compilers. It runs them, then compare their results

v1="boot"
v2="gsc"
prog=$1
shift

#make rsc-gsc.exe
./rsc-clean.exe -ps -f- debug-gc -f+ debug -f- debug-print -f- debug-phases -t c -l r4rs -l prim-wrap  -l boot-host -e original -f+ v-port -o rsc-bootstrap1.c rsc.scm
gcc rsc-bootstrap1.c -o rsc-bootstrap1.exe

cmd1="./rsc-bootstrap1.exe $@ -o $prog.$v1.c"
cmd2="gsi rsc.scm $@ -o $prog.$v2.c"

echo "Running $cmd1"
$cmd1 | tee out1
echo "Running $cmd2"
$cmd2 | tee out2

echo ""
echo "=== Comparing outputs ==="
echo ""
diff out1 out2

echo ""
echo "=== Comparing generated files ==="
echo ""
diff $prog.$v1.c $prog.$v2.c

exit $?
