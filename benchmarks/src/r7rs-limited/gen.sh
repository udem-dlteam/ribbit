# Utilitary script to generate benchmarks from r7rs-limited benchmark suite
#
# It is not ment to be called directly, rather this file is ment to be
# imported. Use script/generate_benchmark to run this script.
# 

. "$BENCH_ROOT/scripts/common.sh" # import common functions

SCRIPT_ROOT=$BENCH_ROOT/src/r7rs-limited

generate_benchmark () {
  local system repetitions junk_size outfile
  parse_args system=ribbit repetitions=1 junk_size=0 outfile=? benchmark=? -- $@

  cat $SCRIPT_ROOT/prefix/all.scm > $outfile
  echo "(define repetitions $repetitions)" >> $outfile

  if [ "$junk_size" -gt 0 ]; then
    echo "(define %_____junk-data_____ (make-list $JUNKSIZE 0))" >> $outfile
  fi

  cat $SCRIPT_ROOT/prefix/$system.scm >> $outfile
  cat $SCRIPT_ROOT/$benchmark.scm >> $outfile

  if [ "$junk_size" -gt 0 ]; then
    echo "((lambda (x) (if (null? x) x (car x))) %_____junk-data_____)" >> $outfile
  fi

  unset system repetitions junk_size outfile benchmark
}

get_benchmark_repetitions () {
    case $1 in
        ack) echo 5 ;;
        array1) echo 20 ;;
        boyer) echo 5 ;;
        browse) echo 300 ;;
        bv2string) echo 1 ;;
        chudnovsky) echo 1 ;;
        cpstak) echo 75 ;;
        ctak) echo 75 ;;
        deriv) echo 150000 ;;
        destruc) echo 40 ;;
        diviter) echo 500000 ;;
        divrec) echo 500000 ;;
        earley) echo 50 ;;
        equal) echo 2 ;;
        fib) echo 180 ;;
        fibc) echo 40 ;;
        gcbench) echo 2 ;;
        graphs) echo 1 ;;
        lattice) echo 3000 ;;
        matrix) echo 25 ;;
        maze) echo 100 ;;
        mazefun) echo 100 ;;
        mperm) echo 5 ;;
        nboyer) echo 2 ;;
        nqueens) echo 5 ;;
        ntakl) echo 1 ;;
        paraffins) echo 60 ;;
        peval) echo 20 ;;
        primes) echo 6 ;;
        puzzle) echo 1 ;;
        quicksort) echo 2 ;;
        sboyer) echo 2 ;;
        scheme) echo 2000 ;;
        string) echo 2 ;;
        sum) echo 300 ;;
        tak) echo 100 ;;
        micro_loop_make_list) echo 1000 ;;
        micro_primitive_make_list) echo 1000 ;;
        micro_set_make_list) echo 1000 ;;
        *) return 1 ;;
    esac
}
