# This profile compares the execution between the Arborescent garbage collector
# and other GCs within the C host


# Three phases: Compilation of Ribbit
BENCH_ROOT="$(cd -- $(dirname $0)/.. && pwd -P)"

. $BENCH_ROOT/scripts/gen_benchmarks.sh

#generate_benchmarks bench_suite="r7rs-limited"

compile_add_arg_toggle "debug" "-f+ debug"
compile_add_arg_toggle "cycle_profiling" "-f+ c/gc/profile-cycles"
compile_add_arg_toggle "flat_closures" "-f+ flat-closure"
compile_add_arg_list "gc" \
  "stop_and_copy" "" \
  "mark_and_sweep" "-f+ c/gc/mark-sweep" \
  "even-shiloach" "-f+ c/gc/es -f+ queue-no-remove -r host/c/agc.c" \
  "even-shiloach-no-global-rank-counter" "-f+ c/gc/es -f+ no-global-rank-counter -f+ queue-no-remove -r host/c/agc.c" \
  "ett" "-f+ c/gc/ett -r host/c/agc.c"

compile_add_arg_toggle "profile_min_heap" "-f+ c/gc/min-heap-size"

add_compile_arg_number__rerank_down_limit () { 
  if [ "$1" == "0" ]
    echo ""
  else
    "-f+ adupt-drop-depth-$1"
  fi
}
add_compile_arg_func "rerank_down_limit" add_compile_arg_number__rerank_down_limit

default_compile_args debug=false cycle_profiling=false flat_closures=false gc=stop_and_copy profile_min_heap=false rerank_down_limit=0






compile_benchmarks 







# BENCHMARK_SUITE="r7rs-limited"




# compare_host_setting ".." "c" "out"

