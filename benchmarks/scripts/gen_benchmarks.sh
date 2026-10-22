
. "$BENCH_ROOT/scripts/common.sh" # import common functions

generate_benchmarks () {
  local repetitions outdir benchmarks bench_suite should_clean
  parse_args repetitions=default outdir="$BENCH_ROOT/out/benchs" benchmarks="*" bench_suite="r7rs-limited" should_clean="yes" -- $@

  echo "Generating benchmarks from $bench_suite into $outdir..."

  if ! [ -d  $outdir ]; then
    mkdir $outdir
  fi

  if [ "$should_clean" == "yes" ]; then
    echo "Cleaning up $outdir"
    rm $outdir/*
  fi

  # Import gen.sh
  bench_suite_root="$BENCH_ROOT/src/$bench_suite/"
  . $bench_suite_root/gen.sh

  if [ "$benchmarks" == "*" ]; then
    benchmarks=$(ls $bench_suite_root | grep .scm | sed 's/\.scm$//')
  fi

  for bench in $benchmarks; do
    if [ "$repetitions" == "default" ]; then
      default_rep=$(get_benchmark_repetitions "$bench")
    else
      echo "Error: invalid repetitions"
      exit 2
    fi
    generate_benchmark outfile="$outdir/${bench}__reps=$default_rep.scm" benchmark="$bench" repetitions="$default_rep"
  done


  unset repetitions outdir benchmarks bench_suite should_clean bench
}


