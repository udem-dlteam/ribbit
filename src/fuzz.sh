if [ "$1" == "" ] || [ "$2" == "" ]; then
  echo "Usage: $0 <number of iterations> <graph_size>"
  exit 1
fi

TIMEOUT=60 # seconds

function error {
  echo_ "==> ERROR: $1"
  echo_ "  - Iteration: $3"
  echo_ "  - Config: $4"
  echo_ "  - Saving files to $2"

  file_dir="$2"
  iters="$3"
  config="$4"

  mkdir -p $file_dir/$iters
  dst_dir=$file_dir/$iters

  echo "$1" > $dst_dir/$config.error;
  
  cp $file_dir/run.c         $dst_dir/$config.c;
  cp $file_dir/out           $dst_dir/$config.out;
  cp $file_dir/run.scm       $dst_dir/run.scm;
  cp $file_dir/run.scm.G.dot $dst_dir/G.dot;
  cp $file_dir/run.scm.T.dot $dst_dir/T.dot;
}

function compile_and_run {
  file_dir=$1
  args=$2
  iters=$3
  readable_config=$(echo $args | sed "s/ /_/g" | sed "s/-f+//g" | sed "s/\//_/g" )

  echo_ "==> $4 Compiling & Running with '$args'"
  ./rsc.exe --rvm ./host/c/es.c -t c -l test-es -f+ c/gc/es -o $file_dir/run.c -x $file_dir/run.exe $file_dir/run.scm $args
  if [ "$?" != "0" ]; then
    error "Failed to compile" "$file_dir" "$iters" "$readable_config"
    return 1;
  fi
  timeout $TIMEOUT $file_dir/run.exe > $file_dir/out 2>&1
  status=$?
  if [ "$status" != "0" ]; then
    if [ "$status" == "124" ]; then
      error "Test timed out"  "$file_dir" "$iters" "$readable_config"
    else
      echo_ $(cat $file_dir/out)
      error "Failed to run test" "$file_dir" "$iters" "$readable_config"
    fi
    return 1;
  fi
  cat $file_dir/out | sed "s/\*\*\*REMAINING_RIBS = 0//g" | sed "s/\*\*\*ALIVE_BUT_COLLECTED = 0//g" | grep "\*\*\*"
  if [ "$?" == "0" ]; then
    error "Test didnt collect all garbage" "$file_dir" "$iters" "$readable_config"
    return 1;
  fi
}

function run_fuzz {
  graph_size=$1
  FUZZ_RUN_DIR=$2
  iters=$3
  echo "===== Generating random program"
  python3 ./host/c/gen_graph/gen.py $graph_size $FUZZ_RUN_DIR/run.scm
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ queue-no-remove" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ linked-list" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es  -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ queue-no-remove -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ linked-list -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es  -f+ es-roots" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ queue-no-remove -f+ es-roots" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ gc/c/es -f+ linked-list -f+ es-roots" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ queue-no-remove" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ linked-list" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es  -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ queue-no-remove -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ linked-list -f+ no-adopt" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es  -f+ es-roots" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ queue-no-remove -f+ es-roots" "$iters"
  compile_and_run "$FUZZ_RUN_DIR" "-f+ update-ranks -f+ gc/c/es -f+ linked-list -f+ es-roots" "$iters"
}

function echo_ {
  echo $@ | tee -a $FUZZ_RUN_DIR/fuzz.log
}

FUZZ_DIR=.fuzz_fail
mkdir -p .fuzz_fail

FUZZ_INDEX=0
while ! mkdir $FUZZ_DIR/$FUZZ_INDEX; do
  FUZZ_INDEX=$((FUZZ_INDEX + 1))
done

FUZZ_RUN_DIR=$FUZZ_DIR/$FUZZ_INDEX

echo "Starting fuzzing, using $FUZZ_RUN_DIR as temp data"
echo "TIMEOUT=$TIMEOUT"
echo "FUZZ_INDEX=$FUZZ_INDEX"

num_iterations=$1
graph_size=$2
i=$((num_iterations - 1))
iterations=0
while [ $i -ne 0 ]; do
  echo_ "==> Running $iterations"
  run_fuzz $graph_size $FUZZ_RUN_DIR $num_iterations
  i=$((i - 1))
  iterations=$((iterations + 1))
done

