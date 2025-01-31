if [ "$1" == "" ] || [ "$2" == "" ]; then
  echo "Usage: $0 <number of iterations> <graph_size>"
  exit 1
fi

function compile_and_run {
  echo "===== Compiling & Running with '$@'"
  ./rsc.exe --rvm ./host/c/es.c -t c -l test-es -f+ c/gc/es -x run-test $@
  $(./run-test > .fuzz/out)
  if [ "$?" != "0" ]; then
    echo "===== ERROR while running program ";
  	n=`ls .fuzz/errors | wc -l`;
  	n=$(($n + 1));
    echo "writing to .fuzz/errors/$n.*";
  	cp .fuzz/run.scm .fuzz/errors/$n.scm;
  	cp .fuzz/out .fuzz/errors/$n.out;
  	cp .fuzz/run.scm.G.dot .fuzz/errors/$n.G.dot;
  	cp .fuzz/run.scm.T.dot .fuzz/errors/$n.T.dot;
    return 1;
  fi
  cat .fuzz/out | sed "s/\*\*\*REMAINING_RIBS = 0//g" | sed "s/\*\*\*ALIVE_BUT_COLLECTED = 0//g" | grep "\*\*\*"
  if [ "$?" == "0" ]; then
    echo "===== ERROR didn't collect all ribs";
    n=`ls .fuzz/errors | wc -l`;
    n=$(($n + 1));
    echo "writing to .fuzz/errors/$n.*";
  	cp .fuzz/run.scm .fuzz/errors/$n.scm;
  	cp .fuzz/out .fuzz/errors/$n.out;
  	cp .fuzz/run.scm.G.dot .fuzz/errors/$n.G.dot;
  	cp .fuzz/run.scm.T.dot .fuzz/errors/$n.T.dot;
    return 1;
  fi
}

function run_fuzz {
  graph_size=$1
  mkdir -p .fuzz
  mkdir -p .fuzz/errors
  echo "===== Generating random program"
  python3 ./host/c/gen_graph/gen.py $graph_size .fuzz/run.scm
  compile_and_run -f+ gc/c/es
  compile_and_run -f+ gc/c/es -f+ queue-no-remove
  compile_and_run -f+ gc/c/es -f+ linked-list
  compile_and_run -f+ gc/c/es  -f+ no-adopt
  compile_and_run -f+ gc/c/es -f+ queue-no-remove -f+ no-adopt
  compile_and_run -f+ gc/c/es -f+ linked-list -f+ no-adopt
  compile_and_run -f+ gc/c/es  -f+ es-roots
  compile_and_run -f+ gc/c/es -f+ queue-no-remove -f+ es-roots
  compile_and_run -f+ gc/c/es -f+ linked-list -f+ es-roots
  compile_and_run -f+ update-ranks -f+ gc/c/es
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ queue-no-remove
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ linked-list
  compile_and_run -f+ update-ranks -f+ gc/c/es  -f+ no-adopt
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ queue-no-remove -f+ no-adopt
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ linked-list -f+ no-adopt
  compile_and_run -f+ update-ranks -f+ gc/c/es  -f+ es-roots
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ queue-no-remove -f+ es-roots
  compile_and_run -f+ update-ranks -f+ gc/c/es -f+ linked-list -f+ es-roots

}

num_iterations=$1
graph_size=$2
i=$((num_iterations - 1))
echo "$i"
iterations=0
while [ $i -ne 0 ]; do
  echo "===== $iterations"
  run_fuzz $graph_size
  i=$((i - 1))
  iterations=$((iterations + 1))
done


