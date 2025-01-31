
if [ "$1" == "" ]; then
  echo "Usage: $0 <number of iterations>"
  exit 1
fi


function run_fuzz {
  mkdir -p .fuzz
  mkdir -p .fuzz/errors
  echo "===== Generating random program"
  python3 ./host/c/gen_graph/gen.py 250 > .fuzz/temp.scm
  echo "===== Compiling random program"
  ./rsc.exe --rvm ./host/c/es.c -t c -l test-es -f+ c/gc/es -x run-test .fuzz/temp.scm
  echo "===== Running random program"
  ./run-test > .fuzz/result.txt
  if [ "$?" != "0" ]; then \
    echo "===== ERROR"; \
  	n=`ls .fuzz/errors | wc -l`; \
  	n=$(($n + 1)); \
  	cp .fuzz/temp.scm .fuzz/errors/$n.scm; \
  	cp .fuzz/result.scm .fuzz/errors/$n.out; \
    exit 1; \
  fi
  cat .fuzz/result.txt | sed "s/\*\*\*REMAINING_RIBS = 0//g" | sed "s/\*\*\*ALIVE_BUT_COLLECTED = 0//g" | grep "\*\*\*"
  if [ "$?" == "0" ]; then \
    echo "===== ERROR"; \
    n=`ls .fuzz/errors | wc -l`; \
    n=$(($n + 1)); \
    cp .fuzz/temp.scm .fuzz/errors/$n.scm; \
    cp .fuzz/result.scm .fuzz/errors/$n.out; \
    exit 1; \
  fi
}


for i in {1..10}; do
  echo "===== $i"
  run_fuzz
done


