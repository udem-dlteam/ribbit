host=$1;
repl=$2;
prog=$3;

TEST_DIR=tests;
TEST_FILTER="*";

for prog in `ls $TEST_DIR/01-r4rs/*.scm`; do
  echo "     testing in repl: $prog";
  filename=repl-$(basename $prog).${host}
  echo "(load \"$prog\")" | $repl | sed "s/^> //g" > $filename.out;
  echo "writing output to $(pwd)/$filename.out";
  if [ "$?" != "0" ]; then
    exit 1;
  fi;
  lines=$(wc -l < $filename.out);
  lines_to_keep=$((lines - 2));
  head -n $lines_to_keep $filename.out > $filename.expected;
  sed -e '1,/;;;expected:/d' -e 's/^;;;//' $prog | diff - $filename.expected;
  if [ "$?" != "0" ]; then
    exit 1;
  fi;
done
