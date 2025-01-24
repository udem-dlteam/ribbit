#!/bin/sh
# This file execute tests for ribbit

TEMP_DIR=".tests"

if [ "$HOST_COMPILER" = "" ] && [ "$HOST_INTERPRETER" = "" ]; then
  echo "Error, either HOST_COMPILER or HOST_INTERPRETER must be defined"
  exit 1
fi

run_file () {
  file=$1
  out_file=$2
  argv=$3
  if [ "${HOST_INTERPRETER}" != "" ]; then
    ${HOST_INTERPRETER} $file $argv > $out_file;
  else
    ${HOST_COMPILER} $file.exe $file;
    ./$file.exe $argv > $out_file;
  fi;
}

get_attribute(){
  test_file=$1
  attribute=$2
  echo `sed -n -e "/;;;$attribute:/p" $test_file | sed -e "s/^;;;$attribute://" | tr '\n' ',' | sed -e 's/,$//'`
}

test_path=$1
cleanup=`get_attribute $test_path "cleanup"`
argv=`get_attribute $test_path "argv"`
input=`get_attribute $test_path "input"`

mkdir -p $TEMP_DIR

prog=$TEMP_DIR/$(basename $1)

echo "-------------------- $test_path : \c"
test_ran="0"
success="1"
for tag in $(echo $TEST_TAGS | tr ' ' '\n'); do 
  if [ "$tag" = "core" ]; then
    runs=`get_attribute $test_path "run"`
  else
    runs=`get_attribute $test_path "$tag-run"`
  fi

  for feature in "" $(echo $TEST_FEATURES | tr ',' '\n' | sed -e "s/ /,/g" ); do
    for run in $(echo $runs | tr ',' '\n' | sed -e "s/ /,/g"); do
      echo ".\c"
      run=`echo $run | sed -e "s/,/ /g"`
      feature=`echo $feature | sed -e "s/,/ /g"`
      
      # Compile test
      cmd="$RSC_COMPILER -t $HOST $run $feature -o $prog.$HOST $test_path"
      `$cmd` > $prog.err  2>&1 
      if [ "$?" != "0" ]; then
        if [ "$success" = "1" ]; then
          success=0
          echo "❌"
        fi
        echo ">>>>>> [run: '$run' features: '$feature' input: '$input' argv: '$argv']"
        echo ">>>>>> Error during compilation (see $prog.err)"
        echo ">>>>>> Compile with : '$cmd'"
        echo ">>>>>> See error below."
        cat $prog.err
        echo ""
        continue
      fi

      # Run test
      echo $input | run_file $prog.$HOST $prog.out $argv > $prog.err 2>&1 
      if [ "$?" != 0 ]; then
        if [ $success -eq 1 ]; then
          success=0
          echo "❌"
        fi
        echo ">>>>>> [run: '$run' features: '$feature' input: '$input' argv: '$argv']"
        echo ">>>>>> Error during test execution (see $prog.err)"
        echo ">>>>>> Compile with : '$cmd'"
        echo ">>>>>> See error below."
        tail -n 10 $prog.out
        cat $prog.err
        echo ""
        continue
      fi

      # Check output
      sed -e '1,/;;;expected:/d' -e 's/^;;;//' $test_path > $prog.expected;
      if [ "$POST_TEST_VALIDATION" != "" ]; then
        echo $POST_TEST_VALIDATION >> $prog.expected;
      fi
      cat $prog.expected | diff - $prog.out > $prog.diff 2>&1;

      if [ "$?" = "0" ]; then
        test_ran=1
      else
        if [ "$success" = "1" ]; then
          success=0
          echo "❌"
        fi
        echo ">>>>>> [run: '$run' features: '$feature' input: '$input' argv: '$argv']"
        echo ">>>>>> Results doesn't match expected value (see See $prog.diff)"
        echo ">>>>>> Compile with : '$cmd'"
        echo ">>>>>> See error below."
        cat $prog.diff
        echo ""
        test_err=1
      fi

      # Cleanup
      if [ "$cleanup" != "" ]; then
        sh -c "$cleanup" > $prog.err 2>&1;
        if [ "$?" != "0" ]; then
          if [ "$success" = "1" ]; then
            success=0
            echo "❌"
          fi
            echo ">>> $test_path [options: $run]"
            tail -n 10 $prog.err
            echo "Error during cleaning up : see $prog.err for more details."
        fi;
      fi;
    done
  done
done

if [ $success -eq 1 ]; then
  if [ $test_ran -eq 1 ]; then
    echo "✅"
  else
    echo "skipped"
  fi
  exit 0;
else
  exit 1;
fi

