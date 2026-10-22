#!/bin/sh

BENCH_ROOT="$(cd -- $(dirname $0)/.. && pwd -P)"

if [ "$1" == "" ]; then
  echo "Usage: $0 <benchmark-name> outfile=path [option=value ...]"
  exit 2
fi

gen_script="$BENCH_ROOT/src/$1/gen.sh"

if ! [ -f "$gen_script" ]; then
  echo "Invalid suite '$1', use one of them:"
  echo ""
  ls $BENCH_ROOT/src
  exit 2

fi

. $gen_script

shift

generate_benchmark $@

