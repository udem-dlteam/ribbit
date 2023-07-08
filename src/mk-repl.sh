if [ "$1" = "-l" ]; then
    lib="$2"
    shift
    shift
else 
    lib='r4rs'
fi

gsi -:r4rs ./rsc.scm -t js -l "$lib" -f+ js/node "$@" -o ./repl.js ./tests/r4rs/repl/repl-test.scm
node --trace-uncaught ./repl.js
