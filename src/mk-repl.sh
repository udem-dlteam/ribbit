if [ "$1" = "full" ]; then
    lib='r4rs/full-r4rs'
    shift
else 
    lib='r4rs'
fi

gsi ./rsc.scm -t js -l "$lib" -f+ js/node "$@" -o ./repl.js ./tests/r4rs/repl/repl-test.scm
node ./repl.js
