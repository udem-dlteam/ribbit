gsi ./rsc.scm -t js -l r4rs -f+ js/node "$@" -o ./repl.js ./tests/r4rs/repl/repl-test.scm
node ./repl.js
