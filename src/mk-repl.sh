if [ "$1" = "-l" ]; then
    lib="$2"
    shift
    shift
else 
    lib='r4rs'
fi

gsi -:r4rs ./rsc.scm -t py -l "$lib" "$@" -o ./repl.py ./tests/r4rs/repl/repl.scm
# node --trace-uncaught ./repl.js
python3 ./repl.py
