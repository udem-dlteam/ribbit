if [ "$1" = "-b" ]; then
    echo -n "[[Building...]]"
    time gsi ./rsc.scm -t js -f+ js/node -f+ v-port -l r4rs/r4rs-v-io -l r4rs/misc -l r4rs/debug -l r4rs/string -l prim-wrap -o ./bootstrap.js ./rsc.scm
    echo "[[Done]]"
    #read -r -p "Press enter to continue"
    shift
else
    echo "[[Skipping build]]"
fi

echo "[[Running...]]"
time node --trace-uncaught ./bootstrap.js -t js -f+ js/node -f+ error-msg -ps "$@"
echo "[[Done]]"
