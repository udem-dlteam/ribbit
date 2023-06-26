if [ "$1" = "-b" ]; then
    echo -n "[[Building...]]"
    time gsi ./rsc.scm -t js -l r4rs -l r4rs/misc -l prim-wrap -f+ js/node -f+ error-msg -o ./bootstrap.js ./rsc-boot.scm
    echo "[[Done]]"
    #read -r -p "Press enter to continue"
else
    echo "[[Skipping build]]"
fi

echo "[[Running...]]"
time node ./bootstrap.js -t js -l empty -ps -o ./test.js ./test.scm
echo "[[Done]]"
