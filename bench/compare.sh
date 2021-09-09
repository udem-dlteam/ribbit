#!/bin/zsh

TIME_FMT="Total time: %E\tUser Mode (s) %U\t"

tests=$(ls *.scm)

crvm() {
    rm -rf rvm
}

rvm() {
    crvm
    pushd ../src/vm_c/
    make clean >> /dev/null
    make >> /dev/null
    cp rVM ../../bench/rvm
    popd
}

cchicken() {
    rm -rf chicken.tar.gz
    rm -rf chicken-5.2.0
}

chicken() {
    cchicken

    wget https://code.call-cc.org/releases/5.2.0/chicken-5.2.0.tar.gz -O chicken.tar.gz > /dev/null 2>&1
    tar -xvf chicken.tar.gz > /dev/null 2>&1

    pushd chicken-5.2.0
    make PLATFORM=linux > /dev/null 2>&1
    cp csi ..
    cp libchicken.so ..
    popd

    ln -s libchicken.so libchicken.so.11
}

cqscheme() {
    rm -rf qscheme
    rm -rf qscheme.tar.gz
}

qscheme() {
    cqscheme
    wget https://www.sof.ch/dan/qscheme/files/qscheme-0.5.1.tar.gz -O qscheme.tar.gz
    tar -xvf qscheme.tar.gz
    cp -r qscheme-0.5.1 qscheme

    pushd qscheme
    ./configure
    make
    popd 
}

ctinyscheme() {
    rm -rf tinyscheme
    rm -rf tinyscheme.tar.gz
    rm -rf tinyscheme-1.41
}

tinyscheme() {
    ctinyscheme
    wget https://pilotfiber.dl.sourceforge.net/project/tinyscheme/tinyscheme/tinyscheme-1.41/tinyscheme-1.41.tar.gz -O tinyscheme.tar.gz > /dev/null 2>&1
    tar -xvf tinyscheme.tar.gz > /dev/null 2>&1

    pushd tinyscheme-1.41
    make clean > /dev/null 2>&1
    make > /dev/null 2>&1
    cp scheme ../tinyscheme
    popd

}

cmitscm() {
    rm -rf fmitscm
    rm -rf mitscm
    rm -rf mitscm.zip
}

mitscm() {
    cmitscm
    wget https://groups.csail.mit.edu/mac/ftpdir/users/jaffer/scm.zip -O mitscm.zip > /dev/null 2>&1
    unzip mitscm.zip > /dev/null 2>&1
    mv scm fmitscm > /dev/null 2>&1

    pushd fmitscm
    {
        ./configure --prefix=/usr/local 
        make scmlit
        cp scmlit ../mitscm
    }> /dev/null 2>&1 
popd
}

cbitscm() {
    rm -rf bit-scheme
}

bitscm() {
    cbitscm
    git clone git@github.com:melvinzhang/bit-scheme.git bit-scheme > /dev/null 2>&1 
    pushd bit-scheme 
    sed -i 's/csi/gsi/' Makefile > /dev/null 2>&1 
    popd
}


smallspace() {
    echo
    echo
}

space() {
    smallspace
    smallspace
}

cpico() {
    rm -rf fpicobit
}

picobit() {
    cpico
    git clone git@github.com:SamuelYvon/picobit.git fpicobit > /dev/null 2>&1
    pushd fpicobit

    make clean > /dev/null 2>&1
    make > /dev/null 2>&1

    cp picobit ..
    cp picobit-vm ..

    popd 

}

cminischeme() {
    rm -rf minischeme
    rm -rf fminischeme
}

minischeme() {
    git clone git@github.com:ignorabimus/minischeme.git fminischeme
    pushd minischeme/src make > /dev/null 2>&1
    make > /dev/null 2>&1
    cp minischeme ../../minischeme
    popd
}

clean() {
    rm *.csv
    cpico
    crvm
    cchicken
    cqscheme
    ctinyscheme
    cmitscm
    cbitscm
}

run() {
    space
    echo "=============================================="
    exe="$1"
    echo "Testing $exe"
    echo "=============================================="

    if command -v hyperfine > /dev/null 2>&1; then

        benches=()
        for test in $(echo "$tests")
        do
            if [[ -f "./$exe" ]]; then
                benches=(${benches[@]} "cat $test | ./$exe")
            else
                benches=(${benches[@]} "cat $test | $exe")
            fi

        done

        hyperfine -i --export-csv "bench-$exe.csv" ${benches[@]}
    else
        for test in $tests
        do
            echo "$exe : $test"

            if [[ -f "./$exe" ]]; then
                cat "$test" | time -f "$TIME_FMT" "./$exe"
            else
                cat "$test" | time -f "$TIME_FMT" "$exe"
            fi

            smallspace
        done
    fi
}

if [[ "$1" == "--clean" ]]; then
    clean
    exit 0
fi

rvm
tinyscheme
bitscm
mitscm
picobit
minischeme

run minischeme
run bit.sh
run pico.sh
run rvm
run mitscm
run gsi
run tinyscheme
