#!/bin/zsh

tests=$(ls *.scm)

crvm() {
    rm -rf rvm
    rm -rf rvm1
    rm -rf rvm2
    rm -rf rvm3
}

rvm() {
    crvm
    pushd ../src/host/c
    make clean >> /dev/null
    make >> /dev/null
    cp rVM ../../../bench/rvm
    cp rVM1 ../../../bench/rvm1
    cp rVM2 ../../../bench/rvm2
    cp rVM3 ../../../bench/rvm3
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
    # https://people.csail.mit.edu/jaffer/scm/index.html
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
    rm -rf picobit
    rm -rf picobit-vm
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
    rm -rf minischeme.tar.gz
    rm -rf minischeme
    rm -rf fminischeme
    rm minischeme
}

minischeme() {
    wget ftp://ftp.cs.indiana.edu/pub/scheme-repository/imp/minischeme.tar.gz -O minischeme.tar.gz > /dev/null 2>&1
    tar xvf minischeme.tar.gz > /dev/null 2>&1
    mv minischeme fminischeme > /dev/null 2>&1
    pushd fminischeme > /dev/null 2>&1
    mv makefile Makefile
    make > /dev/null 2>&1
    cp miniscm ../minischeme
    popd
}

clean() {
    rm *.zip
    rm *.csv
    cpico
    crvm
    cchicken
    cqscheme
    ctinyscheme
    cminischeme
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

        hyperfine --min-runs 2 --max-runs 2 -i --export-csv "bench-$exe.csv" ${benches[@]}
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

echo "== Preparing Schemes =="
rvm
tinyscheme
bitscm
mitscm
picobit
minischeme
echo "==       READY       =="

run rvm
run rvm1
run rvm3
run minischeme
run bit.sh
run pico.sh
run rvm
run mitscm
run gsi
run tinyscheme
