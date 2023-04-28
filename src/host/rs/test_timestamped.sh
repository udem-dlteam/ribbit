#!/bin/bash

timestamp="$(date +"%d_%H:%M")"

cd logs
mkdir -pv $timestamp
cd ..

BS_FILE=std.btsp.log
BS_EFILE=err.btsp.log
FILE=std.log
EFILE=err.log
F_FILE=std.fancy.log
F_EFILE=err.fancy.log

touch $FILE
touch $EFILE



echo "Checking Tests...\n"
make check 2> $EFILE | tee $FILE #> /dev/null

mv -t logs/$timestamp $FILE $EFILE
   
cd ../..

touch $BS_FILE
touch $BS_EFILE

 export HOST=rs


echo "Checking Bootstrap Tests...\n"
make check-bootstrap 2> $BS_EFILE | tee $BS_FILE > /dev/null

mv -t host/rs/logs/$timestamp $BS_FILE $BS_EFILE

touch $F_FILE
touch $F_EFILE


echo "Checking Fancy Tests...\n"
make check-fancy 2> $F_EFILE | tee $F_FILE #> /dev/null

mv -t host/rs/logs/$timestamp $F_FILE $F_EFILE

