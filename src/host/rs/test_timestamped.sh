#!/bin/bash

timestamp="$(date +"%H:%M_%d-%m")"


BS_FILE=report_$timestamp.btsp.log
BS_EFILE=err_warn_$timestamp.btsp.log
FILE=report_$timestamp.log
EFILE=err_warn_$timestamp.log

touch $FILE
touch $EFILE
touch $BS_FILE
touch $BS_EFILE

echo "Checking Bootstrap Tests...\n"
make check-bootstrap 2> $BS_EFILE | tee $BS_FILE > /dev/null

echo "Checking Tests...\n"
make check 2> $EFILE | tee $FILE #> /dev/null


cd ../..

touch fancy_$FILE
touch fancy_$EFILE

export HOST=rs

echo "Checking Fancy Tests...\n"
make check-fancy 2> fancy_$EFILE | tee fancy_$FILE #> /dev/null

mv -t host/rs/logs fancy_$FILE fancy_$EFILE

cd host/rs

mv -t logs $FILE $EFILE
mv -t bs_logs $BS_FILE $BS_EFILE
