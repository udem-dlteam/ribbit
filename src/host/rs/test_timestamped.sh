#!/bin/bash

timestamp="$(date +"%H:%M:%S_%d-%m")"

if [ "$1" == "-b" ]
then
FILE=report_$timestamp.btsp.log
EFILE=err_warn_$timestamp.btsp.log
else
FILE=report_$timestamp.log
EFILE=err_warn_$timestamp.log
fi    

touch $FILE
touch $EFILE


if [ "$1" == "-b" ]
then
    make check-bootstrap 2> $EFILE | tee $FILE
else
    make check 2> $EFILE | tee $FILE #> /dev/null
fi


mv -t logs $FILE $EFILE
