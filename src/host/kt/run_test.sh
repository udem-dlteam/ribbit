#!/bin/sh

makedir target
kotlinc rvm.kt -d target
kotlinc run_test.kt -include-runtime -cp target/ -d target/test.jar
cd ../../..
kotlin -cp ./src/host/kt/target Run_testKt