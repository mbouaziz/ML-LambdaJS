#!/bin/bash

P=`dirname $0`
FILE=$1
F=`basename $FILE`
output=`mktemp -t jsx.$F.XXXXX`
errOutput=`mktemp -t jsx.$F.err.XXXXX`

$P/../../../../src/jsx.native $P/ljs_harness.js $FILE $P/ljs-run.js -env $P/../../../data/es5-lib.es5 -no-symb > $output 2> $errOutput

failed1=`grep Fail $output`
failed2=`grep fail $output`
failed3=`grep exception $errOutput`
failed4=`grep xeval $output`
failed="$failed1$failed2$failed3$failed4"

if [[ $failed != "" ]]; 
then
    #we failed this test
    exit 1;
fi
#we passed this test
exit 0;
