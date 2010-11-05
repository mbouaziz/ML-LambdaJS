#!/bin/bash

P=`dirname $0`
FILE=$1
F=`basename $FILE`
output=`mktemp -t ljses5.XXXX`
errOutput=`mktemp -t ljses5err.XXXX`

$P/../../../build/jsc.native $P/ljs_harness.js $FILE $P/ljs-run.js -env $P/../../../data/es5-lib.es5 -full-desugar -eval > $output 2> $errOutput

failed1=`grep Failed $output`
failed2=`grep failed $output`
failed3=`grep exception $errOutput`
failed="$failed1$failed2$failed3"

if [[ $failed != "" ]]; 
then
    #we failed this test
    echo -e "$F [\e[0;31mFAILED\e[0m]"
    exit 1;
fi
#we passed this test
if [[ $2 == "-full-output" ]];
then 
    #we want to output the passed test cases as well
    echo -e "$F [\e[0;32mPASSED\e[0m]"
fi
exit 0;
