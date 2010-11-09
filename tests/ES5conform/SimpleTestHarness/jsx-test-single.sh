#!/bin/bash

P=`dirname $0`
FILE=$1
F=`basename $FILE`
output=`mktemp -t jsx.$F.XXX`
failed="1"

$P/../../../../src/jsx.native $P/ljs_harness.js $FILE $P/ljs-run.js -env $P/../../../data/es5-lib.es5 -no-symb -fatal 2>&1 > $output && failed=`grep -i "\(fail\|exception\|xeval\)" $output`

if [[ $failed != "" ]]; 
then
    #we failed this test
    exit 1;
fi
#we passed this test
exit 0;
