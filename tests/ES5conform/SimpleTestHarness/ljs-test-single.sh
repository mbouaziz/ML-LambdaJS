#!/bin/bash

P=`dirname $0`
FILE=$1
F=`basename $FILE`
output=`mktemp -t jsc.$F.XXX`
failed="1"

$P/../../../jsc.native $P/ljs_harness.js $FILE $P/ljs-run.js -full-desugar -env-rc $P/../../../data/es5-lib.cache -eval 2>&1 > $output && failed=`grep -i "\(fail\|exception\)" $output`

if [[ $failed != "" ]]; 
then
    #we failed this test
    exit 1;
fi
#we passed this test
exit 0;
