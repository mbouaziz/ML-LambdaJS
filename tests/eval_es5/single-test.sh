#!/bin/bash

P=`dirname $0`
FILE=$1

echo $1 "..."
$P/../../jsc.native $FILE -env $P/test_env -env $P/../../data/es5-lib.es5 -full-desugar -eval
