#!/bin/bash

P=`dirname $0`
FILE=$1

$P/../../../jsc.native $P/ljs_harness.js $FILE $P/ljs-run.js -env $P/../../../data/es5-lib.es5 -full-desugar -pretty -eval
