#!/bin/bash

P=`dirname $0`

for FILE in $P/Tests/*/*.js
do
  $P/../../jsc.native $FILE $P/Tests/shell.js `dirname $FILE`/shell.js -env $P/../../data/es5-lib.es5 -full-desugar
done
